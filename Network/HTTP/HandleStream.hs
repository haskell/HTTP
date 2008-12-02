-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.HandleStream
-- Copyright   :  (c) Warrick Gray 2002, Bjorn Bringert 2003-2005, 2007 Robin Bate Boerop, 2008 Sigbjorn Finne
-- License     :  BSD
-- 
-- Maintainer  :  Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- A HandleStream version of Network.HTTP.Stream's public offerings.
--
-----------------------------------------------------------------------------
module Network.HTTP.HandleStream 
       ( simpleHTTP     -- :: HTTPRequest ty -> IO (Result (HTTPResponse ty))
       , simpleHTTP_    -- :: HStream ty => HandleStream ty -> HTTPRequest ty -> IO (Result (HTTPResponse ty))
       , sendHTTP       -- :: HStream ty => HandleStream ty -> HTTPRequest ty -> IO (Result (HTTResponse ty))
       , receiveHTTP    -- :: HStream ty => HandleStream ty -> IO (Result (HTTPRequest ty))
       , respondHTTP    -- :: HStream ty => HandleStream ty -> HTTPResponse ty -> IO ()
       
       , simpleHTTP_debug -- :: FilePath -> HTTPRequest DebugString -> IO (HTTPResponse DebugString)
       ) where

-----------------------------------------------------------------
------------------ Imports --------------------------------------
-----------------------------------------------------------------

import Network.BufferType
import Network.Stream ( ConnError(..), bindE, Result )
import Network.StreamDebugger ( debugByteStream )
import Network.TCP (HStream(..), HandleStream )

import Network.HTTP.Base
import Network.HTTP.Headers
import Network.HTTP.Utils ( trim )

import Control.Exception as Exception (IOException)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Control.Monad (when)

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO a h = Prelude.catch a h

-----------------------------------------------------------------
------------------ Misc -----------------------------------------
-----------------------------------------------------------------

-- | Simple way to get a resource across a non-persistant connection.
-- Headers that may be altered:
--  Host        Altered only if no Host header is supplied, HTTP\/1.1
--              requires a Host header.
--  Connection  Where no allowance is made for persistant connections
--              the Connection header will be set to "close"
simpleHTTP :: HStream ty => HTTPRequest ty -> IO (Result (HTTPResponse ty))
simpleHTTP r = do 
  auth <- getAuth r
  c <- openStream (host auth) (fromMaybe 80 (port auth))
  simpleHTTP_ c r

simpleHTTP_debug :: HStream ty => FilePath -> HTTPRequest ty -> IO (Result (HTTPResponse ty))
simpleHTTP_debug httpLogFile r = do 
  auth <- getAuth r
  c0 <- openStream (host auth) (fromMaybe 80 (port auth))
  c <- debugByteStream httpLogFile c0
  simpleHTTP_ c r

-- | Like 'simpleHTTP', but acting on an already opened stream.
simpleHTTP_ :: HStream ty => HandleStream ty -> HTTPRequest ty -> IO (Result (HTTPResponse ty))
simpleHTTP_ s r = do 
  auth <- getAuth r
  let r' = normalizeRequestURI auth r 
  rsp <- sendHTTP s r'
  return rsp

sendHTTP :: HStream ty => HandleStream ty -> HTTPRequest ty -> IO (Result (HTTPResponse ty))
sendHTTP conn rq = do
  let a_rq = normalizeHostHeader rq
  rsp <- catchIO (sendMain conn a_rq)
                 (\e -> do { close conn; ioError e })
  let fn list = when (or $ map findConnClose list)
                     (close conn)
  either (\_ -> fn [rqHeaders rq])
         (\r -> fn [rqHeaders rq,rspHeaders r])
         rsp
  return rsp

-- From RFC 2616, section 8.2.3:
-- 'Because of the presence of older implementations, the protocol allows
-- ambiguous situations in which a client may send "Expect: 100-
-- continue" without receiving either a 417 (Expectation Failed) status
-- or a 100 (Continue) status. Therefore, when a client sends this
-- header field to an origin server (possibly via a proxy) from which it
-- has never seen a 100 (Continue) status, the client SHOULD NOT wait
-- for an indefinite period before sending the request body.'
--
-- Since we would wait forever, I have disabled use of 100-continue for now.
sendMain :: HStream ty => HandleStream ty -> HTTPRequest ty -> IO (Result (HTTPResponse ty))
sendMain conn rqst = do
      --let str = if null (rqBody rqst)
      --              then show rqst
      --              else show (insertHeader HdrExpect "100-continue" rqst)
  writeBlock conn (buf_fromStr bufferOps $ show rqst)
    -- write body immediately, don't wait for 100 CONTINUE
  writeBlock conn (rqBody rqst)
  rsp <- getResponseHead conn
  switchResponse conn True False rsp rqst

   -- Hmmm, this could go bad if we keep getting "100 Continue"
   -- responses...  Except this should never happen according
   -- to the RFC.

switchResponse :: HStream ty
               => HandleStream ty
	       -> Bool {- allow retry? -}
               -> Bool {- is body sent? -}
               -> Result ResponseData
               -> HTTPRequest ty
               -> IO (Result (HTTPResponse ty))
switchResponse _ _ _ (Left e) _ = return (Left e)
                -- retry on connreset?
                -- if we attempt to use the same socket then there is an excellent
                -- chance that the socket is not in a completely closed state.

switchResponse conn allow_retry bdy_sent (Right (cd,rn,hdrs)) rqst = 
   case matchResponse (rqMethod rqst) cd of
     Continue
      | not bdy_sent -> do {- Time to send the body -}
        writeBlock conn (rqBody rqst) >>= either (return . Left)
	   (\ _ -> do
              rsp <- getResponseHead conn
              switchResponse conn allow_retry True rsp rqst)
      | otherwise    -> do {- keep waiting -}
        rsp <- getResponseHead conn
        switchResponse conn allow_retry bdy_sent rsp rqst

     Retry -> do {- Request with "Expect" header failed.
                    Trouble is the request contains Expects
                    other than "100-Continue" -}
        writeBlock conn ((buf_append bufferOps)
		                     (buf_fromStr bufferOps (show rqst))
			             (rqBody rqst))
        rsp <- getResponseHead conn
        switchResponse conn False bdy_sent rsp rqst
                     
     Done -> return (Right $ Response cd rn hdrs (buf_empty bufferOps))

     DieHorribly str -> return $ Left $ ErrorParse ("Invalid response: " ++ str)

     ExpectEntity -> do
       rslt <- 
        case tc of
          Nothing -> 
            case cl of
              Nothing -> hopefulTransfer bo (readLine conn) []
              Just x  -> 
	        case reads x of
		  ((v,_):_) ->  linearTransfer (readBlock conn) v
		  _ -> return (Left (ErrorParse ("unrecognized content-length value " ++ x)))
          Just x  -> 
            case map toLower (trim x) of
              "chunked" -> chunkedTransfer bo (readLine conn) (readBlock conn)
              _         -> uglyDeathTransfer
       return $ rslt `bindE` \(ftrs,bdy) -> Right (Response cd rn (hdrs++ftrs) bdy)

      where
       tc = lookupHeader HdrTransferEncoding hdrs
       cl = lookupHeader HdrContentLength hdrs
       bo = bufferOps
                    
-- reads and parses headers
getResponseHead :: HStream ty => HandleStream ty -> IO (Result ResponseData)
getResponseHead conn = do
   lor <- readTillEmpty1 bufferOps (readLine conn)
   return $ lor `bindE` \es -> parseResponseHead (map (buf_toStr bufferOps) es)

-- | Receive and parse a HTTP request from the given Stream. Should be used 
--   for server side interactions.
receiveHTTP :: HStream bufTy => HandleStream bufTy -> IO (Result (HTTPRequest bufTy))
receiveHTTP conn = getRequestHead >>= either (return . Left) processRequest
    where
        -- reads and parses headers
        getRequestHead :: IO (Result RequestData)
        getRequestHead =
            do { lor <- readTillEmpty1 bufferOps (readLine conn)
               ; return $ lor `bindE` \es -> parseRequestHead (map (buf_toStr bufferOps) es)
               }
	
	processRequest (rm,uri,hdrs) = do
           rslt <- 
	     case tc of
               Nothing ->
                 case cl of
                   Nothing -> return (Right ([], buf_empty bo)) -- hopefulTransfer ""
                   Just x  -> 
		     case reads x of
		       ((v,_):_) -> linearTransfer (readBlock conn) v
		       _ -> return (Left (ErrorParse ("unrecognized content-length value " ++ x)))
               Just x  ->
                 case map toLower (trim x) of
                   "chunked" -> chunkedTransfer bo (readLine conn) (readBlock conn)
                   _         -> uglyDeathTransfer
               
           return $ rslt `bindE` \(ftrs,bdy) -> Right (Request uri rm (hdrs++ftrs) bdy)
	  where
	     -- FIXME : Also handle 100-continue.
            tc = lookupHeader HdrTransferEncoding hdrs
            cl = lookupHeader HdrContentLength hdrs
	    bo = bufferOps

-- | Very simple function, send a HTTP response over the given stream. This 
--   could be improved on to use different transfer types.
respondHTTP :: HStream ty => HandleStream ty -> HTTPResponse ty -> IO ()
respondHTTP conn rsp = do 
  writeBlock conn (buf_fromStr bufferOps $ show rsp)
   -- write body immediately, don't wait for 100 CONTINUE
  writeBlock conn (rspBody rsp)
  return ()
