-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.HandleStream
-- Copyright   :  (c) 2008- Sigbjorn Finne
-- License     :  BSD
-- 
-- Maintainer  :  Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- A 'HandleStream'-based version of "Network.HTTP" interface.
--
-- For more detailed information about what the individual exports do, please consult
-- the documentation for "Network.HTTP". /Notice/ however that the functions here do
-- not perform any kind of normalization prior to transmission (or receipt); you are
-- responsible for doing any such yourself, or, if you prefer, just switch to using
-- "Network.HTTP" function instead.
-- 
-----------------------------------------------------------------------------
module Network.HTTP.HandleStream 
       ( simpleHTTP      -- :: Request ty -> IO (Result (Response ty))
       , simpleHTTP_     -- :: HStream ty => HandleStream ty -> Request ty -> IO (Result (Response ty))
       , sendHTTP        -- :: HStream ty => HandleStream ty -> Request ty -> IO (Result (Response ty))
       , sendHTTP_notify -- :: HStream ty => HandleStream ty -> Request ty -> IO () -> IO (Result (Response ty))
       , receiveHTTP     -- :: HStream ty => HandleStream ty -> IO (Result (Request ty))
       , respondHTTP     -- :: HStream ty => HandleStream ty -> Response ty -> IO ()
       
       , simpleHTTP_debug -- :: FilePath -> Request DebugString -> IO (Response DebugString)
       ) where

-----------------------------------------------------------------
------------------ Imports --------------------------------------
-----------------------------------------------------------------

import Network.BufferType
import Network.Stream ( fmapE, Result )
import Network.StreamDebugger ( debugByteStream )
import Network.TCP (HStream(..), HandleStream )

import Network.HTTP.Base
import Network.HTTP.Headers
import Network.HTTP.Utils ( trim, readsOne )

import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Control.Monad (when)

-----------------------------------------------------------------
------------------ Misc -----------------------------------------
-----------------------------------------------------------------

-- | @simpleHTTP@ transmits a resource across a non-persistent connection.
simpleHTTP :: HStream ty => Request ty -> IO (Result (Response ty))
simpleHTTP r = do 
  auth <- getAuth r
  c <- openStream (host auth) (fromMaybe 80 (port auth))
  simpleHTTP_ c r

-- | @simpleHTTP_debug debugFile req@ behaves like 'simpleHTTP', but logs
-- the HTTP operation via the debug file @debugFile@.
simpleHTTP_debug :: HStream ty => FilePath -> Request ty -> IO (Result (Response ty))
simpleHTTP_debug httpLogFile r = do 
  auth <- getAuth r
  c0   <- openStream (host auth) (fromMaybe 80 (port auth))
  c    <- debugByteStream httpLogFile c0
  simpleHTTP_ c r

-- | Like 'simpleHTTP', but acting on an already opened stream.
simpleHTTP_ :: HStream ty => HandleStream ty -> Request ty -> IO (Result (Response ty))
simpleHTTP_ s r = sendHTTP s r

-- | @sendHTTP hStream httpRequest@ transmits @httpRequest@ over
-- @hStream@, but does not alter the status of the connection, nor request it to be
-- closed upon receiving the response.
sendHTTP :: HStream ty => HandleStream ty -> Request ty -> IO (Result (Response ty))
sendHTTP conn rq = sendHTTP_notify conn rq (return ())

-- | @sendHTTP_notify hStream httpRequest action@ behaves like 'sendHTTP', but
-- lets you supply an IO @action@ to execute once the request has been successfully
-- transmitted over the connection. Useful when you want to set up tracing of
-- request transmission and its performance.
sendHTTP_notify :: HStream ty
                => HandleStream ty
		-> Request ty
		-> IO ()
		-> IO (Result (Response ty))
sendHTTP_notify conn rq onSendComplete = do
  rsp <- catchIO (sendMain conn rq onSendComplete)
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
sendMain :: HStream ty
         => HandleStream ty
	 -> Request ty
	 -> (IO ())
	 -> IO (Result (Response ty))
sendMain conn rqst onSendComplete = do
      --let str = if null (rqBody rqst)
      --              then show rqst
      --              else show (insertHeader HdrExpect "100-continue" rqst)
  writeBlock conn (buf_fromStr bufferOps $ show rqst)
    -- write body immediately, don't wait for 100 CONTINUE
  writeBlock conn (rqBody rqst)
  onSendComplete
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
               -> Request ty
               -> IO (Result (Response ty))
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

     DieHorribly str -> return (responseParseError "Invalid response:" str)
     ExpectEntity -> 
       fmapE (\ (ftrs,bdy) -> Right (Response cd rn (hdrs++ftrs) bdy)) $
        maybe (maybe (hopefulTransfer bo (readLine conn) [])
	             (\ x -> 
		        readsOne (linearTransfer (readBlock conn))
		                 (return$responseParseError "unrecognized content-length value" x)
				 x)
		     cl)
	      (ifChunked (chunkedTransfer bo (readLine conn) (readBlock conn))
	                 (uglyDeathTransfer "sendHTTP"))
              tc
      where
       tc = lookupHeader HdrTransferEncoding hdrs
       cl = lookupHeader HdrContentLength hdrs
       bo = bufferOps
                    
-- reads and parses headers
getResponseHead :: HStream ty => HandleStream ty -> IO (Result ResponseData)
getResponseHead conn = 
   fmapE (\es -> parseResponseHead (map (buf_toStr bufferOps) es))
         (readTillEmpty1 bufferOps (readLine conn))

-- | @receiveHTTP hStream@ reads a 'Request' from the 'HandleStream' @hStream@
receiveHTTP :: HStream bufTy => HandleStream bufTy -> IO (Result (Request bufTy))
receiveHTTP conn = getRequestHead >>= either (return . Left) processRequest
  where
    -- reads and parses headers
   getRequestHead :: IO (Result RequestData)
   getRequestHead = do
      fmapE (\es -> parseRequestHead (map (buf_toStr bufferOps) es))
            (readTillEmpty1 bufferOps (readLine conn))
	
   processRequest (rm,uri,hdrs) =
      fmapE (\ (ftrs,bdy) -> Right (Request uri rm (hdrs++ftrs) bdy)) $
	     maybe 
	      (maybe (return (Right ([], buf_empty bo))) -- hopefulTransfer ""
	             (\ x -> readsOne (linearTransfer (readBlock conn))
			              (return$responseParseError "unrecognized Content-Length value" x)
				      x)
				      
		     cl)
  	      (ifChunked (chunkedTransfer bo (readLine conn) (readBlock conn))
	                 (uglyDeathTransfer "receiveHTTP"))
              tc
    where
     -- FIXME : Also handle 100-continue.
     tc = lookupHeader HdrTransferEncoding hdrs
     cl = lookupHeader HdrContentLength hdrs
     bo = bufferOps

-- | @respondHTTP hStream httpResponse@ transmits an HTTP 'Response' over
-- the 'HandleStream' @hStream@. It could be used to implement simple web
-- server interactions, performing the dual role to 'sendHTTP'.
respondHTTP :: HStream ty => HandleStream ty -> Response ty -> IO ()
respondHTTP conn rsp = do 
  writeBlock conn (buf_fromStr bufferOps $ show rsp)
   -- write body immediately, don't wait for 100 CONTINUE
  writeBlock conn (rspBody rsp)
  return ()

------------------------------------------------------------------------------

headerName :: String -> String
headerName x = map toLower (trim x)

ifChunked :: a -> a -> String -> a
ifChunked a b s = 
  case headerName s of
    "chunked" -> a
    _ -> b

