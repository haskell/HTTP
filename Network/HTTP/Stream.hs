-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.Stream
-- Copyright   :  (c) Warrick Gray 2002, Bjorn Bringert 2003-2005, 2007 Robin Bate Boerop
-- License     :  BSD
-- 
-- Maintainer  :  Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- Transmitting HTTP requests and responses holding @String@ in their payload bodies.
-- This is one of the implementation modules for the "Network.HTTP" interface, representing
-- request and response content as @String@s and transmitting them in non-packed form
-- (cf. "Network.HTTP.HandleStream" and its use of @ByteString@s.) over 'Stream' handles.
-- It is mostly here for backwards compatibility, representing how requests and responses
-- were transmitted up until the 4.x releases of the HTTP package.
--
-- For more detailed information about what the individual exports do, please consult
-- the documentation for "Network.HTTP". /Notice/ however that the functions here do
-- not perform any kind of normalization prior to transmission (or receipt); you are
-- responsible for doing any such yourself, or, if you prefer, just switch to using
-- "Network.HTTP" function instead.
-- 
-----------------------------------------------------------------------------
module Network.HTTP.Stream 
       ( module Network.Stream

       , simpleHTTP      -- :: Request_String -> IO (Result Response_String)
       , simpleHTTP_     -- :: Stream s => s -> Request_String -> IO (Result Response_String)
       , sendHTTP        -- :: Stream s => s -> Request_String -> IO (Result Response_String)
       , sendHTTP_notify -- :: Stream s => s -> Request_String -> IO () -> IO (Result Response_String)
       , receiveHTTP     -- :: Stream s => s -> IO (Result Request_String)
       , respondHTTP     -- :: Stream s => s -> Response_String -> IO ()
       
       ) where

-----------------------------------------------------------------
------------------ Imports --------------------------------------
-----------------------------------------------------------------

import Network.Stream
import Network.StreamDebugger (debugStream)
import Network.TCP (openTCPPort)
import Network.BufferType ( stringBufferOp )

import Network.HTTP.Base
import Network.HTTP.Headers
import Network.HTTP.Utils ( trim )

import Data.Char     (toLower)
import Data.Maybe    (fromMaybe)
import Control.Monad (when)


-- Turn on to enable HTTP traffic logging
debug :: Bool
debug = False

-- File that HTTP traffic logs go to
httpLogFile :: String
httpLogFile = "http-debug.log"

-----------------------------------------------------------------
------------------ Misc -----------------------------------------
-----------------------------------------------------------------


-- | Simple way to transmit a resource across a non-persistent connection.
simpleHTTP :: Request_String -> IO (Result Response_String)
simpleHTTP r = do 
   auth <- getAuth r
   c    <- openTCPPort (host auth) (fromMaybe 80 (port auth))
   simpleHTTP_ c r

-- | Like 'simpleHTTP', but acting on an already opened stream.
simpleHTTP_ :: Stream s => s -> Request_String -> IO (Result Response_String)
simpleHTTP_ s r
 | not debug    = sendHTTP s r
 | otherwise    = do
      s' <- debugStream httpLogFile s
      sendHTTP s' r
    -- already done by sendHTTP because of "Connection: close" header
    --; close s 

sendHTTP :: Stream s => s -> Request_String -> IO (Result Response_String)
sendHTTP conn rq = sendHTTP_notify conn rq (return ())

sendHTTP_notify :: Stream s => s -> Request_String -> IO () -> IO (Result Response_String)
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
sendMain :: Stream s => s -> Request_String -> IO () -> IO (Result Response_String)
sendMain conn rqst onSendComplete =  do 
    --let str = if null (rqBody rqst)
    --              then show rqst
    --              else show (insertHeader HdrExpect "100-continue" rqst)
   writeBlock conn (show rqst)
    -- write body immediately, don't wait for 100 CONTINUE
   writeBlock conn (rqBody rqst)
   onSendComplete
   rsp <- getResponseHead conn
   switchResponse conn True False rsp rqst
        
-- reads and parses headers
getResponseHead :: Stream s => s -> IO (Result ResponseData)
getResponseHead conn = do
   lor <- readTillEmpty1 stringBufferOp (readLine conn)
   return $ lor >>= parseResponseHead

-- Hmmm, this could go bad if we keep getting "100 Continue"
-- responses...  Except this should never happen according
-- to the RFC.
switchResponse :: Stream s
               => s
	       -> Bool {- allow retry? -}
               -> Bool {- is body sent? -}
               -> Result ResponseData
               -> Request_String
               -> IO (Result Response_String)
switchResponse _ _ _ (Left e) _ = return (Left e)
        -- retry on connreset?
        -- if we attempt to use the same socket then there is an excellent
        -- chance that the socket is not in a completely closed state.
switchResponse conn allow_retry bdy_sent (Right (cd,rn,hdrs)) rqst =
            case matchResponse (rqMethod rqst) cd of
                Continue
                    | not bdy_sent -> {- Time to send the body -}
                        do { val <- writeBlock conn (rqBody rqst)
                           ; case val of
                                Left e -> return (Left e)
                                Right _ ->
                                    do { rsp <- getResponseHead conn
                                       ; switchResponse conn allow_retry True rsp rqst
                                       }
                           }
                    | otherwise -> {- keep waiting -}
                        do { rsp <- getResponseHead conn
                           ; switchResponse conn allow_retry bdy_sent rsp rqst                           
                           }

                Retry -> {- Request with "Expect" header failed.
                                Trouble is the request contains Expects
                                other than "100-Continue" -}
                    do { writeBlock conn (show rqst ++ rqBody rqst)
                       ; rsp <- getResponseHead conn
                       ; switchResponse conn False bdy_sent rsp rqst
                       }   
                     
                Done ->
                    return (Right $ Response cd rn hdrs "")

                DieHorribly str ->
                    return $ responseParseError "sendHTTP" ("Invalid response: " ++ str)

                ExpectEntity ->
                    let tc = lookupHeader HdrTransferEncoding hdrs
                        cl = lookupHeader HdrContentLength hdrs
                    in
                    do { rslt <- case tc of
                          Nothing -> 
                              case cl of
                                  Just x  -> linearTransfer (readBlock conn) (read x :: Int)
                                  Nothing -> hopefulTransfer stringBufferOp {-null (++) []-} (readLine conn) []
                          Just x  -> 
                              case map toLower (trim x) of
                                  "chunked" -> chunkedTransfer stringBufferOp
				                               (readLine conn) (readBlock conn)
                                  _         -> uglyDeathTransfer "sendHTTP"
                       ; return $ do
		            (ftrs,bdy) <- rslt
			    return (Response cd rn (hdrs++ftrs) bdy)
                       }

-- | Receive and parse a HTTP request from the given Stream. Should be used 
--   for server side interactions.
receiveHTTP :: Stream s => s -> IO (Result Request_String)
receiveHTTP conn = getRequestHead >>= processRequest
    where
        -- reads and parses headers
        getRequestHead :: IO (Result RequestData)
        getRequestHead =
            do { lor <- readTillEmpty1 stringBufferOp (readLine conn)
               ; return $ lor >>= parseRequestHead
               }
	
        processRequest (Left e) = return $ Left e
	processRequest (Right (rm,uri,hdrs)) = 
	    do -- FIXME : Also handle 100-continue.
               let tc = lookupHeader HdrTransferEncoding hdrs
                   cl = lookupHeader HdrContentLength hdrs
	       rslt <- case tc of
                          Nothing ->
                              case cl of
                                  Just x  -> linearTransfer (readBlock conn) (read x :: Int)
                                  Nothing -> return (Right ([], "")) -- hopefulTransfer ""
                          Just x  ->
                              case map toLower (trim x) of
                                  "chunked" -> chunkedTransfer stringBufferOp
				                               (readLine conn) (readBlock conn)
                                  _         -> uglyDeathTransfer "receiveHTTP"
               
               return $ do
	          (ftrs,bdy) <- rslt
		  return (Request uri rm (hdrs++ftrs) bdy)

-- | Very simple function, send a HTTP response over the given stream. This 
--   could be improved on to use different transfer types.
respondHTTP :: Stream s => s -> Response_String -> IO ()
respondHTTP conn rsp = do writeBlock conn (show rsp)
                          -- write body immediately, don't wait for 100 CONTINUE
                          writeBlock conn (rspBody rsp)
			  return ()
