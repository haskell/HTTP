-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP
-- Copyright   :  (c) Warrick Gray 2002, Bjorn Bringert 2003-2005, 2007 Robin Bate Boerop
-- License     :  BSD
-- 
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- An easy HTTP interface enjoy.
--
-- * Changes by Robin Bate Boerop <robin@bateboerop.name>:
--      - Made dependencies explicit in import statements.
--      - Removed false dependencies in import statements.
--      - Added missing type signatures.
--      - Moved Header-related code to Network.HTTP.Headers module.
--
-- * Changes by Simon Foster:
--      - Split module up into to sepearate Network.[Stream,TCP,HTTP] modules
--      - Created functions receiveHTTP and responseHTTP to allow server side interactions
--        (although 100-continue is unsupported and I haven't checked for standard compliancy).
--      - Pulled the transfer functions from sendHTTP to global scope to allow access by
--        above functions.
--
-- * Changes by Graham Klyne:
--      - export httpVersion
--      - use new URI module (similar to old, but uses revised URI datatype)
--
-- * Changes by Bjorn Bringert:
--
--      - handle URIs with a port number
--      - added debugging toggle
--      - disabled 100-continue transfers to get HTTP\/1.0 compatibility
--      - change 'ioError' to 'throw'
--      - Added simpleHTTP_, which takes a stream argument.
--
-- * Changes from 0.1
--      - change 'openHTTP' to 'openTCP', removed 'closeTCP' - use 'close' from 'Stream' class.
--      - added use of inet_addr to openHTTP, allowing use of IP "dot" notation addresses.
--      - reworking of the use of Stream, including alterations to make 'sendHTTP' generic
--        and the addition of a debugging stream.
--      - simplified error handling.
-- 
-- * TODO
--     - request pipelining
--     - https upgrade (includes full TLS, i.e. SSL, implementation)
--         - use of Stream classes will pay off
--         - consider C implementation of encryption\/decryption
--     - comm timeouts
--     - MIME & entity stuff (happening in separate module)
--     - support \"*\" uri-request-string for OPTIONS request method
-- 
-- 
-- * Header notes:
--
--     [@Host@]
--                  Required by HTTP\/1.1, if not supplied as part
--                  of a request a default Host value is extracted
--                  from the request-uri.
-- 
--     [@Connection@] 
--                  If this header is present in any request or
--                  response, and it's value is "close", then
--                  the current request\/response is the last 
--                  to be allowed on that connection.
-- 
--     [@Expect@]
--                  Should a request contain a body, an Expect
--                  header will be added to the request.  The added
--                  header has the value \"100-continue\".  After
--                  a 417 \"Expectation Failed\" response the request
--                  is attempted again without this added Expect
--                  header.
--                  
--     [@TransferEncoding,ContentLength,...@]
--                  if request is inconsistent with any of these
--                  header values then you may not receive any response
--                  or will generate an error response (probably 4xx).
--
--
-- * Response code notes
-- Some response codes induce special behaviour:
--
--   [@1xx@]   \"100 Continue\" will cause any unsent request body to be sent.
--             \"101 Upgrade\" will be returned.
--             Other 1xx responses are ignored.
-- 
--   [@417@]   The reason for this code is \"Expectation failed\", indicating
--             that the server did not like the Expect \"100-continue\" header
--             added to a request.  Receipt of 417 will induce another
--             request attempt (without Expect header), unless no Expect header
--             had been added (in which case 417 response is returned).
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


-- | Simple way to get a resource across a non-persistant connection.
-- Headers that may be altered:
--  Host        Altered only if no Host header is supplied, HTTP\/1.1
--              requires a Host header.
--  Connection  Where no allowance is made for persistant connections
--              the Connection header will be set to "close"
simpleHTTP :: Request_String -> IO (Result Response_String)
simpleHTTP r = do 
   auth <- getAuth r
   c    <- openTCPPort (host auth) (fromMaybe 80 (port auth))
   simpleHTTP_ c r

-- | Like 'simpleHTTP', but acting on an already opened stream.
simpleHTTP_ :: Stream s => s -> Request_String -> IO (Result Response_String)
simpleHTTP_ s r = do 
   auth <- getAuth r
   if not debug 
    then sendHTTP s r
    else do
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
