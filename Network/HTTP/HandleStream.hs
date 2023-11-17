-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.HandleStream
-- Copyright   :  See LICENSE file
-- License     :  BSD
--
-- Maintainer  :  Ganesh Sittampalam <ganesh@earth.li>
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
-- /NOTE:/ This package only supports HTTP; it does not support HTTPS.
-- Attempts to use HTTPS result in an error.
-----------------------------------------------------------------------------
module Network.HTTP.HandleStream
       ( simpleHTTP      -- :: Request ty -> IO (Result (Response ty))
       , simpleHTTP_     -- :: HStream ty => HandleStream ty -> Request ty -> IO (Result (Response ty))
       , sendHTTP        -- :: HStream ty => HandleStream ty -> Request ty -> IO (Result (Response ty))
       , sendHTTP_notify -- :: HStream ty => HandleStream ty -> Request ty -> IO () -> IO (Result (Response ty))
       , receiveHTTP     -- :: HStream ty => HandleStream ty -> IO (Result (Request ty))
       , respondHTTP     -- :: HStream ty => HandleStream ty -> Response ty -> IO ()

       , tryE            -- :: IO a -> IO (Either IOException a)
       ) where

-----------------------------------------------------------------
------------------ Imports --------------------------------------
-----------------------------------------------------------------

import Network.BufferType
import Network.Stream ( Result, failWith, failMisc )
import Network.StreamDebugger ( debugByteStream )
import Network.TCP (HStream(..), HandleStream )

import Network.HTTP.Base
import Network.HTTP.Headers
import Network.HTTP.Utils ( trim, readsOne )

import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Control.Exception ( IOException, try ) -- Import tryE from Control.Exception
import Control.Monad (when)

-----------------------------------------------------------------
------------------ Misc -----------------------------------------
-----------------------------------------------------------------

-- | @simpleHTTP req@ transmits the 'Request' @req@ by opening a /direct/, non-persistent
-- connection to the HTTP server that @req@ is destined for, followed by transmitting
-- it and gathering up the response as a 'Result'. Prior to sending the request,
-- it is normalized (via 'normalizeRequest'). If you have to mediate the request
-- via an HTTP proxy, you will have to normalize the request yourself. Or switch to
-- using 'Network.Browser' instead.
--
-- Examples:
--
-- > simpleHTTP (getRequest "http://hackage.haskell.org/")
-- > simpleHTTP (getRequest "http://hackage.haskell.org:8012/")
--
-- If an exception occurs during the transmission, the function returns 'Left (ErrorMisc msg)'
-- where 'msg' is the exception message.
simpleHTTP :: (HStream ty) => Request ty -> IO (Result (Response ty))
simpleHTTP r = do
  auth <- getAuth r
  failHTTPS (rqURI r)
  c <- openStream (host auth) (fromMaybe 80 (port auth))
  let norm_r = normalizeRequest defaultNormalizeRequestOptions{normDoClose=True} r
  simpleHTTP_ c norm_r

-- | Identical to 'simpleHTTP', but acting on an already opened stream.
simpleHTTP_ :: HStream ty => HandleStream ty -> Request ty -> IO (Result (Response ty))
simpleHTTP_ s r = do
  let norm_r = normalizeRequest defaultNormalizeRequestOptions{normDoClose=True} r
  sendHTTP s norm_r

-- | @sendHTTP hStream httpRequest@ transmits @httpRequest@ (after normalization) over
-- @hStream@, but does not alter the status of the connection, nor request it to be
-- closed upon receiving the response.
--
-- If an exception occurs during the transmission, the function returns 'Left (ErrorMisc msg)'
-- where 'msg' is the exception message.
sendHTTP :: HStream ty => HandleStream ty -> Request ty -> IO (Result (Response ty))
sendHTTP conn rq = do
  let norm_r = normalizeRequest defaultNormalizeRequestOptions rq
  sendHTTP_notify conn norm_r (return ())

-- | @sendHTTP_notify hStream httpRequest action@ behaves like 'sendHTTP', but
-- lets you supply an IO @action@ to execute once the request has been successfully
-- transmitted over the connection. Useful when you want to set up tracing of
-- request transmission and its performance.
--
-- If an exception occurs during the transmission, the function returns 'Left (ErrorMisc msg)'
-- where 'msg' is the exception message.
sendHTTP_notify :: HStream ty
                => HandleStream ty
                -> Request ty
                -> IO ()
                -> IO (Result (Response ty))
sendHTTP_notify conn rq onSendComplete = do
  when providedClose $ (closeOnEnd conn True)
  -- Use tryE to catch any exceptions from sendMain
  tryE (sendMain conn rq onSendComplete) >>= either (return . failMisc . show) return
 where
  providedClose = findConnClose (rqHeaders rq)

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
  -- TODO review throwing away of result
  _ <- writeBlock conn (buf_fromStr bufferOps $ show rqst)
    -- write body immediately, don't wait for 100 CONTINUE
  -- TODO review throwing away of result
  _ <- writeBlock conn (rqBody rqst)
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
        -- TODO review throwing away of result
        _ <- writeBlock conn ((buf_append bufferOps)
                                     (buf_fromStr bufferOps (show rqst))
                                     (rqBody rqst))
        rsp <- getResponseHead conn
        switchResponse conn False bdy_sent rsp rqst

     Done -> do
       when (findConnClose hdrs)
            (closeOnEnd conn True)
       return (Right $ Response cd rn hdrs (buf_empty bufferOps))

     DieHorribly str -> do
       close conn
       return (responseParseError "Invalid response:" str)
     ExpectEntity -> do
       r <- fmapE (\ (ftrs,bdy) -> Right (Response cd rn (hdrs++ftrs) bdy)) $
             maybe (maybe (hopefulTransfer bo (readLine conn) [])
                       (\ x ->
                          readsOne (linearTransfer (readBlock conn))
                                   (return$responseParseError "unrecognized content-length value" x)
                                   x)
                        cl)
                   (ifChunked (chunkedTransfer bo (readLine conn) (readBlock conn))
                              (uglyDeathTransfer "sendHTTP"))
                   tc
       case r of
         Left{} -> do
           close conn
           return r
         Right (Response _ _ hs _) -> do
           when (findConnClose hs)
                (closeOnEnd conn True)
           return r

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
  -- TODO: review throwing away of result
  _ <- writeBlock conn (buf_fromStr bufferOps $ show rsp)
   -- write body immediately, don't wait for 100 CONTINUE
  -- TODO: review throwing away of result
  _ <- writeBlock conn (rspBody rsp)
  return ()

------------------------------------------------------------------------------

headerName :: String -> String
headerName x = map toLower (trim x)

ifChunked :: a -> a -> String -> a
ifChunked a b s =
  case headerName s of
    "chunked" -> a
    _ -> b

-- | A convenience function for catching exceptions from IO actions
tryE :: IO a -> IO (Either IOException a)
tryE = try
