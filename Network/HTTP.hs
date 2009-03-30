-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP
-- Copyright   :  (c) Warrick Gray 2002, Bjorn Bringert 2003-2005, 2007 Robin Bate Boerop
-- License     :  BSD
-- 
-- Maintainer  :  Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- The 'Network.HTTP' module provides a simple interface for sending and
-- receiving content over HTTP in Haskell. Here's how to fetch a document from
-- a URL and return it as a String:
--
-- >
-- >    simpleHTTP (getRequest "http://www.haskell.org/") >>= fmap (take 100) . getResponseBody
-- >        -- fetch document and return it (as a 'String'.)
--
-- Other functions let you control the submission and transfer of HTTP
-- 'Request's and 'Response's more carefully, letting you integrate the use
-- of 'Network.HTTP' functionality into your application.
--
-- The module also exports the main types of the package, 'Request' and 'Response',
-- along with 'Header' and functions for working with these.
--
-- The actual functionality is implemented by modules in the @Network.HTTP.*@
-- namespace, letting you either use the default implementation here
-- by importing @Network.HTTP@ or, for more specific uses, selectively
-- import the modules in @Network.HTTP.*@. To wit, more than one kind of
-- representation of the bulk data that flows across a HTTP connection is 
-- supported. (see "Network.HTTP.HandleStream".)
-- 
-- /NOTE:/ The 'Request' send actions will normalize the @Request@ prior to transmission.
-- Normalization such as having the request path be in the expected form and, possibly,
-- introduce a default @Host:@ header if one isn't already present. If you do not 
-- want the requests tampered with, but sent as-is, please import and use the
-- the "Network.HTTP.HandleStream" or "Network.HTTP.Stream" modules instead. They
-- export the same functions, but leaves construction and any normalization of 
-- @Request@s to the user.
--
-----------------------------------------------------------------------------
module Network.HTTP 
       ( module Network.HTTP.Base
       , module Network.HTTP.Headers

         {- the functionality that the implementation modules, 
	    Network.HTTP.HandleStream and Network.HTTP.Stream,
	    exposes:
	 -}
       , simpleHTTP      -- :: Request -> IO (Result Response)
       , simpleHTTP_     -- :: Stream s => s -> Request -> IO (Result Response)
       , sendHTTP        -- :: Stream s => s -> Request -> IO (Result Response)
       , sendHTTP_notify -- :: Stream s => s -> Request -> IO () -> IO (Result Response)
       , receiveHTTP     -- :: Stream s => s -> IO (Result Request)
       , respondHTTP     -- :: Stream s => s -> Response -> IO ()

       , module Network.TCP
       
       , getRequest      -- :: String -> Request_String
       , postRequest     -- :: String -> Request_String
       
       , getResponseBody -- :: Requesty ty -> ty
       ) where

-----------------------------------------------------------------
------------------ Imports --------------------------------------
-----------------------------------------------------------------

import Network.HTTP.Headers
import Network.HTTP.Base
import qualified Network.HTTP.HandleStream as S
-- old implementation: import Network.HTTP.Stream
import Network.TCP
import Network.Stream ( Result )
import Network.URI    ( parseURI )

import Data.Maybe ( fromMaybe )

{-
 Note: if you switch over/back to using Network.HTTP.Stream here, you'll
 have to wrap the results from 'openStream' as Connections via 'hstreamToConnection'
 prior to delegating to the Network.HTTP.Stream functions.
-}

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

simpleHTTP :: (HStream ty) => Request ty -> IO (Result (Response ty))
simpleHTTP r = do
  auth <- getAuth r
  c <- openStream (host auth) (fromMaybe 80 (port auth))
  let norm_r = normalizeRequest defaultNormalizeRequestOptions{normDoClose=True} r
  simpleHTTP_ c norm_r
   
-- | Identical to 'simpleHTTP', but acting on an already opened stream.
simpleHTTP_ :: HStream ty => HandleStream ty -> Request ty -> IO (Result (Response ty))
simpleHTTP_ s r = do 
  let norm_r = normalizeRequest defaultNormalizeRequestOptions{normDoClose=True} r
  S.sendHTTP s norm_r

-- | @sendHTTP hStream httpRequest@ transmits @httpRequest@ (after normalization) over
-- @hStream@, but does not alter the status of the connection, nor request it to be
-- closed upon receiving the response.
sendHTTP :: HStream ty => HandleStream ty -> Request ty -> IO (Result (Response ty))
sendHTTP conn rq = do
  let norm_r = normalizeRequest defaultNormalizeRequestOptions rq 
  S.sendHTTP conn norm_r

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
  let norm_r = normalizeRequest defaultNormalizeRequestOptions rq 
  S.sendHTTP_notify conn norm_r onSendComplete

-- | @receiveHTTP hStream@ reads a 'Request' from the 'HandleStream' @hStream@
receiveHTTP :: HStream ty => HandleStream ty -> IO (Result (Request ty))
receiveHTTP conn = S.receiveHTTP conn

-- | @respondHTTP hStream httpResponse@ transmits an HTTP 'Response' over
-- the 'HandleStream' @hStream@. It could be used to implement simple web
-- server interactions, performing the dual role to 'sendHTTP'.
respondHTTP :: HStream ty => HandleStream ty -> Response ty -> IO ()
respondHTTP conn rsp = S.respondHTTP conn rsp

-- | @getRequest urlString@ is convenience constructor for basic GET 'Request's. If
-- @urlString@ isn't a syntactically valid URL, the function raises an error.
getRequest :: String -> Request_String
getRequest urlString = 
  case parseURI urlString of
    Nothing -> error ("getRequest: Not a valid URL - " ++ urlString)
    Just u  -> mkRequest GET u

-- | @postRequest urlString@ is convenience constructor for POST 'Request's. If
-- @urlString@ isn\'t a syntactically valid URL, the function raises an error.
postRequest :: String -> Request_String
postRequest urlString = 
  case parseURI urlString of
    Nothing -> error ("postRequest: Not a valid URL - " ++ urlString)
    Just u  -> mkRequest POST u

-- | @getResponseBody response@ takes the response of a HTTP requesting action and
-- tries to extricate the body of the 'Response' @response@. If the request action
-- returned an error, an IO exception is raised.
getResponseBody :: Result (Response ty) -> IO ty
getResponseBody (Left err) = fail (show err)
getResponseBody (Right r)  = return (rspBody r)

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
