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
-- An easy HTTP interface, toplevel module.
--
-- The 'Network.HTTP' module provides functionality for sending
-- HTTP requests and processing their responses, along with a supporting
-- cast of types and utility functions.
--
-- The actual functionality is implemented by modules in the @Network.HTTP.*@
-- namespace, allowing the user to either use the default implementation 
-- by importing @Network.HTTP@ or, for more fine-grained control, selectively
-- import the modules in @Network.HTTP.*@. To wit, more than one kind of
-- representation of the bulk data that flows across a HTTP connection is 
-- supported. Now selectable by importing @Network.HTTP.HandleStream@ (say.)
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
module Network.HTTP 
       ( module Network.HTTP.HandleStream
       , module Network.HTTP.Base
       , module Network.HTTP.Headers

{- the functionality that Network.HTTP.HandleStream and Network.HTTP.Stream
   exposes:
       , simpleHTTP     -- :: Request -> IO (Result Response)
       , simpleHTTP_    -- :: Stream s => s -> Request -> IO (Result Response)
       , sendHTTP       -- :: Stream s => s -> Request -> IO (Result Response)
       , receiveHTTP    -- :: Stream s => s -> IO (Result Request)
       , respondHTTP    -- :: Stream s => s -> Response -> IO ()
-}
       , module Network.TCP
       ) where

-----------------------------------------------------------------
------------------ Imports --------------------------------------
-----------------------------------------------------------------

import Network.HTTP.Headers
import Network.HTTP.Base
--import Network.HTTP.Stream
import Network.HTTP.HandleStream
import Network.TCP



