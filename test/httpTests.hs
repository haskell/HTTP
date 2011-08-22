import Control.Concurrent

import Data.Functor

import Control.Concurrent (threadDelay)

import qualified Network.Shed.Httpd as Httpd

import Network.Browser
import Network.HTTP
import Network.Stream (Result)
import Network.URI (uriPath)

import System.Environment (getArgs)
import System.IO (getChar)

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit


basicGetRequest :: Assertion
basicGetRequest = do
  response <- simpleHTTP (getRequest (testUrl "/basic/get"))
  code <- getResponseCode response
  assertEqual "HTTP status code" (2, 0, 0) code
  body <- getResponseBody response
  assertEqual "Receiving expected response" "It works." body


-- A vanilla HTTP request using Browser shouln't send a cookie header
browserNoCookie :: Assertion
browserNoCookie = do
  (_, response) <- browse $ do
    setOutHandler (const $ return ())
    request $ getRequest (testUrl "/browser/no-cookie")
  let code = rspCode response
  assertEqual "HTTP status code" (2, 0, 0) code


-- Regression test
--  * Browser sends vanilla request to server
--  * Server sets one cookie "hello=world"
--  * Browser sends a second request
--
-- Expected: Server gets single cookie with "hello=world"
-- Actual:   Server gets 3 extra cookies, which are actually cookie attributes:
--           "$Version=0;hello=world;$Domain=localhost:8080\r"
browserOneCookie :: Assertion
browserOneCookie = do
  (_, response) <- browse $ do
    setOutHandler (const $ return ())
    -- This first requests returns a single Set-Cookie: hello=world
    _ <- request $ getRequest (testUrl "/browser/one-cookie/1")

    -- This second request should send a single Cookie: hello=world
    request $ getRequest (testUrl "/browser/one-cookie/2")
  let body = rspBody response
  assertEqual "Receiving expected response" "" body
  let code = rspCode response
  assertEqual "HTTP status code" (2, 0, 0) code

browserTwoCookies :: Assertion
browserTwoCookies = do
  (_, response) <- browse $ do
    setOutHandler (const $ return ())
    -- This first request returns two cookies
    _ <- request $ getRequest (testUrl "/browser/two-cookies/1")

    -- This second request should send them back
    request $ getRequest (testUrl "/browser/two-cookies/2")
  let body = rspBody response
  assertEqual "Receiving expected response" "" body
  let code = rspCode response
  assertEqual "HTTP status code" (2, 0, 0) code

processRequest :: Httpd.Request -> IO Httpd.Response
processRequest req = do
  case (Httpd.reqMethod req, Network.URI.uriPath (Httpd.reqURI req)) of 
    ("GET", "/basic/get") -> return $ Httpd.Response 200 [] "It works."
    ("GET", "/browser/no-cookie") ->
      case lookup "Cookie" (Httpd.reqHeaders req) of
        Nothing -> return $ Httpd.Response 200 [] ""
        Just s  -> return $ Httpd.Response 500 [] s
    ("GET", "/browser/one-cookie/1") ->
      return $ Httpd.Response 200 [("Set-Cookie", "hello=world")] ""
    ("GET", "/browser/one-cookie/2") ->
      case lookup "Cookie" (Httpd.reqHeaders req) of
        -- TODO: is it correct to expect the \r at the end?
        Just "hello=world\r" -> return $ Httpd.Response 200 [] ""
        Just s               -> return $ Httpd.Response 500 [] s
        Nothing              -> return $ Httpd.Response 500 [] (show $ Httpd.reqHeaders req)
    ("GET", "/browser/two-cookies/1") ->
      return $ Httpd.Response 200
                              [("Set-Cookie", "hello=world")
                              ,("Set-Cookie", "goodbye=cruelworld")]
                              ""
    ("GET", "/browser/two-cookies/2") ->
      case lookup "Cookie" (Httpd.reqHeaders req) of
        -- TODO: is it correct to expect the \r at the end?
        -- TODO generalise the cookie parsing to allow for whitespace/ordering variations
        Just "goodbye=cruelworld; hello=world\r" -> return $ Httpd.Response 200 [] ""
        Just s               -> return $ Httpd.Response 500 [] s
        Nothing              -> return $ Httpd.Response 500 [] (show $ Httpd.reqHeaders req)
    _                     -> return $ Httpd.Response 500 [] "Unknown request"

getResponseCode :: Result (Response a) -> IO ResponseCode
getResponseCode (Left err) = fail (show err)
getResponseCode (Right r)  = return (rspCode r)

tests =
  [ testGroup "Basic tests"
    [ testCase "Basic GET request" basicGetRequest
    ]
  , testGroup "Browser tests"
    [ testCase "No cookie header" browserNoCookie
    , testCase "One cookie" browserOneCookie
    , testCase "Two cookies" browserTwoCookies
    ]
  ]

portNum :: Int
portNum = 8080

testUrl :: String -> String
testUrl p = "http://localhost:" ++ show portNum ++ p

main :: IO ()
main = do
  args <- getArgs
  case args of
     ["server"] -> do -- run only the harness server for diagnostic/debug purposes
                      -- halt on any keypress
        _ <- forkIO (() <$ Httpd.initServer portNum processRequest)
        _ <- getChar
        return ()
     [] -> do -- run the test harness as normal
        _ <- forkIO (() <$ Httpd.initServer portNum processRequest)
        _ <- threadDelay 1000000 -- Give the server time to start :-(
        defaultMain tests
     _ -> error "Unknown arguments to test harness"

