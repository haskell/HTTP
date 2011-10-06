import Control.Concurrent

import Control.Applicative ((<$))

import Control.Concurrent (threadDelay)

import qualified Network.Shed.Httpd as Httpd

import Network.Browser
import Network.HTTP
import Network.Stream (Result)
import Network.URI (uriPath)

import System.Environment (getArgs)
import System.IO (getChar)

import Test.Framework (defaultMainWithArgs, testGroup)
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
    setMaxPoolSize (Just 0) -- TODO remove this: workaround for github issue 14
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
    setMaxPoolSize (Just 0) -- TODO remove this: workaround for github issue 14
    -- This first request returns two cookies
    _ <- request $ getRequest (testUrl "/browser/two-cookies/1")

    -- This second request should send them back
    request $ getRequest (testUrl "/browser/two-cookies/2")
  let body = rspBody response
  assertEqual "Receiving expected response" "" body
  let code = rspCode response
  assertEqual "HTTP status code" (2, 0, 0) code

browserAlt :: Assertion
browserAlt = do
  (response) <- browse $ do

    setOutHandler (const $ return ())

    (_, response1) <- request $ getRequest (altTestUrl "/basic/get")

    return response1

  assertEqual "Receiving expected response from alternate server"
              ((2, 0, 0), "This is the alternate server.")
              (rspCode response, rspBody response)

-- test that requests to multiple servers on the same host
-- don't get confused with each other
browserBoth :: Assertion
browserBoth = do
  (response1, response2) <- browse $ do
    setOutHandler (const $ return ())

    (_, response1) <- request $ getRequest (testUrl "/basic/get")
    (_, response2) <- request $ getRequest (altTestUrl "/basic/get")

    return (response1, response2)

  assertEqual "Receiving expected response from main server"
              ((2, 0, 0), "It works.")
              (rspCode response1, rspBody response1)

  assertEqual "Receiving expected response from alternate server"
              ((2, 0, 0), "This is the alternate server.")
              (rspCode response2, rspBody response2)

-- test that requests to multiple servers on the same host
-- don't get confused with each other
browserBothReversed :: Assertion
browserBothReversed = do
  (response1, response2) <- browse $ do
    setOutHandler (const $ return ())

    (_, response2) <- request $ getRequest (altTestUrl "/basic/get")
    (_, response1) <- request $ getRequest (testUrl "/basic/get")

    return (response1, response2)

  assertEqual "Receiving expected response from main server"
              ((2, 0, 0), "It works.")
              (rspCode response1, rspBody response1)

  assertEqual "Receiving expected response from alternate server"
              ((2, 0, 0), "This is the alternate server.")
              (rspCode response2, rspBody response2)


browserTwoRequests :: Assertion
browserTwoRequests = do
  (response1, response2) <- browse $ do
    setOutHandler (const $ return ())

    (_, response1) <- request $ getRequest (testUrl "/basic/get")
    (_, response2) <- request $ getRequest (testUrl "/basic/get2")

    return (response1, response2)

  assertEqual "Receiving expected response from main server"
              ((2, 0, 0), "It works.")
              (rspCode response1, rspBody response1)

  assertEqual "Receiving expected response from main server"
              ((2, 0, 0), "It works (2).")
              (rspCode response2, rspBody response2)


browserTwoRequestsAlt :: Assertion
browserTwoRequestsAlt = do
  (response1, response2) <- browse $ do

    setOutHandler (const $ return ())

    (_, response1) <- request $ getRequest (altTestUrl "/basic/get")
    (_, response2) <- request $ getRequest (altTestUrl "/basic/get2")

    return (response1, response2)

  assertEqual "Receiving expected response from alternate server"
              ((2, 0, 0), "This is the alternate server.")
              (rspCode response1, rspBody response1)

  assertEqual "Receiving expected response from alternate server"
              ((2, 0, 0), "This is the alternate server (2).")
              (rspCode response2, rspBody response2)

browserTwoRequestsBoth :: Assertion
browserTwoRequestsBoth = do
  (response1, response2, response3, response4) <- browse $ do
    setOutHandler (const $ return ())

    (_, response1) <- request $ getRequest (testUrl "/basic/get")
    (_, response2) <- request $ getRequest (altTestUrl "/basic/get")
    (_, response3) <- request $ getRequest (testUrl "/basic/get2")
    (_, response4) <- request $ getRequest (altTestUrl "/basic/get2")

    return (response1, response2, response3, response4)

  assertEqual "Receiving expected response from main server"
              ((2, 0, 0), "It works.")
              (rspCode response1, rspBody response1)

  assertEqual "Receiving expected response from alternate server"
              ((2, 0, 0), "This is the alternate server.")
              (rspCode response2, rspBody response2)

  assertEqual "Receiving expected response from main server"
              ((2, 0, 0), "It works (2).")
              (rspCode response3, rspBody response3)

  assertEqual "Receiving expected response from alternate server"
              ((2, 0, 0), "This is the alternate server (2).")
              (rspCode response4, rspBody response4)


processRequest :: Httpd.Request -> IO Httpd.Response
processRequest req = do
  case (Httpd.reqMethod req, Network.URI.uriPath (Httpd.reqURI req)) of 
    ("GET", "/basic/get") -> return $ Httpd.Response 200 [] "It works."
    ("GET", "/basic/get2") -> return $ Httpd.Response 200 [] "It works (2)."
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

altProcessRequest :: Httpd.Request -> IO Httpd.Response
altProcessRequest req = do
  case (Httpd.reqMethod req, Network.URI.uriPath (Httpd.reqURI req)) of 
    ("GET", "/basic/get") -> return $ Httpd.Response 200 [] "This is the alternate server."
    ("GET", "/basic/get2") -> return $ Httpd.Response 200 [] "This is the alternate server (2)."
    _                     -> return $ Httpd.Response 500 [] "Unknown request"

getResponseCode :: Result (Response a) -> IO ResponseCode
getResponseCode (Left err) = fail (show err)
getResponseCode (Right r)  = return (rspCode r)

maybeTestGroup True name xs = testGroup name xs
maybeTestGroup False name _ = testGroup name []

tests port80Server =
  [ testGroup "Basic tests"
    [ testCase "Basic GET request" basicGetRequest
    ]
  , testGroup "Browser tests"
    [ testCase "No cookie header" browserNoCookie
    , testCase "One cookie" browserOneCookie
    , testCase "Two cookies" browserTwoCookies
    -- github issue 14
    -- , testCase "Two requests" browserTwoRequests
    ]
  , maybeTestGroup port80Server "Multiple servers"
    [ testCase "Alternate server" browserAlt
    , testCase "Both servers" browserBoth
    , testCase "Both servers (reversed)" browserBothReversed
    -- github issue 14
    -- , testCase "Two requests - alternate server" browserTwoRequestsAlt
    -- , testCase "Two requests - both servers" browserTwoRequestsBoth
    ]
  ]

portNum :: Int
portNum = 5812

altPortNum :: Int
altPortNum = 80

urlRoot :: Int -> String
urlRoot 80 = "http://localhost"
urlRoot n = "http://localhost:" ++ show n

testUrl :: String -> String
testUrl p = urlRoot portNum ++ p

altTestUrl :: String -> String
altTestUrl p = urlRoot altPortNum ++ p

main :: IO ()
main = do
  args <- getArgs
  case args of
     ["server"] -> do -- run only the harness servers for diagnostic/debug purposes
                      -- halt on any keypress
        _ <- forkIO (() <$ Httpd.initServer portNum processRequest)
        _ <- forkIO (() <$ Httpd.initServer altPortNum altProcessRequest)
        _ <- getChar
        return ()
     ("--withport80":args) -> do
        _ <- forkIO (() <$ Httpd.initServer portNum processRequest)
        _ <- forkIO (() <$ Httpd.initServer altPortNum altProcessRequest)
        _ <- threadDelay 1000000 -- Give the server time to start :-(
        defaultMainWithArgs (tests True) args
     args -> do -- run the test harness as normal
        _ <- forkIO (() <$ Httpd.initServer portNum processRequest)
        _ <- threadDelay 1000000 -- Give the server time to start :-(
        defaultMainWithArgs (tests False) args

