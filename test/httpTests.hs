{-# LANGUAGE ViewPatterns #-}
import Control.Concurrent

import Control.Applicative ((<$))
import Control.Concurrent (threadDelay)
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

import qualified Network.Shed.Httpd as Httpd

import Network.Browser
import Network.HTTP
import Network.HTTP.Auth
import Network.HTTP.Headers
import Network.Stream (Result)
import Network.URI (uriPath, parseURI)

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

basicPostRequest :: Assertion
basicPostRequest = do
  let sendBody = "body"
  response <- simpleHTTP (postRequest (testUrl "/basic/post") sendBody)
  code <- getResponseCode response
  assertEqual "HTTP status code" (2, 0, 0) code
  body <- getResponseBody response
  assertEqual "Receiving expected response" sendBody body

basicAuthFailure :: Assertion
basicAuthFailure = do
  response <- simpleHTTP (getRequest (testUrl "/auth/basic"))
  code <- getResponseCode response
  body <- getResponseBody response
  assertEqual "HTTP status code" ((4, 0, 1), "Nothing") (code, body)

credentialsBasic = AuthBasic "Testing realm" "test" "password"
                             (fromJust . parseURI . testUrl $ "/auth/basic")

basicAuthSuccess :: Assertion
basicAuthSuccess = do
  let req = getRequest (testUrl "/auth/basic")
  let authString = withAuthority credentialsBasic req
  let reqWithAuth = req { rqHeaders = mkHeader HdrAuthorization authString:rqHeaders req }
  response <- simpleHTTP reqWithAuth
  code <- getResponseCode response
  body <- getResponseBody response
  assertEqual "Receiving expected response" ((2, 0, 0), "Here's the secret") (code, body)

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


browserFollowsRedirect :: Int -> Assertion
browserFollowsRedirect n = do
  (_, response) <- browse $ do
    setOutHandler (const $ return ())
    setMaxPoolSize (Just 0) -- TODO remove this: workaround for github issue 14
    request $ getRequest (testUrl "/browser/redirect/relative/" ++ show n ++ "/basic/get")
  assertEqual "Receiving expected response from server"
              ((2, 0, 0), "It works.")
              (rspCode response, rspBody response)

browserReturnsRedirect :: Int -> Assertion
browserReturnsRedirect n = do
  (_, response) <- browse $ do
    setOutHandler (const $ return ())
    setMaxPoolSize (Just 0) -- TODO remove this: workaround for github issue 14
    request $ getRequest (testUrl "/browser/redirect/relative/" ++ show n ++ "/basic/get")
  assertEqual "Receiving expected response from server"
              ((n `div` 100, n `mod` 100 `div` 10, n `mod` 10), "")
              (rspCode response, rspBody response)

authGenBasic _ "Testing realm" = return $ Just ("test", "password")
authGenBasic _ realm = fail $ "Unexpected realm " ++ realm

browserBasicAuth :: Assertion
browserBasicAuth = do
  (_, response) <- browse $ do
    setOutHandler (const $ return ())
    setMaxPoolSize (Just 0) -- TODO remove this: workaround for github issue 14

    setAuthorityGen authGenBasic

    request $ getRequest (testUrl "/auth/basic")

  assertEqual "Receiving expected response from server"
              ((2, 0, 0), "Here's the secret")
              (rspCode response, rspBody response)

authGenDigest _ "Digest testing realm" = return $ Just ("test", "digestpassword")
authGenDigest _ realm = fail $ "Unexpected digest realm " ++ realm

browserDigestAuth :: Assertion
browserDigestAuth = do
  (_, response) <- browse $ do
    setOutHandler (const $ return ())
    setMaxPoolSize (Just 0) -- TODO remove this: workaround for github issue 14

    setAuthorityGen authGenDigest

    request $ getRequest (testUrl "/auth/digest")

  assertEqual "Receiving expected response from server"
              ((2, 0, 0), "Here's the digest secret")
              (rspCode response, rspBody response)



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

hasPrefix :: String -> String -> Maybe String
hasPrefix [] ys = Just ys
hasPrefix (x:xs) (y:ys) | x == y = hasPrefix xs ys
hasPrefix _ _ = Nothing

maybeRead :: Read a => String -> Maybe a
maybeRead s =
   case reads s of
     [(v, "")] -> Just v
     _ -> Nothing

splitFields = map (toPair '=' . trim isSpace) . splitOn ","

toPair c str = case break (==c) str of
                 (left, _:right) -> (left, right)
                 _ -> error $ "No " ++ show c ++ " in " ++ str
trim f = dropWhile f . reverse . dropWhile f . reverse

isSubsetOf xs ys = all (`elem` ys) xs

processRequest :: Httpd.Request -> IO Httpd.Response
processRequest req = do
  case (Httpd.reqMethod req, Network.URI.uriPath (Httpd.reqURI req)) of 
    ("GET", "/basic/get") -> return $ Httpd.Response 200 [] "It works."
    ("GET", "/basic/get2") -> return $ Httpd.Response 200 [] "It works (2)."
    ("POST", "/basic/post") -> return $ Httpd.Response 200 [] (Httpd.reqBody req)

    ("GET", "/auth/basic") ->
      case lookup "Authorization" (Httpd.reqHeaders req) of
        Just "Basic dGVzdDpwYXNzd29yZA==\r" -> return $ Httpd.Response 200 [] "Here's the secret"
        x -> return $ Httpd.Response 401 [("WWW-Authenticate", "Basic realm=\"Testing realm\"")] (show x)

    ("GET", "/auth/digest") ->
      case lookup "Authorization" (Httpd.reqHeaders req) of
        Just (hasPrefix "Digest " -> Just (splitFields -> items))
          | [("username", show "test"), ("realm", show "Digest testing realm"), ("nonce", show "87e4"),
             ("uri", show (testUrl "/auth/digest")), ("opaque", show "057d"),
             ("response", show "ace810a3cfb830489a3b48e90a02b2ae")] `isSubsetOf` items
          -> return $ Httpd.Response 200 [] "Here's the digest secret"
          | [("username", show "test"), ("realm", show "Digest testing realm"), ("nonce", show "87e4"),
             ("uri", show "/auth/digest"), ("opaque", show "057d"),
             ("response", show "4845c3faf4dcb125b8dcc88b5c20bb89")] `isSubsetOf` items
          -> return $ Httpd.Response 200 [] "Here's the digest secret"
        x -> return $ Httpd.Response
                        401
                        [("WWW-Authenticate",
                          "Digest realm=\"Digest testing realm\", opaque=\"057d\", nonce=\"87e4\"")]
                        (show x)

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
    ("GET", hasPrefix "/browser/redirect/relative/" -> Just (break (=='/') -> (maybeRead -> Just n, rest))) ->
      return $ Httpd.Response n [("Location", rest)] ""
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
    , testCase "Basic POST request" basicPostRequest
    , testCase "Basic Auth failure" basicAuthFailure
    , testCase "Basic Auth success" basicAuthSuccess
    ]
  , testGroup "Browser tests"
    [ testGroup "Basic"
      [
        -- github issue 14
        -- testCase "Two requests" browserTwoRequests
      ]
    , testGroup "Cookies"
      [ testCase "No cookie header" browserNoCookie
      , testCase "One cookie" browserOneCookie
      , testCase "Two cookies" browserTwoCookies
      ]
    , testGroup "Redirection"
      [ -- See http://en.wikipedia.org/wiki/List_of_HTTP_status_codes#3xx_Redirection
        -- 300 Multiple Choices: client has to handle this
        testCase "300" (browserReturnsRedirect 300)
        -- 301 Moved Permanently: should follow
      , testCase "301" (browserFollowsRedirect 301)
        -- 302 Found: should follow
      , testCase "302" (browserFollowsRedirect 302)
        -- 303 See Other: should follow (directly for GETs)
      , testCase "303" (browserFollowsRedirect 303)
        -- 304 Not Modified: maybe Browser could do something intelligent based on
        -- being given locally cached content and sending If-Modified-Since, but it
        -- doesn't at the moment
      , testCase "304" (browserReturnsRedirect 304)
      -- 305 Use Proxy: test harness doesn't have a proxy (yet)
      -- 306 Switch Proxy: obsolete
      -- 307 Temporary Redirect: should follow
      , testCase "307" (browserFollowsRedirect 307)
      -- 308 Resume Incomplete: no support for Resumable HTTP so client has to handle this
      , testCase "308" (browserReturnsRedirect 308)
      ]
    , testGroup "Authentication"
      [ testCase "Basic" browserBasicAuth
      , testCase "Digest" browserDigestAuth
      ]
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

