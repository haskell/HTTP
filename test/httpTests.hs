{-# LANGUAGE ViewPatterns #-}
import Control.Concurrent

import Control.Applicative ((<$))
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import System.IO.Error (userError)

import qualified Httpd

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

basicExample :: Assertion
basicExample = do
  result <-
    -- sample code from Network.HTTP haddock, with URL changed
    -- Note there's also a copy of the example in the .cabal file
    simpleHTTP (getRequest (testUrl "/basic/example")) >>= fmap (take 100) . getResponseBody
  assertEqual "Receiving expected response" (take 100 haskellOrgText) result

secureGetRequest :: Assertion
secureGetRequest = do
  response <- try $ simpleHTTP (getRequest (secureTestUrl "/anything"))
  assertEqual "Threw expected exception"
              (Left (userError "https not supported"))
              (fmap show response) -- fmap show because Response isn't in Eq

basicPostRequest :: Assertion
basicPostRequest = do
  let sendBody = "body"
  response <- simpleHTTP $ postRequestWithBody (testUrl "/basic/post")
                                               "text/plain"
                                               sendBody
  code <- getResponseCode response
  assertEqual "HTTP status code" (2, 0, 0) code
  body <- getResponseBody response
  assertEqual "Receiving expected response"
              (show (Just "text/plain", Just "4", sendBody))
              body

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

browserExample :: Assertion
browserExample = do
  result <-
    -- sample code from Network.Browser haddock, with URL changed
    -- Note there's also a copy of the example in the .cabal file
    do 
      (_, rsp)
         <- Network.Browser.browse $ do
               setAllowRedirects True -- handle HTTP redirects
               request $ getRequest (testUrl "/browser/example")
      return (take 100 (rspBody rsp))
  assertEqual "Receiving expected response" (take 100 haskellOrgText) result

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

browserSecureRequest :: Assertion
browserSecureRequest = do
  res <- try $ browse $ do
    setOutHandler (const $ return ())

    request $ getRequest (secureTestUrl "/anything")

  assertEqual "Threw expected exception"
              (Left (userError "https not supported"))
              (fmap show res) -- fmap show because Response isn't in Eq

-- in case it tries to reuse the connection
browserSecureRequestAfterInsecure :: Assertion
browserSecureRequestAfterInsecure = do
  res <- try $ browse $ do
    setOutHandler (const $ return ())

    request $ getRequest (testUrl "/basic/get")
    request $ getRequest (secureTestUrl "/anything")

  assertEqual "Threw expected exception"
              (Left (userError "https not supported"))
              (fmap show res) -- fmap show because Response isn't in Eq

browserRedirectToSecure :: Assertion
browserRedirectToSecure = do
  res <- try $ browse $ do
    setOutHandler (const $ return ())
    setErrHandler fail

    request $ getRequest (testUrl "/browser/redirect/secure/301/anything")

  assertEqual "Threw expected exception"
              (Left (userError $ "Unable to handle redirect, unsupported scheme: " ++ secureTestUrl "/anything"))
              (fmap show res) -- fmap show because Response isn't in Eq

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

-- first bits of result text from haskell.org (just to give some representative text)
haskellOrgText =
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\
\<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\" dir=\"ltr\">\
\\t<head>\
\\t\t<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />\
\\t\t\t\t<meta name=\"keywords\" content=\"Haskell,Applications and libraries,Books,Foreign Function Interface,Functional programming,Hac Boston,HakkuTaikai,HaskellImplementorsWorkshop/2011,Haskell Communities and Activities Report,Haskell in education,Haskell in industry\" />"

processRequest :: Httpd.Request -> IO Httpd.Response
processRequest req = do
  case (Httpd.reqMethod req, Network.URI.uriPath (Httpd.reqURI req)) of 
    ("GET", "/basic/get") -> return $ Httpd.mkResponse 200 [] "It works."
    ("GET", "/basic/get2") -> return $ Httpd.mkResponse 200 [] "It works (2)."
    ("POST", "/basic/post") ->
        let typ = lookup "Content-Type" (Httpd.reqHeaders req)
            len = lookup "Content-Length" (Httpd.reqHeaders req)
            body = Httpd.reqBody req
        in return $ Httpd.mkResponse 200 [] (show (typ, len, body))

    ("GET", "/basic/example") ->
      return $ Httpd.mkResponse 200 [] haskellOrgText

    ("GET", "/auth/basic") ->
      case lookup "Authorization" (Httpd.reqHeaders req) of
        Just "Basic dGVzdDpwYXNzd29yZA==" -> return $ Httpd.mkResponse 200 [] "Here's the secret"
        x -> return $ Httpd.mkResponse 401 [("WWW-Authenticate", "Basic realm=\"Testing realm\"")] (show x)

    ("GET", "/auth/digest") ->
      case lookup "Authorization" (Httpd.reqHeaders req) of
        Just (hasPrefix "Digest " -> Just (splitFields -> items))
          | [("username", show "test"), ("realm", show "Digest testing realm"), ("nonce", show "87e4"),
             ("uri", show (testUrl "/auth/digest")), ("opaque", show "057d"),
             ("response", show "ace810a3cfb830489a3b48e90a02b2ae")] `isSubsetOf` items
          -> return $ Httpd.mkResponse 200 [] "Here's the digest secret"
          | [("username", show "test"), ("realm", show "Digest testing realm"), ("nonce", show "87e4"),
             ("uri", show "/auth/digest"), ("opaque", show "057d"),
             ("response", show "4845c3faf4dcb125b8dcc88b5c20bb89")] `isSubsetOf` items
          -> return $ Httpd.mkResponse 200 [] "Here's the digest secret"
        x -> return $ Httpd.mkResponse
                        401
                        [("WWW-Authenticate",
                          "Digest realm=\"Digest testing realm\", opaque=\"057d\", nonce=\"87e4\"")]
                        (show x)

    ("GET", "/browser/example") ->
      return $ Httpd.mkResponse 200 [] haskellOrgText
    ("GET", "/browser/no-cookie") ->
      case lookup "Cookie" (Httpd.reqHeaders req) of
        Nothing -> return $ Httpd.mkResponse 200 [] ""
        Just s  -> return $ Httpd.mkResponse 500 [] s
    ("GET", "/browser/one-cookie/1") ->
      return $ Httpd.mkResponse 200 [("Set-Cookie", "hello=world")] ""
    ("GET", "/browser/one-cookie/2") ->
      case lookup "Cookie" (Httpd.reqHeaders req) of
        Just "hello=world" -> return $ Httpd.mkResponse 200 [] ""
        Just s               -> return $ Httpd.mkResponse 500 [] s
        Nothing              -> return $ Httpd.mkResponse 500 [] (show $ Httpd.reqHeaders req)
    ("GET", "/browser/two-cookies/1") ->
      return $ Httpd.mkResponse 200
                              [("Set-Cookie", "hello=world")
                              ,("Set-Cookie", "goodbye=cruelworld")]
                              ""
    ("GET", "/browser/two-cookies/2") ->
      case lookup "Cookie" (Httpd.reqHeaders req) of
        -- TODO generalise the cookie parsing to allow for whitespace/ordering variations
        Just "goodbye=cruelworld; hello=world" -> return $ Httpd.mkResponse 200 [] ""
        Just s               -> return $ Httpd.mkResponse 500 [] s
        Nothing              -> return $ Httpd.mkResponse 500 [] (show $ Httpd.reqHeaders req)
    ("GET", hasPrefix "/browser/redirect/relative/" -> Just (break (=='/') -> (maybeRead -> Just n, rest))) ->
      return $ Httpd.mkResponse n [("Location", rest)] ""
    ("GET", hasPrefix "/browser/redirect/absolute/" -> Just (break (=='/') -> (maybeRead -> Just n, rest))) ->
      return $ Httpd.mkResponse n [("Location", testUrl rest)] ""
    ("GET", hasPrefix "/browser/redirect/secure/" -> Just (break (=='/') -> (maybeRead -> Just n, rest))) ->
      return $ Httpd.mkResponse n [("Location", secureTestUrl rest)] ""
    _                     -> return $ Httpd.mkResponse 500 [] "Unknown request"

altProcessRequest :: Httpd.Request -> IO Httpd.Response
altProcessRequest req = do
  case (Httpd.reqMethod req, Network.URI.uriPath (Httpd.reqURI req)) of 
    ("GET", "/basic/get") -> return $ Httpd.mkResponse 200 [] "This is the alternate server."
    ("GET", "/basic/get2") -> return $ Httpd.mkResponse 200 [] "This is the alternate server (2)."
    _                     -> return $ Httpd.mkResponse 500 [] "Unknown request"

getResponseCode :: Result (Response a) -> IO ResponseCode
getResponseCode (Left err) = fail (show err)
getResponseCode (Right r)  = return (rspCode r)

maybeTestGroup True name xs = testGroup name xs
maybeTestGroup False name _ = testGroup name []

tests port80Server =
  [ testGroup "Basic tests"
    [ testCase "Basic GET request" basicGetRequest
    , testCase "Network.HTTP example code" basicExample
    , testCase "Secure GET request" secureGetRequest
    , testCase "Basic POST request" basicPostRequest
    , testCase "Basic Auth failure" basicAuthFailure
    , testCase "Basic Auth success" basicAuthSuccess
    ]
  , testGroup "Browser tests"
    [ testGroup "Basic"
      [
        -- github issue 14
        -- testCase "Two requests" browserTwoRequests
        testCase "Network.Browser example code" browserExample
      ]
    , testGroup "Secure"
      [
        testCase "Secure request" browserSecureRequest
      , testCase "After insecure" browserSecureRequestAfterInsecure
      , testCase "Redirection" browserRedirectToSecure
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

secureRoot :: Int -> String
secureRoot 443 = "https://localhost"
secureRoot n = "https://localhost:" ++ show n

testUrl :: String -> String
testUrl p = urlRoot portNum ++ p

altTestUrl :: String -> String
altTestUrl p = urlRoot altPortNum ++ p

secureTestUrl :: String -> String
secureTestUrl p = secureRoot portNum ++ p

main :: IO ()
main = do
  args <- getArgs
  let server = Httpd.shed
  case args of
     ["server"] -> do -- run only the harness servers for diagnostic/debug purposes
                      -- halt on any keypress
        _ <- forkIO $ server portNum processRequest
        _ <- forkIO $ server altPortNum altProcessRequest
        _ <- getChar
        return ()
     ("--withport80":args) -> do
        _ <- forkIO $ server portNum processRequest
        _ <- forkIO $ server altPortNum altProcessRequest
        _ <- threadDelay 1000000 -- Give the server time to start :-(
        defaultMainWithArgs (tests True) args
     args -> do -- run the test harness as normal
        _ <- forkIO $ server portNum processRequest
        _ <- threadDelay 1000000 -- Give the server time to start :-(
        defaultMainWithArgs (tests False) args

