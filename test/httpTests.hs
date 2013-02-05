{-# LANGUAGE ImplicitParams, ViewPatterns, NoMonomorphismRestriction #-}
import Control.Concurrent

import Control.Applicative ((<$))
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import qualified Data.ByteString.Lazy.Char8 as BL (pack)
import Data.Char (isSpace)
import qualified Data.Digest.Pure.MD5 as MD5 (md5)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import System.IO.Error (userError)

import qualified Httpd

import Network.Browser
import Network.HTTP
import Network.HTTP.Base
import Network.HTTP.Auth
import Network.HTTP.Headers
import Network.Stream (Result)
import Network.URI (uriPath, parseURI)

import System.Environment (getArgs)
import System.IO (getChar)

import Test.Framework (defaultMainWithArgs, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit


basicGetRequest :: (?testUrl :: ServerAddress) => Assertion
basicGetRequest = do
  response <- simpleHTTP (getRequest (?testUrl "/basic/get"))
  code <- getResponseCode response
  assertEqual "HTTP status code" (2, 0, 0) code
  body <- getResponseBody response
  assertEqual "Receiving expected response" "It works." body

basicGetRequestLBS :: (?testUrl :: ServerAddress) => Assertion
basicGetRequestLBS = do
  response <- simpleHTTP (mkRequest GET (fromJust (parseURI (?testUrl ("/basic/get")))))
  code <- getResponseCode response
  assertEqual "HTTP status code" (2, 0, 0) code
  body <- getResponseBody response
  assertEqual "Receiving expected response" (BL.pack "It works.") body

basicHeadRequest :: (?testUrl :: ServerAddress) => Assertion
basicHeadRequest = do
  response <- simpleHTTP (headRequest (?testUrl "/basic/head"))
  code <- getResponseCode response
  assertEqual "HTTP status code" (2, 0, 0) code
  body <- getResponseBody response
  -- the body should be empty, since this is a HEAD request
  assertEqual "Receiving expected response" "" body

basicExample :: (?testUrl :: ServerAddress) => Assertion
basicExample = do
  result <-
    -- sample code from Network.HTTP haddock, with URL changed
    -- Note there's also a copy of the example in the .cabal file
    simpleHTTP (getRequest (?testUrl "/basic/example")) >>= fmap (take 100) . getResponseBody
  assertEqual "Receiving expected response" (take 100 haskellOrgText) result

secureGetRequest :: (?secureTestUrl :: ServerAddress) => Assertion
secureGetRequest = do
  response <- try $ simpleHTTP (getRequest (?secureTestUrl "/anything"))
  assertEqual "Threw expected exception"
              (Left (userError "https not supported"))
              (fmap show response) -- fmap show because Response isn't in Eq

basicPostRequest :: (?testUrl :: ServerAddress) => Assertion
basicPostRequest = do
  let sendBody = "body"
  response <- simpleHTTP $ postRequestWithBody (?testUrl "/basic/post")
                                               "text/plain"
                                               sendBody
  code <- getResponseCode response
  assertEqual "HTTP status code" (2, 0, 0) code
  body <- getResponseBody response
  assertEqual "Receiving expected response"
              (show (Just "text/plain", Just "4", sendBody))
              body

basicAuthFailure :: (?testUrl :: ServerAddress) => Assertion
basicAuthFailure = do
  response <- simpleHTTP (getRequest (?testUrl "/auth/basic"))
  code <- getResponseCode response
  body <- getResponseBody response
  assertEqual "HTTP status code" ((4, 0, 1), "Nothing") (code, body)

credentialsBasic :: (?testUrl :: ServerAddress) => Authority
credentialsBasic = AuthBasic "Testing realm" "test" "password"
                             (fromJust . parseURI . ?testUrl $ "/auth/basic")

basicAuthSuccess :: (?testUrl :: ServerAddress) => Assertion
basicAuthSuccess = do
  let req = getRequest (?testUrl "/auth/basic")
  let authString = withAuthority credentialsBasic req
  let reqWithAuth = req { rqHeaders = mkHeader HdrAuthorization authString:rqHeaders req }
  response <- simpleHTTP reqWithAuth
  code <- getResponseCode response
  body <- getResponseBody response
  assertEqual "Receiving expected response" ((2, 0, 0), "Here's the secret") (code, body)

utf8URLEncode :: Assertion
utf8URLEncode = do
  assertEqual "Normal URL" (urlEncode "what-a_mess.com") "what-a_mess.com"
  assertEqual "Chinese URL" (urlEncode "好") "%E5%A5%BD"
  assertEqual "Russian URL" (urlEncode "ололо") "%D0%BE%D0%BB%D0%BE%D0%BB%D0%BE"

utf8URLDecode :: Assertion
utf8URLDecode = do
  assertEqual "Normal URL" (urlDecode "what-a_mess.com") "what-a_mess.com"
  assertEqual "Mixed URL" (urlDecode "UTFin进入-wow") "UTFin进入-wow"
  assertEqual "Chinese URL" (urlDecode "%E5%A5%BD") "好"
  assertEqual "Russian URL" (urlDecode "%D0%BE%D0%BB%D0%BE%D0%BB%D0%BE") "ололо"

browserExample :: (?testUrl :: ServerAddress) => Assertion
browserExample = do
  result <-
    -- sample code from Network.Browser haddock, with URL changed
    -- Note there's also a copy of the example in the .cabal file
    do 
      (_, rsp)
         <- Network.Browser.browse $ do
               setAllowRedirects True -- handle HTTP redirects
               request $ getRequest (?testUrl "/browser/example")
      return (take 100 (rspBody rsp))
  assertEqual "Receiving expected response" (take 100 haskellOrgText) result

-- A vanilla HTTP request using Browser shouln't send a cookie header
browserNoCookie :: (?testUrl :: ServerAddress) => Assertion
browserNoCookie = do
  (_, response) <- browse $ do
    setOutHandler (const $ return ())
    request $ getRequest (?testUrl "/browser/no-cookie")
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
browserOneCookie :: (?testUrl :: ServerAddress) => Assertion
browserOneCookie = do
  (_, response) <- browse $ do
    setOutHandler (const $ return ())
    setMaxPoolSize (Just 0) -- TODO remove this: workaround for github issue 14
    -- This first requests returns a single Set-Cookie: hello=world
    _ <- request $ getRequest (?testUrl "/browser/one-cookie/1")

    -- This second request should send a single Cookie: hello=world
    request $ getRequest (?testUrl "/browser/one-cookie/2")
  let body = rspBody response
  assertEqual "Receiving expected response" "" body
  let code = rspCode response
  assertEqual "HTTP status code" (2, 0, 0) code

browserTwoCookies :: (?testUrl :: ServerAddress) => Assertion
browserTwoCookies = do
  (_, response) <- browse $ do
    setOutHandler (const $ return ())
    setMaxPoolSize (Just 0) -- TODO remove this: workaround for github issue 14
    -- This first request returns two cookies
    _ <- request $ getRequest (?testUrl "/browser/two-cookies/1")

    -- This second request should send them back
    request $ getRequest (?testUrl "/browser/two-cookies/2")
  let body = rspBody response
  assertEqual "Receiving expected response" "" body
  let code = rspCode response
  assertEqual "HTTP status code" (2, 0, 0) code


browserFollowsRedirect :: (?testUrl :: ServerAddress) => Int -> Assertion
browserFollowsRedirect n = do
  (_, response) <- browse $ do
    setOutHandler (const $ return ())
    setMaxPoolSize (Just 0) -- TODO remove this: workaround for github issue 14
    request $ getRequest (?testUrl "/browser/redirect/relative/" ++ show n ++ "/basic/get")
  assertEqual "Receiving expected response from server"
              ((2, 0, 0), "It works.")
              (rspCode response, rspBody response)

browserReturnsRedirect :: (?testUrl :: ServerAddress) => Int -> Assertion
browserReturnsRedirect n = do
  (_, response) <- browse $ do
    setOutHandler (const $ return ())
    setMaxPoolSize (Just 0) -- TODO remove this: workaround for github issue 14
    request $ getRequest (?testUrl "/browser/redirect/relative/" ++ show n ++ "/basic/get")
  assertEqual "Receiving expected response from server"
              ((n `div` 100, n `mod` 100 `div` 10, n `mod` 10), "")
              (rspCode response, rspBody response)

authGenBasic _ "Testing realm" = return $ Just ("test", "password")
authGenBasic _ realm = fail $ "Unexpected realm " ++ realm

browserBasicAuth :: (?testUrl :: ServerAddress) => Assertion
browserBasicAuth = do
  (_, response) <- browse $ do
    setOutHandler (const $ return ())
    setMaxPoolSize (Just 0) -- TODO remove this: workaround for github issue 14

    setAuthorityGen authGenBasic

    request $ getRequest (?testUrl "/auth/basic")

  assertEqual "Receiving expected response from server"
              ((2, 0, 0), "Here's the secret")
              (rspCode response, rspBody response)

authGenDigest _ "Digest testing realm" = return $ Just ("test", "digestpassword")
authGenDigest _ realm = fail $ "Unexpected digest realm " ++ realm

browserDigestAuth :: (?testUrl :: ServerAddress) => Assertion
browserDigestAuth = do
  (_, response) <- browse $ do
    setOutHandler (const $ return ())
    setMaxPoolSize (Just 0) -- TODO remove this: workaround for github issue 14

    setAuthorityGen authGenDigest

    request $ getRequest (?testUrl "/auth/digest")

  assertEqual "Receiving expected response from server"
              ((2, 0, 0), "Here's the digest secret")
              (rspCode response, rspBody response)



browserAlt :: (?altTestUrl :: ServerAddress) => Assertion
browserAlt = do
  (response) <- browse $ do

    setOutHandler (const $ return ())

    (_, response1) <- request $ getRequest (?altTestUrl "/basic/get")

    return response1

  assertEqual "Receiving expected response from alternate server"
              ((2, 0, 0), "This is the alternate server.")
              (rspCode response, rspBody response)

-- test that requests to multiple servers on the same host
-- don't get confused with each other
browserBoth :: (?testUrl :: ServerAddress, ?altTestUrl :: ServerAddress) => Assertion
browserBoth = do
  (response1, response2) <- browse $ do
    setOutHandler (const $ return ())

    (_, response1) <- request $ getRequest (?testUrl "/basic/get")
    (_, response2) <- request $ getRequest (?altTestUrl "/basic/get")

    return (response1, response2)

  assertEqual "Receiving expected response from main server"
              ((2, 0, 0), "It works.")
              (rspCode response1, rspBody response1)

  assertEqual "Receiving expected response from alternate server"
              ((2, 0, 0), "This is the alternate server.")
              (rspCode response2, rspBody response2)

-- test that requests to multiple servers on the same host
-- don't get confused with each other
browserBothReversed :: (?testUrl :: ServerAddress, ?altTestUrl :: ServerAddress) => Assertion
browserBothReversed = do
  (response1, response2) <- browse $ do
    setOutHandler (const $ return ())

    (_, response2) <- request $ getRequest (?altTestUrl "/basic/get")
    (_, response1) <- request $ getRequest (?testUrl "/basic/get")

    return (response1, response2)

  assertEqual "Receiving expected response from main server"
              ((2, 0, 0), "It works.")
              (rspCode response1, rspBody response1)

  assertEqual "Receiving expected response from alternate server"
              ((2, 0, 0), "This is the alternate server.")
              (rspCode response2, rspBody response2)

browserSecureRequest :: (?secureTestUrl :: ServerAddress) => Assertion
browserSecureRequest = do
  res <- try $ browse $ do
    setOutHandler (const $ return ())

    request $ getRequest (?secureTestUrl "/anything")

  assertEqual "Threw expected exception"
              (Left (userError "https not supported"))
              (fmap show res) -- fmap show because Response isn't in Eq

-- in case it tries to reuse the connection
browserSecureRequestAfterInsecure :: (?testUrl :: ServerAddress, ?secureTestUrl :: ServerAddress) => Assertion
browserSecureRequestAfterInsecure = do
  res <- try $ browse $ do
    setOutHandler (const $ return ())

    request $ getRequest (?testUrl "/basic/get")
    request $ getRequest (?secureTestUrl "/anything")

  assertEqual "Threw expected exception"
              (Left (userError "https not supported"))
              (fmap show res) -- fmap show because Response isn't in Eq

browserRedirectToSecure :: (?testUrl :: ServerAddress, ?secureTestUrl :: ServerAddress) => Assertion
browserRedirectToSecure = do
  res <- try $ browse $ do
    setOutHandler (const $ return ())
    setErrHandler fail

    request $ getRequest (?testUrl "/browser/redirect/secure/301/anything")

  assertEqual "Threw expected exception"
              (Left (userError $ "Unable to handle redirect, unsupported scheme: " ++ ?secureTestUrl "/anything"))
              (fmap show res) -- fmap show because Response isn't in Eq

browserTwoRequests :: (?testUrl :: ServerAddress) => Assertion
browserTwoRequests = do
  (response1, response2) <- browse $ do
    setOutHandler (const $ return ())

    (_, response1) <- request $ getRequest (?testUrl "/basic/get")
    (_, response2) <- request $ getRequest (?testUrl "/basic/get2")

    return (response1, response2)

  assertEqual "Receiving expected response from main server"
              ((2, 0, 0), "It works.")
              (rspCode response1, rspBody response1)

  assertEqual "Receiving expected response from main server"
              ((2, 0, 0), "It works (2).")
              (rspCode response2, rspBody response2)


browserTwoRequestsAlt :: (?altTestUrl :: ServerAddress) => Assertion
browserTwoRequestsAlt = do
  (response1, response2) <- browse $ do

    setOutHandler (const $ return ())

    (_, response1) <- request $ getRequest (?altTestUrl "/basic/get")
    (_, response2) <- request $ getRequest (?altTestUrl "/basic/get2")

    return (response1, response2)

  assertEqual "Receiving expected response from alternate server"
              ((2, 0, 0), "This is the alternate server.")
              (rspCode response1, rspBody response1)

  assertEqual "Receiving expected response from alternate server"
              ((2, 0, 0), "This is the alternate server (2).")
              (rspCode response2, rspBody response2)

browserTwoRequestsBoth :: (?testUrl :: ServerAddress, ?altTestUrl :: ServerAddress) => Assertion
browserTwoRequestsBoth = do
  (response1, response2, response3, response4) <- browse $ do
    setOutHandler (const $ return ())

    (_, response1) <- request $ getRequest (?testUrl "/basic/get")
    (_, response2) <- request $ getRequest (?altTestUrl "/basic/get")
    (_, response3) <- request $ getRequest (?testUrl "/basic/get2")
    (_, response4) <- request $ getRequest (?altTestUrl "/basic/get2")

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

digestMatch
  username realm password
  nonce opaque
  method relativeURI makeAbsolute
  headers
  =
  common `isSubsetOf` headers && (relative `isSubsetOf` headers || absolute `isSubsetOf` headers)
 where
   common = [("username", show username), ("realm", show realm), ("nonce", show nonce),
             ("opaque", show opaque)]
   md5 = show . MD5.md5 . BL.pack
   ha1 = md5 (username++":"++realm++":"++password)
   ha2 uri = md5 (method++":"++uri)
   response uri = md5 (ha1 ++ ":" ++ nonce ++ ":" ++ ha2 uri)
   mkUncommon uri hash = [("uri", show uri), ("response", show hash)]
   relative = mkUncommon relativeURI (response relativeURI)
   absoluteURI = makeAbsolute relativeURI
   absolute = mkUncommon absoluteURI (response absoluteURI)

processRequest :: (?testUrl :: ServerAddress, ?secureTestUrl :: ServerAddress)
               => Httpd.Request
               -> IO Httpd.Response
processRequest req = do
  case (Httpd.reqMethod req, Network.URI.uriPath (Httpd.reqURI req)) of 
    ("GET", "/basic/get") -> return $ Httpd.mkResponse 200 [] "It works."
    ("GET", "/basic/get2") -> return $ Httpd.mkResponse 200 [] "It works (2)."
    ("GET", "/basic/head") -> return $ Httpd.mkResponse 200 [] "Body for /basic/head."
    ("HEAD", "/basic/head") -> return $ Httpd.mkResponse 200 [] "Body for /basic/head."
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
          | digestMatch "test" "Digest testing realm" "digestpassword"
                        "87e4" "057d"
                        "GET" "/auth/digest" ?testUrl
                        items
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
      return $ Httpd.mkResponse n [("Location", ?testUrl rest)] ""
    ("GET", hasPrefix "/browser/redirect/secure/" -> Just (break (=='/') -> (maybeRead -> Just n, rest))) ->
      return $ Httpd.mkResponse n [("Location", ?secureTestUrl rest)] ""
    _                     -> return $ Httpd.mkResponse 500 [] "Unknown request"

altProcessRequest :: Httpd.Request -> IO Httpd.Response
altProcessRequest req = do
  case (Httpd.reqMethod req, Network.URI.uriPath (Httpd.reqURI req)) of 
    ("GET", "/basic/get") -> return $ Httpd.mkResponse 200 [] "This is the alternate server."
    ("GET", "/basic/get2") -> return $ Httpd.mkResponse 200 [] "This is the alternate server (2)."
    _                     -> return $ Httpd.mkResponse 500 [] "Unknown request"

maybeTestGroup True name xs = testGroup name xs
maybeTestGroup False name _ = testGroup name []

basicTests =
    testGroup "Basic tests"
    [ testCase "Basic GET request" basicGetRequest
    , testCase "Basic GET request (lazy bytestring)" basicGetRequestLBS
    , testCase "Network.HTTP example code" basicExample
    , testCase "Secure GET request" secureGetRequest
    , testCase "Basic POST request" basicPostRequest
    , testCase "Basic HEAD request" basicHeadRequest
    , testCase "Basic Auth failure" basicAuthFailure
    , testCase "Basic Auth success" basicAuthSuccess
    , testCase "UTF-8 urlEncode" utf8URLEncode
    , testCase "UTF-8 urlDecode" utf8URLDecode
    ]

browserTests =
    testGroup "Browser tests"
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

port80Tests =
    testGroup "Multiple servers"
    [ testCase "Alternate server" browserAlt
    , testCase "Both servers" browserBoth
    , testCase "Both servers (reversed)" browserBothReversed
    -- github issue 14
    -- , testCase "Two requests - alternate server" browserTwoRequestsAlt
    -- , testCase "Two requests - both servers" browserTwoRequestsBoth
    ]

urlRoot :: Int -> String
urlRoot 80 = "http://localhost"
urlRoot n = "http://localhost:" ++ show n

secureRoot :: Int -> String
secureRoot 443 = "https://localhost"
secureRoot n = "https://localhost:" ++ show n

type ServerAddress = String -> String

httpAddress, httpsAddress :: Int -> ServerAddress
httpAddress port p = urlRoot port ++ p
httpsAddress port p = secureRoot port ++ p

main :: IO ()
main = do
  args <- getArgs

  let servers = [("httpd-shed", Httpd.shed), ("warp", Httpd.warp)]
      basePortNum, altPortNum :: Int
      basePortNum = 5812
      altPortNum = 80
      numberedServers = zip [basePortNum..] servers

  let setupNormalTests = do
      flip mapM numberedServers $ \(portNum, (serverName, server)) -> do
         let ?testUrl = httpAddress portNum
             ?secureTestUrl = httpsAddress portNum
         _ <- forkIO $ server portNum processRequest
         return $ testGroup serverName [basicTests, browserTests]

  let setupAltTests = do
      let (portNum, (_, server)) = head numberedServers
      let ?testUrl = httpAddress portNum
          ?altTestUrl = httpAddress altPortNum
      _ <- forkIO $ server altPortNum altProcessRequest
      return port80Tests

  case args of
     ["server"] -> do -- run only the harness servers for diagnostic/debug purposes
                      -- halt on any keypress
        _ <- setupNormalTests
        _ <- setupAltTests
        _ <- getChar
        return ()
     ("--withport80":args) -> do
        normalTests <- setupNormalTests
        altTests <- setupAltTests
        _ <- threadDelay 1000000 -- Give the server time to start :-(
        defaultMainWithArgs (normalTests ++ [altTests]) args
     args -> do -- run the test harness as normal
        normalTests <- setupNormalTests
        _ <- threadDelay 1000000 -- Give the server time to start :-(
        defaultMainWithArgs normalTests args
