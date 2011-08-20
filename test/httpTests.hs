import Control.Concurrent

import Data.Functor

import Control.Concurrent (threadDelay)

import qualified Network.Shed.Httpd as Httpd

import Network.Browser
import Network.HTTP
import Network.Stream (Result)
import Network.URI (uriPath)

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


processRequest :: Httpd.Request -> IO Httpd.Response
processRequest req = do
  case (Httpd.reqMethod req, Network.URI.uriPath (Httpd.reqURI req)) of 
    ("GET", "/basic/get") -> return $ Httpd.Response 200 [] "It works."
    ("GET", "/browser/no-cookie") ->
      case lookup "Cookie" (Httpd.reqHeaders req) of
        Nothing -> return $ Httpd.Response 200 [] ""
        Just s  -> return $ Httpd.Response 500 [] s
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
    ]
  ]

portNum :: Int
portNum = 8080

testUrl :: String -> String
testUrl p = "http://localhost:" ++ show portNum ++ p

main :: IO ()
main = do
  _ <- forkIO (() <$ Httpd.initServer portNum processRequest)
  _ <- threadDelay 1000000 -- Give the server time to start :-(
  defaultMain tests

