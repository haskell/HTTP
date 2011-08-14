import Control.Exception

import Data.Functor
import Data.Maybe

import System
import System.Posix.Types (ProcessID)
import System.Posix.Unistd (sleep)
import System.Posix.Process (forkProcess, exitImmediately)
import System.Posix.Signals (sigTERM, signalProcess)

import qualified Network.Shed.Httpd as Httpd

import Network.HTTP
import Network.URI ( parseURI, uriPath )

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

-- Our local HTTP server will listen for connections on this port.
portNum = 8080

-- We'll send our requests to this URL
testURL path = "http://localhost:" ++ show portNum ++ path

-- Fork a process to run an HTTP server, using the argument function
-- to process requests and issue responses.
startServer :: (Httpd.Request -> IO Httpd.Response) -> IO ProcessID
startServer processRequest = forkProcess (() <$ Httpd.initServer portNum processRequest)

-- Kill the server process, rather brutally.
stopServer :: ProcessID -> IO ()
stopServer pid = signalProcess sigTERM pid

-- Process a single request
processRequest req = do
  case (Httpd.reqMethod req, Network.URI.uriPath (Httpd.reqURI req)) of 
    ("GET", "/test")    -> return $ Httpd.Response 200 [] "It works."

basicGetRequest :: Assertion
basicGetRequest = do 
    response <- simpleHTTP (getRequest (testURL "/test"))
    body <- getResponseBody response
    assertEqual "Receiving expected response" "It works." body
    
tests = [
  testGroup "Basic tests" [
     testCase "Basic GET request" basicGetRequest
     ]
  ]

main :: IO ()
main = do
  pid <- startServer processRequest
  sleep 1 -- Give the server time to start :-(
  finally (defaultMain tests)
          (stopServer pid)
