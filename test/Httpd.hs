module Httpd
    ( Request, Response, initServer
    , mkResponse
    , reqMethod, reqURI, reqHeaders, reqBody
    )
    where

import Control.Applicative
import Network.URI ( URI )

import qualified Network.Shed.Httpd as Shed
    ( Request, Response(Response), initServer
    , reqMethod, reqURI, reqHeaders, reqBody
    )

type Request = Shed.Request
type Response = Shed.Response

mkResponse :: Int -> [(String, String)] -> String -> Response
mkResponse = Shed.Response

initServer :: Int -> (Request -> IO Response) -> IO ()
initServer port handler = () <$ Shed.initServer port handler

reqMethod :: Request -> String
reqMethod = Shed.reqMethod

reqURI :: Request -> URI
reqURI = Shed.reqURI

reqHeaders :: Request -> [(String, String)]
reqHeaders = Shed.reqHeaders

reqBody :: Request -> String
reqBody = Shed.reqBody
