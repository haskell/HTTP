module Httpd
    ( Request, Response, Server
    , mkResponse
    , reqMethod, reqURI, reqHeaders, reqBody
    , shed
    )
    where

import Control.Applicative
import Control.Monad
import Network.URI ( URI )

import qualified Network.Shed.Httpd as Shed
    ( Request, Response(Response), initServer
    , reqMethod, reqURI, reqHeaders, reqBody
    )

data Request = Request
    {
     reqMethod :: String,
     reqURI :: URI,
     reqHeaders :: [(String, String)],
     reqBody :: String
    }

data Response = Response
    {
     respStatus :: Int,
     respHeaders :: [(String, String)],
     respBody :: String
    }

mkResponse :: Int -> [(String, String)] -> String -> Response
mkResponse = Response

type Server = Int -> (Request -> IO Response) -> IO ()

shed :: Server
shed port handler =
    () <$ Shed.initServer
           port
           (liftM responseToShed . handler . requestFromShed)
  where
     responseToShed (Response status hdrs body) =
         Shed.Response status hdrs body
     requestFromShed request =
         Request
         {
          reqMethod = Shed.reqMethod request,
          reqURI = Shed.reqURI request,
          reqHeaders = Shed.reqHeaders request,
          reqBody = Shed.reqBody request
         }

