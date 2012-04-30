module Httpd
    ( Request, Response, initServer
    , mkRequest, mkResponse
    , reqMethod, reqURI, reqHeaders, reqBody
    )
    where

import Network.Shed.Httpd
    ( Request(Request), Response(Response), initServer
    , reqMethod, reqURI, reqHeaders, reqBody
    )

mkRequest = Request
mkResponse = Response
