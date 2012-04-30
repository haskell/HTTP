module Httpd
    ( Request, Response, initServer
    , mkResponse
    , reqMethod, reqURI, reqHeaders, reqBody
    )
    where

import Network.Shed.Httpd
    ( Request, Response(Response), initServer
    , reqMethod, reqURI, reqHeaders, reqBody
    )

mkResponse = Response
