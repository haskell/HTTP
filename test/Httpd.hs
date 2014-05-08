{-# LANGUAGE CPP #-}

module Httpd
    ( Request, Response, Server
    , mkResponse
    , reqMethod, reqURI, reqHeaders, reqBody
    , shed, warp
    )
    where

import Control.Applicative
import Control.Arrow ( (***) )
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans ( lift )
import Data.ByteString as B ( empty, concat, length, ByteString )
import Data.ByteString.Char8 as BC ( pack, unpack )
import Data.ByteString.Lazy.Char8 as BLC ( pack )
import qualified Data.CaseInsensitive as CI ( mk, original )
import Data.Maybe ( fromJust )
import Network.URI ( URI, parseRelativeReference )

import Network.Socket
    ( getAddrInfo, AddrInfo, defaultHints, addrAddress, addrFamily
      , addrFlags, addrSocketType, AddrInfoFlag(AI_PASSIVE), socket, Family(AF_UNSPEC,AF_INET6)
      , defaultProtocol, bind, SocketType(Stream), listen, setSocketOption
      , SocketOption(ReuseAddr,ReusePort)
    )

import qualified Network.Shed.Httpd as Shed
    ( Request, Response(Response), initServer
    , reqMethod, reqURI, reqHeaders, reqBody
    )

import qualified Data.Conduit.Lazy as Warp
    ( lazyConsume )
import qualified Network.HTTP.Types as Warp
    ( Status(..) )
import qualified Network.Wai as Warp
    ( Request(requestMethod, requestHeaders, rawPathInfo, requestBody)
    , responseLBS )
import qualified Network.Wai.Handler.Warp as Warp
    ( runSettingsSocket, defaultSettings, setPort )

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
     chomp = reverse . strip '\r' . reverse
     strip c (c':str) | c == c' = str
     strip c str = str
     requestFromShed request =
         Request
         {
          reqMethod = Shed.reqMethod request,
          reqURI = Shed.reqURI request,
          reqHeaders = map (id *** chomp) $ Shed.reqHeaders request,
          reqBody = Shed.reqBody request
         }

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData B.ByteString where
   rnf = rnf . B.length
#endif

warp :: Bool -> Server
warp ipv6 port handler = do
    addrinfos <- getAddrInfo (Just $ defaultHints { addrFamily = AF_UNSPEC, addrSocketType = Stream })
                             (Just $ if ipv6 then "::1" else "127.0.0.1")
                             (Just . show $ port)
    case addrinfos of
        [] -> fail "Couldn't obtain address information in warp"
        (addri:_) -> do
            sock <- socket (addrFamily addri) Stream defaultProtocol
            bind sock (addrAddress addri)
            listen sock 5
            Warp.runSettingsSocket (Warp.setPort port Warp.defaultSettings) sock $ \warpRequest -> do
               request <- requestFromWarp warpRequest
               response <- handler request
               return (responseToWarp response)
  where
     responseToWarp (Response status hdrs body) =
         Warp.responseLBS
                 (Warp.Status status B.empty)
                 (map headerToWarp hdrs)
                 (BLC.pack body)
     headerToWarp (name, value) = (CI.mk (BC.pack name), BC.pack value)
     headerFromWarp (name, value) =
         (BC.unpack (CI.original name), BC.unpack value)
     requestFromWarp request = do
         body <- Warp.lazyConsume (Warp.requestBody request)
         body `deepseq` return ()
         return $
                Request
                {
                 reqMethod = BC.unpack (Warp.requestMethod request),
                 reqURI = fromJust . parseRelativeReference .
                          BC.unpack . Warp.rawPathInfo $
                          request,
                 reqHeaders = map headerFromWarp (Warp.requestHeaders request),
                 reqBody = BC.unpack (B.concat body)
                }
