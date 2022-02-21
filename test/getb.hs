-- A simple test program which takes a url on the commandline
-- and outputs the contents to stdout.

-- ghc --make -package HTTP get.hs -o get

import Data.Char (intToDigit)
import Network.HTTP
import Network.URI
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Network.BufferType
import Network.TCP
import Network.Stream
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

main =
    do
    args <- getArgs
    case args of
        [addr0] ->
          let (isLazy, isString, addr) =
                case addr0 of
                  '+':xs -> (True,False,xs)
                  '-':xs -> (False,True,xs)
                  _      -> (False,False,addr0)
          in
          case parseURI addr of
                       Nothing -> err "Could not parse URI"
                       Just uri
                         | isLazy    -> get uri >>= putStr . show . Lazy.length
                         | isString  -> get uri >>= \ x -> putStr $ show (length (x::String))
                         | otherwise -> get uri >>= putStr . show . Strict.length
        _ -> err "Usage: get <url>"

err :: String -> IO a
err msg = do
          hPutStrLn stderr msg
          exitFailure

get :: HStream ty => URI -> IO ty
get uri = do
    eresp <- simpleHTTP (request uri)
    resp <- handleE (err . show) eresp
    case rspCode resp of
                      (2,0,0) -> return (rspBody resp)
                      _ -> err (httpError resp)
    where
    showRspCode (a,b,c) = map intToDigit [a,b,c]
    httpError resp = showRspCode (rspCode resp) ++ " " ++ rspReason resp

request :: HStream ty => URI -> HTTPRequest ty
request uri = req
  where
   req = Request{ rqURI = uri
                , rqMethod = GET
                , rqHeaders = []
                , rqBody = nullVal
                }
   nullVal = buf_empty bufferOps


handleE :: Monad m => (ConnError -> m a) -> Either ConnError a -> m a
handleE h (Left e) = h e
handleE _ (Right v) = return v
