module Network.HTTP.UserAgent where

import Control.Monad.Error

import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BS

import Network.HTTP
import Network.URI
import Data.Maybe ( maybe )

-----------------------------------------------------------------------------

defaultRequest :: Request
defaultRequest = Request { rqURI     = undefined
                         , rqMethod  = GET
                         , rqHeaders = []
                         , rqBody    = BS.empty
                         }
uri :: (MonadError e m, Monad m, HTTPError e) => String -> Request -> m Request
uri str r = do u <- parseURI' str; return $ r { rqURI = u } 

headerAdderM :: Monad m => m Header -> Request -> m Request
headerAdderM hm r@(Request{rqHeaders = hs}) = 
  do h <- hm; return $ r { rqHeaders = h:hs }

headerAdder :: Monad m => Header -> Request -> m Request
headerAdder h r@(Request{rqHeaders = hs}) = return $ r { rqHeaders = h:hs }

          
hExpires :: Monad m => Int -> Request -> m Request
hExpires n = headerAdder (Header HdrExpires (show n))
         
hReferer :: (MonadError e m, HTTPError e, Monad m) => String -> Request -> m Request
hReferer str = headerAdderM $ 
                 do uri <- parseURI' str
                    return $ Header HdrReferer (show uri)
                    
exampleRequest :: (MonadError e m, Monad m, HTTPError e) => m Request
exampleRequest = mkRequest [uri "http://www.google.com", hExpires 4]
          
mkRequest :: (MonadError e m, Monad m, HTTPError e) => 
             [Request -> m Request] -> m Request
mkRequest mods = f mods defaultRequest
  where f [] x     = return x
        f (m:ms) x = m x >>= f ms

parseURI' :: (MonadError e m, HTTPError e) => String -> m URI
parseURI' str = maybe (throwHTTPError URIErr) return $ parseURI str 


-- | Perform HTTP request after parsing the URI string.
get :: (MonadError e m, MonadIO m, HTTPError e) => String -> m Response 
get uriString =  mkRequest [uri uriString] >>= simpleHTTP

-- | Directly get the content of the given String which is parsed as a URI.
-- FIXME: Not sure this is a good name
getContent :: (MonadError e m, MonadIO m, HTTPError e) =>
              String -> m ByteString
getContent uriString = get uriString >>= return . rspBody
