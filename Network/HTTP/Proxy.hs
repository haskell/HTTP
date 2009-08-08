{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.Proxy
-- Copyright   :  (c) 2009 Eric Kow
-- License     :  BSD
-- 
-- Maintainer  :  Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Author      :  Eric Kow <E.Y.Kow@brighton.ac.uk>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- Handling proxy server settings and their resolution.
-- 
-----------------------------------------------------------------------------
module Network.HTTP.Proxy
       ( Proxy(..)
       , noProxy     -- :: Proxy
       , fetchProxy  -- :: Bool -> IO Proxy
       , parseProxy  -- :: String -> Maybe Proxy
       ) where

import Control.Monad ( when, mplus, join, liftM2)

import Network.HTTP.Utils ( dropWhileTail, chopAtDelim )
import Network.HTTP.Auth
import Network.URI
   ( URI(..), URIAuth(..), parseAbsoluteURI )
import System.IO ( hPutStrLn, stderr )
import System.Environment

{-
#if !defined(WIN32) && defined(mingw32_HOST_OS)
#define WIN32 1
#endif
-}

#if defined(WIN32)
import System.Win32.Types   ( DWORD, HKEY )
import System.Win32.Registry( hKEY_CURRENT_USER, regOpenKey, regCloseKey, regQueryValue, regQueryValueEx )
import Control.Exception    ( bracket )
import Foreign              ( toBool, Storable(peek, sizeOf), castPtr, alloca )
#endif

-- | HTTP proxies (or not) are represented via 'Proxy', specifying if a
-- proxy should be used for the request (see 'Network.Browser.setProxy')
data Proxy 
 = NoProxy                 -- ^ Don't use a proxy.
 | Proxy String
         (Maybe Authority) -- ^ Use the proxy given. Should be of the
                           -- form "http:\/\/host:port", "host", "host:port", or "http:\/\/host".
                           -- Additionally, an optional 'Authority' for authentication with the proxy.


noProxy :: Proxy
noProxy = NoProxy

-- | @envProxyString@ locates proxy server settings by looking
-- up env variable @HTTP_PROXY@ (or its lower-case equivalent.)
-- If no mapping found, returns @Nothing@.
envProxyString :: IO (Maybe String)
envProxyString = do
  env <- getEnvironment
  return (lookup "http_proxy" env `mplus` lookup "HTTP_PROXY" env)

-- | @proxyString@ tries to locate the user's proxy server setting.
-- Consults environment variable, and in case of Windows, by querying
-- the Registry (cf. @registryProxyString@.)
proxyString :: IO (Maybe String)
proxyString = liftM2 mplus envProxyString registryProxyString

registryProxyString :: IO (Maybe String)
#if !defined(WIN32)
registryProxyString = return Nothing
#else
registryProxyLoc :: (HKEY,String)
registryProxyLoc = (hive, path)
  where
    -- some sources say proxy settings should be at 
    -- HKEY_LOCAL_MACHINE\SOFTWARE\Policies\Microsoft\Windows
    --                   \CurrentVersion\Internet Settings\ProxyServer
    -- but if the user sets them with IE connection panel they seem to
    -- end up in the following place:
    hive  = hKEY_CURRENT_USER
    path = "Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings"

-- read proxy settings from the windows registry; this is just a best
-- effort and may not work on all setups. 
registryProxyString = Prelude.catch
  (bracket (uncurry regOpenKey registryProxyLoc) regCloseKey $ \hkey -> do
    enable <- fmap toBool $ regQueryValueDWORD hkey "ProxyEnable"
    if enable
        then fmap Just $ regQueryValue hkey (Just "ProxyServer")
        else return Nothing)
  (\_ -> return Nothing)
#endif

-- | @fetchProxy flg@ gets the local proxy settings and parse the string
-- into a @Proxy@ value. If you want to be informed of ill-formed proxy
-- configuration strings, supply @True@ for @flg@.
-- Proxy settings are sourced from the @HTTP_PROXY@ environment variable,
-- and in the case of Windows platforms, by consulting IE/WinInet's proxy
-- setting in the Registry.
fetchProxy :: Bool -> IO Proxy
fetchProxy warnIfIllformed = do
  mstr <- proxyString
  case mstr of
    Nothing     -> return NoProxy
    Just str    -> case parseProxy str of
        Just p  -> return p                   
        Nothing -> do
            when warnIfIllformed $ System.IO.hPutStrLn System.IO.stderr $ unlines
                    [ "invalid http proxy uri: " ++ show str
                    , "proxy uri must be http with a hostname"
                    , "ignoring http proxy, trying a direct connection"
                    ]
            return NoProxy

-- | @parseProxy str@ translates a proxy server string into a @Proxy@ value;
-- returns @Nothing@ if not well-formed.
parseProxy :: String -> Maybe Proxy
parseProxy str = join
                   . fmap uri2proxy
                   $ parseHttpURI str
             `mplus` parseHttpURI ("http://" ++ str)
  where
   parseHttpURI str' =
    case parseAbsoluteURI str' of
      Just uri@URI{uriAuthority = Just{}} -> Just (fixUserInfo uri)
      _  -> Nothing

     -- Note: we need to be able to parse non-URIs like @\"wwwcache.example.com:80\"@
     -- which lack the @\"http://\"@ URI scheme. The problem is that
     -- @\"wwwcache.example.com:80\"@ is in fact a valid URI but with scheme
     -- @\"wwwcache.example.com:\"@, no authority part and a path of @\"80\"@.
     --
     -- So our strategy is to try parsing as normal uri first and if it lacks the
     -- 'uriAuthority' then we try parsing again with a @\"http://\"@ prefix.
     --

-- | tidy up user portion, don't want the trailing "\@".
fixUserInfo :: URI -> URI
fixUserInfo uri = uri{ uriAuthority = f `fmap` uriAuthority uri }
  where
   f a@URIAuth{uriUserInfo=s} = a{uriUserInfo=dropWhileTail (=='@') s}

-- 
uri2proxy :: URI -> Maybe Proxy
uri2proxy uri@URI{ uriScheme    = "http:"
                 , uriAuthority = Just (URIAuth auth' hst prt)
                 } =
 Just (Proxy (hst ++ prt) auth)
  where
   auth =
     case auth' of
       [] -> Nothing
       as -> Just (AuthBasic "" usr pwd uri)
        where
	 (usr,pwd) = chopAtDelim ':' as

uri2proxy _ = Nothing

-- utilities
#if defined(WIN32)
regQueryValueDWORD :: HKEY -> String -> IO DWORD
regQueryValueDWORD hkey name = alloca $ \ptr -> do
  regQueryValueEx hkey name (castPtr ptr) (sizeOf (undefined :: DWORD))
  peek ptr

#endif
