{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.Proxy
-- Copyright   :  See LICENSE file
-- License     :  BSD
-- 
-- Maintainer  :  Ganesh Sittampalam <ganesh@earth.li>
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

{-
#if !defined(WIN32) && defined(mingw32_HOST_OS)
#define WIN32 1
#endif
-}

import Control.Monad ( when, mplus, join, liftM2 )

#if defined(WIN32)
import Network.HTTP.Base ( catchIO )
import Control.Monad ( liftM )
import Data.List ( isPrefixOf )
#endif
import Network.HTTP.Utils ( dropWhileTail, chopAtDelim )
import Network.HTTP.Auth
import Network.URI
   ( URI(..), URIAuth(..), parseAbsoluteURI, unEscapeString )
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
proxyString = liftM2 mplus envProxyString windowsProxyString

windowsProxyString :: IO (Maybe String)
#if !defined(WIN32)
windowsProxyString = return Nothing
#else
windowsProxyString = liftM (>>= parseWindowsProxy) registryProxyString

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
registryProxyString :: IO (Maybe String)
registryProxyString = catchIO
  (bracket (uncurry regOpenKey registryProxyLoc) regCloseKey $ \hkey -> do
    enable <- fmap toBool $ regQueryValueDWORD hkey "ProxyEnable"
    if enable
        then fmap Just $ regQueryValue hkey (Just "ProxyServer")
        else return Nothing)
  (\_ -> return Nothing)

-- the proxy string is in the format "http=x.x.x.x:yyyy;https=...;ftp=...;socks=..."
-- even though the following article indicates otherwise
-- https://support.microsoft.com/en-us/kb/819961
--
-- to be sure, parse strings where each entry in the ';'-separated list above is
-- either in the format "protocol=..." or "protocol://..."
--
-- only return the first "http" of them, if it exists
parseWindowsProxy :: String -> Maybe String
parseWindowsProxy s =
  case proxies of
    x:_ -> Just x
    _   -> Nothing
  where
    parts = split ';' s
    pr x = case break (== '=') x of
      (p, []) -> p  -- might be in format http://
      (p, u)  -> p ++ "://" ++ drop 1 u

    proxies = filter (isPrefixOf "http://") . map pr $ parts

    split :: Eq a => a -> [a] -> [[a]]
    split _ [] = []
    split a xs = case break (a ==) xs of
      (ys, [])   -> [ys]
      (ys, _:zs) -> ys:split a zs

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
       as -> Just (AuthBasic "" (unEscapeString usr) (unEscapeString pwd) uri)
        where
         (usr,pwd) = chopAtDelim ':' as

uri2proxy _ = Nothing

-- utilities
#if defined(WIN32)
regQueryValueDWORD :: HKEY -> String -> IO DWORD
regQueryValueDWORD hkey name = alloca $ \ptr -> do
  -- TODO: this throws away the key type returned by regQueryValueEx
  -- we should check it's what we expect instead
  _ <- regQueryValueEx hkey name (castPtr ptr) (sizeOf (undefined :: DWORD))
  peek ptr

#endif
