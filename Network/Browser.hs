{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, CPP #-}
{- |

Module      :  Network.Browser
Copyright   :  See LICENSE file
License     :  BSD
 
Maintainer  :  Ganesh Sittampalam <http@projects.haskell.org>
Stability   :  experimental
Portability :  non-portable (not tested)

Session-level interactions over HTTP.
 
The "Network.Browser" goes beyond the basic "Network.HTTP" functionality in 
providing support for more involved, and real, request/response interactions over 
HTTP. Additional features supported are:

* HTTP Authentication handling

* Transparent handling of redirects

* Cookie stores + transmission.

* Transaction logging

* Proxy-mediated connections.

Example use:

>    do
>      (_, rsp)
>         <- Network.Browser.browse $ do
>               setAllowRedirects True -- handle HTTP redirects
>               request $ getRequest "http://www.haskell.org/"
>      return (take 100 (rspBody rsp))
 
-}
module Network.Browser 
       ( BrowserState
       , BrowserAction      -- browser monad, effectively a state monad.
       , Proxy(..)
       
       , browse             -- :: BrowserAction a -> IO a
       , request            -- :: Request -> BrowserAction Response
    
       , getBrowserState    -- :: BrowserAction t (BrowserState t)
       , withBrowserState   -- :: BrowserState t -> BrowserAction t a -> BrowserAction t a
       
       , setAllowRedirects  -- :: Bool -> BrowserAction t ()
       , getAllowRedirects  -- :: BrowserAction t Bool

       , setMaxRedirects    -- :: Int -> BrowserAction t ()
       , getMaxRedirects    -- :: BrowserAction t (Maybe Int)
       
       , Authority(..)
       , getAuthorities
       , setAuthorities
       , addAuthority
       , Challenge(..)
       , Qop(..)
       , Algorithm(..)
       
       , getAuthorityGen
       , setAuthorityGen
       , setAllowBasicAuth
       , getAllowBasicAuth
       
       , setMaxErrorRetries  -- :: Maybe Int -> BrowserAction t ()
       , getMaxErrorRetries  -- :: BrowserAction t (Maybe Int)

       , setMaxPoolSize     -- :: Int -> BrowserAction t ()
       , getMaxPoolSize     -- :: BrowserAction t (Maybe Int)

       , setMaxAuthAttempts  -- :: Maybe Int -> BrowserAction t ()
       , getMaxAuthAttempts  -- :: BrowserAction t (Maybe Int)

       , setCookieFilter     -- :: (URI -> Cookie -> IO Bool) -> BrowserAction t ()
       , getCookieFilter     -- :: BrowserAction t (URI -> Cookie -> IO Bool)
       , defaultCookieFilter -- :: URI -> Cookie -> IO Bool
       , userCookieFilter    -- :: URI -> Cookie -> IO Bool
       
       , Cookie(..)
       , getCookies        -- :: BrowserAction t [Cookie]
       , setCookies        -- :: [Cookie] -> BrowserAction t ()
       , addCookie         -- :: Cookie   -> BrowserAction t ()
       
       , setErrHandler     -- :: (String -> IO ()) -> BrowserAction t ()
       , setOutHandler     -- :: (String -> IO ()) -> BrowserAction t ()
    
       , setEventHandler   -- :: (BrowserEvent -> BrowserAction t ()) -> BrowserAction t ()
       
       , BrowserEvent(..)
       , BrowserEventType(..)
       , RequestID
       
       , setProxy         -- :: Proxy -> BrowserAction t ()
       , getProxy         -- :: BrowserAction t Proxy

       , setCheckForProxy -- :: Bool -> BrowserAction t ()
       , getCheckForProxy -- :: BrowserAction t Bool

       , setDebugLog      -- :: Maybe String -> BrowserAction t ()
       
       , getUserAgent     -- :: BrowserAction t String
       , setUserAgent     -- :: String -> BrowserAction t ()
       
       , out              -- :: String -> BrowserAction t ()
       , err              -- :: String -> BrowserAction t ()
       , ioAction         -- :: IO a -> BrowserAction a

       , defaultGETRequest
       , defaultGETRequest_
       
       , formToRequest
       , uriDefaultTo
       
         -- old and half-baked; don't use:
       , Form(..)
       , FormVar
       ) where

import Network.URI
   ( URI(..)
   , URIAuth(..)
   , parseURI, parseURIReference, relativeTo
   )
import Network.StreamDebugger (debugByteStream)
import Network.HTTP hiding ( sendHTTP_notify )
import Network.HTTP.HandleStream ( sendHTTP_notify )
import Network.HTTP.Auth
import Network.HTTP.Cookie
import Network.HTTP.Proxy

import Network.Stream ( ConnError(..), Result )
import Network.BufferType

import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes )
import Control.Applicative (Applicative (..), (<$>))
#ifdef MTL1
import Control.Monad (filterM, when, ap)
#else
import Control.Monad (filterM, when)
#endif
import Control.Monad.State (StateT (..), MonadIO (..), modify, gets, withStateT, evalStateT, MonadState (..))

import qualified System.IO
   ( hSetBuffering, hPutStr, stdout, stdin, hGetChar
   , BufferMode(NoBuffering, LineBuffering)
   )
import System.Time ( ClockTime, getClockTime )


------------------------------------------------------------------
----------------------- Cookie Stuff -----------------------------
------------------------------------------------------------------

-- | @defaultCookieFilter@ is the initial cookie acceptance filter.
-- It welcomes them all into the store @:-)@
defaultCookieFilter :: URI -> Cookie -> IO Bool
defaultCookieFilter _url _cky = return True

-- | @userCookieFilter@ is a handy acceptance filter, asking the
-- user if he/she is willing to accept an incoming cookie before
-- adding it to the store.
userCookieFilter :: URI -> Cookie -> IO Bool
userCookieFilter url cky = do
    do putStrLn ("Set-Cookie received when requesting: " ++ show url)
       case ckComment cky of
          Nothing -> return ()
          Just x  -> putStrLn ("Cookie Comment:\n" ++ x)
       let pth = maybe "" ('/':) (ckPath cky)
       putStrLn ("Domain/Path: " ++ ckDomain cky ++ pth)
       putStrLn (ckName cky ++ '=' : ckValue cky)
       System.IO.hSetBuffering System.IO.stdout System.IO.NoBuffering
       System.IO.hSetBuffering System.IO.stdin System.IO.NoBuffering
       System.IO.hPutStr System.IO.stdout "Accept [y/n]? "
       x <- System.IO.hGetChar System.IO.stdin
       System.IO.hSetBuffering System.IO.stdin System.IO.LineBuffering
       System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering
       return (toLower x == 'y')

-- | @addCookie c@ adds a cookie to the browser state, removing duplicates.
addCookie :: Cookie -> BrowserAction t ()
addCookie c = modify (\b -> b{bsCookies = c : filter (/=c) (bsCookies b) })

-- | @setCookies cookies@ replaces the set of cookies known to
-- the browser to @cookies@. Useful when wanting to restore cookies
-- used across 'browse' invocations.
setCookies :: [Cookie] -> BrowserAction t ()
setCookies cs = modify (\b -> b { bsCookies=cs })

-- | @getCookies@ returns the current set of cookies known to
-- the browser.
getCookies :: BrowserAction t [Cookie]
getCookies = gets bsCookies

-- ...get domain specific cookies...
-- ... this needs changing for consistency with rfc2109...
-- ... currently too broad.
getCookiesFor :: String -> String -> BrowserAction t [Cookie]
getCookiesFor dom path =
    do cks <- getCookies
       return (filter cookiematch cks)
    where
        cookiematch :: Cookie -> Bool
        cookiematch = cookieMatch (dom,path)
      

-- | @setCookieFilter fn@ sets the cookie acceptance filter to @fn@.
setCookieFilter :: (URI -> Cookie -> IO Bool) -> BrowserAction t ()
setCookieFilter f = modify (\b -> b { bsCookieFilter=f })

-- | @getCookieFilter@ returns the current cookie acceptance filter.
getCookieFilter :: BrowserAction t (URI -> Cookie -> IO Bool)
getCookieFilter = gets bsCookieFilter

------------------------------------------------------------------
----------------------- Authorisation Stuff ----------------------
------------------------------------------------------------------

{-

The browser handles 401 responses in the following manner:
  1) extract all WWW-Authenticate headers from a 401 response
  2) rewrite each as a Challenge object, using "headerToChallenge"
  3) pick a challenge to respond to, usually the strongest
     challenge understood by the client, using "pickChallenge"
  4) generate a username/password combination using the browsers
     "bsAuthorityGen" function (the default behaviour is to ask
     the user)
  5) build an Authority object based upon the challenge and user
     data, store this new Authority in the browser state
  6) convert the Authority to a request header and add this
     to a request using "withAuthority"
  7) send the amended request

Note that by default requests are annotated with authority headers
before the first sending, based upon previously generated Authority
objects (which contain domain information).  Once a specific authority
is added to a rejected request this predictive annotation is suppressed.

407 responses are handled in a similar manner, except
   a) Authorities are not collected, only a single proxy authority
      is kept by the browser
   b) If the proxy used by the browser (type Proxy) is NoProxy, then
      a 407 response will generate output on the "err" stream and
      the response will be returned.


Notes:
 - digest authentication so far ignores qop, so fails to authenticate 
   properly with qop=auth-int challenges
 - calculates a1 more than necessary
 - doesn't reverse authenticate
 - doesn't properly receive AuthenticationInfo headers, so fails
   to use next-nonce etc

-}

-- | Return authorities for a given domain and path.
-- Assumes "dom" is lower case
getAuthFor :: String -> String -> BrowserAction t [Authority]
getAuthFor dom pth = getAuthorities >>= return . (filter match)
   where
    match :: Authority -> Bool
    match au@AuthBasic{}  = matchURI (auSite au)
    match au@AuthDigest{} = or (map matchURI (auDomain au))

    matchURI :: URI -> Bool
    matchURI s = (uriToAuthorityString s == dom) && (uriPath s `isPrefixOf` pth)
    

-- | @getAuthorities@ return the current set of @Authority@s known
-- to the browser.
getAuthorities :: BrowserAction t [Authority]
getAuthorities = gets bsAuthorities

-- @setAuthorities as@ replaces the Browser's known set
-- of 'Authority's to @as@.
setAuthorities :: [Authority] -> BrowserAction t ()
setAuthorities as = modify (\b -> b { bsAuthorities=as })

-- @addAuthority a@ adds 'Authority' @a@ to the Browser's
-- set of known authorities.
addAuthority :: Authority -> BrowserAction t ()
addAuthority a = modify (\b -> b { bsAuthorities=a:bsAuthorities b })

-- | @getAuthorityGen@ returns the current authority generator
getAuthorityGen :: BrowserAction t (URI -> String -> IO (Maybe (String,String)))
getAuthorityGen = gets bsAuthorityGen

-- | @setAuthorityGen genAct@ sets the auth generator to @genAct@.
setAuthorityGen :: (URI -> String -> IO (Maybe (String,String))) -> BrowserAction t ()
setAuthorityGen f = modify (\b -> b { bsAuthorityGen=f })

-- | @setAllowBasicAuth onOff@ enables\/disables HTTP Basic Authentication.
setAllowBasicAuth :: Bool -> BrowserAction t ()
setAllowBasicAuth ba = modify (\b -> b { bsAllowBasicAuth=ba })

getAllowBasicAuth :: BrowserAction t Bool
getAllowBasicAuth = gets bsAllowBasicAuth

-- | @setMaxAuthAttempts mbMax@ sets the maximum number of authentication attempts
-- to do. If @Nothing@, rever to default max.
setMaxAuthAttempts :: Maybe Int -> BrowserAction t ()
setMaxAuthAttempts mb 
 | fromMaybe 0 mb < 0 = return ()
 | otherwise          = modify (\ b -> b{bsMaxAuthAttempts=mb})

-- | @getMaxAuthAttempts@ returns the current max auth attempts. If @Nothing@,
-- the browser's default is used.
getMaxAuthAttempts :: BrowserAction t (Maybe Int)
getMaxAuthAttempts = gets bsMaxAuthAttempts

-- | @setMaxErrorRetries mbMax@ sets the maximum number of attempts at
-- transmitting a request. If @Nothing@, rever to default max.
setMaxErrorRetries :: Maybe Int -> BrowserAction t ()
setMaxErrorRetries mb
 | fromMaybe 0 mb < 0 = return ()
 | otherwise          = modify (\ b -> b{bsMaxErrorRetries=mb})

-- | @getMaxErrorRetries@ returns the current max number of error retries.
getMaxErrorRetries :: BrowserAction t (Maybe Int)
getMaxErrorRetries = gets bsMaxErrorRetries

-- TO BE CHANGED!!!
pickChallenge :: Bool -> [Challenge] -> Maybe Challenge
pickChallenge allowBasic []
 | allowBasic = Just (ChalBasic "/") -- manufacture a challenge if one missing; more robust.
pickChallenge _ ls = listToMaybe ls

-- | Retrieve a likely looking authority for a Request.
anticipateChallenge :: Request ty -> BrowserAction t (Maybe Authority)
anticipateChallenge rq =
    let uri = rqURI rq in
    do { authlist <- getAuthFor (uriAuthToString $ reqURIAuth rq) (uriPath uri)
       ; return (listToMaybe authlist)
       }

-- | Asking the user to respond to a challenge
challengeToAuthority :: URI -> Challenge -> BrowserAction t (Maybe Authority)
challengeToAuthority uri ch
 | not (answerable ch) = return Nothing
 | otherwise = do
      -- prompt user for authority
    prompt <- getAuthorityGen
    userdetails <- liftIO $ prompt uri (chRealm ch)
    case userdetails of
     Nothing    -> return Nothing
     Just (u,p) -> return (Just $ buildAuth ch u p)
 where
  answerable :: Challenge -> Bool
  answerable ChalBasic{} = True
  answerable chall       = (chAlgorithm chall) == Just AlgMD5

  buildAuth :: Challenge -> String -> String -> Authority
  buildAuth (ChalBasic r) u p = 
       AuthBasic { auSite=uri
                 , auRealm=r
                 , auUsername=u
                 , auPassword=p
                 }

    -- note to self: this is a pretty stupid operation
    -- to perform isn't it? ChalX and AuthX are so very
    -- similar.
  buildAuth (ChalDigest r d n o _stale a q) u p =
            AuthDigest { auRealm=r
                       , auUsername=u
                       , auPassword=p
                       , auDomain=d
                       , auNonce=n
                       , auOpaque=o
                       , auAlgorithm=a
                       , auQop=q
                       }


------------------------------------------------------------------
------------------ Browser State Actions -------------------------
------------------------------------------------------------------


-- | @BrowserState@ is the (large) record type tracking the current
-- settings of the browser.
data BrowserState connection
 = BS { bsErr, bsOut      :: String -> IO ()
      , bsCookies         :: [Cookie]
      , bsCookieFilter    :: URI -> Cookie -> IO Bool
      , bsAuthorityGen    :: URI -> String -> IO (Maybe (String,String))
      , bsAuthorities     :: [Authority]
      , bsAllowRedirects  :: Bool
      , bsAllowBasicAuth  :: Bool
      , bsMaxRedirects    :: Maybe Int
      , bsMaxErrorRetries :: Maybe Int
      , bsMaxAuthAttempts :: Maybe Int
      , bsMaxPoolSize     :: Maybe Int
      , bsConnectionPool  :: [connection]
      , bsCheckProxy      :: Bool
      , bsProxy           :: Proxy
      , bsDebug           :: Maybe String
      , bsEvent           :: Maybe (BrowserEvent -> BrowserAction connection ())
      , bsRequestID       :: RequestID
      , bsUserAgent       :: Maybe String
      }

instance Show (BrowserState t) where
    show bs =  "BrowserState { " 
            ++ shows (bsCookies bs) ("\n"
           {- ++ show (bsAuthorities bs) ++ "\n"-}
            ++ "AllowRedirects: " ++ shows (bsAllowRedirects bs) "} ")

-- | @BrowserAction@ is the IO monad, but carrying along a 'BrowserState'.
newtype BrowserAction conn a
 = BA { unBA :: StateT (BrowserState conn) IO a }
#ifdef MTL1
 deriving (Functor, Monad, MonadIO, MonadState (BrowserState conn))

instance Applicative (BrowserAction conn) where
  pure  = return
  (<*>) = ap
#else
 deriving (Functor, Applicative, Monad, MonadIO, MonadState (BrowserState conn))
#endif

runBA :: BrowserState conn -> BrowserAction conn a -> IO a
runBA bs = flip evalStateT bs . unBA

-- | @browse act@ is the toplevel action to perform a 'BrowserAction'.
-- Example use: @browse (request (getRequest yourURL))@.
browse :: BrowserAction conn a -> IO a
browse = runBA defaultBrowserState

-- | The default browser state has the settings 
defaultBrowserState :: BrowserState t
defaultBrowserState = res
 where
   res = BS
     { bsErr              = putStrLn
     , bsOut              = putStrLn
     , bsCookies          = []
     , bsCookieFilter     = defaultCookieFilter
     , bsAuthorityGen     = \ _uri _realm -> do
          bsErr res "No action for prompting/generating user+password credentials provided (use: setAuthorityGen); returning Nothing"
          return Nothing
     , bsAuthorities      = []
     , bsAllowRedirects   = True
     , bsAllowBasicAuth   = False
     , bsMaxRedirects     = Nothing
     , bsMaxErrorRetries  = Nothing
     , bsMaxAuthAttempts  = Nothing
     , bsMaxPoolSize      = Nothing
     , bsConnectionPool   = []
     , bsCheckProxy       = defaultAutoProxyDetect
     , bsProxy            = noProxy
     , bsDebug            = Nothing 
     , bsEvent            = Nothing
     , bsRequestID        = 0
     , bsUserAgent        = Nothing
     }

{-# DEPRECATED getBrowserState "Use Control.Monad.State.get instead." #-}
-- | @getBrowserState@ returns the current browser config. Useful
-- for restoring state across 'BrowserAction's.
getBrowserState :: BrowserAction t (BrowserState t)
getBrowserState = get

-- | @withBrowserAction st act@ performs @act@ with 'BrowserState' @st@.
withBrowserState :: BrowserState t -> BrowserAction t a -> BrowserAction t a
withBrowserState bs = BA . withStateT (const bs) . unBA

-- | @nextRequest act@ performs the browser action @act@ as
-- the next request, i.e., setting up a new request context
-- before doing so.
nextRequest :: BrowserAction t a -> BrowserAction t a
nextRequest act = do
  let updReqID st = 
       let 
        rid = succ (bsRequestID st)
       in
       rid `seq` st{bsRequestID=rid}
  modify updReqID
  act

-- | Lifts an IO action into the 'BrowserAction' monad.
{-# DEPRECATED ioAction "Use Control.Monad.Trans.liftIO instead." #-}
ioAction :: IO a -> BrowserAction t a
ioAction = liftIO

-- | @setErrHandler@ sets the IO action to call when
-- the browser reports running errors. To disable any
-- such, set it to @const (return ())@.
setErrHandler :: (String -> IO ()) -> BrowserAction t ()
setErrHandler h = modify (\b -> b { bsErr=h })

-- | @setOutHandler@ sets the IO action to call when
-- the browser chatters info on its running. To disable any
-- such, set it to @const (return ())@.
setOutHandler :: (String -> IO ()) -> BrowserAction t ()
setOutHandler h = modify (\b -> b { bsOut=h })

out, err :: String -> BrowserAction t ()
out s = do { f <- gets bsOut ; liftIO $ f s }
err s = do { f <- gets bsErr ; liftIO $ f s }

-- | @setAllowRedirects onOff@ toggles the willingness to
-- follow redirects (HTTP responses with 3xx status codes).
setAllowRedirects :: Bool -> BrowserAction t ()
setAllowRedirects bl = modify (\b -> b {bsAllowRedirects=bl})

-- | @getAllowRedirects@ returns current setting of the do-chase-redirects flag.
getAllowRedirects :: BrowserAction t Bool
getAllowRedirects = gets bsAllowRedirects

-- | @setMaxRedirects maxCount@ sets the maxiumum number of forwarding hops
-- we are willing to jump through. A no-op if the count is negative; if zero,
-- the max is set to whatever default applies. Notice that setting the max
-- redirects count does /not/ enable following of redirects itself; use
-- 'setAllowRedirects' to do so.
setMaxRedirects :: Maybe Int -> BrowserAction t ()
setMaxRedirects c 
 | fromMaybe 0 c < 0  = return ()
 | otherwise          = modify (\b -> b{bsMaxRedirects=c})

-- | @getMaxRedirects@ returns the current setting for the max-redirect count.
-- If @Nothing@, the "Network.Browser"'s default is used.
getMaxRedirects :: BrowserAction t (Maybe Int)
getMaxRedirects = gets bsMaxRedirects

-- | @setMaxPoolSize maxCount@ sets the maximum size of the connection pool
-- that is used to cache connections between requests
setMaxPoolSize :: Maybe Int -> BrowserAction t ()
setMaxPoolSize c = modify (\b -> b{bsMaxPoolSize=c})

-- | @getMaxPoolSize@ gets the maximum size of the connection pool
-- that is used to cache connections between requests.
-- If @Nothing@, the "Network.Browser"'s default is used.
getMaxPoolSize :: BrowserAction t (Maybe Int)
getMaxPoolSize = gets bsMaxPoolSize

-- | @setProxy p@ will disable proxy usage if @p@ is @NoProxy@.
-- If @p@ is @Proxy proxyURL mbAuth@, then @proxyURL@ is interpreted
-- as the URL of the proxy to use, possibly authenticating via 
-- 'Authority' information in @mbAuth@.
setProxy :: Proxy -> BrowserAction t ()
setProxy p =
   -- Note: if user _explicitly_ sets the proxy, we turn
   -- off any auto-detection of proxies.
  modify (\b -> b {bsProxy = p, bsCheckProxy=False})

-- | @getProxy@ returns the current proxy settings. If
-- the auto-proxy flag is set to @True@, @getProxy@ will
-- perform the necessary 
getProxy :: BrowserAction t Proxy
getProxy = do
  p <- gets bsProxy
  case p of
      -- Note: if there is a proxy, no need to perform any auto-detect.
      -- Presumably this is the user's explicit and preferred proxy server.
    Proxy{} -> return p
    NoProxy{} -> do
     flg <- gets bsCheckProxy
     if not flg
      then return p 
      else do
       np <- liftIO $ fetchProxy True{-issue warning on stderr if ill-formed...-}
        -- note: this resets the check-proxy flag; a one-off affair.
       setProxy np
       return np

-- | @setCheckForProxy flg@ sets the one-time check for proxy
-- flag to @flg@. If @True@, the session will try to determine
-- the proxy server is locally configured. See 'Network.HTTP.Proxy.fetchProxy'
-- for details of how this done.
setCheckForProxy :: Bool -> BrowserAction t ()
setCheckForProxy flg = modify (\ b -> b{bsCheckProxy=flg})

-- | @getCheckForProxy@ returns the current check-proxy setting.
-- Notice that this may not be equal to @True@ if the session has
-- set it to that via 'setCheckForProxy' and subsequently performed
-- some HTTP protocol interactions. i.e., the flag return represents
-- whether a proxy will be checked for again before any future protocol
-- interactions.
getCheckForProxy :: BrowserAction t Bool
getCheckForProxy = gets bsCheckProxy

-- | @setDebugLog mbFile@ turns off debug logging iff @mbFile@
-- is @Nothing@. If set to @Just fStem@, logs of browser activity
-- is appended to files of the form @fStem-url-authority@, i.e.,
-- @fStem@ is just the prefix for a set of log files, one per host/authority.
setDebugLog :: Maybe String -> BrowserAction t ()
setDebugLog v = modify (\b -> b {bsDebug=v})

-- | @setUserAgent ua@ sets the current @User-Agent:@ string to @ua@. It
-- will be used if no explicit user agent header is found in subsequent requests.
--
-- A common form of user agent string is @\"name\/version (details)\"@. For
-- example @\"cabal-install/0.10.2 (HTTP 4000.1.2)\"@. Including the version
-- of this HTTP package can be helpful if you ever need to track down HTTP
-- compatability quirks. This version is available via 'httpPackageVersion'.
-- For more info see <http://en.wikipedia.org/wiki/User_agent>.
--
setUserAgent :: String -> BrowserAction t ()
setUserAgent ua = modify (\b -> b{bsUserAgent=Just ua})

-- | @getUserAgent@ returns the current @User-Agent:@ default string.
getUserAgent :: BrowserAction t String
getUserAgent  = do
  n <- gets bsUserAgent
  return (maybe defaultUserAgent id n)

-- | @RequestState@ is an internal tallying type keeping track of various 
-- per-connection counters, like the number of authorization attempts and 
-- forwards we've gone through.
data RequestState 
  = RequestState
      { reqDenies     :: Int   -- ^ number of 401 responses so far
      , reqRedirects  :: Int   -- ^ number of redirects so far
      , reqRetries    :: Int   -- ^ number of retries so far
      , reqStopOnDeny :: Bool  -- ^ whether to pre-empt 401 response
      }

type RequestID = Int -- yeah, it will wrap around.

nullRequestState :: RequestState
nullRequestState = RequestState
      { reqDenies     = 0
      , reqRedirects  = 0
      , reqRetries    = 0
      , reqStopOnDeny = True
      }

-- | @BrowserEvent@ is the event record type that a user-defined handler, set
-- via 'setEventHandler', will be passed. It indicates various state changes
-- encountered in the processing of a given 'RequestID', along with timestamps
-- at which they occurred.
data BrowserEvent
 = BrowserEvent
      { browserTimestamp  :: ClockTime
      , browserRequestID  :: RequestID
      , browserRequestURI :: {-URI-}String
      , browserEventType  :: BrowserEventType
      }

-- | 'BrowserEventType' is the enumerated list of events that the browser
-- internals will report to a user-defined event handler.
data BrowserEventType
 = OpenConnection
 | ReuseConnection
 | RequestSent
 | ResponseEnd ResponseData
 | ResponseFinish
{- not yet, you will have to determine these via the ResponseEnd event.
 | Redirect
 | AuthChallenge
 | AuthResponse
-}
 
-- | @setEventHandler onBrowserEvent@ configures event handling.
-- If @onBrowserEvent@ is @Nothing@, event handling is turned off;
-- setting it to @Just onEv@ causes the @onEv@ IO action to be
-- notified of browser events during the processing of a request
-- by the Browser pipeline.
setEventHandler :: Maybe (BrowserEvent -> BrowserAction ty ()) -> BrowserAction ty ()
setEventHandler mbH = modify (\b -> b { bsEvent=mbH})

buildBrowserEvent :: BrowserEventType -> {-URI-}String -> RequestID -> IO BrowserEvent
buildBrowserEvent bt uri reqID = do
  ct <- getClockTime
  return BrowserEvent 
         { browserTimestamp  = ct
         , browserRequestID  = reqID
         , browserRequestURI = uri
         , browserEventType  = bt
         }

reportEvent :: BrowserEventType -> {-URI-}String -> BrowserAction t ()
reportEvent bt uri = do
  st <- get
  case bsEvent st of
    Nothing -> return ()
    Just evH -> do
       evt <- liftIO $ buildBrowserEvent bt uri (bsRequestID st)
       evH evt -- if it fails, we fail.

-- | The default number of hops we are willing not to go beyond for 
-- request forwardings.
defaultMaxRetries :: Int
defaultMaxRetries = 4

-- | The default number of error retries we are willing to perform.
defaultMaxErrorRetries :: Int
defaultMaxErrorRetries = 4

-- | The default maximum HTTP Authentication attempts we will make for
-- a single request.
defaultMaxAuthAttempts :: Int
defaultMaxAuthAttempts = 2

-- | The default setting for auto-proxy detection.
-- You may change this within a session via 'setAutoProxyDetect'.
-- To avoid initial backwards compatibility issues, leave this as @False@.
defaultAutoProxyDetect :: Bool
defaultAutoProxyDetect = False

-- | @request httpRequest@ tries to submit the 'Request' @httpRequest@
-- to some HTTP server (possibly going via a /proxy/, see 'setProxy'.)
-- Upon successful delivery, the URL where the response was fetched from
-- is returned along with the 'Response' itself.
request :: HStream ty
        => Request ty
	-> BrowserAction (HandleStream ty) (URI,Response ty)
request req = nextRequest $ do
  res <- request' nullVal initialState req
  reportEvent ResponseFinish (show (rqURI req))
  case res of
    Right r -> return r
    Left e  -> do
     let errStr = ("Network.Browser.request: Error raised " ++ show e)
     err errStr
     fail errStr
 where
  initialState = nullRequestState
  nullVal      = buf_empty bufferOps

-- | Internal helper function, explicitly carrying along per-request 
-- counts.
request' :: HStream ty
         => ty
	 -> RequestState
	 -> Request ty
	 -> BrowserAction (HandleStream ty) (Result (URI,Response ty))
request' nullVal rqState rq = do
   let uri = rqURI rq
   failHTTPS uri
   let uria = reqURIAuth rq 
     -- add cookies to request
   cookies <- getCookiesFor (uriAuthToString uria) (uriPath uri)
{- Not for now:
   (case uriUserInfo uria of
     "" -> id
     xs ->
       case chopAtDelim ':' xs of
         (_,[])    -> id
	 (usr,pwd) -> withAuth
	                  AuthBasic{ auUserName = usr
                                   , auPassword = pwd
			           , auRealm    = "/"
			           , auSite     = uri
			           }) $ do
-}
   when (not $ null cookies) 
        (out $ "Adding cookies to request.  Cookie names: "  ++ unwords (map ckName cookies))
    -- add credentials to request
   rq' <- 
    if not (reqStopOnDeny rqState) 
     then return rq 
     else do 
       auth <- anticipateChallenge rq
       case auth of
         Nothing -> return rq
         Just x  -> return (insertHeader HdrAuthorization (withAuthority x rq) rq)
   let rq'' = if not $ null cookies then insertHeaders [cookiesToHeader cookies] rq' else rq'
   p <- getProxy
   def_ua <- gets bsUserAgent
   let defaultOpts = 
         case p of 
	   NoProxy     -> defaultNormalizeRequestOptions{normUserAgent=def_ua}
	   Proxy _ ath ->
	      defaultNormalizeRequestOptions
	        { normForProxy  = True
		, normUserAgent = def_ua
		, normCustoms   = 
		    maybe []
		          (\ authS -> [\ _ r -> insertHeader HdrProxyAuthorization (withAuthority authS r) r])
			  ath
		}
   let final_req = normalizeRequest defaultOpts rq''
   out ("Sending:\n" ++ show final_req)
   e_rsp <- 
     case p of
       NoProxy        -> dorequest (reqURIAuth rq'') final_req
       Proxy str _ath -> do
          let notURI 
	       | null pt || null hst = 
	         URIAuth{ uriUserInfo = ""
	                , uriRegName  = str
			, uriPort     = ""
			}
	       | otherwise = 
	         URIAuth{ uriUserInfo = ""
	                , uriRegName  = hst
			, uriPort     = pt
			}
                  -- If the ':' is dropped from port below, dorequest will assume port 80. Leave it!
                 where (hst, pt) = span (':'/=) str
           -- Proxy can take multiple forms - look for http://host:port first,
           -- then host:port. Fall back to just the string given (probably a host name).
          let proxyURIAuth =
                maybe notURI
                      (\parsed -> maybe notURI id (uriAuthority parsed))
                      (parseURI str)

          out $ "proxy uri host: " ++ uriRegName proxyURIAuth ++ ", port: " ++ uriPort proxyURIAuth
	  dorequest proxyURIAuth final_req
   mbMx <- getMaxErrorRetries
   case e_rsp of
    Left v 
     | (reqRetries rqState < fromMaybe defaultMaxErrorRetries mbMx) && 
       (v == ErrorReset || v == ErrorClosed) -> do
       --empty connnection pool in case connection has become invalid
       modify (\b -> b { bsConnectionPool=[] })       
       request' nullVal rqState{reqRetries=succ (reqRetries rqState)} rq
     | otherwise -> 
       return (Left v)
    Right rsp -> do 
     out ("Received:\n" ++ show rsp)
      -- add new cookies to browser state
     handleCookies uri (uriAuthToString $ reqURIAuth rq) 
                       (retrieveHeaders HdrSetCookie rsp)
     mbMxAuths <- getMaxAuthAttempts
     case rspCode rsp of
      (4,0,1) -- Credentials not sent or refused.
        | reqDenies rqState > fromMaybe defaultMaxAuthAttempts mbMxAuths -> do
          out "401 - credentials again refused; exceeded retry count (2)"
	  return (Right (uri,rsp))
	| otherwise -> do
          out "401 - credentials not supplied or refused; retrying.."
          let hdrs = retrieveHeaders HdrWWWAuthenticate rsp
	  flg <- getAllowBasicAuth
          case pickChallenge flg (catMaybes $ map (headerToChallenge uri) hdrs) of
            Nothing -> do
	      out "no challenge"
	      return (Right (uri,rsp))   {- do nothing -}
            Just x  -> do
              au <- challengeToAuthority uri x
              case au of
                Nothing  -> do
		  out "no auth"
		  return (Right (uri,rsp)) {- do nothing -}
                Just au' -> do
                  out "Retrying request with new credentials"
		  request' nullVal
			   rqState{ reqDenies     = succ(reqDenies rqState)
			          , reqStopOnDeny = False
				  }
                           (insertHeader HdrAuthorization (withAuthority au' rq) rq)

      (4,0,7)  -- Proxy Authentication required
        | reqDenies rqState > fromMaybe defaultMaxAuthAttempts mbMxAuths -> do
          out "407 - proxy authentication required; max deny count exceeeded (2)"
          return (Right (uri,rsp))
        | otherwise -> do
          out "407 - proxy authentication required"
          let hdrs = retrieveHeaders HdrProxyAuthenticate rsp
	  flg <- getAllowBasicAuth
          case pickChallenge flg (catMaybes $ map (headerToChallenge uri) hdrs) of
            Nothing -> return (Right (uri,rsp))   {- do nothing -}
            Just x  -> do
              au <- challengeToAuthority uri x
              case au of
               Nothing  -> return (Right (uri,rsp))  {- do nothing -}
               Just au' -> do
                 pxy <- gets bsProxy
                 case pxy of
                   NoProxy -> do
                     err "Proxy authentication required without proxy!"
                     return (Right (uri,rsp))
                   Proxy px _ -> do
                     out "Retrying with proxy authentication"
                     setProxy (Proxy px (Just au'))
                     request' nullVal
			      rqState{ reqDenies     = succ(reqDenies rqState)
			             , reqStopOnDeny = False
				     }
			      rq

      (3,0,x) | x `elem` [2,3,1,7]  ->  do
        out ("30" ++ show x ++  " - redirect")
	allow_redirs <- allowRedirect rqState
	case allow_redirs of
	  False -> return (Right (uri,rsp))
	  _ -> do
           case retrieveHeaders HdrLocation rsp of
            [] -> do 
	      err "No Location: header in redirect response"
              return (Right (uri,rsp))
            (Header _ u:_) -> 
	      case parseURIReference u of
                Nothing -> do
                  err ("Parse of Location: header in a redirect response failed: " ++ u)
                  return (Right (uri,rsp))
                Just newURI
	         | {-uriScheme newURI_abs /= uriScheme uri && -}(not (supportedScheme newURI_abs)) -> do
	            err ("Unable to handle redirect, unsupported scheme: " ++ show newURI_abs)
	            return (Right (uri, rsp))
                 | otherwise -> do		     
  	            out ("Redirecting to " ++ show newURI_abs ++ " ...") 
                    
                    -- Redirect using GET request method, depending on
                    -- response code.
                    let toGet = x `elem` [2,3]
                        method = if toGet then GET else rqMethod rq
                        rq1 = rq { rqMethod=method, rqURI=newURI_abs }
                        rq2 = if toGet then (replaceHeader HdrContentLength "0") (rq1 {rqBody = nullVal}) else rq1
                    
                    request' nullVal
	         	    rqState{ reqDenies     = 0
	         	           , reqRedirects  = succ(reqRedirects rqState)
	         		   , reqStopOnDeny = True
	         		   }
                             rq2
                 where
                   newURI_abs = uriDefaultTo newURI uri

      (3,0,5) ->
        case retrieveHeaders HdrLocation rsp of
         [] -> do 
	   err "No Location header in proxy redirect response."
           return (Right (uri,rsp))
         (Header _ u:_) -> 
	   case parseURIReference u of
            Nothing -> do
             err ("Parse of Location header in a proxy redirect response failed: " ++ u)
             return (Right (uri,rsp))
            Just newuri -> do
             out ("Retrying with proxy " ++ show newuri ++ "...")
             setProxy (Proxy (uriToAuthorityString newuri) Nothing)
             request' nullVal rqState{ reqDenies     = 0
	                             , reqRedirects  = 0
				     , reqRetries    = succ (reqRetries rqState)
				     , reqStopOnDeny = True
				     }
				     rq
      _       -> return (Right (uri,rsp))

-- | The internal request handling state machine.
dorequest :: (HStream ty)
          => URIAuth
	  -> Request ty
	  -> BrowserAction (HandleStream ty)
	                   (Result (Response ty))
dorequest hst rqst = do
  pool <- gets bsConnectionPool
  let uPort = uriAuthPort Nothing{-ToDo: feed in complete URL-} hst
  conn <- liftIO $ filterM (\c -> c `isTCPConnectedTo` EndPoint (uriRegName hst) uPort) pool
  rsp <- 
    case conn of
      [] -> do 
        out ("Creating new connection to " ++ uriAuthToString hst)
	reportEvent OpenConnection (show (rqURI rqst))
        c <- liftIO $ openStream (uriRegName hst) uPort
	updateConnectionPool c
	dorequest2 c rqst
      (c:_) -> do
        out ("Recovering connection to " ++ uriAuthToString hst)
	reportEvent ReuseConnection (show (rqURI rqst))
        dorequest2 c rqst
  case rsp of 
     Right (Response a b c _) -> 
         reportEvent (ResponseEnd (a,b,c)) (show (rqURI rqst)) ; _ -> return ()
  return rsp
 where
  dorequest2 c r = do
    dbg <- gets bsDebug
    st  <- get
    let 
     onSendComplete =
       maybe (return ())
             (\evh -> do
	        x <- buildBrowserEvent RequestSent (show (rqURI r)) (bsRequestID st)
		runBA st (evh x)
		return ())
             (bsEvent st)
    liftIO $ 
      maybe (sendHTTP_notify c r onSendComplete)
            (\ f -> do
               c' <- debugByteStream (f++'-': uriAuthToString hst) c
	       sendHTTP_notify c' r onSendComplete)
	    dbg

updateConnectionPool :: HStream hTy
                     => HandleStream hTy
		     -> BrowserAction (HandleStream hTy) ()
updateConnectionPool c = do
   pool <- gets bsConnectionPool
   let len_pool = length pool
   maxPoolSize <- fromMaybe defaultMaxPoolSize <$> gets bsMaxPoolSize
   when (len_pool > maxPoolSize)
        (liftIO $ close (last pool))
   let pool' 
	| len_pool > maxPoolSize = init pool
	| otherwise              = pool
   when (maxPoolSize > 0) $ modify (\b -> b { bsConnectionPool=c:pool' })
   return ()
                             
-- | Default maximum number of open connections we are willing to have active.
defaultMaxPoolSize :: Int
defaultMaxPoolSize = 5

handleCookies :: URI -> String -> [Header] -> BrowserAction t ()
handleCookies _   _              [] = return () -- cut short the silliness.
handleCookies uri dom cookieHeaders = do
  when (not $ null errs)
       (err $ unlines ("Errors parsing these cookie values: ":errs))
  when (not $ null newCookies)
       (out $ foldl (\x y -> x ++ "\n  " ++ show y) "Cookies received:" newCookies)
  filterfn    <- getCookieFilter
  newCookies' <- liftIO (filterM (filterfn uri) newCookies)
  when (not $ null newCookies')
       (out $ "Accepting cookies with names: " ++ unwords (map ckName newCookies'))
  mapM_ addCookie newCookies'
 where
  (errs, newCookies) = processCookieHeaders dom cookieHeaders

------------------------------------------------------------------
----------------------- Miscellaneous ----------------------------
------------------------------------------------------------------

allowRedirect :: RequestState -> BrowserAction t Bool
allowRedirect rqState = do
  rd <- getAllowRedirects
  mbMxRetries <- getMaxRedirects
  return (rd && (reqRedirects rqState <= fromMaybe defaultMaxRetries mbMxRetries))

-- | Return @True@ iff the package is able to handle requests and responses
-- over it.
supportedScheme :: URI -> Bool
supportedScheme u = uriScheme u == "http:"

-- | @uriDefaultTo a b@ returns a URI that is consistent with the first
-- argument URI @a@ when read in the context of the second URI @b@.
-- If the second argument is not sufficient context for determining
-- a full URI then anarchy reins.
uriDefaultTo :: URI -> URI -> URI
#if MIN_VERSION_network(2,4,0)
uriDefaultTo a b = a `relativeTo` b
#else
uriDefaultTo a b = maybe a id (a `relativeTo` b)
#endif


-- This form junk is completely untested...

type FormVar = (String,String)

data Form = Form RequestMethod URI [FormVar]

formToRequest :: Form -> Request_String
formToRequest (Form m u vs) =
    let enc = urlEncodeVars vs
    in case m of
        GET -> Request { rqMethod=GET
                       , rqHeaders=[ Header HdrContentLength "0" ]
                       , rqBody=""
                       , rqURI=u { uriQuery= '?' : enc }  -- What about old query?
                       }
        POST -> Request { rqMethod=POST
                        , rqHeaders=[ Header HdrContentType "application/x-www-form-urlencoded",
                                      Header HdrContentLength (show $ length enc) ]
                        , rqBody=enc
                        , rqURI=u
                        }
        _ -> error ("unexpected request: " ++ show m)


