-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Browser
-- Copyright   :  (c) Warrick Gray 2002
-- License     :  BSD
-- 
-- Maintainer  :  Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- An HTTP\/1.1 compatible wrapper for the HTTP module.
-----------------------------------------------------------------------------
 
{-
  Changes by Robin Bate Boerop <robin@bateboerop.name>:
   - Made dependencies explicit in import statements.
   - Added type signatures.
   - Imported new StreamDebugger module.

  Change Log:
   - altered 'closeTCP' to 'close', for consistency with altered HTTP
   - added debugging settings to browser.

  To Do: 
   - testing!!!
   - remove BrowserAction type? Possibly replace with IORef?
   - (more todo's in the HTTP mod)

-}

module Network.Browser (
    BrowserState,
    BrowserAction,      -- browser monad, effectively a state monad.
    Cookie,
    Form(..),
    Proxy(..),
    
    browse,             -- BrowserAction a -> IO a
    request,            -- Request -> BrowserAction Response
    
    getBrowserState,
    withBrowserState,
    
    setAllowRedirects,
    getAllowRedirects,
    
    Authority(..),
    getAuthorities, 
    setAuthorities, 
    addAuthority, 
    getAuthorityGen, 
    setAuthorityGen, 
    setAllowBasicAuth,

    setCookieFilter,
    defaultCookieFilter,
    userCookieFilter,
    
    getCookies,
    setCookies,
    addCookie,

    setErrHandler,         -- :: (String -> IO ()) -> BrowserAction t ()
    setOutHandler,         -- :: (String -> IO ()) -> BrowserAction t ()
    
    setEventHandler,         -- :: (BrowserEvent t -> BrowserAction t ()) -> BrowserAction t ()
    
    BrowserEvent(..),
    BrowserEventType(..),
    RequestID,

    setProxy,

    setDebugLog,

    out,
    err,
    ioAction,           -- :: IO a -> BrowserAction a

    defaultGETRequest,
    defaultGETRequest_,
    formToRequest,
    uriDefaultTo,
    uriTrimHost
) where

import Network.URI
   ( URI(uriAuthority, uriScheme, uriPath, uriQuery)
   , URIAuth(..)
   , parseURI, parseURIReference, relativeTo
   )
import Network.StreamDebugger (debugByteStream)
import Network.HTTP
import qualified Network.HTTP.MD5 as MD5 (hash)
import qualified Network.HTTP.Base64 as Base64 (encode)
import Network.Stream ( ConnError(..), Result )
import Network.BufferType

import Network.HTTP.Utils ( trim, splitBy )

import Data.Char (toLower,isAlphaNum,isSpace)
import Data.List (isPrefixOf,isSuffixOf)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes, fromJust, isJust)
import Control.Monad (foldM, filterM, liftM, when)
import Text.ParserCombinators.Parsec
   ( Parser, char, many, many1, satisfy, parse, option, try
   , (<|>), spaces, sepBy1
   )
import qualified System.IO
   ( hSetBuffering, hPutStr, stdout, stdin, hGetChar
   , BufferMode(NoBuffering, LineBuffering)
   )
import System.Time ( ClockTime, getClockTime )

import Data.Word (Word8)

type Octet = Word8

------------------------------------------------------------------
----------------------- Miscellaneous ----------------------------
------------------------------------------------------------------

word, quotedstring :: Parser String
quotedstring =
    do { char '"'  -- "
       ; str <- many (satisfy $ not . (=='"'))
       ; char '"'
       ; return str
       }

word = many1 (satisfy (\x -> isAlphaNum x || x=='_' || x=='.' || x=='-' || x==':'))


-- | Returns a URI that is consistent with the first
-- argument uri when read in the context of a second.
-- If second argument is not sufficient context for
-- determining a full URI then anarchy reins.
uriDefaultTo :: URI -> URI -> URI
uriDefaultTo a b = maybe a id (a `relativeTo` b)

uriTrimHost :: URI -> URI
uriTrimHost uri = uri { uriScheme="", uriAuthority=Nothing }

------------------------------------------------------------------
----------------------- Cookie Stuff -----------------------------
------------------------------------------------------------------

-- Some conventions: 
--     assume ckDomain is lowercase
--
data Cookie 
 = MkCookie 
    { ckDomain  :: String
    , ckName    :: String
    , ckValue   :: String
    , ckPath    :: Maybe String
    , ckComment :: Maybe String
    , ckVersion :: Maybe String
    }
    deriving(Show,Read)


instance Eq Cookie where
    a == b  =  ckDomain a == ckDomain b 
            && ckName a == ckName b 
            && ckPath a == ckPath b



defaultCookieFilter :: URI -> Cookie -> IO Bool
defaultCookieFilter _url _cky = return True

userCookieFilter :: URI -> Cookie -> IO Bool
userCookieFilter url cky =
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
       


-- | Serialise a Cookie for inclusion in a request.
cookieToHeader :: Cookie -> Header
cookieToHeader ck = Header HdrCookie text
    where
        path = maybe "" (";$Path="++) (ckPath ck)
        text = "$Version=" ++ fromMaybe "0" (ckVersion ck)
             ++ ';' : ckName ck ++ "=" ++ ckValue ck ++ path
             ++ (case ckPath ck of
                     Nothing -> ""
                     Just x  -> ";$Path=" ++ x)
             ++ ";$Domain=" ++ ckDomain ck


{- replace "error" call with [] in final version? -}
headerToCookies :: String -> Header -> [Cookie]
headerToCookies dom (Header HdrSetCookie val) = 
    case parse cookies "" val of
        Left e  -> error ("Cookie parse failure on: " ++ val ++ " " ++ show e) 
        Right x -> x
    where
        cookies :: Parser [Cookie]
        cookies = sepBy1 cookie (char ',')

        cookie :: Parser Cookie
        cookie =
            do { name <- word
               ; spaces_l
               ; char '='
               ; spaces_l
               ; val1 <- cvalue
               ; args <- cdetail
               ; return $ mkCookie name val1 args
               }

        cvalue :: Parser String
        
        spaces_l = many (satisfy isSpace)

        cvalue = quotedstring <|> many1 (satisfy $ not . (==';')) <|> return ""
       
        -- all keys in the result list MUST be in lower case
        cdetail :: Parser [(String,String)]
        cdetail = many $
            try (do { spaces_l
               ; char ';'
               ; spaces_l
               ; s1 <- word
               ; spaces_l
               ; s2 <- option "" (do { char '=' ; spaces_l ; v <- cvalue ; return v })
               ; return (map toLower s1,s2)
               })

        mkCookie :: String -> String -> [(String,String)] -> Cookie
        mkCookie nm cval more = 
	  MkCookie { ckName    = nm
                   , ckValue   = cval
                   , ckDomain  = map toLower (fromMaybe dom (lookup "domain" more))
                   , ckPath    = lookup "path" more
                   , ckVersion = lookup "version" more
                   , ckComment = lookup "comment" more
                   }

headerToCookies _ _ = []

      

-- | Adds a cookie to the browser state, removing duplicates.
addCookie :: Cookie -> BrowserAction t ()
addCookie c = alterBS (\b -> b { bsCookies=c : fn (bsCookies b) })
    where
        fn = filter (not . (==c))

setCookies :: [Cookie] -> BrowserAction t ()
setCookies cs = alterBS (\b -> b { bsCookies=cs })

getCookies :: BrowserAction t [Cookie]
getCookies = getBS bsCookies

-- ...get domain specific cookies...
-- ... this needs changing for consistency with rfc2109...
-- ... currently too broad.
getCookiesFor :: String -> String -> BrowserAction t [Cookie]
getCookiesFor dom path =
    do cks <- getCookies
       return (filter cookiematch cks)
    where
        cookiematch :: Cookie -> Bool
        cookiematch ck = ckDomain ck `isSuffixOf` dom
                      && case ckPath ck of
                             Nothing -> True
                             Just p  -> p `isPrefixOf` path
      

setCookieFilter :: (URI -> Cookie -> IO Bool) -> BrowserAction t ()
setCookieFilter f = alterBS (\b -> b { bsCookieFilter=f })

getCookieFilter :: BrowserAction t (URI -> Cookie -> IO Bool)
getCookieFilter = getBS bsCookieFilter

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


data Algorithm = AlgMD5 | AlgMD5sess
    deriving(Eq)

instance Show Algorithm where
    show AlgMD5 = "md5"
    show AlgMD5sess = "md5-sess"


data Qop = QopAuth | QopAuthInt
    deriving(Eq,Show)


data Challenge = ChalBasic  { chRealm   :: String }
               | ChalDigest { chRealm   :: String
                            , chDomain  :: [URI]
                            , chNonce   :: String
                            , chOpaque  :: Maybe String
                            , chStale   :: Bool
                            , chAlgorithm ::Maybe Algorithm
                            , chQop     :: [Qop]
                            }

-- | Convert WWW-Authenticate header into a Challenge object
headerToChallenge :: URI -> Header -> Maybe Challenge
headerToChallenge baseURI (Header _ str) =
    case parse challenge "" str of
        Left{} -> Nothing
        Right (name,props) -> case name of
            "basic"  -> mkBasic props
            "digest" -> mkDigest props
            _        -> Nothing
    where
        challenge :: Parser (String,[(String,String)])
        challenge =
            do { nme <- word
               ; spaces
               ; pps <- cprops
               ; return (map toLower nme,pps)
               }

        cprops = sepBy1 cprop comma

        comma = do { spaces ; char ',' ; spaces }

        cprop =
            do { nm <- word
               ; char '='
               ; val <- quotedstring
               ; return (map toLower nm,val)
               }

        mkBasic, mkDigest :: [(String,String)] -> Maybe Challenge

        mkBasic params = fmap ChalBasic (lookup "realm" params)

        mkDigest params =
            -- with Maybe monad
            do { r <- lookup "realm" params
               ; n <- lookup "nonce" params
               ; return $ 
                    ChalDigest { chRealm  = r
                               , chDomain = (annotateURIs 
                                            $ map parseURI
                                            $ words 
                                            $ fromMaybe [] 
                                            $ lookup "domain" params)
                               , chNonce  = n
                               , chOpaque = lookup "opaque" params
                               , chStale  = "true" == (map toLower
                                           $ fromMaybe "" (lookup "stale" params))
                               , chAlgorithm= readAlgorithm (fromMaybe "MD5" $ lookup "algorithm" params)
                               , chQop    = readQop (fromMaybe "" $ lookup "qop" params)
                               }
               }

        annotateURIs :: [Maybe URI] -> [URI]
        annotateURIs = (map (\u -> fromMaybe u (u `relativeTo` baseURI))) . catMaybes

        -- Change These:
        readQop :: String -> [Qop]
        readQop = catMaybes . (map strToQop) . (splitBy ',')

        strToQop qs = case map toLower (trim qs) of
            "auth"     -> Just QopAuth
            "auth-int" -> Just QopAuthInt
            _          -> Nothing

        readAlgorithm astr = case map toLower (trim astr) of
            "md5"      -> Just AlgMD5
            "md5-sess" -> Just AlgMD5sess
            _          -> Nothing


data Authority = AuthBasic { auRealm    :: String
                           , auUsername :: String
                           , auPassword :: String
                           , auSite     :: URI
                           }
               | AuthDigest { auRealm     :: String
                            , auUsername  :: String
                            , auPassword  :: String
                            , auNonce     :: String
                            , auAlgorithm :: Maybe Algorithm
                            , auDomain    :: [URI]
                            , auOpaque    :: Maybe String
                            , auQop       :: [Qop]
                            }


-- | Return authorities for a given domain and path.
-- Assumes "dom" is lower case
getAuthFor :: String -> String -> BrowserAction t [Authority]
getAuthFor dom pth =
    do { list <- getAuthorities
       ; return (filter match list)
       }
    where
        match :: Authority -> Bool
        match (AuthBasic _ _ _ s) = matchURI s
        match (AuthDigest _ _ _ _ _ ds _ _) = or (map matchURI ds)            

        matchURI :: URI -> Bool
        matchURI s = (uriToAuthorityString s == dom) && (uriPath s `isPrefixOf` pth)
    

-- | Interacting with browser state:
getAuthorities :: BrowserAction t [Authority]
getAuthorities = getBS bsAuthorities

setAuthorities :: [Authority] -> BrowserAction t ()
setAuthorities as = alterBS (\b -> b { bsAuthorities=as })

addAuthority :: Authority -> BrowserAction t ()
addAuthority a = alterBS (\b -> b { bsAuthorities=a:bsAuthorities b })

getAuthorityGen :: BrowserAction t (URI -> String -> IO (Maybe (String,String)))
getAuthorityGen = getBS bsAuthorityGen

setAuthorityGen :: (URI -> String -> IO (Maybe (String,String))) -> BrowserAction t ()
setAuthorityGen f = alterBS (\b -> b { bsAuthorityGen=f })

setAllowBasicAuth :: Bool -> BrowserAction t ()
setAllowBasicAuth ba = alterBS (\b -> b { bsAllowBasicAuth=ba })




-- TO BE CHANGED!!!
pickChallenge :: [Challenge] -> Maybe Challenge
pickChallenge = listToMaybe



-- | Retrieve a likely looking authority for a Request.
anticipateChallenge :: Request ty -> BrowserAction t (Maybe Authority)
anticipateChallenge rq =
    let uri = rqURI rq in
    do { authlist <- getAuthFor (uriAuthToString $ reqURIAuth rq) (uriPath uri)
       ; return (listToMaybe authlist)
       }


-- | Asking the user to respond to a challenge
challengeToAuthority :: URI -> Challenge -> BrowserAction t (Maybe Authority)
challengeToAuthority uri ch =
    -- prompt user for authority
    if answerable ch then
        do { prompt <- getAuthorityGen
           ; userdetails <- ioAction $ prompt uri (chRealm ch)
           ; case userdetails of
               Nothing    -> return Nothing
               Just (u,p) -> return (Just $ buildAuth ch u p)
           }
    else return Nothing
    where
        answerable :: Challenge -> Bool
        answerable (ChalBasic _) = True
        answerable chall         = (chAlgorithm chall) == Just AlgMD5

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


-- | Generating a credentials value from an Authority, in
-- the context of a specific request.  If a client nonce
-- was to be used then this function might need to
-- be of type ... -> BrowserAction String
withAuthority :: Authority -> Request ty -> String
withAuthority a rq = case a of
        AuthBasic{}  -> "Basic " ++ base64encode (auUsername a ++ ':' : auPassword a)
        AuthDigest{} ->
            "Digest username=\"" ++ auUsername a -- "
              ++ "\",realm=\"" ++ auRealm a
              ++ "\",nonce=\"" ++ auNonce a
              ++ "\",uri=\"" ++ digesturi
              ++ ",response=\"" ++ rspdigest 
              ++ "\""
              -- plus optional stuff:
              ++ ( if isJust (auAlgorithm a) then "" else ",algorithm=\"" ++ show (fromJust $ auAlgorithm a) ++ "\"" )
              ++ ( if isJust (auOpaque a) then "" else ",opaque=\"" ++ (fromJust $ auOpaque a) ++ "\"" )
              ++ ( if null (auQop a) then "" else ",qop=auth" )
    where
        rspdigest = "\"" 
                 ++ map toLower (kd (md5 a1) (noncevalue ++ ":" ++ md5 a2))
                 ++ "\""

        a1, a2 :: String
        a1 = auUsername a ++ ":" ++ auRealm a ++ ":" ++ auPassword a
        
        {-
        If the "qop" directive's value is "auth" or is unspecified, then A2
        is:
           A2  = Method ":" digest-uri-value
        If the "qop" value is "auth-int", then A2 is:
           A2  = Method ":" digest-uri-value ":" H(entity-body)
        -}
        a2 = show (rqMethod rq) ++ ":" ++ digesturi

        digesturi = show (rqURI rq)
        noncevalue = auNonce a

-- FIXME: these probably only work right for latin-1 strings
stringToOctets :: String -> [Octet]
stringToOctets = map (fromIntegral . fromEnum)

octetsToString :: [Octet] -> String
octetsToString = map (toEnum . fromIntegral)

base64encode :: String -> String
base64encode = Base64.encode . stringToOctets

md5 :: String -> String
md5 = octetsToString . MD5.hash . stringToOctets

kd :: String -> String -> String
kd a b = md5 (a ++ ":" ++ b)


------------------------------------------------------------------
------------------ Proxy Stuff -----------------------------------
------------------------------------------------------------------

-- | Specifies if a proxy should be used for the request.
data Proxy = NoProxy -- ^ Don't use a proxy.
           | Proxy String (Maybe Authority) -- ^ Use the proxy given. Should be of the form "http:\/\/host:port", "host", "host:port", or "http:\/\/host"


------------------------------------------------------------------
------------------ Browser State Actions -------------------------
------------------------------------------------------------------


data BrowserState connection
 = BS { bsErr, bsOut     :: String -> IO ()
      , bsCookies        :: [Cookie]
      , bsCookieFilter   :: URI -> Cookie -> IO Bool
      , bsAuthorityGen   :: URI -> String -> IO (Maybe (String,String))
      , bsAuthorities    :: [Authority]
      , bsAllowRedirects :: Bool
      , bsAllowBasicAuth :: Bool
      , bsConnectionPool :: [connection]
      , bsProxy          :: Proxy
      , bsDebug          :: Maybe String
      , bsEvent          :: Maybe (BrowserEvent connection -> BrowserAction connection ())
      , bsRequestID      :: RequestID
      }

instance Show (BrowserState t) where
    show bs =  "BrowserState { " 
            ++ shows (bsCookies bs) ("\n"
           {- ++ show (bsAuthorities bs) ++ "\n"-}
            ++ "AllowRedirects: " ++ shows (bsAllowRedirects bs) "} ")


-- Simple DIY stateful behaviour, with IO
data BrowserAction conn a 
 = BA { lift :: BrowserState conn -> IO (BrowserState conn,a) }

instance Monad (BrowserAction conn) where
    a >>= f  =  BA (\b -> do { (nb,v) <- lift a b ; lift (f v) nb})
    return x =  BA (\b -> return (b,x))

instance Functor (BrowserAction conn) where
    fmap f   = liftM f

-- | Apply a browser action to a state.
browse :: BrowserAction conn a -> IO a
browse act = do x <- lift act defaultBrowserState
                return (snd x)

defaultBrowserState :: BrowserState t
defaultBrowserState = res
 where
   res = BS
     { bsErr              = putStrLn
     , bsOut              = putStrLn
     , bsCookies          = []
     , bsCookieFilter     = defaultCookieFilter
     , bsAuthorityGen     = \ _uri _realm -> 
          (bsErr res) ("No action for prompting/generating user+password credentials provided (use: setAuthorityGen); returning Nothing") >> return Nothing
     , bsAuthorities      = []
     , bsAllowRedirects   = True
     , bsAllowBasicAuth   = False
     , bsConnectionPool   = []
     , bsProxy            = NoProxy
     , bsDebug            = Nothing 
     , bsEvent            = Nothing
     , bsRequestID        = 0
     }

-- | Alter browser state
alterBS :: (BrowserState t -> BrowserState t) -> BrowserAction t ()
alterBS f = BA (\b -> return (f b,()))

getBS :: (BrowserState t -> a) -> BrowserAction t a
getBS f = BA (\b -> return (b,f b))

getBrowserState :: BrowserAction t (BrowserState t)
getBrowserState = getBS id

withBrowserState :: BrowserState t -> BrowserAction t a -> BrowserAction t a
withBrowserState bs act = BA $ \ _ -> lift act bs

newRequest :: BrowserAction t a -> BrowserAction t a
newRequest act = do
  let updReqID st = 
       let 
        rid = 1 + bsRequestID st
       in
       rid `seq` st{bsRequestID=rid}
  alterBS updReqID
  act

-- | Do an io action
ioAction :: IO a -> BrowserAction t a
ioAction a = BA (\b -> a >>= \v -> return (b,v))

-- Stream handlers
setErrHandler, setOutHandler :: (String -> IO ()) -> BrowserAction t ()
setErrHandler h = alterBS (\b -> b { bsErr=h })
setOutHandler h = alterBS (\b -> b { bsOut=h })

out, err :: String -> BrowserAction t ()
out s = do { f <- getBS bsOut ; ioAction $ f s }
err s = do { f <- getBS bsErr ; ioAction $ f s }

-- Redirects
setAllowRedirects :: Bool -> BrowserAction t ()
setAllowRedirects bl = alterBS (\b -> b {bsAllowRedirects=bl})

getAllowRedirects :: BrowserAction t Bool
getAllowRedirects = getBS bsAllowRedirects


-- Proxy
setProxy :: Proxy -> BrowserAction t ()
setProxy p = alterBS (\b -> b {bsProxy = p})

getProxy :: BrowserAction t Proxy
getProxy = getBS bsProxy


-- Debug
setDebugLog :: Maybe String -> BrowserAction t ()
setDebugLog v = alterBS (\b -> b {bsDebug=v})


-- Page control
data RequestState 
  = RequestState
      { reqDenies     :: Int   -- ^ number of 401 responses so far
      , reqRedirects  :: Int   -- ^ number of redirects so far
      , reqRetries    :: Int   -- ^ number of retrys so far
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

-- | 'BrowserEvent' is the event record type that a user-defined handler, set
-- via 'setEventHandler', will be passed. It indicates various state changes
-- in the processing of a given Request ID.
data BrowserEvent ty
 = BrowserEvent
      { browserTimestamp  :: ClockTime
      , browserRequestID  :: RequestID
      , browserRequestURI :: {-URI-}String
      , browserEventType  :: BrowserEventType ty
      }

-- | 'BrowserEventType' is the enumerated list of events that the browser
-- internals will report to a user-defined event handler.
data BrowserEventType ty
 = OpenConnection
 | ReuseConnection
 | RequestSent
{- not yet, you will have to determine these via the ResponseEnd event.
 | Redirect
 | AuthChallenge
 | AuthResponse
-}
 | ResponseEnd ResponseData
 | ResponseFinish
 
setEventHandler :: (BrowserEvent ty -> BrowserAction ty ()) -> BrowserAction ty ()
setEventHandler h = alterBS (\b -> b { bsEvent=Just h})

buildBrowserEvent :: BrowserEventType t -> {-URI-}String -> RequestID -> IO (BrowserEvent t)
buildBrowserEvent bt uri reqID = do
  ct <- getClockTime
  return BrowserEvent 
      { browserTimestamp  = ct
      , browserRequestID  = reqID
      , browserRequestURI = uri
      , browserEventType  = bt
      }

reportEvent :: BrowserEventType t -> {-URI-}String -> BrowserAction t ()
reportEvent bt uri = do
  st <- getBrowserState
  case bsEvent st of
    Nothing -> return ()
    Just evH -> do
       evt <- ioAction $ buildBrowserEvent bt uri (bsRequestID st)
       evH evt -- if it fails, we fail.

-- limits we are willing to not go beyond for method retries and number of auth deny responses.
maxRetries :: Int
maxRetries = 4

maxDenies :: Int
maxDenies = 2

-- Surely the most important bit:
request :: HStream ty
        => Request ty
	-> BrowserAction (HandleStream ty) (URI,Response ty)
request req = newRequest $ do
                 res <- request' nullVal initialState req
		 reportEvent ResponseFinish (show (rqURI req))
		 return res
  where
   initialState = nullRequestState
   nullVal      = buf_empty bufferOps

request' :: HStream ty
         => ty
	 -> RequestState
	 -> Request ty
	 -> BrowserAction (HandleStream ty) (URI,Response ty)
request' nullVal rqState rq = do
     -- add cookies to request
   let uri = rqURI rq
   cookies <- getCookiesFor (uriAuthToString $ reqURIAuth rq) (uriPath uri)
   when (not $ null cookies) 
        (out $ "Adding cookies to request.  Cookie names: "  ++
               foldl spaceappend "" (map ckName cookies))
    -- add credentials to request
   rq' <- 
    if not (reqStopOnDeny rqState) 
     then return rq 
     else do 
       auth <- anticipateChallenge rq
       case auth of
         Nothing -> return rq
         Just x  -> return (insertHeader HdrAuthorization (withAuthority x rq) rq)
   let rq'' = insertHeaders (map cookieToHeader cookies) rq'
   p <- getProxy
   let rq_to_go = normalizeRequestURI False{-no close-}
                                      (uriToAuthorityString $ rqURI rq'')
				      rq''
   out ("Sending:\n" ++ show rq_to_go) 
   e_rsp <- 
     case p of
       NoProxy       -> dorequest (reqURIAuth rq'') rq_to_go
       Proxy str ath -> do
            -- note: don't split off the authority for proxies..
          let rq_to_go' = maybe rq''
	                    (\x -> insertHeader HdrProxyAuthorization
			                        (withAuthority x rq'') rq'')
		           ath
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
          dorequest proxyURIAuth rq_to_go'
   case e_rsp of
    Left v 
     | (reqRetries rqState < maxRetries) && (v == ErrorReset || v == ErrorClosed) ->
       request' nullVal rqState{reqRetries=reqRetries rqState + 1} rq
     | otherwise -> error ("Exception raised in request: " ++ show v)
    Right rsp -> do 
     out ("Received:\n" ++ show rsp)
      -- add new cookies to browser state
     let cookieheaders = retrieveHeaders HdrSetCookie rsp
     let newcookies = concat (map (headerToCookies $ uriAuthToString $ reqURIAuth rq) cookieheaders)

     when (not $ null newcookies)
          (out $ foldl (\x y -> x ++ "\n  " ++ show y) "Cookies received:" newcookies)
               
     filterfn <- getCookieFilter
     newcookies' <- ioAction (filterM (filterfn uri) newcookies)
     foldM (\_ -> addCookie) () newcookies'

     when (not $ null newcookies)
          (out $ "Accepting cookies with names: " ++ foldl spaceappend "" (map ckName newcookies'))
       
     case rspCode rsp of
      (4,0,1) -- Credentials not sent or refused.
        | reqDenies rqState > maxDenies -> do
          out "401 - credentials again refused; exceeded retry count (2)"
	  return (uri,rsp)
	| otherwise -> do
          out "401 - credentials not supplied or refused; retrying.."
          let hdrs = retrieveHeaders HdrWWWAuthenticate rsp
          case pickChallenge (catMaybes $ map (headerToChallenge uri) hdrs) of
            Nothing -> return (uri,rsp)   {- do nothing -}
            Just x  -> do
              au <- challengeToAuthority uri x
              case au of
                Nothing  -> return (uri,rsp)   {- do nothing -}
                Just au' -> do
                  out "Retrying request with new credentials"
		  request' nullVal
			   rqState{reqDenies=reqDenies rqState + 1, reqStopOnDeny=False}
                           (insertHeader HdrAuthorization (withAuthority au' rq) rq)

      (4,0,7)  -- Proxy Authentication required
        | reqDenies rqState > maxDenies -> do
          out "407 - proxy authentication required; max deny count exceeeded (2)"
          return (uri,rsp)
        | otherwise -> do
          out "407 - proxy authentication required"
          let hdrs = retrieveHeaders HdrProxyAuthenticate rsp
          case pickChallenge (catMaybes $ map (headerToChallenge uri) hdrs) of
            Nothing -> return (uri,rsp)   {- do nothing -}
            Just x  -> do
              au <- challengeToAuthority uri x
              case au of
               Nothing  -> return (uri,rsp)   {- do nothing -}
               Just au' -> do
                 pxy <- getBS bsProxy
                 case pxy of
                   NoProxy -> do
                     err "Proxy authentication required without proxy!"
                     return (uri,rsp)
                   Proxy px _ -> do
                     out "Retrying with proxy authentication"
                     setProxy (Proxy px (Just au'))
                     request' nullVal
			      rqState{reqDenies=reqDenies rqState + 1, reqStopOnDeny=False}
			      rq

      (3,0,x) | x == 3 || x == 2 ->  do -- Redirect using GET request method.
        out ("30" ++ show x ++  " - redirect using GET")
        rd <- getAllowRedirects
        if not rd || reqRedirects rqState > maxRetries 
	 then return (uri,rsp)
	 else 
          case retrieveHeaders HdrLocation rsp of
           [] -> do 
	     err "No Location header in redirect response"
             return (uri,rsp)
           (Header _ u:_) -> 
	     case parseURIReference u of
               Nothing -> do
                 err ("Parse of Location header in a redirect response failed: " ++ u)
                 return (uri,rsp)
               Just newuri -> do
	         out ("Redirecting to " ++ show newuri' ++ " ...") 
		 let rq1 = rq { rqMethod=GET, rqURI=newuri', rqBody=nullVal }
                 request' nullVal
			  rqState{reqDenies=0, reqRedirects=reqRedirects rqState + 1, reqStopOnDeny=True}
                          (replaceHeader HdrContentLength "0" rq1)
                where
                  newuri' = maybe newuri id (newuri `relativeTo` uri)

      (3,0,5) ->
        case retrieveHeaders HdrLocation rsp of
         [] -> do 
	   err "No Location header in proxy redirect response."
           return (uri,rsp)
         (Header _ u:_) -> 
	   case parseURIReference u of
            Nothing -> do
             err ("Parse of Location header in a proxy redirect response failed: " ++ u)
             return (uri,rsp)
            Just newuri -> do
             out ("Retrying with proxy " ++ show newuri ++ "...")
             setProxy (Proxy (uriToAuthorityString newuri) Nothing)
             request' nullVal rqState{ reqDenies=0
	                             , reqRedirects=0
				     , reqRetries=reqRetries rqState + 1
				     , reqStopOnDeny=True
				     }
				     rq
      (3,_,_) ->  redirect uri rsp
      _       -> return (uri,rsp)

   where      
     redirect uri rsp = do
       rd <- getAllowRedirects
       if not rd || reqRedirects rqState > maxRetries
        then return (uri,rsp) 
	else do
         case retrieveHeaders HdrLocation rsp of
          [] -> do 
	    err "No Location header in redirect response."
            return (uri,rsp)
          (Header _ u:_) -> 
	    case parseURIReference u of
              Just newuri -> do
                let newuri' = maybe newuri id (newuri `relativeTo` uri)
                out ("Redirecting to " ++ show newuri' ++ " ...") 
                request' nullVal
		         rqState{reqDenies=0, reqRedirects=reqRedirects rqState + 1, reqStopOnDeny=True}
		         rq{rqURI=newuri'}
              Nothing -> do
                err ("Parse of Location header in a redirect response failed: " ++ u)
                return (uri,rsp)

spaceappend :: String -> String -> String
spaceappend x y = x ++ ' ' : y


dorequest :: (HStream ty)
          => URIAuth
	  -> Request ty
	  -> BrowserAction (HandleStream ty)
	                   (Result (Response ty))
dorequest hst rqst = 
            do { pool <- getBS bsConnectionPool
               ; conn <- ioAction $ filterM (\c -> c `isTCPConnectedTo` uriAuthToString hst) pool
               ; rsp <- case conn of
                    [] -> do { out ("Creating new connection to " ++ uriAuthToString hst)
                             ; let aport = case uriPort hst of
                                            (':':s) -> 
					      case reads s of { ((v,_):_) -> v ; _ -> 80}
                                            _       -> 80
		             ; reportEvent OpenConnection (show (rqURI rqst))
                             ; c <- ioAction $ openStream (uriRegName hst) aport
			     ; let len_pool = length pool
                             ; when (len_pool > 5)
                                    (ioAction $ close (last pool))
                             ; let pool' 
			            | len_pool > 5 = init pool
				    | otherwise    = pool
                             ; alterBS (\b -> b { bsConnectionPool=c:pool' })
                             ; dorequest2 c rqst
                             }
                    (c:_) ->
                        do { out ("Recovering connection to " ++ uriAuthToString hst)
			   ; reportEvent ReuseConnection (show (rqURI rqst))
                           ; dorequest2 c rqst
                           }
               ; 
	       ; case rsp of { Right (Response a b c _) -> reportEvent (ResponseEnd (a,b,c)) (show (rqURI rqst)) ; _ -> return ()}
               ; return rsp
               }
  where
   dorequest2 c r = do
     dbg <- getBS bsDebug
     st  <- getBrowserState
     onSendComplete <- 
        case bsEvent st of
	  Nothing  -> return (return ())
	  Just evh -> return $ do
	               x <- buildBrowserEvent RequestSent (show (rqURI r)) (bsRequestID st)
		       (lift (evh x)) st
		       return ()
     ioAction $ 
       case dbg of
         Nothing -> sendHTTP_notify c r onSendComplete
         Just f  -> do
	   c' <- debugByteStream (f++'-': uriAuthToString hst) c
	   sendHTTP_notify c' r onSendComplete


reqURIAuth :: Request ty -> URIAuth
reqURIAuth req = 
  case uriAuthority (rqURI req) of
    Just ua -> ua
    _ -> case lookupHeader HdrHost (rqHeaders req) of
           Nothing -> error ("reqURIAuth: no URI authority for: " ++ show req)
	   Just h  -> URIAuth { uriUserInfo = ""
	                      , uriRegName  = h
			      , uriPort     = ""
			      }

------------------------------------------------------------------
------------------ Request Building ------------------------------
------------------------------------------------------------------

libUA :: String
libUA = "hs-HTTP/4.0.3"

defaultGETRequest :: URI -> Request_String
defaultGETRequest uri = defaultGETRequest_ uri


defaultGETRequest_ :: BufferType a => URI -> Request a
defaultGETRequest_ uri = req
 where
  empty = buf_empty (toBufOps req)
  
  req = 
    Request { rqURI=uri
            , rqBody=empty
            , rqHeaders=[ Header HdrContentLength "0"
                        , Header HdrUserAgent libUA
                        ]
            , rqMethod=GET
            }

  toBufOps :: BufferType a => Request a -> BufferOp a
  toBufOps _ = bufferOps

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
