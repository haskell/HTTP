-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.Base
-- Copyright   :  (c) Warrick Gray 2002, Bjorn Bringert 2003-2005, 2007 Robin Bate Boerop, 2008 Sigbjorn Finne
-- License     :  BSD
-- 
-- Maintainer  :  Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- Definitions of @Request@ and @Response@ types along with functions
-- for normalizing them. It is assumed to be an internal module; user
-- code should, if possible, import @Network.HTTP@ to access the functionality
-- that this module provides.
--
-- Additionally, the module exports internal functions for working with URLs,
-- and for handling the processing of requests and responses coming back.
--
-----------------------------------------------------------------------------
module Network.HTTP.Base
       (
          -- ** Constants
         httpVersion                 -- :: String

          -- ** HTTP
       , Request(..)
       , Response(..)
       , RequestMethod(..)
       
       , Request_String
       , Response_String
       , HTTPRequest
       , HTTPResponse
       
          -- ** URL Encoding
       , urlEncode
       , urlDecode
       , urlEncodeVars

          -- ** URI authority parsing
       , URIAuthority(..)
       , parseURIAuthority
       
          -- internal
       , uriToAuthorityString   -- :: URI     -> String
       , uriAuthToString        -- :: URIAuth -> String
       , uriAuthPort            -- :: Maybe URI -> URIAuth -> Int
       , reqURIAuth             -- :: Request ty -> URIAuth

       , parseResponseHead      -- :: [String] -> Result ResponseData
       , parseRequestHead       -- :: [String] -> Result RequestData

       , ResponseNextStep(..)
       , matchResponse
       , ResponseData
       , ResponseCode
       , RequestData
       
       , NormalizeRequestOptions(..) 
       , defaultNormalizeRequestOptions -- :: NormalizeRequestOptions ty
       , RequestNormalizer

       , normalizeRequest   -- :: NormalizeRequestOptions ty -> Request ty -> Request ty

       , splitRequestURI

       , getAuth
       , normalizeRequestURI
       , normalizeHostHeader
       , findConnClose

         -- internal export (for the use by Network.HTTP.{Stream,ByteStream} )
       , linearTransfer
       , hopefulTransfer
       , chunkedTransfer
       , uglyDeathTransfer
       , readTillEmpty1
       , readTillEmpty2
       
       , defaultGETRequest
       , defaultGETRequest_
       , mkRequest

       , defaultUserAgent
       , libUA  {- backwards compatibility, will disappear..soon -}
       
       , catchIO
       , catchIO_
       , responseParseError
       
       , getRequestVersion
       , getResponseVersion
       , setRequestVersion
       , setResponseVersion
       
       ) where

import Network.URI
   ( URI(uriAuthority, uriPath, uriScheme)
   , URIAuth(URIAuth, uriUserInfo, uriRegName, uriPort)
   , parseURIReference
   )

import Control.Monad ( guard )
import Control.Monad.Error ()
import Data.Char     ( digitToInt, intToDigit, toLower, isDigit,
                       isAscii, isAlphaNum )
import Data.List     ( partition, find )
import Data.Maybe    ( listToMaybe, fromMaybe )
import Numeric       ( readHex )

import Network.Stream
import Network.BufferType ( BufferOp(..), BufferType(..) )
import Network.HTTP.Headers
import Network.HTTP.Utils ( trim, crlf, sp, readsOne )

import Text.Read.Lex (readDecP)
import Text.ParserCombinators.ReadP
   ( ReadP, readP_to_S, char, (<++), look, munch )

import Control.Exception as Exception (IOException)

-----------------------------------------------------------------
------------------ URI Authority parsing ------------------------
-----------------------------------------------------------------

data URIAuthority = URIAuthority { user :: Maybe String, 
				   password :: Maybe String,
				   host :: String,
				   port :: Maybe Int
				 } deriving (Eq,Show)

-- | Parse the authority part of a URL.
--
-- > RFC 1732, section 3.1:
-- >
-- >       //<user>:<password>@<host>:<port>/<url-path>
-- >  Some or all of the parts "<user>:<password>@", ":<password>",
-- >  ":<port>", and "/<url-path>" may be excluded.
parseURIAuthority :: String -> Maybe URIAuthority
parseURIAuthority s = listToMaybe (map fst (readP_to_S pURIAuthority s))


pURIAuthority :: ReadP URIAuthority
pURIAuthority = do
		(u,pw) <- (pUserInfo `before` char '@') 
			  <++ return (Nothing, Nothing)
		h <- munch (/=':')
		p <- orNothing (char ':' >> readDecP)
		look >>= guard . null 
		return URIAuthority{ user=u, password=pw, host=h, port=p }

pUserInfo :: ReadP (Maybe String, Maybe String)
pUserInfo = do
	    u <- orNothing (munch (`notElem` ":@"))
	    p <- orNothing (char ':' >> munch (/='@'))
	    return (u,p)

before :: Monad m => m a -> m b -> m a
before a b = a >>= \x -> b >> return x

orNothing :: ReadP a -> ReadP (Maybe a)
orNothing p = fmap Just p <++ return Nothing

-- This function duplicates old Network.URI.authority behaviour.
uriToAuthorityString :: URI -> String
uriToAuthorityString u = maybe "" uriAuthToString (uriAuthority u)

uriAuthToString :: URIAuth -> String
uriAuthToString ua = 
  concat [ uriUserInfo ua 
         , uriRegName ua
	 , uriPort ua
	 ]

uriAuthPort :: Maybe URI -> URIAuth -> Int
uriAuthPort mbURI u = 
  case uriPort u of
    (':':s) -> readsOne id (default_port mbURI) s
    _       -> default_port mbURI
 where
  default_port Nothing = default_http
  default_port (Just url) = 
    case map toLower $ uriScheme url of
      "http:" -> default_http
      "https:" -> default_https
        -- todo: refine
      _ -> default_http

  default_http  = 80
  default_https = 443

-- Fish out the authority from a possibly normalized Request, i.e.,
-- the information may either be in the request's URI or inside
-- the Host: header.
reqURIAuth :: Request ty -> URIAuth
reqURIAuth req = 
  case uriAuthority (rqURI req) of
    Just ua -> ua
    _ -> case lookupHeader HdrHost (rqHeaders req) of
           Nothing -> error ("reqURIAuth: no URI authority for: " ++ show req)
	   Just h  -> 
	      case toHostPort h of
	        (ht,p) -> URIAuth { uriUserInfo = ""
	                          , uriRegName  = ht
			          , uriPort     = p
			          }
  where
    -- Note: just in case you're wondering..the convention is to include the ':'
    -- in the port part..
   toHostPort h = break (==':') h

-----------------------------------------------------------------
------------------ HTTP Messages --------------------------------
-----------------------------------------------------------------


-- Protocol version
httpVersion :: String
httpVersion = "HTTP/1.1"


-- | The HTTP request method, to be used in the 'Request' object.
-- We are missing a few of the stranger methods, but these are
-- not really necessary until we add full TLS.
data RequestMethod = HEAD | PUT | GET | POST | DELETE | OPTIONS | TRACE | CONNECT | Custom String
    deriving(Eq)

instance Show RequestMethod where
  show x = 
    case x of
      HEAD     -> "HEAD"
      PUT      -> "PUT"
      GET      -> "GET"
      POST     -> "POST"
      DELETE   -> "DELETE"
      OPTIONS  -> "OPTIONS"
      TRACE    -> "TRACE"
      CONNECT  -> "CONNECT"
      Custom c -> c

rqMethodMap :: [(String, RequestMethod)]
rqMethodMap = [("HEAD",    HEAD),
	       ("PUT",     PUT),
	       ("GET",     GET),
	       ("POST",    POST),
               ("DELETE",  DELETE),
	       ("OPTIONS", OPTIONS),
	       ("TRACE",   TRACE),
	       ("CONNECT", CONNECT)]

-- 
-- for backwards-ish compatibility; suggest
-- migrating to new Req/Resp by adding type param.
-- 
type Request_String  = Request String
type Response_String = Response String

-- Hmm..I really want to use these for the record
-- type, but it will upset codebases wanting to
-- migrate (and live with using pre-HTTPbis versions.)
type HTTPRequest a  = Request  a
type HTTPResponse a = Response a

-- | An HTTP Request.
-- The 'Show' instance of this type is used for message serialisation,
-- which means no body data is output.
data Request a =
     Request { rqURI       :: URI   -- ^ might need changing in future
                                    --  1) to support '*' uri in OPTIONS request
                                    --  2) transparent support for both relative
                                    --     & absolute uris, although this should
                                    --     already work (leave scheme & host parts empty).
             , rqMethod    :: RequestMethod
             , rqHeaders   :: [Header]
             , rqBody      :: a
             }

-- Notice that request body is not included,
-- this show function is used to serialise
-- a request for the transport link, we send
-- the body separately where possible.
instance Show (Request a) where
    show req@(Request u m h _) =
        show m ++ sp ++ alt_uri ++ sp ++ ver ++ crlf
        ++ foldr (++) [] (map show (dropHttpVersion h)) ++ crlf
        where
	    ver = fromMaybe httpVersion (getRequestVersion req)
            alt_uri = show $ if null (uriPath u) || head (uriPath u) /= '/' 
                        then u { uriPath = '/' : uriPath u } 
                        else u

instance HasHeaders (Request a) where
    getHeaders = rqHeaders
    setHeaders rq hdrs = rq { rqHeaders=hdrs }

-- | For easy pattern matching, HTTP response codes @xyz@ are
-- represented as @(x,y,z)@.
type ResponseCode  = (Int,Int,Int)

-- | @ResponseData@ contains the head of a response payload;
-- HTTP response code, accompanying text description + header
-- fields.
type ResponseData  = (ResponseCode,String,[Header])

-- | @RequestData@ contains the head of a HTTP request; method,
-- its URL along with the auxillary/supporting header data.
type RequestData   = (RequestMethod,URI,[Header])

-- | An HTTP Response.
-- The 'Show' instance of this type is used for message serialisation,
-- which means no body data is output, additionally the output will
-- show an HTTP version of 1.1 instead of the actual version returned
-- by a server.
data Response a =
    Response { rspCode     :: ResponseCode
             , rspReason   :: String
             , rspHeaders  :: [Header]
             , rspBody     :: a
             }
                   
-- This is an invalid representation of a received response, 
-- since we have made the assumption that all responses are HTTP/1.1
instance Show (Response a) where
    show rsp@(Response (a,b,c) reason headers _) =
        ver ++ ' ' : map intToDigit [a,b,c] ++ ' ' : reason ++ crlf
        ++ foldr (++) [] (map show (dropHttpVersion headers)) ++ crlf
     where
      ver = fromMaybe httpVersion (getResponseVersion rsp)

instance HasHeaders (Response a) where
    getHeaders = rspHeaders
    setHeaders rsp hdrs = rsp { rspHeaders=hdrs }


------------------------------------------------------------------
------------------ Request Building ------------------------------
------------------------------------------------------------------
libUA :: String
libUA = "hs-HTTP-4000.0.5"

defaultUserAgent :: String
defaultUserAgent = libUA

defaultGETRequest :: URI -> Request_String
defaultGETRequest uri = defaultGETRequest_ uri

defaultGETRequest_ :: BufferType a => URI -> Request a
defaultGETRequest_ uri = mkRequest GET uri 

-- | 'mkRequest method uri' constructs a well formed
-- request for the given HTTP method and URI. It does not
-- normalize the URI for the request _nor_ add the required 
-- Host: header. That is done either explicitly by the user
-- or when requests are normalized prior to transmission.
mkRequest :: BufferType ty => RequestMethod -> URI -> Request ty
mkRequest meth uri = req
 where
  req = 
    Request { rqURI      = uri
            , rqBody     = empty
            , rqHeaders  = [ Header HdrContentLength "0"
                           , Header HdrUserAgent     defaultUserAgent
                           ]
            , rqMethod   = meth
            }

  empty = buf_empty (toBufOps req)

{-
    -- stub out the user info.
  updAuth = fmap (\ x -> x{uriUserInfo=""}) (uriAuthority uri)

  withHost = 
    case uriToAuthorityString uri{uriAuthority=updAuth} of
      "" -> id
      h  -> ((Header HdrHost h):)

  uri_req 
   | forProxy  = uri
   | otherwise = snd (splitRequestURI uri)
-}


toBufOps :: BufferType a => Request a -> BufferOp a
toBufOps _ = bufferOps

-----------------------------------------------------------------
------------------ Parsing --------------------------------------
-----------------------------------------------------------------

-- Parsing a request
parseRequestHead :: [String] -> Result RequestData
parseRequestHead         [] = Left ErrorClosed
parseRequestHead (com:hdrs) = do
  (version,rqm,uri) <- requestCommand com (words com)
  hdrs'              <- parseHeaders hdrs
  return (rqm,uri,withVer version hdrs')
 where
  withVer [] hs = hs
  withVer (h:_) hs = withVersion h hs

  requestCommand l _yes@(rqm:uri:version) =
    case (parseURIReference uri, lookup rqm rqMethodMap) of
     (Just u, Just r) -> return (version,r,u)
     (Just u, Nothing) -> return (version,Custom rqm,u)
     _                -> parse_err l
  requestCommand l _
   | null l    = failWith ErrorClosed
   | otherwise = parse_err l

  parse_err l = responseParseError "parseRequestHead"
                   ("Request command line parse failure: " ++ l)

-- Parsing a response
parseResponseHead :: [String] -> Result ResponseData
parseResponseHead []         = failWith ErrorClosed
parseResponseHead (sts:hdrs) = do
  (version,code,reason)  <- responseStatus sts (words sts)
  hdrs'                  <- parseHeaders hdrs
  return (code,reason, withVersion version hdrs')
 where
  responseStatus _l _yes@(version:code:reason) =
    return (version,match code,concatMap (++" ") reason)
  responseStatus l _no 
    | null l    = failWith ErrorClosed  -- an assumption
    | otherwise = parse_err l

  parse_err l = 
    responseParseError 
        "parseResponseHead"
        ("Response status line parse failure: " ++ l)

  match [a,b,c] = (digitToInt a,
                   digitToInt b,
                   digitToInt c)
  match _ = (-1,-1,-1)  -- will create appropriate behaviour

-- To avoid changing the @RequestData@ and @ResponseData@ types
-- just for this (and the upstream backwards compat. woes that
-- will result in), encode version info as a custom header.
-- Used by 'parseResponseData' and 'parseRequestData'.
--
-- Note: the Request and Response types do not currently represent
-- the version info explicitly in their record types. You have to use
-- {get,set}{Request,Response}Version for that.
withVersion :: String -> [Header] -> [Header]
withVersion v hs 
 | v == httpVersion = hs  -- don't bother adding it if the default.
 | otherwise        = (Header (HdrCustom "X-HTTP-Version") v) : hs

-- | @getRequestVersion req@ returns the HTTP protocol version of
-- the request @req@. If @Nothing@, the default 'httpVersion' can be assumed.
getRequestVersion :: Request a -> Maybe String
getRequestVersion r = getHttpVersion r

-- | @setRequestVersion v req@ returns a new request, identical to
-- @req@, but with its HTTP version set to @v@.
setRequestVersion :: String -> Request a -> Request a
setRequestVersion s r = setHttpVersion r s


-- | @getResponseVersion rsp@ returns the HTTP protocol version of
-- the response @rsp@. If @Nothing@, the default 'httpVersion' can be 
-- assumed.
getResponseVersion :: Response a -> Maybe String
getResponseVersion r = getHttpVersion r

-- | @setResponseVersion v rsp@ returns a new response, identical to
-- @rsp@, but with its HTTP version set to @v@.
setResponseVersion :: String -> Response a -> Response a
setResponseVersion s r = setHttpVersion r s

-- internal functions for accessing HTTP-version info in
-- requests and responses. Not exported as it exposes ho
-- version info is represented internally.

getHttpVersion :: HasHeaders a => a -> Maybe String
getHttpVersion r = 
  fmap toVersion      $
   find isHttpVersion $
    getHeaders r
 where
  toVersion (Header _ x) = x

setHttpVersion :: HasHeaders a => a -> String -> a
setHttpVersion r v = 
  setHeaders r $
   withVersion v  $
    dropHttpVersion $
     getHeaders r

dropHttpVersion :: [Header] -> [Header]
dropHttpVersion hs = filter (not.isHttpVersion) hs

isHttpVersion :: Header -> Bool
isHttpVersion (Header (HdrCustom "X-HTTP-Version") _) = True
isHttpVersion _ = False    



-----------------------------------------------------------------
------------------ HTTP Send / Recv ----------------------------------
-----------------------------------------------------------------

data ResponseNextStep
 = Continue
 | Retry
 | Done
 | ExpectEntity
 | DieHorribly String

matchResponse :: RequestMethod -> ResponseCode -> ResponseNextStep
matchResponse rqst rsp =
    case rsp of
        (1,0,0) -> Continue
        (1,0,1) -> Done        -- upgrade to TLS
        (1,_,_) -> Continue    -- default
        (2,0,4) -> Done
        (2,0,5) -> Done
        (2,_,_) -> ans
        (3,0,4) -> Done
        (3,0,5) -> Done
        (3,_,_) -> ans
        (4,1,7) -> Retry       -- Expectation failed
        (4,_,_) -> ans
        (5,_,_) -> ans
        (a,b,c) -> DieHorribly ("Response code " ++ map intToDigit [a,b,c] ++ " not recognised")
    where
        ans | rqst == HEAD = Done
            | otherwise    = ExpectEntity
        

        
-----------------------------------------------------------------
------------------ A little friendly funtionality ---------------
-----------------------------------------------------------------


{-
    I had a quick look around but couldn't find any RFC about
    the encoding of data on the query string.  I did find an
    IETF memo, however, so this is how I justify the urlEncode
    and urlDecode methods.

    Doc name: draft-tiwari-appl-wxxx-forms-01.txt  (look on www.ietf.org)

    Reserved chars:  ";", "/", "?", ":", "@", "&", "=", "+", ",", and "$" are reserved.
    Unwise: "{" | "}" | "|" | "\" | "^" | "[" | "]" | "`"
    URI delims: "<" | ">" | "#" | "%" | <">
    Unallowed ASCII: <US-ASCII coded characters 00-1F and 7F hexadecimal>
                     <US-ASCII coded character 20 hexadecimal>
    Also unallowed:  any non-us-ascii character

    Escape method: char -> '%' a b  where a, b :: Hex digits
-}

urlDecode :: String -> String
urlDecode ('%':a:b:rest) = toEnum (16 * digitToInt a + digitToInt b)
                         : urlDecode rest
urlDecode (h:t) = h : urlDecode t
urlDecode [] = []


urlEncode :: String -> String
urlEncode     [] = []
urlEncode (ch:t) 
  | (isAscii ch && isAlphaNum ch) || ch `elem` "-_.~" = ch : urlEncode t
  | not (isAscii ch) = foldr escape (urlEncode t) (eightBs [] (fromEnum ch))
  | otherwise = escape (fromEnum ch) (urlEncode t)
    where
     escape b rs = '%':showH (b `div` 16) (showH (b `mod` 16) rs)
     
     showH x xs
       | x <= 9    = toEnum (o_0 + x) : xs
       | otherwise = toEnum (o_A + (x-10)) : xs
      where
       o_0 = fromEnum '0'
       o_A = fromEnum 'A'

     eightBs :: [Int]  -> Int -> [Int]
     eightBs acc x
      | x <= 0xff = (x:acc)
      | otherwise = eightBs ((x `mod` 256) : acc) (x `div` 256)

-- Encode form variables, useable in either the
-- query part of a URI, or the body of a POST request.
-- I have no source for this information except experience,
-- this sort of encoding worked fine in CGI programming.
urlEncodeVars :: [(String,String)] -> String
urlEncodeVars ((n,v):t) =
    let (same,diff) = partition ((==n) . fst) t
    in urlEncode n ++ '=' : foldl (\x y -> x ++ ',' : urlEncode y) (urlEncode $ v) (map snd same)
       ++ urlEncodeRest diff
       where urlEncodeRest [] = []
             urlEncodeRest diff = '&' : urlEncodeVars diff
urlEncodeVars [] = []

-- | @getAuth req@ fishes out the authority portion of the URL in a request's @Host@
-- header.
getAuth :: Monad m => Request ty -> m URIAuthority
getAuth r = 
   -- ToDo: verify that Network.URI functionality doesn't take care of this (now.)
  case parseURIAuthority auth of
    Just x -> return x 
    Nothing -> fail $ "Network.HTTP.Base.getAuth: Error parsing URI authority '" ++ auth ++ "'"
 where 
  auth = maybe (uriToAuthorityString uri) id (findHeader HdrHost r)
  uri  = rqURI r

{-# DEPRECATED normalizeRequestURI "Please use Network.HTTP.Base.normalizeRequest instead" #-}
normalizeRequestURI :: Bool{-do close-} -> {-URI-}String -> Request ty -> Request ty
normalizeRequestURI doClose h r = 
  (if doClose then replaceHeader HdrConnection "close" else id) $
  insertHeaderIfMissing HdrHost h $
    r { rqURI = (rqURI r){ uriScheme = ""
                         , uriAuthority = Nothing
			 }}

-- | @NormalizeRequestOptions@ brings together the various defaulting\/normalization options
-- over 'Request's. Use 'defaultNormalizeRequestOptions' for the standard selection of option
data NormalizeRequestOptions ty
 = NormalizeRequestOptions
     { normDoClose   :: Bool
     , normForProxy  :: Bool
     , normUserAgent :: Maybe String
     , normCustoms   :: [RequestNormalizer ty]
     }

-- | @RequestNormalizer@ is the shape of a (pure) function that rewrites
-- a request into some normalized form.
type RequestNormalizer ty = NormalizeRequestOptions ty -> Request ty -> Request ty

defaultNormalizeRequestOptions :: NormalizeRequestOptions ty
defaultNormalizeRequestOptions = NormalizeRequestOptions
     { normDoClose   = False
     , normForProxy  = False
     , normUserAgent = Just defaultUserAgent
     , normCustoms   = []
     }

-- | @normalizeRequest opts req@ is the entry point to use to normalize your
-- request prior to transmission (or other use.) Normalization is controlled
-- via the @NormalizeRequestOptions@ record.
normalizeRequest :: NormalizeRequestOptions ty
                 -> Request ty
		 -> Request ty
normalizeRequest opts req = foldr (\ f -> f opts) req normalizers
 where
  --normalizers :: [RequestNormalizer ty]
  normalizers = 
     ( normalizeHostURI
     : normalizeConnectionClose
     : normalizeUserAgent 
     : normCustoms opts
     )

-- | @normalizeUserAgent ua x req@ augments the request @req@ with 
-- a @User-Agent: ua@ header if @req@ doesn't already have a 
-- a @User-Agent:@ set.
normalizeUserAgent :: RequestNormalizer ty
normalizeUserAgent opts req = 
  case normUserAgent opts of
    Nothing -> req
    Just ua -> 
     case findHeader HdrUserAgent req of
       Nothing -> setHeaders req (mkHeader HdrUserAgent ua : getHeaders req)
       Just{}  -> req

-- | @normalizeConnectionClose opts req@ sets the header @Connection: close@ 
-- to indicate one-shot behavior iff @normDoClose@ is @True@. i.e., it then
-- _replaces_ any an existing @Connection:@ header in @req@.
normalizeConnectionClose :: RequestNormalizer ty
normalizeConnectionClose opts req 
 | normDoClose opts = replaceHeader HdrConnection "close" req
 | otherwise        = req

-- | @normalizeHostURI forProxy req@ rewrites your request to have it
-- follow the expected formats by the receiving party (proxy or server.)
-- 
normalizeHostURI :: RequestNormalizer ty
normalizeHostURI opts req = 
  case splitRequestURI uri of
    ("",_uri_abs)
      | forProxy -> 
         case findHeader HdrHost req of
	   Nothing -> req -- no host/authority in sight..not much we can do.
	   Just h  -> req{rqURI=uri{ uriAuthority=Just URIAuth{uriUserInfo="", uriRegName=hst, uriPort=pNum}
	                           , uriScheme=if (null (uriScheme uri)) then "http" else uriScheme uri
				   }}
            where 
	      hst = case span (/='@') user_hst of
	               (as,'@':bs) -> 
		          case span (/=':') as of
			    (_,_:_) -> bs
			    _ -> user_hst
		       _ -> user_hst

	      (user_hst, pNum) = 
	         case span isDigit (reverse h) of
		   (ds,':':bs) -> (reverse bs, ':':reverse ds)
		   _ -> (h,"")
      | otherwise -> 
         case findHeader HdrHost req of
	   Nothing -> req -- no host/authority in sight..not much we can do...complain?
	   Just{}  -> req
    (h,uri_abs) 
      | forProxy  -> insertHeaderIfMissing HdrHost h req 
      | otherwise -> replaceHeader HdrHost h req{rqURI=uri_abs} -- Note: _not_ stubbing out user:pass
 where
   uri0     = rqURI req 
     -- stub out the user:pass 
   uri      = uri0{uriAuthority=fmap (\ x -> x{uriUserInfo=""}) (uriAuthority uri0)}

   forProxy = normForProxy opts

{- Comments re: above rewriting:
    RFC 2616, section 5.1.2:
     "The most common form of Request-URI is that used to identify a
      resource on an origin server or gateway. In this case the absolute
      path of the URI MUST be transmitted (see section 3.2.1, abs_path) as
      the Request-URI, and the network location of the URI (authority) MUST
      be transmitted in a Host header field." 
   We assume that this is the case, so we take the host name from
   the Host header if there is one, otherwise from the request-URI.
   Then we make the request-URI an abs_path and make sure that there
   is a Host header.
-}

splitRequestURI :: URI -> ({-authority-}String, URI)
splitRequestURI uri = (uriToAuthorityString uri, uri{uriScheme="", uriAuthority=Nothing})

-- Adds a Host header if one is NOT ALREADY PRESENT..
{-# DEPRECATED normalizeHostHeader "Please use Network.HTTP.Base.normalizeRequest instead" #-}
normalizeHostHeader :: Request ty -> Request ty
normalizeHostHeader rq = 
  insertHeaderIfMissing HdrHost
                        (uriToAuthorityString $ rqURI rq)
			rq
                                     
-- Looks for a "Connection" header with the value "close".
-- Returns True when this is found.
findConnClose :: [Header] -> Bool
findConnClose hdrs =
  maybe False
        (\ x -> map toLower (trim x) == "close")
	(lookupHeader HdrConnection hdrs)

-- | Used when we know exactly how many bytes to expect.
linearTransfer :: (Int -> IO (Result a)) -> Int -> IO (Result ([Header],a))
linearTransfer readBlk n = fmapE (\str -> Right ([],str)) (readBlk n)

-- | Used when nothing about data is known,
--   Unfortunately waiting for a socket closure
--   causes bad behaviour.  Here we just
--   take data once and give up the rest.
hopefulTransfer :: BufferOp a
                -> IO (Result a)
		-> [a]
		-> IO (Result ([Header],a))
hopefulTransfer bufOps readL strs 
    = readL >>= 
      either (\v -> return $ Left v)
             (\more -> if (buf_isEmpty bufOps more)
                         then return (Right ([],foldr (flip (buf_append bufOps)) (buf_empty bufOps) strs))
                         else hopefulTransfer bufOps readL (more:strs))

-- | A necessary feature of HTTP\/1.1
--   Also the only transfer variety likely to
--   return any footers.
chunkedTransfer :: BufferOp a
		-> IO (Result a)
                -> (Int -> IO (Result a))
                -> IO (Result ([Header], a))
chunkedTransfer bufOps readL readBlk = chunkedTransferC bufOps readL readBlk [] 0

chunkedTransferC :: BufferOp a
                 -> IO (Result a)
                 -> (Int -> IO (Result a))
		 -> [a]
		 -> Int
		 -> IO (Result ([Header], a))
chunkedTransferC bufOps readL readBlk acc n = do
  v <- readL
  case v of
    Left e -> return (Left e)
    Right line 
     | size == 0 -> 
         -- last chunk read; look for trailing headers..
        fmapE (\ strs -> do
	         ftrs <- parseHeaders (map (buf_toStr bufOps) strs)
		   -- insert (computed) Content-Length header.
		 let ftrs' = Header HdrContentLength (show n) : ftrs
                 return (ftrs',buf_concat bufOps (reverse acc)))

	      (readTillEmpty2 bufOps readL [])

     | otherwise -> do
         some <- readBlk size
	 case some of
	   Left e -> return (Left e)
	   Right cdata -> do
	       readL -- CRLF is mandated after the chunk block; ToDo: check that the line is empty.?
	       chunkedTransferC bufOps readL readBlk (cdata:acc) (n+size)
     where
      size 
       | buf_isEmpty bufOps line = 0
       | otherwise = 
	 case readHex (buf_toStr bufOps line) of
          (hx,_):_ -> hx
          _        -> 0

-- | Maybe in the future we will have a sensible thing
--   to do here, at that time we might want to change
--   the name.
uglyDeathTransfer :: String -> IO (Result ([Header],a))
uglyDeathTransfer loc = return (responseParseError loc "Unknown Transfer-Encoding")

-- | Remove leading crlfs then call readTillEmpty2 (not required by RFC)
readTillEmpty1 :: BufferOp a
	       -> IO (Result a)
               -> IO (Result [a])
readTillEmpty1 bufOps readL =
  readL >>=
    either (return . Left)
           (\ s -> 
	       if buf_isLineTerm bufOps s
                then readTillEmpty1 bufOps readL
                else readTillEmpty2 bufOps readL [s])

-- | Read lines until an empty line (CRLF),
--   also accepts a connection close as end of
--   input, which is not an HTTP\/1.1 compliant
--   thing to do - so probably indicates an
--   error condition.
readTillEmpty2 :: BufferOp a
	       -> IO (Result a)
	       -> [a]
	       -> IO (Result [a])
readTillEmpty2 bufOps readL list =
    readL >>=
      either (return . Left)
             (\ s ->
	        if buf_isLineTerm bufOps s || buf_isEmpty bufOps s
                 then return (Right $ reverse (s:list))
                 else readTillEmpty2 bufOps readL (s:list))

--
-- Misc
--

-- | @catchIO a h@ handles IO action exceptions throughout codebase; version-specific
-- tweaks better go here.
catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO a h = Prelude.catch a h

catchIO_ :: IO a -> IO a -> IO a
catchIO_ a h = Prelude.catch a (const h)

responseParseError :: String -> String -> Result a
responseParseError loc v = failWith (ErrorParse (loc ++ ' ':v))
