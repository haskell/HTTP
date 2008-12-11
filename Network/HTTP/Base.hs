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
-- An easy HTTP interface; base types.
--
-----------------------------------------------------------------------------
module Network.HTTP.Base
       (
          -- ** Constants
         httpVersion

          -- ** HTTP
       , Request
       , Response
       , RequestMethod(..)
       
       , HTTPRequest(..)
       , HTTPResponse(..)
       
          -- ** URL Encoding
       , urlEncode
       , urlDecode
       , urlEncodeVars

          -- ** URI authority parsing
       , URIAuthority(..)
       , parseURIAuthority
       
          -- internal
       , crlf
       , sp
       , uriToAuthorityString   -- :: URI    -> String
       , uriAuthToString        -- :: URIAuth -> String
       , parseResponseHead
       , parseRequestHead
       , ResponseNextStep(..)
       , matchResponse
       , ResponseData
       , ResponseCode
       , RequestData
       
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
       
       , catchIO
       , catchIO_
       ) where

import Network.URI
   ( URI(uriAuthority, uriPath, uriScheme)
   , URIAuth(uriUserInfo, uriRegName, uriPort)
   , parseURIReference
   )

import Control.Monad ( guard )
import Data.Char     ( ord, digitToInt, intToDigit, toLower )
import Data.List     ( partition )
import Data.Maybe    ( listToMaybe )
import Numeric       ( showHex, readHex )

import Network.Stream
import Network.BufferType ( BufferOp(..) )
import Network.HTTP.Headers
import Network.HTTP.Utils ( trim )

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

-----------------------------------------------------------------
------------------ HTTP Messages --------------------------------
-----------------------------------------------------------------


-- Protocol version
httpVersion :: String
httpVersion = "HTTP/1.1"


-- | The HTTP request method, to be used in the 'Request' object.
-- We are missing a few of the stranger methods, but these are
-- not really necessary until we add full TLS.
data RequestMethod = HEAD | PUT | GET | POST | DELETE | OPTIONS | TRACE
    deriving(Show,Eq)

rqMethodMap :: [(String, RequestMethod)]
rqMethodMap = [("HEAD",    HEAD),
	       ("PUT",     PUT),
	       ("GET",     GET),
	       ("POST",    POST),
               ("DELETE",  DELETE),
	       ("OPTIONS", OPTIONS),
	       ("TRACE",   TRACE)]

type Request  = HTTPRequest  String
type Response = HTTPResponse String

-- | An HTTP Request.
-- The 'Show' instance of this type is used for message serialisation,
-- which means no body data is output.
data HTTPRequest a =
     Request { rqURI       :: URI   -- ^ might need changing in future
                                    --  1) to support '*' uri in OPTIONS request
                                    --  2) transparent support for both relative
                                    --     & absolute uris, although this should
                                    --     already work (leave scheme & host parts empty).
             , rqMethod    :: RequestMethod             
             , rqHeaders   :: [Header]
             , rqBody      :: a
             }



crlf, sp :: String
crlf = "\r\n"
sp   = " "

-- Notice that request body is not included,
-- this show function is used to serialise
-- a request for the transport link, we send
-- the body separately where possible.
instance Show (HTTPRequest a) where
    show (Request u m h _) =
        show m ++ sp ++ alt_uri ++ sp ++ httpVersion ++ crlf
        ++ foldr (++) [] (map show h) ++ crlf
        where
            alt_uri = show $ if null (uriPath u) || head (uriPath u) /= '/' 
                        then u { uriPath = '/' : uriPath u } 
                        else u

instance HasHeaders (HTTPRequest a) where
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
data HTTPResponse a =
    Response { rspCode     :: ResponseCode
             , rspReason   :: String
             , rspHeaders  :: [Header]
             , rspBody     :: a
             }
                   
-- This is an invalid representation of a received response, 
-- since we have made the assumption that all responses are HTTP/1.1
instance Show (HTTPResponse a) where
    show (Response (a,b,c) reason headers _) =
        httpVersion ++ ' ' : map intToDigit [a,b,c] ++ ' ' : reason ++ crlf
        ++ foldr (++) [] (map show headers) ++ crlf

instance HasHeaders (HTTPResponse a) where
    getHeaders = rspHeaders
    setHeaders rsp hdrs = rsp { rspHeaders=hdrs }

-----------------------------------------------------------------
------------------ Parsing --------------------------------------
-----------------------------------------------------------------

-- Parsing a request
parseRequestHead :: [String] -> Result RequestData
parseRequestHead [] = Left ErrorClosed
parseRequestHead (com:hdrs) =
    requestCommand com `bindE` \(_version,rqm,uri) ->
    parseHeaders hdrs `bindE` \hdrs' ->
    Right (rqm,uri,hdrs')
    where
        requestCommand line =
	  case words line of
            _yes@(rqm:uri:version) -> 
	     case (parseURIReference uri, lookup rqm rqMethodMap) of
  	       (Just u, Just r) -> Right (version,r,u)
	       _                -> Left parse_err
	    _no 
	     | null line -> Left ErrorClosed
	     | otherwise -> Left parse_err
          where
	   parse_err = ErrorParse ("Request command line parse failure: " ++ line)

-- Parsing a response
parseResponseHead :: [String] -> Result ResponseData
parseResponseHead [] = Left ErrorClosed
parseResponseHead (sts:hdrs) = 
    responseStatus sts `bindE` \(_version,code,reason) ->
    parseHeaders hdrs `bindE` \hdrs' ->
    Right (code,reason,hdrs')
    where
        responseStatus line =
          case words line of
            _yes@(version:code:reason) -> 
	     Right (version,match code,concatMap (++" ") reason)
            _no
	     | null line -> Left ErrorClosed  -- an assumption
	     | otherwise -> Left parse_err
          where
	   parse_err = (ErrorParse $ "Response status line parse failure: " ++ line)

        match [a,b,c] = (digitToInt a,
                         digitToInt b,
                         digitToInt c)
        match _ = (-1,-1,-1)  -- will create appropriate behaviour


        

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

urlEncode, urlDecode :: String -> String

urlDecode ('%':a:b:rest) = toEnum (16 * digitToInt a + digitToInt b)
                         : urlDecode rest
urlDecode (h:t) = h : urlDecode t
urlDecode [] = []

urlEncode (h:t) =
    let str = if reserved (ord h) then escape h else [h]
    in str ++ urlEncode t
    where
        reserved x
            | x >= ord 'a' && x <= ord 'z' = False
            | x >= ord 'A' && x <= ord 'Z' = False
            | x >= ord '0' && x <= ord '9' = False
            | x <= 0x20 || x >= 0x7F = True
            | otherwise = x `elem` map ord [';','/','?',':','@','&'
                                           ,'=','+',',','$','{','}'
                                           ,'|','\\','^','[',']','`'
                                           ,'<','>','#','%','"']
        -- wouldn't it be nice if the compiler
        -- optimised the above for us?

        escape x = '%':showHex (ord x) ""

urlEncode [] = []
            


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
getAuth :: Monad m => HTTPRequest ty -> m URIAuthority
getAuth r = 
   -- ToDo: verify that Network.URI functionality doesn't take care of this (now.)
  case parseURIAuthority auth of
    Just x -> return x 
    Nothing -> fail $ "Network.HTTP.Base.getAuth: Error parsing URI authority '" ++ auth ++ "'"
 where 
   auth = 
    case findHeader HdrHost r of
      Just h  -> h
      Nothing -> uriToAuthorityString (rqURI r)

  {- RFC 2616, section 5.1.2:
     "The most common form of Request-URI is that used to identify a
      resource on an origin server or gateway. In this case the absolute
      path of the URI MUST be transmitted (see section 3.2.1, abs_path) as
      the Request-URI, and the network location of the URI (authority) MUST
      be transmitted in a Host header field." -}
  -- we assume that this is the case, so we take the host name from
  -- the Host header if there is one, otherwise from the request-URI.
  -- Then we make the request-URI an abs_path and make sure that there
  -- is a Host header.
normalizeRequestURI :: URIAuthority -> HTTPRequest ty -> HTTPRequest ty
normalizeRequestURI URIAuthority{host=h} r = 
  replaceHeader HdrConnection "close" $
  insertHeaderIfMissing HdrHost h $
    r { rqURI = (rqURI r){ uriScheme = ""
                         , uriAuthority = Nothing
			 }}

-- Adds a Host header if one is NOT ALREADY PRESENT
normalizeHostHeader :: HTTPRequest ty -> HTTPRequest ty
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
linearTransfer readBlk n
    = do info <- readBlk n
         return $ info `bindE` \str -> Right ([],str)

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
chunkedTransfer bufOps readL readBlk = do
    v <- chunkedTransferC bufOps readL readBlk 0
    return $ v `bindE` \(ftrs,count,info) ->
             let myftrs = Header HdrContentLength (show count) : ftrs              
             in Right (myftrs,info)

chunkedTransferC :: BufferOp a
                 -> IO (Result a)
                 -> (Int -> IO (Result a))
		 -> Int
		 -> IO (Result ([Header],Int,a))
chunkedTransferC bufOps readL readBlk n = do
  v <- readL
  case v of
    Left e -> return (Left e)
    Right line 
     | size == 0 -> do
         rs <- readTillEmpty2 bufOps readL []
         return $
           rs `bindE` \strs ->
           parseHeaders (map (buf_toStr bufOps) strs) `bindE` \ftrs ->
           Right (ftrs,n,buf_empty bufOps)
     | otherwise -> do
         some <- readBlk size
         readL
         more <- chunkedTransferC bufOps {-nullVal isNull isLineEnd toStr append -} readL readBlk (n+size)
         return $ 
           some `bindE` \cdata ->
           more `bindE` \(ftrs,m,mdata) -> 
           Right (ftrs,m,buf_append bufOps cdata mdata) 
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
uglyDeathTransfer :: IO (Result ([Header],a))
uglyDeathTransfer 
    = return $ Left $ ErrorParse "Unknown Transfer-Encoding"

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

