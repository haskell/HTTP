-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.Headers
-- Copyright   :  (c) Warrick Gray 2002, Bjorn Bringert 2003-2005, 2007 Robin Bate Boerop, 2008- Sigbjorn Finne
-- License     :  BSD
-- 
-- Maintainer  :  Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- This module provides the data types for representing HTTP headers, and
-- operations for looking up header values and working with sequences of
-- header values in 'Request's and 'Response's. To avoid having to provide
-- separate set of operations for doing so, we introduce a type class 'HasHeaders'
-- to facilitate writing such processing using overloading instead.
-- 
-----------------------------------------------------------------------------
module Network.HTTP.Headers
   ( HasHeaders(..)     -- type class

   , Header(..)
   , mkHeader           -- :: HeaderName -> String -> Header
   , hdrName            -- :: Header     -> HeaderName
   , hdrValue           -- :: Header     -> String

   , HeaderName(..)

   , insertHeader          -- :: HasHeaders a => HeaderName -> String -> a -> a
   , insertHeaderIfMissing -- :: HasHeaders a => HeaderName -> String -> a -> a
   , insertHeaders         -- :: HasHeaders a => [Header] -> a -> a
   , retrieveHeaders       -- :: HasHeaders a => HeaderName -> a -> [Header]
   , replaceHeader         -- :: HasHeaders a => HeaderName -> String -> a -> a
   , findHeader            -- :: HasHeaders a => HeaderName -> a -> Maybe String
   , lookupHeader          -- :: HeaderName -> [Header] -> Maybe String

   , parseHeader           -- :: parseHeader :: String -> Result Header
   , parseHeaders          -- :: [String] -> Result [Header]
   
   , HeaderSetter
   ) where

import Data.Char (toLower)
import Network.Stream (Result, failParse)
import Network.HTTP.Utils ( trim, split, crlf )

-- | The @Header@ data type pairs header names & values.
data Header = Header HeaderName String

hdrName :: Header -> HeaderName
hdrName (Header h _) = h

hdrValue :: Header -> String
hdrValue (Header _ v) = v

-- | Header constructor as a function, hiding above rep.
mkHeader :: HeaderName -> String -> Header
mkHeader = Header

instance Show Header where
    show (Header key value) = shows key (':':' ':value ++ crlf)

-- | HTTP @HeaderName@ type, a Haskell data constructor for each
-- specification-defined header, prefixed with @Hdr@ and CamelCased,
-- (i.e., eliding the @-@ in the process.) Should you require using
-- a custom header, there's the @HdrCustom@ constructor which takes
-- a @String@ argument.
--
-- Encoding HTTP header names differently, as Strings perhaps, is an
-- equally fine choice..no decidedly clear winner, but let's stick
-- with data constructors here.
-- 
data HeaderName 
    -- Generic Headers --
 = HdrCacheControl
 | HdrConnection
 | HdrDate
 | HdrPragma
 | HdrTransferEncoding        
 | HdrUpgrade                
 | HdrVia
    -- Request Headers --
 | HdrAccept
 | HdrAcceptCharset
 | HdrAcceptEncoding
 | HdrAcceptLanguage
 | HdrAuthorization
 | HdrCookie
 | HdrExpect
 | HdrFrom
 | HdrHost
 | HdrIfModifiedSince
 | HdrIfMatch
 | HdrIfNoneMatch
 | HdrIfRange
 | HdrIfUnmodifiedSince
 | HdrMaxForwards
 | HdrProxyAuthorization
 | HdrRange
 | HdrReferer
 | HdrUserAgent
    -- Response Headers
 | HdrAge
 | HdrLocation
 | HdrProxyAuthenticate
 | HdrPublic
 | HdrRetryAfter
 | HdrServer
 | HdrSetCookie
 | HdrTE
 | HdrTrailer
 | HdrVary
 | HdrWarning
 | HdrWWWAuthenticate
    -- Entity Headers
 | HdrAllow
 | HdrContentBase
 | HdrContentEncoding
 | HdrContentLanguage
 | HdrContentLength
 | HdrContentLocation
 | HdrContentMD5
 | HdrContentRange
 | HdrContentType
 | HdrETag
 | HdrExpires
 | HdrLastModified
    -- | MIME entity headers (for sub-parts)
 | HdrContentTransferEncoding
    -- | Allows for unrecognised or experimental headers.
 | HdrCustom String -- not in header map below.
    deriving(Eq)

-- | @headerMap@ is a straight assoc list for translating between header names 
-- and values.
headerMap :: [ (String,HeaderName) ]
headerMap =
   [ p "Cache-Control"        HdrCacheControl
   , p "Connection"           HdrConnection
   , p "Date"                 HdrDate
   , p "Pragma"               HdrPragma
   , p "Transfer-Encoding"    HdrTransferEncoding
   , p "Upgrade"              HdrUpgrade
   , p "Via"                  HdrVia
   , p "Accept"               HdrAccept
   , p "Accept-Charset"       HdrAcceptCharset
   , p "Accept-Encoding"      HdrAcceptEncoding
   , p "Accept-Language"      HdrAcceptLanguage
   , p "Authorization"        HdrAuthorization
   , p "Cookie"               HdrCookie
   , p "Expect"               HdrExpect
   , p "From"                 HdrFrom
   , p "Host"                 HdrHost
   , p "If-Modified-Since"    HdrIfModifiedSince
   , p "If-Match"             HdrIfMatch
   , p "If-None-Match"        HdrIfNoneMatch
   , p "If-Range"             HdrIfRange
   , p "If-Unmodified-Since"  HdrIfUnmodifiedSince
   , p "Max-Forwards"         HdrMaxForwards
   , p "Proxy-Authorization"  HdrProxyAuthorization
   , p "Range"                HdrRange
   , p "Referer"              HdrReferer
   , p "User-Agent"           HdrUserAgent
   , p "Age"                  HdrAge
   , p "Location"             HdrLocation
   , p "Proxy-Authenticate"   HdrProxyAuthenticate
   , p "Public"               HdrPublic
   , p "Retry-After"          HdrRetryAfter
   , p "Server"               HdrServer
   , p "Set-Cookie"           HdrSetCookie
   , p "TE"                   HdrTE
   , p "Trailer"              HdrTrailer
   , p "Vary"                 HdrVary
   , p "Warning"              HdrWarning
   , p "WWW-Authenticate"     HdrWWWAuthenticate
   , p "Allow"                HdrAllow
   , p "Content-Base"         HdrContentBase
   , p "Content-Encoding"     HdrContentEncoding
   , p "Content-Language"     HdrContentLanguage
   , p "Content-Length"       HdrContentLength
   , p "Content-Location"     HdrContentLocation
   , p "Content-MD5"          HdrContentMD5
   , p "Content-Range"        HdrContentRange
   , p "Content-Type"         HdrContentType
   , p "ETag"                 HdrETag
   , p "Expires"              HdrExpires
   , p "Last-Modified"        HdrLastModified
   , p "Content-Transfer-Encoding" HdrContentTransferEncoding
   ]
 where
  p a b = (a,b)

instance Show HeaderName where
    show (HdrCustom s) = s
    show x = case filter ((==x).snd) headerMap of
                [] -> error "headerMap incomplete"
                (h:_) -> fst h

-- | @HasHeaders@ is a type class for types containing HTTP headers, allowing
-- you to write overloaded header manipulation functions
-- for both 'Request' and 'Response' data types, for instance.
class HasHeaders x where
    getHeaders :: x -> [Header]
    setHeaders :: x -> [Header] -> x

-- Header manipulation functions

type HeaderSetter a = HeaderName -> String -> a -> a

-- | @insertHeader hdr val x@ inserts a header with the given header name
-- and value. Does not check for existing headers with same name, allowing
-- duplicates to be introduce (use 'replaceHeader' if you want to avoid this.)
insertHeader :: HasHeaders a => HeaderSetter a
insertHeader name value x = setHeaders x newHeaders
    where
        newHeaders = (Header name value) : getHeaders x

-- | @insertHeaderIfMissing hdr val x@ adds the new header only if no previous
-- header with name @hdr@ exists in @x@.
insertHeaderIfMissing :: HasHeaders a => HeaderSetter a
insertHeaderIfMissing name value x = setHeaders x (newHeaders $ getHeaders x)
    where
        newHeaders list@(h@(Header n _): rest)
            | n == name  = list
            | otherwise  = h : newHeaders rest
        newHeaders [] = [Header name value]

-- | @replaceHeader hdr val o@ replaces the header @hdr@ with the
-- value @val@, dropping any existing 
replaceHeader :: HasHeaders a => HeaderSetter a
replaceHeader name value h = setHeaders h newHeaders
    where
        newHeaders = Header name value : [ x | x@(Header n _) <- getHeaders h, name /= n ]
          
-- | @insertHeaders hdrs x@ appends multiple headers to @x@'s existing
-- set.
insertHeaders :: HasHeaders a => [Header] -> a -> a
insertHeaders hdrs x = setHeaders x (getHeaders x ++ hdrs)

-- | @retrieveHeaders hdrNm x@ gets a list of headers with 'HeaderName' @hdrNm@.
retrieveHeaders :: HasHeaders a => HeaderName -> a -> [Header]
retrieveHeaders name x = filter matchname (getHeaders x)
    where
        matchname (Header n _) = n == name 

-- | @findHeader hdrNm x@ looks up @hdrNm@ in @x@, returning the first
-- header that matches, if any.
findHeader :: HasHeaders a => HeaderName -> a -> Maybe String
findHeader n x = lookupHeader n (getHeaders x)

-- | @lookupHeader hdr hdrs@ locates the first header matching @hdr@ in the
-- list @hdrs@.
lookupHeader :: HeaderName -> [Header] -> Maybe String
lookupHeader _ [] = Nothing
lookupHeader v (Header n s:t)  
  |  v == n   =  Just s
  | otherwise =  lookupHeader v t

-- | @parseHeader headerNameAndValueString@ tries to unscramble a
-- @header: value@ pairing and returning it as a 'Header'.
parseHeader :: String -> Result Header
parseHeader str =
    case split ':' str of
        Nothing -> failParse ("Unable to parse header: " ++ str)
        Just (k,v) -> return $ Header (fn k) (trim $ drop 1 v)
    where
        fn k = case map snd $ filter (match k . fst) headerMap of
                 [] -> (HdrCustom k)
                 (h:_) -> h

        match :: String -> String -> Bool
        match s1 s2 = map toLower s1 == map toLower s2
    
-- | @parseHeaders hdrs@ takes a sequence of strings holding header
-- information and parses them into a set of headers (preserving their
-- order in the input argument.) Handles header values split up over
-- multiple lines.
parseHeaders :: [String] -> Result [Header]
parseHeaders = catRslts [] . 
                 map (parseHeader . clean) . 
		     joinExtended ""
   where
        -- Joins consecutive lines where the second line
        -- begins with ' ' or '\t'.
        joinExtended old      [] = [old]
        joinExtended old (h : t)
	  | isLineExtension h    = joinExtended (old ++ ' ' : tail h) t
          | otherwise            = old : joinExtended h t
	
	isLineExtension (x:_) = x == ' ' || x == '\t'
	isLineExtension _ = False

        clean [] = []
        clean (h:t) | h `elem` "\t\r\n" = ' ' : clean t
                    | otherwise = h : clean t

        -- tolerant of errors?  should parse
        -- errors here be reported or ignored?
        -- currently ignored.
        catRslts :: [a] -> [Result a] -> Result [a]
        catRslts list (h:t) = 
            case h of
                Left _ -> catRslts list t
                Right v -> catRslts (v:list) t
        catRslts list [] = Right $ reverse list            
