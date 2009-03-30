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
-- * Changes by Robin Bate Boerop <robin@bateboerop.name>:
--      - Made dependencies explicit in import statements.
--      - Removed false dependencies in import statements.
--      - Added missing type signatures.
--      - Created Network.HTTP.Headers from Network.HTTP modules.
--
-- See changes history and TODO list in Network.HTTP module.
--
-- * Header notes:
--
--     [@Host@]
--                  Required by HTTP\/1.1, if not supplied as part
--                  of a request a default Host value is extracted
--                  from the request-uri.
-- 
--     [@Connection@] 
--                  If this header is present in any request or
--                  response, and it's value is "close", then
--                  the current request\/response is the last 
--                  to be allowed on that connection.
-- 
--     [@Expect@]
--                  Should a request contain a body, an Expect
--                  header will be added to the request.  The added
--                  header has the value \"100-continue\".  After
--                  a 417 \"Expectation Failed\" response the request
--                  is attempted again without this added Expect
--                  header.
--                  
--     [@TransferEncoding,ContentLength,...@]
--                  if request is inconsistent with any of these
--                  header values then you may not receive any response
--                  or will generate an error response (probably 4xx).
--
-----------------------------------------------------------------------------
module Network.HTTP.Headers
   ( HasHeaders(..)
   , Header(..)
   , HeaderName(..)
   , mkHeader

   , insertHeader
   , insertHeaderIfMissing
   , insertHeaders
   , retrieveHeaders
   , replaceHeader
   , findHeader
   , lookupHeader
   , parseHeaders

   ) where

import Data.Char (toLower)
import Network.Stream (Result, failParse)
import Network.HTTP.Utils ( trim, split, crlf )

-- | The @Header@ data type pairs header names & values.
data Header = Header HeaderName String

-- | Header constructor as a function, hiding above rep.
mkHeader :: HeaderName -> String -> Header
mkHeader = Header

instance Show Header where
    show (Header key value) = shows key (':':' ':value ++ crlf)

-- | HTTP Header Name type:
--  Why include this at all?  I have some reasons
--   1) prevent spelling errors of header names,
--   2) remind everyone of what headers are available,
--   3) might speed up searches for specific headers.
--
--  Arguments against:
--   1) makes customising header names laborious
--   2) increases code volume.
--
-- Long discussions can be had on this topic!
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

-- Translation between header names and values,
-- good candidate for improvement.
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

-- | This class allows us to write generic header manipulation functions
-- for both 'Request' and 'Response' data types.
class HasHeaders x where
    getHeaders :: x -> [Header]
    setHeaders :: x -> [Header] -> x

-- Header manipulation functions
insertHeader, replaceHeader, insertHeaderIfMissing
    :: HasHeaders a => HeaderName -> String -> a -> a

-- | Inserts a header with the given name and value.
-- Allows duplicate header names.
insertHeader name value x = setHeaders x newHeaders
    where
        newHeaders = (Header name value) : getHeaders x

-- | Adds the new header only if no previous header shares
-- the same name.
insertHeaderIfMissing name value x = setHeaders x (newHeaders $ getHeaders x)
    where
        newHeaders list@(h@(Header n _): rest)
            | n == name  = list
            | otherwise  = h : newHeaders rest
        newHeaders [] = [Header name value]

-- | Removes old headers with duplicate name.
replaceHeader name value h = setHeaders h newHeaders
    where
        newHeaders = Header name value : [ x | x@(Header n _) <- getHeaders h, name /= n ]
          
-- | Inserts multiple headers.
insertHeaders :: HasHeaders a => [Header] -> a -> a
insertHeaders hdrs x = setHeaders x (getHeaders x ++ hdrs)

-- | Gets a list of headers with a particular 'HeaderName'.
retrieveHeaders :: HasHeaders a => HeaderName -> a -> [Header]
retrieveHeaders name x = filter matchname (getHeaders x)
    where
        matchname (Header n _)  |  n == name  =  True
        matchname _ = False

-- | Lookup presence of specific HeaderName in a list of Headers
-- Returns the value from the first matching header.
findHeader :: HasHeaders a => HeaderName -> a -> Maybe String
findHeader n x = lookupHeader n (getHeaders x)

-- An anomally really:
lookupHeader :: HeaderName -> [Header] -> Maybe String
lookupHeader v (Header n s:t)  |  v == n   =  Just s
                               | otherwise =  lookupHeader v t
lookupHeader _ _  =  Nothing

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
    
parseHeaders :: [String] -> Result [Header]
parseHeaders =
   catRslts [] . map (parseHeader . clean) . joinExtended ""
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
