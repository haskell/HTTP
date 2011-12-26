-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.Cookie
-- Copyright   :  See LICENSE file
-- License     :  BSD
-- 
-- Maintainer  :  Ganesh Sittampalam <http@projects.haskell.org>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- This module provides the data types and functions for working with HTTP cookies.
-- Right now, it contains mostly functionality needed by 'Network.Browser'.
-- 
-----------------------------------------------------------------------------
module Network.HTTP.Cookie
       ( Cookie(..)
       , cookieMatch          -- :: (String,String) -> Cookie -> Bool

          -- functions for translating cookies and headers.
       , cookiesToHeader      -- :: [Cookie] -> Header
       , processCookieHeaders -- :: String -> [Header] -> ([String], [Cookie])
       ) where

import Network.HTTP.Headers

import Data.Char
import Data.List
import Data.Maybe

import Text.ParserCombinators.Parsec
   ( Parser, char, many, many1, satisfy, parse, option, try
   , (<|>), sepBy1
   )

------------------------------------------------------------------
----------------------- Cookie Stuff -----------------------------
------------------------------------------------------------------

-- | @Cookie@ is the Haskell representation of HTTP cookie values.
-- See its relevant specs for authoritative details.
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

-- | @cookieToHeaders ck@ serialises @Cookie@s to an HTTP request header.
cookiesToHeader :: [Cookie] -> Header
cookiesToHeader cs = Header HdrCookie (mkCookieHeaderValue cs)

-- | Turn a list of cookies into a key=value pair list, separated by
-- semicolons.
mkCookieHeaderValue :: [Cookie] -> String
mkCookieHeaderValue = intercalate "; " . map mkCookieHeaderValue1
  where
    mkCookieHeaderValue1 c = ckName c ++ "=" ++ ckValue c

-- | @cookieMatch (domain,path) ck@ performs the standard cookie
-- match wrt the given domain and path. 
cookieMatch :: (String, String) -> Cookie -> Bool
cookieMatch (dom,path) ck =
 ckDomain ck `isSuffixOf` dom &&
 case ckPath ck of
   Nothing -> True
   Just p  -> p `isPrefixOf` path


-- | @processCookieHeaders dom hdrs@ 
processCookieHeaders :: String -> [Header] -> ([String], [Cookie])
processCookieHeaders dom hdrs = foldr (headerToCookies dom) ([],[]) hdrs

-- | @headerToCookies dom hdr acc@ 
headerToCookies :: String -> Header -> ([String], [Cookie]) -> ([String], [Cookie])
headerToCookies dom (Header HdrSetCookie val) (accErr, accCookie) = 
    case parse cookies "" val of
        Left{}  -> (val:accErr, accCookie)
        Right x -> (accErr, x ++ accCookie)
  where
   cookies :: Parser [Cookie]
   cookies = sepBy1 cookie (char ',')

   cookie :: Parser Cookie
   cookie =
       do name <- word
          _    <- spaces_l
          _    <- char '='
          _    <- spaces_l
          val1 <- cvalue
          args <- cdetail
          return $ mkCookie name val1 args

   cvalue :: Parser String
   
   spaces_l = many (satisfy isSpace)

   cvalue = quotedstring <|> many1 (satisfy $ not . (==';')) <|> return ""
   
   -- all keys in the result list MUST be in lower case
   cdetail :: Parser [(String,String)]
   cdetail = many $
       try (do _  <- spaces_l
               _  <- char ';'
               _  <- spaces_l
               s1 <- word
               _  <- spaces_l
               s2 <- option "" (char '=' >> spaces_l >> cvalue)
               return (map toLower s1,s2)
           )

   mkCookie :: String -> String -> [(String,String)] -> Cookie
   mkCookie nm cval more = 
	  MkCookie { ckName    = nm
                   , ckValue   = cval
                   , ckDomain  = map toLower (fromMaybe dom (lookup "domain" more))
                   , ckPath    = lookup "path" more
                   , ckVersion = lookup "version" more
                   , ckComment = lookup "comment" more
                   }
headerToCookies _ _ acc = acc

      


word, quotedstring :: Parser String
quotedstring =
    do _   <- char '"'  -- "
       str <- many (satisfy $ not . (=='"'))
       _   <- char '"'
       return str

word = many1 (satisfy (\x -> isAlphaNum x || x=='_' || x=='.' || x=='-' || x==':'))
