-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.Auth
-- Copyright   :  (c) Warrick Gray 2002, Bjorn Bringert 2003-2005, 2007 Robin Bate Boerop, 2008- Sigbjorn Finne
-- License     :  BSD
-- 
-- Maintainer  :  Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- Representing HTTP Auth values in Haskell.
-- Right now, it contains mostly functionality needed by 'Network.Browser'.
-- 
-----------------------------------------------------------------------------
module Network.HTTP.Auth
       ( Authority(..)
       , Algorithm(..)
       , Challenge(..)
       , Qop(..)

       , headerToChallenge -- :: URI -> Header -> Maybe Challenge
       , withAuthority     -- :: Authority -> Request ty -> String
       ) where

import Network.URI
import Network.HTTP.Base
import Network.HTTP.Utils
import Network.HTTP.Headers ( Header(..) )
import qualified Network.HTTP.MD5 as MD5 (hash)
import qualified Network.HTTP.Base64 as Base64 (encode)
import Text.ParserCombinators.Parsec
   ( Parser, char, many, many1, satisfy, parse, spaces, sepBy1 )

import Data.Char
import Data.Maybe
import Data.Word ( Word8 )

-- | @Authority@ specifies the HTTP Authentication method to use for
-- a given domain/realm; @Basic@ or @Digest@.
data Authority 
 = AuthBasic { auRealm    :: String
             , auUsername :: String
             , auPassword :: String
             , auSite     :: URI
             }
 | AuthDigest{ auRealm     :: String
             , auUsername  :: String
             , auPassword  :: String
             , auNonce     :: String
             , auAlgorithm :: Maybe Algorithm
             , auDomain    :: [URI]
             , auOpaque    :: Maybe String
             , auQop       :: [Qop]
             }


data Challenge 
 = ChalBasic  { chRealm   :: String }
 | ChalDigest { chRealm   :: String
              , chDomain  :: [URI]
              , chNonce   :: String
              , chOpaque  :: Maybe String
              , chStale   :: Bool
              , chAlgorithm ::Maybe Algorithm
              , chQop     :: [Qop]
              }

-- | @Algorithm@ controls the digest algorithm to, @MD5@ or @MD5Session@.
data Algorithm = AlgMD5 | AlgMD5sess
    deriving(Eq)

instance Show Algorithm where
    show AlgMD5 = "md5"
    show AlgMD5sess = "md5-sess"

-- | 
data Qop = QopAuth | QopAuthInt
    deriving(Eq,Show)

-- | @withAuthority auth req@ generates a credentials value from the @auth@ 'Authority',
-- in the context of the given request.
-- 
-- If a client nonce was to be used then this function might need to be of type ... -> BrowserAction String
withAuthority :: Authority -> Request ty -> String
withAuthority a rq = case a of
        AuthBasic{}  -> "Basic " ++ base64encode (auUsername a ++ ':' : auPassword a)
        AuthDigest{} ->
            "Digest " ++
	     concat [ "username="  ++ quo (auUsername a)
	            , ",realm="    ++ quo (auRealm a)
		    , ",nonce="    ++ quo (auNonce a)
		    , ",uri="      ++ quo digesturi
		    , ",response=" ++ quo rspdigest
                       -- plus optional stuff:
		    , fromMaybe "" (fmap (\ alg -> ",algorithm=" ++ quo (show alg)) (auAlgorithm a))
		    , fromMaybe "" (fmap (\ o   -> ",opaque=" ++ quo o) (auOpaque a))
		    , if null (auQop a) then "" else ",qop=auth"
		    ]
    where
        quo s = '"':s ++ "\""

        rspdigest = map toLower (kd (md5 a1) (noncevalue ++ ":" ++ md5 a2))

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

type Octet = Word8

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




-- | @headerToChallenge base www_auth@ tries to convert the @WWW-Authenticate@ header 
-- @www_auth@  into a 'Challenge' value.
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

word, quotedstring :: Parser String
quotedstring =
    do { char '"'  -- "
       ; str <- many (satisfy $ not . (=='"'))
       ; char '"'
       ; return str
       }

word = many1 (satisfy (\x -> isAlphaNum x || x=='_' || x=='.' || x=='-' || x==':'))
