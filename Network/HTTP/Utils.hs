-----------------------------------------------------------------------------
-- |
-- Module      :  Network.HTTP.Utils
-- Copyright   :  See LICENSE file
-- License     :  BSD
--
-- Maintainer  :  Ganesh Sittampalam <ganesh@earth.li>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- Set of utility functions and definitions used by package modules.
--
module Network.HTTP.Utils
       ( trim     -- :: String -> String
       , trimL    -- :: String -> String
       , trimR    -- :: String -> String

       , crlf     -- :: String
       , lf       -- :: String
       , sp       -- :: String

       , split    -- :: Eq a => a -> [a] -> Maybe ([a],[a])
       , splitBy  -- :: Eq a => a -> [a] -> [[a]]

       , readsOne -- :: Read a => (a -> b) -> b -> String -> b

       , dropWhileTail -- :: (a -> Bool) -> [a] -> [a]
       , chopAtDelim   -- :: Eq a => a -> [a] -> ([a],[a])

       , toUTF8BS
       , fromUTF8BS
       ) where

import Data.Bits
import Data.Char
import Data.List ( elemIndex )
import Data.Maybe ( fromMaybe )
import Data.Word ( Word8 )

import qualified Data.ByteString as BS

-- | @crlf@ is our beloved two-char line terminator.
crlf :: String
crlf = "\r\n"

-- | @lf@ is a tolerated line terminator, per RFC 2616 section 19.3.
lf :: String
lf = "\n"

-- | @sp@ lets you save typing one character.
sp :: String
sp   = " "

-- | @split delim ls@ splits a list into two parts, the @delim@ occurring
-- at the head of the second list.  If @delim@ isn't in @ls@, @Nothing@ is
-- returned.
split :: Eq a => a -> [a] -> Maybe ([a],[a])
split delim list = case delim `elemIndex` list of
    Nothing -> Nothing
    Just x  -> Just $ splitAt x list

-- | @trim str@ removes leading and trailing whitespace from @str@.
trim :: String -> String
trim xs = trimR (trimL xs)

-- | @trimL str@ removes leading whitespace (as defined by 'Data.Char.isSpace')
-- from @str@.
trimL :: String -> String
trimL xs = dropWhile isSpace xs

-- | @trimL str@ removes trailing whitespace (as defined by 'Data.Char.isSpace')
-- from @str@.
trimR :: String -> String
trimR str = fromMaybe "" $ foldr trimIt Nothing str
 where
  trimIt x (Just xs) = Just (x:xs)
  trimIt x Nothing
   | isSpace x = Nothing
   | otherwise = Just [x]

-- | @splitMany delim ls@ removes the delimiter @delim@ from @ls@.
splitBy :: Eq a => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy c xs =
    case break (==c) xs of
      (_,[]) -> [xs]
      (as,_:bs) -> as : splitBy c bs

-- | @readsOne f def str@ tries to 'read' @str@, taking
-- the first result and passing it to @f@. If the 'read'
-- doesn't succeed, return @def@.
readsOne :: Read a => (a -> b) -> b -> String -> b
readsOne f n str =
 case reads str of
   ((v,_):_) -> f v
   _ -> n


-- | @dropWhileTail p ls@ chops off trailing elements from @ls@
-- until @p@ returns @False@.
dropWhileTail :: (a -> Bool) -> [a] -> [a]
dropWhileTail f ls =
 case foldr chop Nothing ls of { Just xs -> xs; Nothing -> [] }
  where
    chop x (Just xs) = Just (x:xs)
    chop x _
     | f x       = Nothing
     | otherwise = Just [x]

-- | @chopAtDelim elt ls@ breaks up @ls@ into two at first occurrence
-- of @elt@; @elt@ is elided too. If @elt@ does not occur, the second
-- list is empty and the first is equal to @ls@.
chopAtDelim :: Eq a => a -> [a] -> ([a],[a])
chopAtDelim elt xs =
  case break (==elt) xs of
    (_,[])    -> (xs,[])
    (as,_:bs) -> (as,bs)

toUTF8BS :: String -> BS.ByteString
toUTF8BS = BS.pack . encodeStringUtf8

fromUTF8BS :: BS.ByteString -> String
fromUTF8BS = decodeStringUtf8 . BS.unpack

-- | Encode 'String' to a list of UTF8-encoded octets
--
-- Code-points in the @U+D800@-@U+DFFF@ range will be encoded
-- as the replacement character (i.e. @U+FFFD@).
--
-- The code is extracted from Cabal library, written originally
-- Herbert Valerio Riedel under BSD-3-Clause license
encodeStringUtf8 :: String -> [Word8]
encodeStringUtf8 []        = []
encodeStringUtf8 (c:cs)
  | c <= '\x07F' = w8
                 : encodeStringUtf8 cs
  | c <= '\x7FF' = (0xC0 .|.  w8ShiftR  6          )
                 : (0x80 .|. (w8          .&. 0x3F))
                 : encodeStringUtf8 cs
  | c <= '\xD7FF'= (0xE0 .|.  w8ShiftR 12          )
                 : (0x80 .|. (w8ShiftR  6 .&. 0x3F))
                 : (0x80 .|. (w8          .&. 0x3F))
                 : encodeStringUtf8 cs
  | c <= '\xDFFF'= 0xEF : 0xBF : 0xBD -- U+FFFD
                 : encodeStringUtf8 cs
  | c <= '\xFFFF'= (0xE0 .|.  w8ShiftR 12          )
                 : (0x80 .|. (w8ShiftR  6 .&. 0x3F))
                 : (0x80 .|. (w8          .&. 0x3F))
                 : encodeStringUtf8 cs
  | otherwise    = (0xf0 .|.  w8ShiftR 18          )
                 : (0x80 .|. (w8ShiftR 12 .&. 0x3F))
                 : (0x80 .|. (w8ShiftR  6 .&. 0x3F))
                 : (0x80 .|. (w8          .&. 0x3F))
                 : encodeStringUtf8 cs
  where
    w8 = fromIntegral (ord c) :: Word8
    w8ShiftR :: Int -> Word8
    w8ShiftR = fromIntegral . shiftR (ord c)

-- | Decode 'String' from UTF8-encoded octets.
--
-- Invalid data in the UTF8 stream (this includes code-points @U+D800@
-- through @U+DFFF@) will be decoded as the replacement character (@U+FFFD@).
--
-- See also 'encodeStringUtf8'
decodeStringUtf8 :: [Word8] -> String
decodeStringUtf8 = go
  where
    go :: [Word8] -> String
    go []       = []
    go (c : cs)
      | c <= 0x7F = chr (fromIntegral c) : go cs
      | c <= 0xBF = replacementChar : go cs
      | c <= 0xDF = twoBytes c cs
      | c <= 0xEF = moreBytes 3 0x800     cs (fromIntegral $ c .&. 0xF)
      | c <= 0xF7 = moreBytes 4 0x10000   cs (fromIntegral $ c .&. 0x7)
      | c <= 0xFB = moreBytes 5 0x200000  cs (fromIntegral $ c .&. 0x3)
      | c <= 0xFD = moreBytes 6 0x4000000 cs (fromIntegral $ c .&. 0x1)
      | otherwise   = replacementChar : go cs

    twoBytes :: Word8 -> [Word8] -> String
    twoBytes c0 (c1:cs')
      | c1 .&. 0xC0 == 0x80
      = let d = (fromIntegral (c0 .&. 0x1F) `shiftL` 6)
             .|. fromIntegral (c1 .&. 0x3F)
         in if d >= 0x80
               then  chr d                : go cs'
               else  replacementChar      : go cs'
    twoBytes _ cs' = replacementChar      : go cs'

    moreBytes :: Int -> Int -> [Word8] -> Int -> [Char]
    moreBytes 1 overlong cs' acc
      | overlong <= acc && acc <= 0x10FFFF && (acc < 0xD800 || 0xDFFF < acc)
      = chr acc : go cs'

      | otherwise
      = replacementChar : go cs'

    moreBytes byteCount overlong (cn:cs') acc
      | cn .&. 0xC0 == 0x80
      = moreBytes (byteCount-1) overlong cs'
          ((acc `shiftL` 6) .|. fromIntegral cn .&. 0x3F)

    moreBytes _ _ cs' _
      = replacementChar : go cs'

    replacementChar = '\xfffd'
