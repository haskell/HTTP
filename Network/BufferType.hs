{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.BufferType
-- Copyright   :  (c) Warrick Gray 2002, Bjorn Bringert 2003-2004, Simon Foster 2004, 2007 Robin Bate Boerop, 2008 Sigbjorn Finne
-- License     :  BSD
--
-- Maintainer  :  Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- Abstract representation of wire-transmitted values.
-- 
-----------------------------------------------------------------------------
module Network.BufferType
       ( 
         BufferType(..)

       , BufferOp(..)
       , strictBufferOp
       , lazyBufferOp
       , stringBufferOp
       ) where


import qualified Data.ByteString       as Strict hiding ( unpack, pack, span )
import qualified Data.ByteString.Char8 as Strict ( unpack, pack, span )
import qualified Data.ByteString.Lazy as Lazy hiding ( pack, unpack,span )
import qualified Data.ByteString.Lazy.Char8 as Lazy ( pack, unpack, span )
import System.IO ( Handle )
import Data.Word ( Word8 )

class BufferType bufType where
   bufferOps :: BufferOp bufType

instance BufferType Lazy.ByteString where
   bufferOps = lazyBufferOp

instance BufferType Strict.ByteString where
   bufferOps = strictBufferOp

instance BufferType String where
   bufferOps = stringBufferOp

-- Encode the I/O operations of the underlying buffer over a Handle 
-- in an (explicit) dictionary type. May not be needed, but gives
-- us flexibility in explicit overriding and wrapping up of these methods.
data BufferOp a
 = BufferOp
     { buf_hGet         :: Handle -> Int -> IO a
     , buf_hGetContents :: Handle -> IO a
     , buf_hPut         :: Handle -> a   -> IO ()
     , buf_hGetLine     :: Handle -> IO a
     , buf_empty        :: a
     , buf_append       :: a -> a -> a
     , buf_fromStr      :: String -> a
     , buf_toStr        :: a -> String
     , buf_snoc         :: a -> Word8 -> a
     , buf_splitAt      :: Int -> a -> (a,a)
     , buf_span         :: (Char  -> Bool) -> a -> (a,a)
     , buf_isLineTerm   :: a -> Bool
     , buf_isEmpty      :: a -> Bool
     }

instance Eq (BufferOp a) where
  _ == _ = False

strictBufferOp :: BufferOp Strict.ByteString
strictBufferOp = 
    BufferOp 
      { buf_hGet = Strict.hGet
      , buf_hGetContents = Strict.hGetContents
      , buf_hPut = Strict.hPut
      , buf_hGetLine = Strict.hGetLine
      , buf_append = Strict.append
      , buf_fromStr = Strict.pack
      , buf_toStr   = Strict.unpack
      , buf_snoc = Strict.snoc
      , buf_splitAt = Strict.splitAt
      , buf_span    = Strict.span
      , buf_empty = Strict.empty
      , buf_isLineTerm = \ b -> Strict.length b == 2 && crlf == b
      , buf_isEmpty   = Strict.null 
      }
   where
    crlf = Strict.pack "\r\n"

lazyBufferOp :: BufferOp Lazy.ByteString
lazyBufferOp = 
    BufferOp 
      { buf_hGet = Lazy.hGet
      , buf_hGetContents = Lazy.hGetContents
      , buf_hPut = Lazy.hPut
      , buf_hGetLine = \ h -> Strict.hGetLine h >>= \ l -> return (Lazy.fromChunks [l])
      , buf_append = Lazy.append
      , buf_fromStr = Lazy.pack
      , buf_toStr   = Lazy.unpack
      , buf_snoc = Lazy.snoc
      , buf_splitAt = \ i x -> Lazy.splitAt (fromIntegral i) x
      , buf_span    = Lazy.span
      , buf_empty = Lazy.empty
      , buf_isLineTerm = \ b -> Lazy.length b == 2 && crlf == b
      , buf_isEmpty   = Lazy.null 
      }
   where
    crlf = Lazy.pack "\r\n"

stringBufferOp :: BufferOp String
stringBufferOp =BufferOp 
      { buf_hGet      = \ h n -> Strict.hGet h n >>= return . Strict.unpack
      , buf_hGetContents = \ h -> Strict.hGetContents h >>= return . Strict.unpack
      , buf_hPut      = \ h s -> Strict.hPut h (Strict.pack s)
      , buf_hGetLine  = \ h   -> Strict.hGetLine h >>= return . Strict.unpack
      , buf_append    = (++)
      , buf_fromStr   = id
      , buf_toStr     = id
      , buf_snoc      = \ a x -> a ++ [toEnum (fromIntegral x)]
      , buf_splitAt   = splitAt
      , buf_span      = \ p a -> 
                           case Strict.span p (Strict.pack a) of
			     (a,b) -> (Strict.unpack a, Strict.unpack b)
      , buf_empty     = []
      , buf_isLineTerm = \ b -> b == crlf
      , buf_isEmpty   = null 
      }
   where
    crlf = "\r\n"
