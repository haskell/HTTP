{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.BufferType
-- Description :  Abstract representation of request and response buffer types.
-- Copyright   :  (c) Warrick Gray 2002, Bjorn Bringert 2003-2004, Simon Foster 2004, 2007 Robin Bate Boerop, 2008 Sigbjorn Finne
-- License     :  BSD
--
-- Maintainer  :  Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- In order to give the user freedom in how request and response content
-- is represented, a sufficiently abstract representation is needed of
-- these internally. The "Network.BufferType" module provides this, defining
-- the 'BufferType' class and its ad-hoc representation of buffer operations
-- via the 'BufferOp' record.
--
-- This module provides definitions for the standard buffer types that the
-- package supports, i.e., for @String@ and @ByteString@ (strict and lazy.)
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

import Network.HTTP.Utils ( crlf )

-- | The @BufferType@ class encodes, in a mixed-mode way, the interface
-- that the library requires to operate over data embedded in HTTP
-- requests and responses. That is, we use explicit dictionaries
-- for the operations, but overload the name of the dicts themselves.
-- 
class BufferType bufType where
   bufferOps :: BufferOp bufType

instance BufferType Lazy.ByteString where
   bufferOps = lazyBufferOp

instance BufferType Strict.ByteString where
   bufferOps = strictBufferOp

instance BufferType String where
   bufferOps = stringBufferOp

-- | @BufferOp@ encodes the I/O operations of the underlying buffer over 
-- a Handle in an (explicit) dictionary type. May not be needed, but gives
-- us flexibility in explicit overriding and wrapping up of these methods.
--
-- Along with IO operations is an ad-hoc collection of functions for working
-- with these abstract buffers, as needed by the internals of the code
-- that processes requests and responses.
--
-- We supply three default @BufferOp@ values, for @String@ along with the
-- strict and lazy versions of @ByteString@. To add others, provide @BufferOp@
-- definitions for 
data BufferOp a
 = BufferOp
     { buf_hGet         :: Handle -> Int -> IO a
     , buf_hGetContents :: Handle -> IO a
     , buf_hPut         :: Handle -> a   -> IO ()
     , buf_hGetLine     :: Handle -> IO a
     , buf_empty        :: a
     , buf_append       :: a -> a -> a
     , buf_concat       :: [a] -> a
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

-- | @strictBufferOp@ is the 'BufferOp' definition over @ByteString@s,
-- the non-lazy kind.
strictBufferOp :: BufferOp Strict.ByteString
strictBufferOp = 
    BufferOp 
      { buf_hGet         = Strict.hGet
      , buf_hGetContents = Strict.hGetContents
      , buf_hPut         = Strict.hPut
      , buf_hGetLine     = Strict.hGetLine
      , buf_append       = Strict.append
      , buf_concat       = Strict.concat
      , buf_fromStr      = Strict.pack
      , buf_toStr        = Strict.unpack
      , buf_snoc         = Strict.snoc
      , buf_splitAt      = Strict.splitAt
      , buf_span         = Strict.span
      , buf_empty        = Strict.empty
      , buf_isLineTerm   = \ b -> Strict.length b == 2 && p_crlf == b
      , buf_isEmpty      = Strict.null 
      }
   where
    p_crlf = Strict.pack crlf

-- | @lazyBufferOp@ is the 'BufferOp' definition over @ByteString@s,
-- the non-strict kind.
lazyBufferOp :: BufferOp Lazy.ByteString
lazyBufferOp = 
    BufferOp 
      { buf_hGet         = Lazy.hGet
      , buf_hGetContents = Lazy.hGetContents
      , buf_hPut         = Lazy.hPut
      , buf_hGetLine     = \ h -> Strict.hGetLine h >>= \ l -> return (Lazy.fromChunks [l])
      , buf_append       = Lazy.append
      , buf_concat       = Lazy.concat
      , buf_fromStr      = Lazy.pack
      , buf_toStr        = Lazy.unpack
      , buf_snoc         = Lazy.snoc
      , buf_splitAt      = \ i x -> Lazy.splitAt (fromIntegral i) x
      , buf_span         = Lazy.span
      , buf_empty        = Lazy.empty
      , buf_isLineTerm   = \ b -> Lazy.length b == 2 && p_crlf == b
      , buf_isEmpty      = Lazy.null 
      }
   where
    p_crlf = Lazy.pack crlf

-- | @stringBufferOp@ is the 'BufferOp' definition over @String@s.
-- It is defined in terms of @strictBufferOp@ operations,
-- unpacking/converting to @String@ when needed.
stringBufferOp :: BufferOp String
stringBufferOp =BufferOp 
      { buf_hGet         = \ h n -> buf_hGet strictBufferOp h n >>= return . Strict.unpack
      , buf_hGetContents = \ h -> buf_hGetContents strictBufferOp h >>= return . Strict.unpack
      , buf_hPut         = \ h s -> buf_hPut strictBufferOp h (Strict.pack s)
      , buf_hGetLine     = \ h   -> buf_hGetLine strictBufferOp h >>= return . Strict.unpack
      , buf_append       = (++)
      , buf_concat       = concat
      , buf_fromStr      = id
      , buf_toStr        = id
      , buf_snoc         = \ a x -> a ++ [toEnum (fromIntegral x)]
      , buf_splitAt      = splitAt
      , buf_span         = \ p a -> 
                             case Strict.span p (Strict.pack a) of
			       (x,y) -> (Strict.unpack x, Strict.unpack y)
      , buf_empty        = []
      , buf_isLineTerm   = \ b -> b == crlf
      , buf_isEmpty      = null 
      }

