-----------------------------------------------------------------------------
-- |
-- Module      :  Network.StreamDebugger
-- Copyright   :  (c) Warrick Gray 2002, Bjorn Bringert 2003-2004, Simon Foster 2004, 2007 Robin Bate Boerop
-- License     :  BSD
--
-- Maintainer  :  Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- Implements debugging of @Stream@s.  Originally part of Gray's\/Bringert's
-- HTTP module.
--
-- * Changes by Robin Bate Boerop <robin@bateboerop.name>:
--      - Created.  Made minor formatting changes.
--      
-----------------------------------------------------------------------------
module Network.StreamDebugger
   ( StreamDebugger
   , debugStream
   , debugByteStream
   ) where

import Network.Stream (Stream(..))
import System.IO
   ( Handle, hFlush, hPutStrLn, IOMode(AppendMode), hClose, openFile
   )
import Network.TCP ( HandleStream, StreamHooks(..), HStream, setStreamHooks )

-- | Allows stream logging.  Refer to 'debugStream' below.
data StreamDebugger x
   = Dbg Handle x

instance (Stream x) => Stream (StreamDebugger x) where
    readBlock (Dbg h x) n =
        do val <- readBlock x n
           hPutStrLn h ("readBlock " ++ show n ++ ' ' : show val)
           return val
    readLine (Dbg h x) =
        do val <- readLine x
           hPutStrLn h ("readLine " ++ show val)
           return val
    writeBlock (Dbg h x) str =
        do val <- writeBlock x str
           hPutStrLn h ("writeBlock " ++ show val ++ ' ' : show str)
           return val
    close (Dbg h x) =
        do hPutStrLn h "closing..."
           hFlush h
           close x
           hPutStrLn h "...closed"
           hClose h

-- | Wraps a stream with logging I\/O.
--   The first argument is a filename which is opened in @AppendMode@.
debugStream :: (Stream a) => FilePath -> a -> IO (StreamDebugger a)
debugStream file stream = 
    do h <- openFile file AppendMode
       hPutStrLn h ("File \"" ++ file ++ "\" opened for appending.")
       return (Dbg h stream)

debugByteStream :: HStream ty => FilePath -> HandleStream ty -> IO (HandleStream ty)
debugByteStream file stream = 
    do h <- openFile file AppendMode
       hPutStrLn h ("File \"" ++ file ++ "\" opened for appending.")
       setStreamHooks stream (debugStreamHooks h)
       return stream

debugStreamHooks :: HStream ty => Handle -> StreamHooks ty
debugStreamHooks h = 
  StreamHooks
    { hook_readBlock = \ toStr n val -> do
       let eval = case val of { Left e -> Left e ; Right v -> Right $ toStr v}
       hPutStrLn h ("readBlock " ++ show n ++ ' ' : show eval)
    , hook_readLine = \ toStr val -> do
	   let eval = case val of { Left e -> Left e ; Right v -> Right $ toStr v}
           hPutStrLn h ("readLine " ++ show eval)
    , hook_writeBlock = \ toStr str val -> do
           hPutStrLn h ("writeBlock " ++ show val ++ ' ' : toStr str)
    , hook_close = do
           hPutStrLn h "closing..."
           hFlush h
           hClose h
    }
