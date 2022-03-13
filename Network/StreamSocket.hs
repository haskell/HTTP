{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.StreamSocket
-- Copyright   :  See LICENSE file
-- License     :  BSD
--
-- Maintainer  :  Ganesh Sittampalam <ganesh@earth.li>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- Socket Stream instance. Originally part of Gray's\/Bringert's HTTP module.
--
-- * Changes by Robin Bate Boerop <robin@bateboerop.name>:
--      - Made dependencies explicit in import statements.
--      - Removed false dependencies in import statements.
--      - Created separate module for instance Stream Socket.
--
-- * Changes by Simon Foster:
--      - Split module up into to separate Network.[Stream,TCP,HTTP] modules
--
-----------------------------------------------------------------------------
module Network.StreamSocket
   ( handleSocketError
   , myrecv
   ) where

import Network.Stream
   ( Stream(..), ConnError(ErrorReset, ErrorMisc), Result
   )
import Network.Socket
   ( Socket, getSocketOption, shutdown
   , ShutdownCmd(ShutdownBoth), SocketOption(SoError)
   )
import Network.Socket.ByteString (send, recv)
import qualified Network.Socket
   ( close )

import Network.HTTP.Base ( catchIO )
import Network.HTTP.Utils ( fromUTF8BS, toUTF8BS )
import Control.Monad (liftM)
import Control.Exception as Exception (IOException)
import System.IO.Error (isEOFError)

-- | Exception handler for socket operations.
handleSocketError :: Socket -> IOException -> IO (Result a)
handleSocketError sk e =
    do se <- getSocketOption sk SoError
       case se of
          0     -> ioError e
          10054 -> return $ Left ErrorReset  -- reset
          _     -> return $ Left $ ErrorMisc $ show se

myrecv :: Socket -> Int -> IO String
myrecv sock len =
    let handler e = if isEOFError e then return [] else ioError e
        in catchIO (fmap fromUTF8BS (recv sock len)) handler

instance Stream Socket where
    readBlock sk n    = readBlockSocket sk n
    readLine sk       = readLineSocket sk
    writeBlock sk str = writeBlockSocket sk str
    close sk          = do
        -- This slams closed the connection (which is considered rude for TCP\/IP)
         shutdown sk ShutdownBoth
         Network.Socket.close sk
    closeOnEnd _sk _  = return () -- can't really deal with this, so do run the risk of leaking sockets here.

readBlockSocket :: Socket -> Int -> IO (Result String)
readBlockSocket sk n = (liftM Right $ fn n) `catchIO` (handleSocketError sk)
  where
   fn x = do { str <- myrecv sk x
             ; let len = length str
             ; if len < x
                then ( fn (x-len) >>= \more -> return (str++more) )
                else return str
             }

-- Use of the following function is discouraged.
-- The function reads in one character at a time,
-- which causes many calls to the kernel recv()
-- hence causes many context switches.
readLineSocket :: Socket -> IO (Result String)
readLineSocket sk = (liftM Right $ fn "") `catchIO` (handleSocketError sk)
  where
   fn str = do
     c <- myrecv sk 1 -- like eating through a straw.
     if null c || c == "\n"
      then return (reverse str++c)
      else fn (head c:str)

writeBlockSocket :: Socket -> String -> IO (Result ())
writeBlockSocket sk str = (liftM Right $ fn str) `catchIO` (handleSocketError sk)
  where
   fn [] = return ()
   fn x  = send sk (toUTF8BS x) >>= \i -> fn (drop i x)

