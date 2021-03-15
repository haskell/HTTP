{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.TCP
-- Copyright   :  See LICENSE file
-- License     :  BSD
--
-- Maintainer  :  Ganesh Sittampalam <ganesh@earth.li>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- Some utility functions for working with the Haskell @network@ package. Mostly
-- for internal use by the @Network.HTTP@ code.
--
-----------------------------------------------------------------------------
module Network.TCP
   ( Connection
   , EndPoint(..)
   , openTCPPort
   , isConnectedTo

   , openTCPConnection
   , socketConnection
   , isTCPConnectedTo
   
   , HandleStream
   , HStream(..)
   
   , StreamHooks(..)
   , nullHooks
   , setStreamHooks
   , getStreamHooks
   , hstreamToConnection

   ) where

import Network.Socket
   ( Socket, SocketOption(KeepAlive)
   , SocketType(Stream), connect
   , shutdown, ShutdownCmd(..)
   , setSocketOption, getPeerName
   , socket, Family(AF_UNSPEC), defaultProtocol, getAddrInfo
   , defaultHints, addrFamily, withSocketsDo
   , addrSocketType, addrAddress
   )
import qualified Network.Socket
   ( close )
import qualified Network.Stream as Stream
   ( Stream(readBlock, readLine, writeBlock, close, closeOnEnd) )
import Network.Stream
   ( ConnError(..)
   , Result
   , failWith
   , failMisc
   )
import Network.BufferType

import Network.HTTP.Base ( catchIO )
import Network.Socket ( socketToHandle )

import Data.Char  ( toLower )
import Data.Word  ( Word8 )
import Control.Concurrent
import Control.Exception ( onException )
import Control.Monad ( liftM, when )
import System.IO ( Handle, hFlush, IOMode(..), hClose )
import System.IO.Error ( isEOFError )

import qualified Data.ByteString      as Strict
import qualified Data.ByteString.Lazy as Lazy

-----------------------------------------------------------------
------------------ TCP Connections ------------------------------
-----------------------------------------------------------------

-- | The 'Connection' newtype is a wrapper that allows us to make
-- connections an instance of the Stream class, without GHC extensions.
-- While this looks sort of like a generic reference to the transport
-- layer it is actually TCP specific, which can be seen in the
-- implementation of the 'Stream Connection' instance.
newtype Connection = Connection (HandleStream String)

newtype HandleStream a = HandleStream {getRef :: MVar (Conn a)}

data EndPoint = EndPoint { epHost :: String, epPort :: Int }

instance Eq EndPoint where
   EndPoint host1 port1 == EndPoint host2 port2 =
     map toLower host1 == map toLower host2 && port1 == port2

data Conn a 
 = MkConn { connSock      :: !Socket
          , connHandle    :: Handle
          , connBuffer    :: BufferOp a
          , connInput     :: Maybe a
          , connEndPoint  :: EndPoint
          , connHooks     :: Maybe (StreamHooks a)
          , connCloseEOF  :: Bool -- True => close socket upon reaching end-of-stream.
          }
 | ConnClosed
   deriving(Eq)

hstreamToConnection :: HandleStream String -> Connection
hstreamToConnection h = Connection h

connHooks' :: Conn a -> Maybe (StreamHooks a)
connHooks' ConnClosed{} = Nothing
connHooks' x = connHooks x

-- all of these are post-op hooks
data StreamHooks ty
 = StreamHooks
     { hook_readLine   :: (ty -> String) -> Result ty -> IO ()
     , hook_readBlock  :: (ty -> String) -> Int -> Result ty -> IO ()
     , hook_writeBlock :: (ty -> String) -> ty  -> Result () -> IO ()
     , hook_close      :: IO ()
     , hook_name       :: String -- hack alert: name of the hook itself.
     }

instance Eq ty => Eq (StreamHooks ty) where
  (==) _ _ = True

nullHooks :: StreamHooks ty
nullHooks = StreamHooks 
     { hook_readLine   = \ _ _   -> return ()
     , hook_readBlock  = \ _ _ _ -> return ()
     , hook_writeBlock = \ _ _ _ -> return ()
     , hook_close      = return ()
     , hook_name       = ""
     }

setStreamHooks :: HandleStream ty -> StreamHooks ty -> IO ()
setStreamHooks h sh = modifyMVar_ (getRef h) (\ c -> return c{connHooks=Just sh})

getStreamHooks :: HandleStream ty -> IO (Maybe (StreamHooks ty))
getStreamHooks h = readMVar (getRef h) >>= return.connHooks

-- | @HStream@ overloads the use of 'HandleStream's, letting you
-- overload the handle operations over the type that is communicated
-- across the handle. It comes in handy for @Network.HTTP@ 'Request'
-- and 'Response's as the payload representation isn't fixed, but overloaded.
--
-- The library comes with instances for @ByteString@s and @String@, but
-- should you want to plug in your own payload representation, defining
-- your own @HStream@ instance _should_ be all that it takes.
-- 
class BufferType bufType => HStream bufType where
  openStream       :: String -> Int -> IO (HandleStream bufType)
  openSocketStream :: String -> Int -> Socket -> IO (HandleStream bufType)
  readLine         :: HandleStream bufType -> IO (Result bufType)
  readBlock        :: HandleStream bufType -> Int -> IO (Result bufType)
  writeBlock       :: HandleStream bufType -> bufType -> IO (Result ())
  close            :: HandleStream bufType -> IO ()
  closeQuick       :: HandleStream bufType -> IO ()
  closeOnEnd       :: HandleStream bufType -> Bool -> IO ()
  
instance HStream Strict.ByteString where
  openStream       = openTCPConnection
  openSocketStream = socketConnection
  readBlock c n    = readBlockBS c n
  readLine c       = readLineBS c
  writeBlock c str = writeBlockBS c str
  close c          = closeIt c Strict.null True
  closeQuick c     = closeIt c Strict.null False
  closeOnEnd c f   = closeEOF c f

instance HStream Lazy.ByteString where
    openStream       = \ a b -> openTCPConnection_ a b True
    openSocketStream = \ a b c -> socketConnection_ a b c True
    readBlock c n    = readBlockBS c n
    readLine c       = readLineBS c
    writeBlock c str = writeBlockBS c str
    close c          = closeIt c Lazy.null True
    closeQuick c     = closeIt c Lazy.null False
    closeOnEnd c f   = closeEOF c f

instance Stream.Stream Connection where
  readBlock (Connection c)     = Network.TCP.readBlock c
  readLine (Connection c)      = Network.TCP.readLine c
  writeBlock (Connection c)    = Network.TCP.writeBlock c 
  close (Connection c)         = Network.TCP.close c
  closeOnEnd (Connection c) f  = Network.TCP.closeEOF c f
  
instance HStream String where
    openStream      = openTCPConnection
    openSocketStream = socketConnection
    readBlock ref n = readBlockBS ref n

    -- This function uses a buffer, at this time the buffer is just 1000 characters.
    -- (however many bytes this is is left to the user to decypher)
    readLine ref = readLineBS ref
    -- The 'Connection' object allows no outward buffering, 
    -- since in general messages are serialised in their entirety.
    writeBlock ref str = writeBlockBS ref str -- (stringToBuf str)

    -- Closes a Connection.  Connection will no longer
    -- allow any of the other Stream functions.  Notice that a Connection may close
    -- at any time before a call to this function.  This function is idempotent.
    -- (I think the behaviour here is TCP specific)
    close c = closeIt c null True
    
    -- Closes a Connection without munching the rest of the stream.
    closeQuick c = closeIt c null False

    closeOnEnd c f = closeEOF c f
    
-- | @openTCPPort uri port@  establishes a connection to a remote
-- host, using 'getHostByName' which possibly queries the DNS system, hence 
-- may trigger a network connection.
openTCPPort :: String -> Int -> IO Connection
openTCPPort uri port = openTCPConnection uri port >>= return.Connection

-- Add a "persistent" option?  Current persistent is default.
-- Use "Result" type for synchronous exception reporting?
openTCPConnection :: BufferType ty => String -> Int -> IO (HandleStream ty)
openTCPConnection uri port = openTCPConnection_ uri port False

openTCPConnection_ :: BufferType ty => String -> Int -> Bool -> IO (HandleStream ty)
openTCPConnection_ uri port stashInput = do
    -- HACK: uri is sometimes obtained by calling Network.URI.uriRegName, and this includes
    -- the surrounding square brackets for an RFC 2732 host like [::1]. It's not clear whether
    -- it should, or whether all call sites should be using something different instead, but
    -- the simplest short-term fix is to strip any surrounding square brackets here.
    -- It shouldn't affect any as this is the only situation they can occur - see RFC 3986.
    let fixedUri =
         case uri of
            '[':(rest@(c:_)) | last rest == ']'
              -> if c == 'v' || c == 'V'
                     then error $ "Unsupported post-IPv6 address " ++ uri
                     else init rest
            _ -> uri


    -- use withSocketsDo here in case the caller hasn't used it, which would make getAddrInfo fail on Windows
    -- although withSocketsDo is supposed to wrap the entire program, in practice it is safe to use it locally
    -- like this as it just does a once-only installation of a shutdown handler to run at program exit,
    -- rather than actually shutting down after the action
    addrinfos <- withSocketsDo $ getAddrInfo (Just $ defaultHints { addrFamily = AF_UNSPEC, addrSocketType = Stream }) (Just fixedUri) (Just . show $ port)
    case addrinfos of
        [] -> fail "openTCPConnection: getAddrInfo returned no address information"
        (a:_) -> do
                s <- socket (addrFamily a) Stream defaultProtocol
                onException (do
                            setSocketOption s KeepAlive 1
                            connect s (addrAddress a)
                            socketConnection_ fixedUri port s stashInput
                            ) (Network.Socket.close s)

-- | @socketConnection@, like @openConnection@ but using a pre-existing 'Socket'.
socketConnection :: BufferType ty
                 => String
                 -> Int
                 -> Socket
                 -> IO (HandleStream ty)
socketConnection hst port sock = socketConnection_ hst port sock False

-- Internal function used to control the on-demand streaming of input
-- for /lazy/ streams.
socketConnection_ :: BufferType ty
                  => String
                  -> Int
                  -> Socket
                  -> Bool
                  -> IO (HandleStream ty)
socketConnection_ hst port sock stashInput = do
    h <- socketToHandle sock ReadWriteMode
    mb <- case stashInput of { True -> liftM Just $ buf_hGetContents bufferOps h; _ -> return Nothing }
    let conn = MkConn 
         { connSock     = sock
         , connHandle   = h
         , connBuffer   = bufferOps
         , connInput    = mb
         , connEndPoint = EndPoint hst port
         , connHooks    = Nothing
         , connCloseEOF = False
         }
    v <- newMVar conn
    return (HandleStream v)

closeConnection :: HStream a => HandleStream a -> IO Bool -> IO ()
closeConnection ref readL = do
    -- won't hold onto the lock for the duration
    -- we are draining it...ToDo: have Connection
    -- into a shutting-down state so that other
    -- threads will simply back off if/when attempting
    -- to also close it.
  c <- readMVar (getRef ref)
  closeConn c `catchIO` (\_ -> return ())
  modifyMVar_ (getRef ref) (\ _ -> return ConnClosed)
 where
   -- Be kind to peer & close gracefully.
  closeConn ConnClosed = return ()
  closeConn conn = do
    let sk = connSock conn
    hFlush (connHandle conn)
    shutdown sk ShutdownSend
    suck readL
    hClose (connHandle conn)
    shutdown sk ShutdownReceive
    Network.Socket.close sk

  suck :: IO Bool -> IO ()
  suck rd = do
    f <- rd
    if f then return () else suck rd

-- | Checks both that the underlying Socket is connected
-- and that the connection peer matches the given
-- host name (which is recorded locally).
isConnectedTo :: Connection -> EndPoint -> IO Bool
isConnectedTo (Connection conn) endPoint = isTCPConnectedTo conn endPoint

isTCPConnectedTo :: HandleStream ty -> EndPoint -> IO Bool
isTCPConnectedTo conn endPoint = do
   v <- readMVar (getRef conn)
   case v of
     ConnClosed -> return False
     _ 
      | connEndPoint v == endPoint ->
          catchIO (getPeerName (connSock v) >> return True) (const $ return False)
      | otherwise -> return False

readBlockBS :: HStream a => HandleStream a -> Int -> IO (Result a)
readBlockBS ref n = onNonClosedDo ref $ \ conn -> do
   x <- bufferGetBlock ref n
   maybe (return ())
         (\ h -> hook_readBlock h (buf_toStr $ connBuffer conn) n x)
         (connHooks' conn)
   return x

-- This function uses a buffer, at this time the buffer is just 1000 characters.
-- (however many bytes this is is left for the user to decipher)
readLineBS :: HStream a => HandleStream a -> IO (Result a)
readLineBS ref = onNonClosedDo ref $ \ conn -> do
   x <- bufferReadLine ref
   maybe (return ())
         (\ h -> hook_readLine h (buf_toStr $ connBuffer conn) x)
         (connHooks' conn)
   return x

-- The 'Connection' object allows no outward buffering, 
-- since in general messages are serialised in their entirety.
writeBlockBS :: HandleStream a -> a -> IO (Result ())
writeBlockBS ref b = onNonClosedDo ref $ \ conn -> do
  x    <- bufferPutBlock (connBuffer conn) (connHandle conn) b
  maybe (return ())
        (\ h -> hook_writeBlock h (buf_toStr $ connBuffer conn) b x)
        (connHooks' conn)
  return x

closeIt :: HStream ty => HandleStream ty -> (ty -> Bool) -> Bool -> IO ()
closeIt c p b = do
   closeConnection c (if b
                      then readLineBS c >>= \ x -> case x of { Right xs -> return (p xs); _ -> return True}
                      else return True)
   conn <- readMVar (getRef c)
   maybe (return ())
         (hook_close)
         (connHooks' conn)

closeEOF :: HandleStream ty -> Bool -> IO ()
closeEOF c flg = modifyMVar_ (getRef c) (\ co -> return co{connCloseEOF=flg})

bufferGetBlock :: HStream a => HandleStream a -> Int -> IO (Result a)
bufferGetBlock ref n = onNonClosedDo ref $ \ conn -> do
   case connInput conn of
    Just c -> do
      let (a,b) = buf_splitAt (connBuffer conn) n c
      modifyMVar_ (getRef ref) (\ co -> return co{connInput=Just b})
      return (return a)
    _ -> do
      catchIO (buf_hGet (connBuffer conn) (connHandle conn) n >>= return.return)
              (\ e ->
                       if isEOFError e
                        then do
                          when (connCloseEOF conn) $ catchIO (closeQuick ref) (\ _ -> return ())
                          return (return (buf_empty (connBuffer conn)))
                        else return (failMisc (show e)))

bufferPutBlock :: BufferOp a -> Handle -> a -> IO (Result ())
bufferPutBlock ops h b = 
  catchIO (buf_hPut ops h b >> hFlush h >> return (return ()))
          (\ e -> return (failMisc (show e)))

bufferReadLine :: HStream a => HandleStream a -> IO (Result a)
bufferReadLine ref = onNonClosedDo ref $ \ conn -> do
  case connInput conn of
   Just c -> do
    let (a,b0)  = buf_span (connBuffer conn) (/='\n') c
    let (newl,b1) = buf_splitAt (connBuffer conn) 1 b0
    modifyMVar_ (getRef ref) (\ co -> return co{connInput=Just b1})
    return (return (buf_append (connBuffer conn) a newl))
   _ -> catchIO
              (buf_hGetLine (connBuffer conn) (connHandle conn) >>= 
                    return . return . appendNL (connBuffer conn))
              (\ e ->
                 if isEOFError e
                  then do
                    when (connCloseEOF conn) $ catchIO (closeQuick ref) (\ _ -> return ())
                    return (return   (buf_empty (connBuffer conn)))
                  else return (failMisc (show e)))
 where
   -- yes, this s**ks.. _may_ have to be addressed if perf
   -- suggests worthiness.
  appendNL ops b = buf_snoc ops b nl
  
  nl :: Word8
  nl = fromIntegral (fromEnum '\n')

onNonClosedDo :: HandleStream a -> (Conn a -> IO (Result b)) -> IO (Result b)
onNonClosedDo h act = do
  x <- readMVar (getRef h)
  case x of
    ConnClosed{} -> return (failWith ErrorClosed)
    _ -> act x
 
