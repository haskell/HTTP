{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.TCP
-- Copyright   :  (c) Warrick Gray 2002, Bjorn Bringert 2003-2004, Simon Foster 2004
-- License     :  BSD
--
-- Maintainer  :  Sigbjorn Finne <sigbjorn.finne@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- Some utility functions for working with the Haskell @network@ package. Mostly
-- for internal use by the @Network.HTTP@ code, but 
--      
-----------------------------------------------------------------------------
module Network.TCP
   ( Connection
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
   , hstreamToConnection

   ) where

import Network.BSD (getHostByName, hostAddresses)
import Network.Socket
   ( Socket, SockAddr(SockAddrInet), SocketOption(KeepAlive)
   , SocketType(Stream), inet_addr, connect
   , shutdown, ShutdownCmd(ShutdownSend, ShutdownReceive)
   , sClose, sIsConnected, setSocketOption
   , socket, Family(AF_INET)
   )
import qualified Network.Stream as Stream
   ( Stream(readBlock, readLine, writeBlock, close) )
import Network.Stream
   ( ConnError(..)
   , Result
   , failWith
   )
import Network.BufferType

import Network.HTTP.Base ( catchIO )
import Network.Socket ( socketToHandle )

import Data.Char  ( toLower )
import Data.Word  ( Word8 )
import Control.Concurrent
import Control.Monad ( liftM )
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

data Conn a 
 = MkConn { connSock     :: ! Socket
	  , connHandle   :: Handle
          , connBuffer   :: BufferOp a
	  , connInput    :: Maybe a
          , connHost     :: String
	  , connHooks    :: Maybe (StreamHooks a)
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
     }

instance Eq ty => Eq (StreamHooks ty) where
  (==) _ _ = True

nullHooks :: StreamHooks ty
nullHooks = StreamHooks 
     { hook_readLine   = \ _ _   -> return ()
     , hook_readBlock  = \ _ _ _ -> return ()
     , hook_writeBlock = \ _ _ _ -> return ()
     , hook_close      = return ()
     }

setStreamHooks :: HandleStream ty -> StreamHooks ty -> IO ()
setStreamHooks h sh = modifyMVar_ (getRef h) (\ c -> return c{connHooks=Just sh})

-- | @HStream@ overloads the use of 'HandleStream's, letting you
-- overload the handle operations over the type that is communicated
-- across the handle. It is used in the context of @Network.HTTP@ to
-- buy us freedom in how HTTP 'Request' and 'Response' payloads are
-- represented. 
--
-- The package provides instances for @ByteString@s and @String@, but
-- should you want to plug in your own payload representation, defining
-- your own @HStream@ instance is all it takes.
-- 
class BufferType bufType => HStream bufType where
  openStream       :: String -> Int -> IO (HandleStream bufType)
  openSocketStream :: String -> Socket -> IO (HandleStream bufType)
  readLine   :: HandleStream bufType -> IO (Result bufType)
  readBlock  :: HandleStream bufType -> Int -> IO (Result bufType)
  writeBlock :: HandleStream bufType -> bufType -> IO (Result ())
  close      :: HandleStream bufType -> IO ()
  
instance HStream Strict.ByteString where
  openStream       = openTCPConnection
  openSocketStream = socketConnection
  readBlock c n    = readBlockBS c n
  readLine c       = readLineBS c
  writeBlock c str = writeBlockBS c str
  close c          = closeIt c Strict.null

instance HStream Lazy.ByteString where
    openStream       = \ a b -> openTCPConnection_ a b True
    openSocketStream = \ a b -> socketConnection_ a b True
    readBlock c n    = readBlockBS c n
    readLine c       = readLineBS c
    writeBlock c str = writeBlockBS c str
    close c          = closeIt c Lazy.null

instance Stream.Stream Connection where
  readBlock (Connection c)  = Network.TCP.readBlock c
  readLine (Connection c)   = Network.TCP.readLine c
  writeBlock (Connection c) = Network.TCP.writeBlock c 
  close (Connection c)      = Network.TCP.close c
  
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
    close c = closeIt c null
    
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
    s <- socket AF_INET Stream 6
    setSocketOption s KeepAlive 1
    hostA <- getHostAddr uri
    let a = SockAddrInet (toEnum port) hostA
    catchIO (connect s a) (\e -> sClose s >> ioError e)
    socketConnection_ uri s stashInput
 where
  getHostAddr h = do
    catchIO (inet_addr uri)    -- handles ascii IP numbers
            (\ _ -> do
	        host <- getHostByName_safe uri
                case hostAddresses host of
                  []     -> fail ("openTCPConnection: no addresses in host entry for " ++ show h)
                  (ha:_) -> return ha)

  getHostByName_safe h = 
    catchIO (getHostByName h)
            (\ _ -> fail ("openTCPConnection: host lookup failure for " ++ show h))

-- | @socketConnection@, like @openConnection@ but using a pre-existing 'Socket'.
socketConnection :: BufferType ty
                 => String
		 -> Socket
		 -> IO (HandleStream ty)
socketConnection hst sock = socketConnection_ hst sock False

-- Internal function used to control the on-demand streaming of input
-- for /lazy/ streams.
socketConnection_ :: BufferType ty
                  => String
		  -> Socket
		  -> Bool
		  -> IO (HandleStream ty)
socketConnection_ hst sock stashInput = do
    h <- socketToHandle sock ReadWriteMode
    mb <- case stashInput of { True -> liftM Just $ buf_hGetContents bufferOps h; _ -> return Nothing }
    let conn = MkConn 
         { connSock   = sock
	 , connHandle = h
	 , connBuffer = bufferOps
	 , connInput  = mb
	 , connHost   = hst
	 , connHooks  = Nothing
	 }
    v <- newMVar conn
    return (HandleStream v)

closeConnection :: HandleStream a -> IO Bool -> IO ()
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
    shutdown sk ShutdownSend
    suck readL
    hClose (connHandle conn)
    shutdown sk ShutdownReceive
    sClose sk

  suck :: IO Bool -> IO ()
  suck rd = do
    f <- rd
    if f then return () else suck rd

-- | Checks both that the underlying Socket is connected
-- and that the connection peer matches the given
-- host name (which is recorded locally).
isConnectedTo :: Connection -> String -> IO Bool
isConnectedTo (Connection conn) name = do
   v <- readMVar (getRef conn)
   case v of
     ConnClosed -> return False
     _ 
      | map toLower (connHost v) == map toLower name -> sIsConnected (connSock v)
      | otherwise -> return False

isTCPConnectedTo :: HandleStream ty -> String -> IO Bool
isTCPConnectedTo conn name = do
   v <- readMVar (getRef conn)
   case v of
     ConnClosed -> return False
     _ 
      | map toLower (connHost v) == map toLower name -> sIsConnected (connSock v)
      | otherwise -> return False

readBlockBS :: HandleStream a -> Int -> IO (Result a)
readBlockBS ref n = onNonClosedDo ref $ \ conn -> do
   x <- bufferGetBlock ref n
   maybe (return ())
         (\ h -> hook_readBlock h (buf_toStr $ connBuffer conn) n x)
	 (connHooks' conn)
   return x

-- This function uses a buffer, at this time the buffer is just 1000 characters.
-- (however many bytes this is is left for the user to decipher)
readLineBS :: HandleStream a -> IO (Result a)
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

closeIt :: HandleStream ty -> (ty -> Bool) -> IO ()
closeIt c p = do
  closeConnection c (readLineBS c >>= \ x -> case x of { Right xs -> return (p xs); _ -> return True})
  conn <- readMVar (getRef c)
  maybe (return ())
        (hook_close)
	(connHooks' conn)

bufferGetBlock :: HandleStream a -> Int -> IO (Result a)
bufferGetBlock ref n = onNonClosedDo ref $ \ conn -> do
   case connInput conn of
    Just c -> do
      let (a,b) = buf_splitAt (connBuffer conn) n c
      modifyMVar_ (getRef ref) (\ co -> return co{connInput=Just b})
      return (return a)
    _ -> do
      Prelude.catch (buf_hGet (connBuffer conn) (connHandle conn) n >>= return.return)
                    (\ e -> if isEOFError e 
			     then return (return (buf_empty (connBuffer conn)))
			     else return (fail (show e)))

bufferPutBlock :: BufferOp a -> Handle -> a -> IO (Result ())
bufferPutBlock ops h b = 
  Prelude.catch (buf_hPut ops h b >> hFlush h >> return (return ()))
                (\ e -> return (fail (show e)))

bufferReadLine :: HandleStream a -> IO (Result a) -- BufferOp a -> Handle -> IO (Result a)
bufferReadLine ref = onNonClosedDo ref $ \ conn -> do
  case connInput conn of
   Just c -> do
    let (a,b0)  = buf_span (connBuffer conn) (/='\n') c
    let (newl,b1) = buf_splitAt (connBuffer conn) 1 b0
    modifyMVar_ (getRef ref) (\ co -> return co{connInput=Just b1})
    return (return (buf_append (connBuffer conn) a newl))
   _ -> Prelude.catch 
              (buf_hGetLine (connBuffer conn) (connHandle conn) >>= 
	            return . return . appendNL (connBuffer conn))
              (\ e -> 
                 if isEOFError e
                  then return (return   (buf_empty (connBuffer conn)))
                  else return (fail (show e)))
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
 
