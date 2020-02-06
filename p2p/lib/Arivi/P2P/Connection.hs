<<<<<<< HEAD
module Arivi.P2P.Connection (
      getConnectionHandle
) where

import           Arivi.P2P.Exception
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.MessageHandler.Utils
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.Handler

import qualified Control.Concurrent.Async.Lifted       as LA (async)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMVar          (putTMVar, takeTMVar)
import           Control.Monad.IO.Class                (liftIO)
import           Data.HashMap.Strict                   as HM
import           Control.Lens
=======
module Arivi.P2P.Connection
    ( getConnectionHandle
    ) where

import Arivi.P2P.Exception
import Arivi.P2P.Handler
import Arivi.P2P.MessageHandler.HandlerTypes
import Arivi.P2P.MessageHandler.Utils
import Arivi.P2P.P2PEnv
import Codec.Serialise
import qualified Control.Concurrent.Async.Lifted as LA (async)
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar (putTMVar, takeTMVar)
import Control.Lens
import Control.Monad.IO.Class (liftIO)

import Data.HashMap.Strict as HM
>>>>>>> breaking out arivi-core from arivi

-- | Obtains the connectionLock on entry and then checks if connection has been made. If yes, then simply returns the connectionHandle; else it tries to openConnection
-- | Returns the connectionHandle or an exception
createConnection ::
<<<<<<< HEAD
       (HasP2PEnv env m r t rmsg pmsg)
    => TVar PeerDetails
    -> TVar NodeIdPeerMap
    -> TransportType
    -> m (Either AriviP2PException ConnectionHandle)
=======
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t rmsg pmsg) =>
           TVar PeerDetails -> TVar NodeIdPeerMap -> TransportType -> m (Either AriviP2PException ConnectionHandle)
>>>>>>> breaking out arivi-core from arivi
createConnection peerDetailsTVar _ transportType = do
    peerDetails <- liftIO $ atomically $ readTVar peerDetailsTVar
    lock <- liftIO $ atomically $ takeTMVar (peerDetails ^. connectionLock)
    connHandleEither <-
        case checkConnection peerDetails transportType of
<<<<<<< HEAD
            Connected connHandle -> return $ Right connHandle
            NotConnected -> networkToP2PException <$> openConnectionToPeer (peerDetails ^. networkConfig) transportType
=======
            Connected connHandle -> do
                return $ Right connHandle
            NotConnected -> do
                networkToP2PException <$> openConnectionToPeer (peerDetails ^. networkConfig) transportType
>>>>>>> breaking out arivi-core from arivi
    liftIO $ atomically $ putTMVar (peerDetails ^. connectionLock) lock
    case connHandleEither of
        Right c -> do
            liftIO $ atomically $ updatePeer transportType (Connected c) peerDetailsTVar
            _ <- LA.async $ readIncomingMessage c peerDetailsTVar
            return (Right c)
<<<<<<< HEAD
        Left e -> return (Left e)

-- | Gets the connection handle for the particular message type. If not present, it will create and return else will throw an exception
getConnectionHandle ::
       (HasP2PEnv env m r t rmsg pmsg)
    => NodeId
    -> TVar NodeIdPeerMap
    -> TransportType
    -> m (Either AriviP2PException ConnectionHandle)
getConnectionHandle peerNodeId nodeToPeerTVar transportType = do
    nodeIdPeerMap <- liftIO $ atomically $ readTVar nodeToPeerTVar
    -- should find an entry in the hashmap
    -- exception if it is not found
=======
        Left e -> do
            return (Left e)

-- | Gets the connection handle for the particular message type. If not present, it will create and return else will throw an exception
getConnectionHandle ::
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t rmsg pmsg) =>
           NodeId -> TVar NodeIdPeerMap -> TransportType -> m (Either AriviP2PException ConnectionHandle)
getConnectionHandle peerNodeId nodeToPeerTVar transportType = do
    nodeIdPeerMap <- liftIO $ atomically $ readTVar nodeToPeerTVar
  -- should find an entry in the hashmap
  -- exception if it is not found
>>>>>>> breaking out arivi-core from arivi
    case HM.lookup peerNodeId nodeIdPeerMap of
        Just peerDetailsTVar -> do
            peerDetails <- liftIO $ atomically $ readTVar peerDetailsTVar
            case getHandlerByMessageType peerDetails transportType of
<<<<<<< HEAD
                Connected connHandle -> return (Right connHandle)
                NotConnected -> createConnection peerDetailsTVar nodeToPeerTVar transportType
=======
                Connected connHandle -> do
                    return (Right connHandle)
                NotConnected -> do
                    createConnection peerDetailsTVar nodeToPeerTVar transportType
>>>>>>> breaking out arivi-core from arivi
        Nothing -> return (Left PeerNotFound)
