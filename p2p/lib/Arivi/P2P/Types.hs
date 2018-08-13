{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DeriveFunctor,
  DeriveTraversable, DeriveAnyClass, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds #-}
{-# LANGUAGE FlexibleInstances, AllowAmbiguousTypes #-}

-- |
-- Module      :  Arivi.P2P.Types
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides different data types that are used in the P2P layer
--
module Arivi.P2P.Types (module Arivi.P2P.Types, NetworkConfig(..)) where

import           Arivi.Network.Types (NetworkConfig(..))
import           Codec.Serialise
import           Data.Time
import           GHC.Generics        (Generic)
import           Data.HashMap.Strict (HashMap)
import           Data.Hashable
import           Data.Proxy

type Map = HashMap

data RpcRequest msg r  = RpcRequest msg r  deriving (Eq, Ord, Show, Generic)
data RpcResponse msg r =  RpcResponse msg r deriving (Eq, Ord, Show, Generic)

instance (Serialise r, Serialise msg) => Serialise (RpcRequest r msg)
instance (Serialise r, Serialise msg) => Serialise (RpcResponse r msg)

data MessageType = Kademlia
                 | Options
                 | Rpc
                 | PubSub
                 deriving (Eq, Show, Ord, Generic, Serialise, Hashable)

data Void

data RequestG (i :: MessageType) msg  where
  RpcRequestG :: (Serialise msg, Serialise r) => RpcRequest msg r -> RequestG 'Rpc msg
  OptionsRequestG :: SRequest -> RequestG 'Options Void

data ResponseG (i :: MessageType) where
  RpcResponseG :: RpcResponse m r -> ResponseG 'Rpc
  OptionsResponseG :: SResponse r -> ResponseG 'Options

data ResponseCode
    = Error
    | Ok
    deriving (Eq, Ord, Show, Generic)

instance Serialise ResponseCode

data PubSub msg t = Subscribe UTCTime t
                  | Notify msg t
                  | Publish msg t
                  | PubSubResponse ResponseCode UTCTime t
                  deriving (Eq, Ord, Show, Generic)

deriving instance Functor (PubSub msg)
deriving instance Foldable (PubSub msg)
deriving instance Traversable (PubSub msg)

instance (Serialise t, Serialise msg) => Serialise (PubSub t msg)

data SRequest = SRequest deriving (Eq, Ord, Show, Generic, Serialise)
data SResponse r = SResponse r deriving (Eq, Ord, Show, Generic, Serialise)

newtype Handler i o m = Handler { runHandler :: i -> m o }

data Kademlia
data Rpc

deriving instance Generic Kademlia
deriving instance Serialise Kademlia

newtype Request i msg  = Request msg deriving (Eq, Ord, Show, Generic, Serialise)
newtype Response i msg = Response msg deriving (Eq, Ord, Show, Generic, Serialise)

class Msg (i :: k) where
  msgType :: Proxy i -> MessageType

instance Msg i => Msg (Request i msg) where
  msgType _ = msgType (Proxy :: Proxy i)


instance Msg i => Msg (Response i msg) where
  msgType _ = msgType (Proxy :: Proxy i)

instance Msg Rpc where
  msgType _ = Rpc

instance Msg Kademlia where
  msgType _ = Kademlia

instance Msg i => Msg (RequestG i) where
  msgType _ = msgType (Proxy :: Proxy i)

instance Msg (ResponseG 'Options) where
  msgType _ = Options

instance Msg 'Rpc where
  msgType _ = Rpc

instance Msg 'Options where
  msgType _ = Options

{-

-- | Sends a request and gets a response. Should be catching all the exceptions thrown and handle them correctly
issueRequest' :: forall i m .(HasP2PEnv m, HasLogging m, Msg i)
    => NodeId
    -> RequestG i
    -> m (ResponseG i)
issueRequest' peerNodeId req = do
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    nodeIdPeerMap <- liftIO $ readTVarIO nodeIdMapTVar
    handleOrFail <-  LE.try $ getConnectionHandle peerNodeId nodeIdMapTVar (msgType (Proxy :: Proxy (RequestG i)))
    let peerDetailsTVarOrFail = HM.lookup peerNodeId nodeIdPeerMap
    case peerDetailsTVarOrFail of
        Nothing -> logWithNodeId peerNodeId "sendRequest called without adding peerNodeId: " >> throw HandlerConnectionDetailsNotFound
        Just peerDetailsTVar ->
            case handleOrFail of
                Left (e::AriviP2PException) -> $(logDebug) "getConnectionHandle failed" >> throw e
                Right connHandle -> do
                    case req of
                      RpcRequestG r -> do
                        (uuid, updatedPeerDetailsTVar) <- sendRequest peerNodeId (msgType (Proxy :: Proxy (RequestG i))) connHandle peerDetailsTVar (serialise r)
                        resp :: RpcResponse Int Int  <- deserialise <$> receiveResponse peerNodeId uuid updatedPeerDetailsTVar
                        return (RpcResponseG resp)
                      OptionsRequestG r -> do
                        (uuid, updatedPeerDetailsTVar) <- sendRequest peerNodeId (msgType (Proxy :: Proxy (RequestG i))) connHandle peerDetailsTVar (serialise r)
                        (OptionsResponseG . deserialise) <$> receiveResponse peerNodeId uuid updatedPeerDetailsTVar

-}

main :: Response Kademlia Int
main = deserialise (serialise (Request 1 :: Request Rpc Int))
