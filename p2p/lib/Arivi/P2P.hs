{-# language DeriveGeneric, DeriveAnyClass, Rank2Types, ScopedTypeVariables  #-}
{-# language GADTs, DataKinds, KindSignatures, TypeFamilies #-}
{-# language PartialTypeSignatures, ApplicativeDo, RecordWildCards,ConstraintKinds #-}

module Arivi.P2P where

import Arivi.P2P.Types hiding (Resource)
import Arivi.P2P.P2PEnv
import Arivi.Network
import Arivi.P2P.MessageHandler.HandlerTypes (HasNetworkConfig(..))
import Arivi.P2P.RPC.Functions
import Arivi.P2P.Kademlia.MessageHandler
import Arivi.P2P.MessageHandler.NodeEndpoint
import Arivi.P2P.RPC.Types
import Arivi.Env
import qualified Arivi.P2P.Kademlia.Types              as T
import qualified CreateConfig                           as Config
import           Arivi.Utils.Statsd

import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.ByteString.Lazy (ByteString)
import Control.Concurrent.Async.Lifted (async, wait)
import Codec.Serialise
import Control.Concurrent.STM
import Control.Monad.Reader
import GHC.Generics
import           Control.Monad.Logger


initP2P :: forall m r msg env . (Eq r, Hashable r, Serialise msg, Serialise r, HasP2PEnv m r msg, MonadReader env m) => Config.Config -> HM.HashMap r ResourceHandler -> r -> msg -> ReaderT (P2PEnv r msg) m ()
initP2P config resourceHandlers resource message = do
    p2pEnv <- lift $ liftIO $ initP2PEnv config resource message
    udpTid <- lift $ async (runUdpServer (show (Config.udpPort config)) newIncomingConnectionHandler)
    return ()


initP2PEnv :: Config.Config -> r -> msg -> IO (P2PEnv r msg)
initP2PEnv config resource message = do
    let networkConfig = NetworkConfig (Config.myNodeId config) (Config.myIp config) (Config.tcpPort config) (Config.udpPort config)
    nodeEndpointEnv <- initNodeEndpoint config networkConfig
    rpcEnv <- initRpc
    kademliaEnv <- initKademlia networkConfig 1 1 1
    newStatsdClient <- createStatsdClient "statsdIP" 8080 "statsdPrefix"
    rt <- initResourceType resource
    mt <- initMessageType message
    return $ P2PEnv nodeEndpointEnv rpcEnv kademliaEnv newStatsdClient rt mt


initResourceType :: r -> IO (ResourceType r)
initResourceType resource = return (ResourceType resource)

initMessageType :: msg -> IO (ServiceMessageType msg)
initMessageType message = return (ServiceMessageType message)

initNodeEndpoint :: Config.Config -> NetworkConfig -> IO NodeEndpointEnv
initNodeEndpoint config networkConfig = do
    let networkEnv = mkAriviEnv (read $ show $ Config.tcpPort config) (read $ show $ Config.udpPort config) (Config.secretKey config)
    let handlers = Handlers { rpc = rpcHandlerHelper
                            , kademlia = kademliaHandlerHelper}
    mkNodeEndpoint networkConfig handlers networkEnv

initRpc :: IO (RpcEnv r)
initRpc = mkRpcEnv

initKademlia :: NetworkConfig -> Int -> Int -> Int -> IO KademliaEnv
initKademlia nc@NetworkConfig{..} sbound pingThreshold kademliaConcurrencyFactor =
    KademliaEnv <$>
        T.createKbucket
            (T.Peer (_nodeId, T.NodeEndPoint _ip _tcpPort _udpPort))
            sbound
            pingThreshold
            kademliaConcurrencyFactor

-- init :: Map k (Handler msg IO) -> IO ()
-- init handlers = do
--   let peers = Map.empty
--   -- loadDefaultPeers
--   -- sendOptions (Map.keys handlers)
--   return ()



-- runHandlerConcurrently :: Handler i o IO -> [i] -> IO [o]
-- runHandlerConcurrently h is = mapConcurrently (runHandler h) is

-- blockRequest :: Request 'Rpc
-- blockRequest = RpcRequestG (RpcRequest "hey" BlockResource)

-- blockResponse :: Response 'Rpc
-- blockResponse = RpcResponseG (RpcResponse "hey" BlockResource)

-- mainOutgoing :: IO ()
-- mainOutgoing = do
--   a <- runHandler (issueRequest coolMap) ("key1", blockRequest)
--   print (fst a)

-- coolMap :: Map NodeId PeerDetails
-- coolMap = Map.insert "key1" "value1" (Map.insert "key2" "value2" Map.empty)

-- issueRequest :: forall i.
--        Map NodeId PeerDetails
--     -> Handler (NodeId, Request i) (NodeId, Response i) IO
-- issueRequest _ =
--     Handler $ \(nId, req) -> do
--         case req of
--           RpcRequestG (RpcRequest msg r) -> do
--             print (serialise msg)
--             print (serialise r)
--             return (nId, RpcResponseG (RpcResponse msg BlockResource))
--           OptionsRequestG SRequest -> do
--             return (nId, OptionsResponseG (SResponse BlockResource))

-- optionsHandler :: [k] -> Handler SRequest (SResponse [k]) IO
-- optionsHandler resources = Handler (const $ return (SResponse resources))

-- {-
-- -- This is assumed to be running in a thread
-- requestOptions ::
--        TVar (Map NodeId PeerDetails)
--     -> [NodeId]
--     -> Map k (TVar [NodeId])
--     -> IO ()
-- requestOptions peers nodes resourcers = do
--     bs <-
--         runHandlerConcurrently
--             (issueRequest peers Option)
--             (zip nodes (repeat SRequest)) :: IO [(NodeId, SResponse Int)]
--     return ()

-- fetchResource ::
--        (Serialise i, Serialise o)
--     => TVar (Map NodeId PeerDetails)
--     -> Map k (TVar [NodeId])
--     -> k
--     -> Handler i (NodeId, o) IO
-- fetchResource peers resourcers resource =
--     Handler $ \x -> (runHandler (issueRequest peers RPC) (nId, x))
--   where
--     nId = undefined -- lookup in resourcers

-- rpcHandler ::
--        (Serialise i, Serialise o)
--     => Map k (Handler (RpcRequest i k) (RpcResponse o k) IO)
--     -> Handler (RpcRequest i k) (RpcResponse o k) IO
-- rpcHandler m = undefined

-- notify ::
--        (Serialise i, Serialise t)
--     => TVar (Map NodeId PeerDetails)
--     -> NodeId
--     -> t
--     -> Handler i () IO
-- notify peers node t = undefined --handleit

-- notifyAll :: (Serialise i, Serialise t) => TVar (Map NodeId PeerDetails) -> Map t (TVar [NodeId]) -> t -> Handler i () IO
-- notifyAll peers nodes t = Handler $ \i -> do return ()
-- -}

-- data Block = Block BlockHeader Int deriving (Eq, Ord, Show, Generic, Serialise)
-- data BlockHeader = BlockHeader Int deriving (Eq, Ord, Show, Generic, Serialise)

-- serviceMessage :: String
-- serviceMessage = "GetBlocks"

-- data Resource = BlockResource
--               | BlockHeaderResource
--               deriving (Eq, Ord, Show, Generic, Serialise, Hashable)

-- blockResourceHandler ::
--        Handler (RpcRequest ByteString Resource) (RpcResponse ByteString Resource) IO
-- blockResourceHandler =
--     Handler $ \b@(RpcRequest m r) -> do
--         print "In blockResourceHandler" >> print b
--         return (RpcResponse m r)

-- blockHeaderResourceHandler ::
--        Handler (RpcRequest ByteString Resource) (RpcResponse ByteString Resource) IO
-- blockHeaderResourceHandler =
--     Handler $ \b@(RpcRequest m r) -> do
--         print "In blockHeaderResourceHandler" >> print b
--         return (RpcResponse m r)

-- handlerMap :: Map Resource (Handler (RpcRequest ByteString Resource) (RpcResponse ByteString Resource) IO)
-- handlerMap =
--     Map.insert
--         BlockResource
--         blockResourceHandler
--         (Map.insert BlockHeaderResource blockHeaderResourceHandler Map.empty)

-- resourcers :: Map Resource (TVar [NodeId])
-- resourcers =
--     Map.insert
--         BlockResource
--         undefined
--         (Map.insert BlockHeaderResource undefined Map.empty)

-- {-
-- getBlock :: IO (NodeId, Block)
-- getBlock =
--     runHandler (fetchResource undefined resourcers BlockResource) serviceMessage
-- -}

-- invoke :: forall k o i.
--           (Serialise i, Serialise k, Eq k, Show k, Hashable k, Show i)
--        => ByteString
--        -> Map k (Handler (RpcRequest i k) (RpcResponse o k) IO)
--        -> IO (RpcResponse o k)
-- invoke bs m = do
--     let rr@(RpcRequest _ k) = deserialise bs :: (RpcRequest i k)
--     print "In invoke "
--     print rr
--     case Map.lookup k m of
--         Just h -> runHandler h rr
--         Nothing -> error "Cover has blown"

-- mainIncoming :: IO ()
-- mainIncoming = do
--   let rpcMessage = serialise (RpcRequest (serialise "abc") BlockResource)
--   a <- invoke rpcMessage handlerMap :: IO (RpcResponse ByteString Resource)
--   print "In main"
--   print a

-- join' :: (Functor m) => m (m a) -> m a
-- join' m = bind m id

-- bind :: (Functor m) => m a -> (a -> m b) -> m b
-- bind m f = join' (fmap f m)

-- newtype Req a = Req a deriving (Show, Eq, Ord, Generic, Serialise)
-- newtype Resp a = Resp a deriving (Show, Eq, Ord, Generic, Serialise)

-- data MType = MRpc deriving (Show, Eq, Ord, Generic, Serialise)

-- class Message a where
--   msgType :: a -> MType

-- class Rpc a where
--   resource :: a -> String

-- instance (Message a) => Message (Req a) where
--   msgType (Req a) = msgType a

-- instance (Message a) => Message (Resp a) where
--   msgType (Resp a) = msgType a

-- instance Rpc a => Rpc (Req a) where
--    resource (Req a) = resource a

-- invoke' ::
--        forall o i.
--        (Serialise i, Eq i, Hashable i, Show i, Rpc i)
--     => ByteString
--     -> Map i (Handler (Req i) (Resp o) IO)
--     -> IO (Resp o)
-- invoke' bs m = do
--     let rr@(Req i) = deserialise bs :: (Req i)
--     print "In invoke "
--     print rr
--     case Map.lookup i m of
--         Just h -> runHandler h rr
--         Nothing -> error "Cover has blown"

-- mainIncoming' :: IO ()
-- mainIncoming' = do
--   let rpcMessage = serialise (RpcRequest (serialise "abc") BlockResource)
--   a <- invoke rpcMessage handlerMap :: IO (RpcResponse ByteString Resource)
--   print "In main"
--   print a
-- -}
