{-# language Rank2Types, ScopedTypeVariables  #-}
{-# language GADTs, DataKinds, TypeFamilies, RecordWildCards #-}

module Arivi.P2P
    ( module Arivi.P2P
    ) where

import Arivi.P2P.P2PEnv
import Arivi.P2P.Handler
import Arivi.Network
import qualified Arivi.P2P.Config as Config
import Arivi.P2P.Types
import Arivi.P2P.MessageHandler.HandlerTypes
import qualified Arivi.P2P.Kademlia.Types as KademliaTypes
import Arivi.P2P.Kademlia.LoadDefaultPeers
import Arivi.P2P.PRT.Instance (getKNodes)
import Arivi.P2P.RPC.Types
import Arivi.P2P.RPC.SendOptions
import Arivi.P2P.RPC.Functions

import qualified Data.HashMap.Strict as HM
import Control.Concurrent.Async.Lifted (async)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Control.Monad.Except (runExceptT)
import Control.Monad (unless)
import Control.Monad.Logger
import Control.Lens

-- | Called by the service in Xoken core
initP2P :: (HasP2PEnv env m r msg) => Config.Config -> HM.HashMap r (ResourceHandler r msg) -> ReaderT (P2PEnv r msg) m ()
initP2P config resourceHandlers = do
    udpTid <- lift $ async (runUdpServer (show (Config.udpPort config)) newIncomingConnectionHandler)
    lift $ loadDefaultPeers (Config.trustedPeers config)
    lift $ mapM_ (\(resource, handler) -> registerResource resource handler Archived) (HM.toList resourceHandlers)
    --call loadReputedPeers here after fetching reputed peers from PRT
    lift $ fillQuotas 5 -- hardcoding here assuming each resource requires 5 peers
    return ()

-- | fills up the peer list for resource. Since Options message is not for a specific resource, check after each invocation of sendOptions if the number of peers if less than required quota for any resource. Recursively keep calling till all the quotas have been satisfied.
fillQuotas :: (HasP2PEnv env m r msg) => Integer -> m ()
fillQuotas numPeers = do
    archivedMapTVar <- archived
    res <- runExceptT $ getKNodes numPeers -- Repetition of peers
    case res of
        Left _ -> return ()
        Right peers -> do
            peerNodeIds <- addPeerFromKademlia peers
            sendOptionsMessage peerNodeIds (Options :: Options r)
            archivedMap <- liftIO (readTVarIO archivedMapTVar)
            filled <- liftIO $ isFilled archivedMap
            unless filled (fillQuotas numPeers)



-- | add the peers returned by Kademlia to the PeerDetails HashMap
addPeerFromKademlia ::
       (HasNodeEndpoint m, MonadIO m)
    => [KademliaTypes.Peer]
    -> m [NodeId]
addPeerFromKademlia = mapM (\peer -> do
    nodeIdMapTVar <- getNodeIdPeerMapTVarP2PEnv
    addPeerFromKademliaHelper peer nodeIdMapTVar)

addPeerFromKademliaHelper ::
       (MonadIO m)
    => KademliaTypes.Peer
    -> TVar NodeIdPeerMap
    -> m NodeId
addPeerFromKademliaHelper peerFromKademlia nodeIdPeerMapTVar =
    liftIO $
        atomically
            (do nodeIdPeerMap <- readTVar nodeIdPeerMapTVar
                let _nodeId = fst $ KademliaTypes.getPeer peerFromKademlia
                    kadNodeEndPoint = snd $ KademliaTypes.getPeer peerFromKademlia
                    mapEntry = HM.lookup _nodeId nodeIdPeerMap
                    _ip = KademliaTypes.nodeIp kadNodeEndPoint
                    _udpPort = KademliaTypes.udpPort kadNodeEndPoint
                    _tcpPort = KademliaTypes.tcpPort kadNodeEndPoint
                case mapEntry of
                    Nothing -> do
                        defaultPeer <-
                            (& networkConfig .~ NetworkConfig {..}) <$>
                            defaultPeerDetails
                        newPeer <- newTVar defaultPeer
                        let newHashMap = HM.insert _nodeId newPeer nodeIdPeerMap
                        writeTVar nodeIdPeerMapTVar newHashMap
                    Just value -> do
                        oldPeerDetails <- readTVar value
                        let newDetails =
                                oldPeerDetails & networkConfig .~
                                NetworkConfig {..}
                        writeTVar value newDetails
                return _nodeId)


-- | Returns true if all the resources have met the minimumNodes quota and false otherwise
isFilledHelper ::Int -> [TVar [a]] -> IO Bool
isFilledHelper _ [] = return True
isFilledHelper minimumNodes l  = not <$> (fmap (any (< minimumNodes)) <$> mapM (fmap length <$> readTVarIO)) l


isFilled :: ArchivedResourceToPeerMap r msg -> IO Bool
isFilled archivedMap = do
    let resourceToPeerList = HM.toList (getArchivedMap archivedMap)
    let minNodes = 5
    isFilledHelper minNodes (fmap (snd . snd) resourceToPeerList)

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
