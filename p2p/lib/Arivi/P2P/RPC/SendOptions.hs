{-# LANGUAGE ScopedTypeVariables, GADTs #-}

module Arivi.P2P.RPC.SendOptions
    ( sendOptionsMessage
    ) where

<<<<<<< HEAD
import           Arivi.P2P.Exception
import           Arivi.P2P.MessageHandler.NodeEndpoint
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.RPC.Types
import           Arivi.P2P.Types
import qualified Control.Concurrent.Async.Lifted       as LAsync (async)

import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Except
import           Control.Monad.STM
import           Data.HashMap.Strict                   as HM

--This function will send the options message to all the peers in [NodeId] on separate threads
--This is the top level function that will be exposed
sendOptionsMessage ::
       ( HasP2PEnv env m r t rmsg pmsg
       )
    => [NodeId]
    -> Options r
    -> m ()
sendOptionsMessage peers optionsMessage =
    mapM_ (LAsync.async . flip sendOptionsToPeer optionsMessage) peers

-- this function runs on each lightweight thread
-- two major functions
-- 1. Formulate and send options message
-- 2. Update the hashMap based on the supported message returned
-- blocks while waiting for a response from the Other Peer
sendOptionsToPeer ::
       forall env m r t rmsg pmsg. (HasP2PEnv env m r t rmsg pmsg)
    => NodeId
    -> Options r
    -> m ()
sendOptionsToPeer recievingPeerNodeId optionsMsg = do
    res <-
        runExceptT $ issueRequest recievingPeerNodeId (OptionRequest optionsMsg)
    case res of
        Left _ -> throw SendOptionsFailedException
        Right (OptionResponse (Supported resources :: Supported [r])) ->
            updateResourcePeers (recievingPeerNodeId, resources)

-- this wrapper will update the hashMap based on the supported message returned by the peer
updateResourcePeers ::
       (HasP2PEnv env m r t rmsg pmsg) => (NodeId, [r]) -> m ()
updateResourcePeers peerResourceTuple = do
    archivedResourceToPeerMapTvar <- archived
    archivedResourceToPeerMap <-
        liftIO $ readTVarIO archivedResourceToPeerMapTvar
    let mNode = fst peerResourceTuple
    let listOfResources = snd peerResourceTuple
    _ <-
        liftIO $
        updateResourcePeersHelper
            mNode
            listOfResources
            archivedResourceToPeerMap
    return ()

-- adds the peer to the TQueue of each resource
-- lookup for the current resource in the HashMap
-- assumes that the resourceIDs are present in the HashMap
-- cannot add new currently because the serviceID is not available
updateResourcePeersHelper :: (Resource r) =>
       NodeId -> [r] -> ArchivedResourceToPeerMap r msg -> IO Int
updateResourcePeersHelper _ [] _ = return 0
updateResourcePeersHelper mNodeId (currResource:listOfResources) archivedResourceToPeerMap = do
    let temp = HM.lookup currResource (getArchivedMap archivedResourceToPeerMap) -- check for lookup returning Nothing
    case temp of
        Nothing ->
            updateResourcePeersHelper
                mNodeId
                listOfResources
                archivedResourceToPeerMap
        Just entry -> do
            let nodeListTVar = snd entry
            atomically
                (do nodeList <- readTVar nodeListTVar
                    let updatedList = nodeList ++ [mNodeId]
                    writeTVar nodeListTVar updatedList)
            tmp <-
                updateResourcePeersHelper
                    mNodeId
                    listOfResources
                    archivedResourceToPeerMap
            return $ 1 + tmp
=======
import Arivi.P2P.Exception
import Arivi.P2P.MessageHandler.HandlerTypes
import Arivi.P2P.MessageHandler.NodeEndpoint
import Arivi.P2P.P2PEnv
import Arivi.P2P.RPC.Env
import Arivi.P2P.RPC.Types
import Arivi.P2P.Types
import Codec.Serialise
import qualified Control.Concurrent.Async.Lifted as LAsync (mapConcurrently_)

import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.STM
import Data.Hashable
import qualified Data.Set as Set

--This function will send the options message to all the peers in [NodeId] on separate threads
sendOptionsMessage ::
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t rmsg pmsg) =>
           [NodeId] -> Options r -> m ()
sendOptionsMessage peers optionsMessage = LAsync.mapConcurrently_ (`sendOptionsToPeer` optionsMessage) peers

-- | Sends the Options message to a single peer and updates the Resourcers table based on the Supported message
sendOptionsToPeer ::
       forall env m r t rmsg pmsg. (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t rmsg pmsg) =>
           NodeId -> Options r -> m (Either AriviP2PException ())
sendOptionsToPeer recievingPeerNodeId optionsMsg = do
    res <- runExceptT $ issueRequest recievingPeerNodeId (OptionRequest optionsMsg)
    case res of
        Left _ -> return (Left SendOptionsFailedException)
        Right (OptionResponse (Supported resources :: Supported [r])) ->
            Right <$> updateResourcers recievingPeerNodeId resources

updateResourcers :: (MonadReader env m, HasRpc env r msg, MonadIO m) => NodeId -> [r] -> m ()
updateResourcers nId resourceList = do
    rpcRecord <- asks rpcEnv
    liftIO $ print ("sendOptions-updateResourcers")
    let resourcers = rpcResourcers rpcRecord
    mapM_ (updateResourcers' nId resourcers) resourceList

updateResourcers' :: (Ord r, Hashable r, MonadIO m) => NodeId -> Resourcers r -> r -> m ()
updateResourcers' nid (Resourcers resourcers) resource =
    case resourcers ^. at resource of
        Just x -> liftIO $ atomically $ modifyTVar x (Set.insert nid)
    -- | Shouldn't reach the nothing branch as all resources should be registered
        Nothing -> return ()
>>>>>>> breaking out arivi-core from arivi
