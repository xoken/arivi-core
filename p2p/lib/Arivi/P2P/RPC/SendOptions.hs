{-# LANGUAGE ScopedTypeVariables, GADTs, DataKinds #-}

module Arivi.P2P.RPC.SendOptions
    ( sendOptionsMessage
    , optionsHandler
    ) where

import           Arivi.P2P.Exception
import           Arivi.P2P.MessageHandler.NodeEndpoint
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.RPC.Types
import           Arivi.P2P.Types
import           Arivi.Utils.Logging
import qualified Control.Concurrent.Async.Lifted       as LAsync (async)

import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Except
import           Control.Monad.STM
import           Data.HashMap.Strict                   as HM

--This function will send the options message to all the peers in [NodeId] on separate threads
--This is the top level function that will be exposed
sendOptionsMessage :: (HasP2PEnv m, HasLogging m, Resource r) => [NodeId] -> Options r -> m ()
sendOptionsMessage [] _  = return ()
sendOptionsMessage (peer:peers) optionsMessage = do
    _ <- LAsync.async (sendOptionsToPeer peer optionsMessage)
    sendOptionsMessage peers optionsMessage

-- this function runs on each lightweight thread
-- two major functions
-- 1. Formulate and send options message
-- 2. Update the hashMap based oh the supported message returned
-- blocks while waiting for a response from the Other Peer
sendOptionsToPeer :: forall m r . (HasP2PEnv m, HasLogging m, Resource r) => NodeId -> Options r -> m ()
sendOptionsToPeer recievingPeerNodeId optionsMsg = do
    res <-
        runExceptT $ issueRequest recievingPeerNodeId (OptionRequest optionsMsg) Nothing
    case res of
        Left _ -> throw SendOptionsFailedException
        Right (OptionResponse (Supported resources :: Supported [r])) ->
            updateResourcePeers (recievingPeerNodeId, resources)

-- this wrapper will update the hashMap based on the supported message returned by the peer
updateResourcePeers :: forall m r .
       (HasP2PEnv m, Resource r) => (NodeId, [r]) -> m ()
updateResourcePeers peerResourceTuple = do
    archivedResourceToPeerMapTvar <- getArchivedResourceToPeerMapP2PEnv
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
updateResourcePeersHelper :: forall r . (Resource r) =>
       NodeId -> [r] -> ArchivedResourceToPeerMap -> IO Int
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

-- | takes an options message and returns a supported message
optionsHandler ::
       (HasP2PEnv m, Resource r) => m (Supported [r])
optionsHandler = do
    tvar <- getArchivedResourceToPeerMapP2PEnv
    archivedResourceMap <- (liftIO . readTVarIO) tvar
    return (Supported (keys (getArchivedMap archivedResourceMap)))
