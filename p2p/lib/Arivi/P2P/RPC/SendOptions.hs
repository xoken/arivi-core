{-# LANGUAGE ScopedTypeVariables, GADTs #-}

module Arivi.P2P.RPC.SendOptions
    ( sendOptionsMessage
    ) where

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
