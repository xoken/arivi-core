{-# LANGUAGE ScopedTypeVariables, GADTs #-}

module Arivi.P2P.RPC.SendOptions
    ( sendOptionsMessage
    ) where

import           Arivi.P2P.Exception
import           Arivi.P2P.MessageHandler.NodeEndpoint
import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.RPC.Types
import           Arivi.P2P.RPC.Env
import           Arivi.P2P.Types
import qualified Control.Concurrent.Async.Lifted       as LAsync(mapConcurrently_)

import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.Hashable
import qualified Data.Set                              as Set

--This function will send the options message to all the peers in [NodeId] on separate threads
sendOptionsMessage ::
       ( HasP2PEnv env m r t rmsg pmsg
       )
    => [NodeId]
    -> Options r
    -> m ()
sendOptionsMessage peers optionsMessage =
    LAsync.mapConcurrently_ (`sendOptionsToPeer` optionsMessage) peers

-- | Sends the Options message to a single peer and updates the Resourcers table based on the Supported message
sendOptionsToPeer ::
       forall env m r t rmsg pmsg. (HasP2PEnv env m r t rmsg pmsg)
    => NodeId
    -> Options r
    -> m (Either AriviP2PException ())
sendOptionsToPeer recievingPeerNodeId optionsMsg = do
    res <-
        runExceptT $ issueRequest recievingPeerNodeId (OptionRequest optionsMsg)
    case res of
        Left _ -> return (Left SendOptionsFailedException)
        Right (OptionResponse (Supported resources :: Supported [r])) ->
            Right <$> updateResourcers recievingPeerNodeId resources

updateResourcers :: (MonadReader env m, HasRpc env r msg, MonadIO m)
    => NodeId
    -> [r]
    -> m ()
updateResourcers nId resourceList = do
    rpcRecord <- asks rpcEnv
    let resourcers = rpcResourcers rpcRecord
    mapM_  (updateResourcers' nId resourcers) resourceList

updateResourcers' ::
    (Ord r, Hashable r, MonadIO m)
    => NodeId
    -> Resourcers r
    -> r
    -> m ()
updateResourcers' nid (Resourcers resourcers) resource =
    case resourcers ^. at resource of
        Just x -> liftIO $ atomically $ modifyTVar x (Set.insert nid)
        -- | Shouldn't reach the nothing branch as all resources should be registered
        Nothing -> return ()
