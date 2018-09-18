{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Arivi.P2P.RPC.Functions
    ( registerResource
    , fetchResource
    , fetchResourceForMessage
    ) where

import           Arivi.P2P.Types
import           Arivi.P2P.Exception
import           Arivi.P2P.MessageHandler.NodeEndpoint
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.PubSub.Class
import           Arivi.P2P.PubSub.Types
import           Arivi.P2P.RPC.Types
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.STM
import qualified Data.HashMap.Strict                   as HM
import qualified Data.Set                              as Set
import           Control.Applicative

-- | Register the resource and it's handler in the ResourceToPeerMap of RPC
registerResource ::
       (HasNodeEndpoint m, HasRpc m r msg, MonadIO m)
    => r
    -> ResourceHandler r msg
    -> ArchivedOrTransient
    -> m ()
registerResource resource resourceHandler resourceType = do
    archivedResourceToPeerMapTvar <- archived
    transientResourceToPeerMapTVar <- transient
    nodeIds <- liftIO $ newTVarIO [] -- create a new empty Tqueue for Peers
    case resourceType of
        Archived -> liftIO $ atomically $ modifyTVar' archivedResourceToPeerMapTvar (funcA nodeIds)
        Transient -> liftIO $ atomically $ modifyTVar' transientResourceToPeerMapTVar (funcT nodeIds)
    where
        funcA l hm = ArchivedResourceToPeerMap $ HM.insert resource (resourceHandler, l) (getArchivedMap hm)
        funcT l hm = TransientResourceToPeerMap $ HM.insert resource (resourceHandler, l) (getTransientMap hm)

-- | Called by the service to fetch a resource. P2P decides best peer to ask for the resource.
fetchResource ::
       ( HasP2PEnv env m r t msg pmsg
       )
    => RpcPayload r msg
    -> m (Either AriviP2PException (RpcPayload r msg))
fetchResource payload@(RpcPayload resource _) = do
    archivedResourceToPeerMapTvar <- archived
    archivedResourceToPeerMap <-
        liftIO $ readTVarIO archivedResourceToPeerMapTvar
    transientResourceToPeerMapTVar <- transient
    transientResourceToPeerMap <-
        liftIO $ readTVarIO transientResourceToPeerMapTVar
    let entryInArchivedResourceMap =
            HM.lookup resource (getArchivedMap archivedResourceToPeerMap)
    let entryInTransientResourceMap =
            HM.lookup resource (getTransientMap transientResourceToPeerMap)
    let entry = entryInArchivedResourceMap <|> entryInTransientResourceMap
    case entry of
        Nothing -> return (Left RPCResourceNotFoundException)
        Just entryMap -> do
            let nodeListTVar = snd entryMap
            nodeList <- liftIO $ atomically $ readTVar nodeListTVar
            liftIO $ print nodeList
            if null nodeList
                then return (Left RPCEmptyNodeListException)
                else sendResourceRequest nodeList payload
fetchResource (RpcError _) = error "Change RpcPayload constructor"


fetchResourceForMessage ::
    (
        HasP2PEnv env m r t msg pmsg
    )
    => pmsg
    -> RpcPayload r msg
    -> m (Either AriviP2PException (RpcPayload r msg))
fetchResourceForMessage storedMsg payload@(RpcPayload _ _) = do
    Inbox inboxed <-  (liftIO . readTVarIO) =<< asks inbox
    case HM.lookup storedMsg inboxed of
        Just nodeListTVar -> do
            nodeList <- (liftIO . readTVarIO) nodeListTVar
            sendResourceRequest (Set.toList nodeList) payload -- does not make sense to pattern match here on the result and call fetchResource again. If the nodes who sent the notification don't have the resource, why request the same resource from guys who didn't send the notification
        Nothing -> fetchResource payload
fetchResourceForMessage _ (RpcError _) = error "Change RpcPayload constructor"

-- | Try fetching resource from a list of nodes. Return first successful response or return an error if didn't get a successfull response from any peer
sendResourceRequest ::
       (HasP2PEnv env m r t msg pmsg)
    => [NodeId]
    -> RpcPayload r msg
    -> m (Either AriviP2PException (RpcPayload r msg))
sendResourceRequest [] _ = return (Left RPCResourceNotFoundException)
sendResourceRequest (currPeer:rest) msg = do
    res <- runExceptT $ issueRequest currPeer (RpcRequest msg)
    case res of
        Left _ -> sendResourceRequest rest msg
        Right (RpcResponse payload) ->
            case payload of
                resp@(RpcPayload _ _ ) -> return (Right resp)
                RpcError _ -> sendResourceRequest rest msg
