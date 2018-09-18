{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Arivi.P2P.RPC.Fetch
    ( fetchResource
    , fetchResourceForMessage
    ) where

import           Arivi.P2P.Types
import           Arivi.P2P.Exception
import           Arivi.P2P.MessageHandler.NodeEndpoint
import           Arivi.P2P.MessageHandler.HandlerTypes (NodeId)
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.PubSub.Class
import           Arivi.P2P.PubSub.Types
import           Arivi.P2P.RPC.Types
import           Arivi.P2P.RPC.Env
import           Control.Concurrent.STM.TVar
import           Control.Lens
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.STM
import qualified Data.Set                              as Set

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

-- | Called by the service to fetch a resource. P2P decides best peer to ask for the resource. 
fetchResource ::
    (HasP2PEnv env m r t msg pmsg)
    => RpcPayload r msg
    -> m (Either AriviP2PException (RpcPayload r msg))
fetchResource payload@(RpcPayload resource _) = do
    rpcRecord <- asks rpcEnv
    let Resourcers resourcers = rpcResourcers rpcRecord
    case resourcers ^. at resource of
        Just x -> do
            nodeSet <- liftIO $ atomically $ readTVar x
            sendResourceRequest (Set.toList nodeSet) payload
        Nothing -> return (Left RPCResourceNotFoundException)
fetchResource (RpcError _) = error "Change RpcPayload constructor"

-- | Called by the service to fetch a resource as a result of a notification
fetchResourceForMessage ::
    (HasP2PEnv env m r t msg pmsg)
    => pmsg
    -> RpcPayload r msg
    -> m (Either AriviP2PException (RpcPayload r msg))
fetchResourceForMessage storedMsg payload@(RpcPayload _ _) = do
    Inbox inboxed <-  join $ liftIO . readTVarIO <$> asks inbox
    case inboxed ^. at storedMsg of
        Just nodeListTVar -> do
            nodeList <- (liftIO . readTVarIO) nodeListTVar
            sendResourceRequest (Set.toList nodeList) payload -- does not make sense to pattern match here on the result and call fetchResource again. If the nodes who sent the notification don't have the resource, why request the same resource from guys who didn't send the notification
        Nothing -> fetchResource payload
fetchResourceForMessage _ (RpcError _) = error "Change RpcPayload constructor"