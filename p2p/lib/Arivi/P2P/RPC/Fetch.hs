{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Arivi.P2P.RPC.Fetch
    ( fetchResource
  --, fetchResourceForMessage
    ) where

import Codec.Serialise

import Arivi.P2P.Exception
import Arivi.P2P.MessageHandler.HandlerTypes (NodeId)
import Arivi.P2P.MessageHandler.NodeEndpoint
import Arivi.P2P.P2PEnv
import Arivi.P2P.PubSub.Class ()
import Arivi.P2P.PubSub.Types ()
import Arivi.P2P.RPC.Env
import Arivi.P2P.RPC.Types
import Arivi.P2P.Types
import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Control.Monad.STM
import qualified Data.Set as Set

-- | Try fetching resource from a list of nodes. Return first successful response or return an error if didn't get a successfull response from any peer
sendResourceRequest ::
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t msg pmsg) =>
           [NodeId] -> RpcPayload r msg -> m (Either AriviP2PException (RpcPayload r msg))
sendResourceRequest [] _ = return (Left RPCResourceNotFoundException)
sendResourceRequest (currPeer:rest) msg = do
    res <- runExceptT $ issueRequest currPeer (RpcRequest msg)
    case res of
        Left _ -> sendResourceRequest rest msg
        Right (RpcResponse payload) ->
            case payload of
                resp@(RpcPayload _ _) -> return (Right resp)
                RpcError _ -> sendResourceRequest rest msg

-- | Called by the service to fetch a resource. P2P decides best peer to ask for the resource.
fetchResource ::
       (Serialise pmsg, Show t)
    => (HasP2PEnv env m r t msg pmsg) =>
           RpcPayload r msg -> m (Either AriviP2PException (RpcPayload r msg))
fetchResource payload@(RpcPayload resource _) = do
    rpcRecord <- asks rpcEnv
    let Resourcers resourcers = rpcResourcers rpcRecord
    case resourcers ^. at resource of
        Just x -> do
            nodeSet <- liftIO $ atomically $ readTVar x
            sendResourceRequest (Set.toList nodeSet) payload
        Nothing -> return (Left RPCResourceNotFoundException)
fetchResource (RpcError _) = error "Change RpcPayload constructor"
