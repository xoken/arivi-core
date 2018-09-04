{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}

module Arivi.P2P.RPC.Handler
    ( optionsHandler
    , rpcHandler
    ) where

import           Arivi.P2P.Types
import           Arivi.P2P.Exception
import           Arivi.P2P.P2PEnv
import           Arivi.P2P.RPC.Types
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Lens
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Except
import qualified Data.HashMap.Strict                   as HM
import           Control.Applicative

rpcHandler ::
       forall m r msg. (HasNodeEndpoint m, HasRpc m r msg, MonadIO m)
    => Request 'Rpc (RpcPayload r msg)
    -> m (Response 'Rpc (RpcPayload r msg))
rpcHandler (RpcRequest payload@(RpcPayload resource _)) = RpcResponse <$> do
    archivedResourceMap <- archived
    archivedMap <- (liftIO . readTVarIO) archivedResourceMap
    transientResourceMap <- transient
    transientMap <- (liftIO . readTVarIO) transientResourceMap
    let entry =  getTransientMap transientMap ^. at resource
             <|> getArchivedMap archivedMap ^. at resource
    case entry of
        Nothing -> throw RPCHandlerResourceNotFoundException
        Just entryMap -> do
            let ResourceHandler resourceHandler = fst entryMap
            return (resourceHandler payload)
rpcHandler (RpcRequest (RpcError _)) = error "Change RpcPayload constructor"


-- | takes an options message and returns a supported message
optionsHandler ::
       forall m r msg. (HasNodeEndpoint m, HasRpc m r msg, MonadIO m)
    => m (Response 'Option (Supported [r]))
optionsHandler = OptionResponse <$> do
    archivedResourceMapTVar <- archived
    archivedResourceMap <- (liftIO . readTVarIO) archivedResourceMapTVar
    return (Supported (HM.keys (getArchivedMap archivedResourceMap)))
