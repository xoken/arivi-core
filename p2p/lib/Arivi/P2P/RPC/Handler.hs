{-# LANGUAGE ScopedTypeVariables #-}

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

-- will need the from NodeId to check the to and from
-- rpcHandler :: (HasNodeEndpoint m, HasRpc m r) => NodeId -> P2PPayload -> P2PPayload
rpcHandler ::
       forall m r msg. (HasNodeEndpoint m, HasRpc m r msg, MonadIO m)
    => RpcPayload r msg
    -> m (RpcPayload r msg)
rpcHandler payload@(RpcPayload resource _) = do
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


-- | takes an options message and returns a supported message
optionsHandler ::
       forall m r msg. (HasNodeEndpoint m, HasRpc m r msg, MonadIO m)
    => m (Supported [r])
optionsHandler = do
    archivedResourceMapTVar <- archived
    archivedResourceMap <- (liftIO . readTVarIO) archivedResourceMapTVar
    return (Supported (HM.keys (getArchivedMap archivedResourceMap)))
