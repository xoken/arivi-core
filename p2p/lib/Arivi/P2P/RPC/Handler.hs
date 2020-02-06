{-# LANGUAGE ScopedTypeVariables #-}
<<<<<<< HEAD
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
=======
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
>>>>>>> breaking out arivi-core from arivi

module Arivi.P2P.RPC.Handler
    ( optionsHandler
    , rpcHandler
    ) where

<<<<<<< HEAD
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
=======
import Arivi.P2P.P2PEnv
import Arivi.P2P.RPC.Env
import Arivi.P2P.RPC.Types
import Arivi.P2P.Types
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HM

rpcHandler ::
       forall env m r t rmsg pmsg. (HasP2PEnv env m r t rmsg pmsg)
    => Request 'Rpc (RpcPayload r rmsg)
    -> m (Response 'Rpc (RpcPayload r rmsg))
rpcHandler (RpcRequest (RpcPayload resource msg)) = do
    h <- asks rpcGlobalHandler
    resp <- h msg
    case resp of
        Just response -> return (RpcResponse (RpcPayload resource response))
        Nothing -> return (RpcResponse (RpcError ResourceNotFound))
rpcHandler (RpcRequest (RpcError _)) = error "Shouldn't get an error message as request"

-- | takes an options message and returns a supported message
optionsHandler ::
       forall env m r msg. (MonadReader env m, HasRpc env r msg)
    => m (Response 'Option (Supported [r]))
optionsHandler =
    OptionResponse <$> do
        rpcRecord <- asks rpcEnv
        let Resourcers r = rpcResourcers rpcRecord
        return (Supported (HM.keys r))
>>>>>>> breaking out arivi-core from arivi
