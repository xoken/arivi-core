{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}

module Arivi.P2P.RPC.Handler
    ( optionsHandler
    , rpcHandler
    ) where

import           Arivi.P2P.P2PEnv
import           Arivi.P2P.Types
import           Arivi.P2P.RPC.Types
import           Arivi.P2P.RPC.Env

import           Control.Monad.Reader
import qualified Data.HashMap.Strict                   as HM

rpcHandler :: forall env m r t rmsg pmsg .
    ( HasP2PEnv env m r t rmsg pmsg)
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
optionsHandler = OptionResponse <$> do
    rpcRecord <- asks rpcEnv
    let Resourcers r = rpcResourcers rpcRecord
    return (Supported (HM.keys r))
