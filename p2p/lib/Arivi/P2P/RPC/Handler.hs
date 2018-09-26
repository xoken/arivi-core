{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}

module Arivi.P2P.RPC.Handler
    ( optionsHandler
    , rpcHandler
    ) where

import           Arivi.P2P.Types
import           Arivi.P2P.RPC.Types
import           Arivi.P2P.RPC.Env

import           Control.Monad.Reader
import qualified Data.HashMap.Strict                   as HM

rpcHandler ::
    ( MonadReader env m
    , HasRpc env r msg, MonadIO m)
    => Request 'Rpc (RpcPayload r msg)
    -> m (Response 'Rpc (RpcPayload r msg))
rpcHandler (RpcRequest (RpcPayload resource msg)) = RpcResponse <$> do
    rpcRecord <- asks rpcEnv
    let ResourceHandler h = rpcHandlers rpcRecord
    RpcPayload resource <$> h msg
rpcHandler (RpcRequest (RpcError _)) = error "Shouldn't get an error message as request"


-- | takes an options message and returns a supported message
optionsHandler ::
       forall env m r msg. (MonadReader env m, HasRpc env r msg)
    => m (Response 'Option (Supported [r]))
optionsHandler = OptionResponse <$> do
    rpcRecord <- asks rpcEnv
    let Resourcers r = rpcResourcers rpcRecord
    return (Supported (HM.keys r))
