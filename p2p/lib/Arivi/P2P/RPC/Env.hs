{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ConstraintKinds       #-}

module Arivi.P2P.RPC.Env
    ( module Arivi.P2P.RPC.Env
    ) where

import Arivi.P2P.RPC.Types

import Codec.Serialise
import Control.Concurrent.STM.TVar (newTVarIO)
import Data.HashMap.Strict as HM
import Data.Hashable
import Data.Set as Set

data RpcEnv r msg = RpcEnv {
      rpcResourcers :: Resourcers r
}

class HasRpcEnv env r msg | env -> r msg where
    rpcEnv :: env -> RpcEnv r msg

type HasRpc env r msg =
    (   HasRpcEnv env r msg
      , Eq r, Ord r, Hashable r, Serialise r
      , Eq msg, Hashable msg, Serialise msg
    )

mkRpc :: (Ord r, Hashable r) => [r] -> IO (RpcEnv r msg)
mkRpc resourceList = do
    resTVars <- mapM (\_ -> newTVarIO Set.empty) resourceList
    RpcEnv <$> pure (Resourcers (HM.fromList (zip resourceList resTVars)))
