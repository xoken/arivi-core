{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Service.HelloWorld
    ( module Service.HelloWorld
    ) where

import Arivi.P2P.P2PEnv
import Arivi.P2P.RPC.Functions
import Arivi.P2P.RPC.Types
import Arivi.P2P.Types

import GHC.Generics
import Codec.Serialise
import Control.Monad.IO.Class
import Data.ByteString.Lazy as Lazy
import Data.Hashable

type ServiceMsg = Lazy.ByteString

data ServiceResource = HelloWorld deriving (Eq, Ord, Show, Generic)

instance Serialise ServiceResource
instance Hashable ServiceResource

handler :: ResourceHandler ServiceResource String
handler = ResourceHandler (\(RpcPayload resource serviceMsg) -> RpcPayload resource (serviceMsg ++ "Praise Jesus"))

-- registerHelloWorld :: (HasP2PEnv env m ServiceResource String String String) => m ()
-- registerHelloWorld =
--     registerResource HelloWorld handler Archived >>
--     liftIO (threadDelay 5000000) >>
--     updatePeerInResourceMap HelloWorld

getHelloWorld :: (HasP2PEnv env m ServiceResource ByteString String ByteString) => m ()
getHelloWorld = do
    resource <- fetchResource (RpcPayload HelloWorld "HelloWorld")
    liftIO $ print "here"
    liftIO $ print resource
