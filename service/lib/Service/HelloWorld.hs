{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Service.HelloWorld
    ( module Service.HelloWorld
    ) where

import Arivi.P2P.P2PEnv
import Arivi.P2P.RPC.Fetch
import Arivi.P2P.RPC.Types
import Arivi.P2P.Types
import Arivi.P2P.PubSub.Types
import Arivi.P2P.PubSub.Publish


import GHC.Generics
import Codec.Serialise
import Control.Monad.IO.Class
import Data.ByteString.Lazy as Lazy
import Data.Hashable

type ServiceMsg = Lazy.ByteString

data ServiceResource = HelloWorld deriving (Eq, Ord, Show, Generic)

data ServiceTopic = HelloWorldHeader deriving (Eq, Ord, Show, Generic)

instance Serialise ServiceResource
instance Hashable ServiceResource

instance Serialise ServiceTopic
instance Hashable ServiceTopic

handlerNew :: ResourceHandler String
handlerNew = ResourceHandler (++ "Praise Jesus")

ioHello :: (MonadIO m) => String -> m Status
ioHello msg =
    if msg == "HelloworldHeader"
        then liftIO (Prelude.putStrLn "Ok") >> return Ok
        else liftIO (Prelude.putStrLn "Error") >> return Error

handlerTopic :: TopicHandler String
handlerTopic = TopicHandler ioHello

-- registerHelloWorld :: (HasP2PEnv env m ServiceResource String String String) => m ()
-- registerHelloWorld =
--     registerResource HelloWorld handler Archived >>
--     liftIO (threadDelay 5000000) >>
--     updatePeerInResourceMap HelloWorld

getHelloWorld :: (HasP2PEnv env m ServiceResource ServiceTopic String String) => m ()
getHelloWorld = do
    resource <- fetchResource (RpcPayload HelloWorld "HelloWorld")
    liftIO $ print "here"
    liftIO $ print resource

stuffPublisher :: (HasP2PEnv env m ServiceResource ServiceTopic String String) => m ()
stuffPublisher = publish (PubSubPayload (HelloWorldHeader, "HelloworldHeader"))
