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
import Control.Concurrent.Async.Lifted
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

globalHandler :: (HasP2PEnv env m ServiceResource ServiceTopic String String) => String -> m Status
globalHandler msg =
    if msg == "HelloworldHeader"
        then do
            liftIO (Prelude.putStrLn "Ok")
            _ <- async (getHelloWorld msg)
            return Ok
        else liftIO (Prelude.putStrLn "Error") >> return Error



-- registerHelloWorld :: (HasP2PEnv env m ServiceResource String String String) => m ()
-- registerHelloWorld =
--     registerResource HelloWorld handler Archived >>
--     liftIO (threadDelay 5000000) >>
--     updatePeerInResourceMap HelloWorld

getHelloWorld :: (HasP2PEnv env m ServiceResource ServiceTopic String String) => String -> m ()
getHelloWorld msg = do
    resource <- fetchResourceForMessage msg (RpcPayload HelloWorld "HelloWorld")
    liftIO $ print "got resource from notify/publish"
    liftIO $ print resource

stuffPublisher :: (HasP2PEnv env m ServiceResource ServiceTopic String String) => m ()
stuffPublisher = publish (PubSubPayload (HelloWorldHeader, "HelloworldHeader"))
