{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}


module Service.HelloWorld
    ( module Service.HelloWorld
    ) where

import Arivi.P2P.P2PEnv
import Arivi.P2P.RPC.Fetch
import Arivi.P2P.Types
import Arivi.P2P.PubSub.Types
import Arivi.P2P.PubSub.Publish
import Arivi.P2P.MessageHandler.HandlerTypes
import Arivi.P2P.PubSub.Env
import Arivi.P2P.PubSub.Class
import Arivi.P2P.RPC.Env

import GHC.Generics
import Codec.Serialise
import Control.Concurrent.Async.Lifted
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Lazy as Lazy
import Data.Hashable

type ServiceMsg = Lazy.ByteString

data ServiceResource = HelloWorld deriving (Eq, Ord, Show, Generic)

data ServiceTopic = HelloWorldHeader deriving (Eq, Ord, Show, Generic)

instance Serialise ServiceResource
instance Hashable ServiceResource

instance Serialise ServiceTopic
instance Hashable ServiceTopic


globalHandlerPubSub :: (HasService env m) => String -> m Status
globalHandlerPubSub msg = do
    val <- asks getSomeVal
    liftIO $ print val
    if msg == "HelloworldHeader"
        then do
            liftIO (Prelude.putStrLn "Ok")
            _ <- async (getHelloWorld msg)
            return Ok
        else liftIO (Prelude.putStrLn "Error") >> return Error

data SomeEnv = SomeEnv {
    someVal :: String
} deriving(Eq, Ord, Show)

class HasSomeEnv env where
    getSomeVal :: env -> SomeEnv

instance HasSomeEnv (ServiceEnv m r t rmsg pmsg) where
    getSomeVal = someEnv

data ServiceEnv m r t rmsg pmsg = ServiceEnv {
      someEnv :: SomeEnv
    , p2pEnv :: P2PEnv m r t rmsg pmsg 
}

type HasService env m = 
    ( HasP2PEnv env m ServiceResource ServiceTopic String String
    , HasSomeEnv env
    , MonadReader env m
    )

globalHandlerRpc :: (MonadIO m) => String -> m (Maybe String)
globalHandlerRpc msg =
    if msg == "HelloWorld" then return (Just (msg ++ " Praise Satoshi"))
    else return Nothing

getHelloWorld :: (HasP2PEnv env m ServiceResource ServiceTopic String String) => String -> m ()
getHelloWorld msg = do
    resource <- fetchResourceForMessage msg (RpcPayload HelloWorld "HelloWorld")
    liftIO $ print "got resource from notify/publish"
    liftIO $ print resource

stuffPublisher :: (HasP2PEnv env m ServiceResource ServiceTopic String String) => m ()
stuffPublisher = publish (PubSubPayload (HelloWorldHeader, "HelloworldHeader"))


instance HasNetworkConfig (ServiceEnv m r t rmsg pmsg) NetworkConfig where
    networkConfig f se =                             
        fmap
            (\nc ->
                 se
                 { p2pEnv =
                       (p2pEnv se)
                       { nodeEndpointEnv =
                             (nodeEndpointEnv (p2pEnv se))
                             {Arivi.P2P.P2PEnv._networkConfig = nc}
                       }       
                 })
            (f ((Arivi.P2P.P2PEnv._networkConfig . nodeEndpointEnv . p2pEnv) se)) 

instance HasTopics (ServiceEnv m r t rmsg pmsg) t where
    topics = pubSubTopics . psEnv . p2pEnv
instance HasSubscribers (ServiceEnv m r t rmsg pmsg) t where
    subscribers = pubSubSubscribers . psEnv. p2pEnv
instance HasNotifiers (ServiceEnv m r t rmsg pmsg) t where
    notifiers = pubSubNotifiers . psEnv . p2pEnv
instance HasInbox (ServiceEnv m r t rmsg pmsg) pmsg where
    inbox = pubSubInbox . psEnv . p2pEnv
instance HasCache (ServiceEnv m r t rmsg pmsg) pmsg where
    cache = pubSubCache . psEnv . p2pEnv
instance HasPubSubEnv (ServiceEnv m r t rmsg pmsg) t pmsg where
    pubSubEnv = psEnv . p2pEnv
instance HasRpcEnv (ServiceEnv m r t rmsg pmsg) r rmsg where
    rpcEnv = rEnv . p2pEnv
instance HasPSGlobalHandler (ServiceEnv m r t rmsg pmsg) m r t rmsg pmsg where
    psGlobalHandler = psHandler . p2pEnv