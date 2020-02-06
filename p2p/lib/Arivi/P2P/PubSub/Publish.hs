<<<<<<< HEAD
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
=======
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
>>>>>>> breaking out arivi-core from arivi

module Arivi.P2P.PubSub.Publish
    ( publish
    ) where

import Arivi.P2P.MessageHandler.NodeEndpoint
import Arivi.P2P.P2PEnv
import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Types
import Arivi.P2P.Types
import Arivi.Utils.Set
<<<<<<< HEAD

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Data.Set (union)

publish :: (HasP2PEnv env m r t rmsg msg) => PubSubPayload t msg -> m ()
publish req@(PubSubPayload (t,_)) = do
    subs <- asks subscribers
    notf <- asks notifiers
    nodes <- liftA2 union (liftIO $ subscribersForTopic t subs) (liftIO $ notifiersForTopic t notf)
    responses <-
        mapSetConcurrently
            (\node -> runExceptT $ issueRequest node (publishRequest req))
            nodes
    void $ traverseSet
        (\case
                Left _ -> return ()
                Right (PubSubResponse Ok) -> return ()
                Right (PubSubResponse Error) -> return ())
        responses
=======
import Codec.Serialise
import Control.Monad.Except
import Control.Monad.Logger (logDebug)
import Control.Monad.Reader

publish ::
       (Serialise msg, Show t)
    => (HasP2PEnv env m r t rmsg msg) =>
           PubSubPayload t msg -> m ()
publish req@(PubSubPayload (t, _)) = do
    $(logDebug) "publish called"
    subs <- asks subscribers
    peers <- liftIO $ subscribersForTopic t subs
    liftIO $ print (t)
    liftIO $ print (peers)
    responses <- mapSetConcurrently (\node -> runExceptT $ issueRequest node (publishRequest req)) peers
    void $
        traverseSet
            (\case
                 Left _ -> return ()
                 Right (PubSubResponse Ok) -> do
                     $(logDebug) "Publish Successful"
                     return ()
                 Right (PubSubResponse Error) -> do
                     $(logDebug) "Publish failed"
                     return ())
            responses
>>>>>>> breaking out arivi-core from arivi

publishRequest :: msg -> Request ('PubSub 'Publish) msg
publishRequest = PubSubRequest
