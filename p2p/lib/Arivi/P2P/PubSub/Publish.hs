{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Arivi.P2P.PubSub.Publish
    ( publish
    ) where

import Arivi.P2P.MessageHandler.NodeEndpoint
import Arivi.P2P.P2PEnv
import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Types
import Arivi.P2P.Types
import Arivi.Utils.Set
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

publishRequest :: msg -> Request ('PubSub 'Publish) msg
publishRequest = PubSubRequest
