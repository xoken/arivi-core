{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Arivi.P2P.PubSub.Publish
    ( publish
    ) where

import Arivi.P2P.MessageHandler.NodeEndpoint
import Arivi.P2P.P2PEnv
import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Types
import Arivi.P2P.Types
import Arivi.Utils.Set

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

publishRequest :: msg -> Request ('PubSub 'Publish) msg
publishRequest = PubSubRequest
