{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Arivi.P2P.PubSub.Notify
    ( notify
    ) where

import Arivi.P2P.MessageHandler.NodeEndpoint
import Arivi.P2P.P2PEnv
import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Types
import Arivi.P2P.Types
import Arivi.Utils.Set

import Control.Monad.Except

notify :: (HasP2PEnv env m r t rmsg msg) => PubSubPayload t msg -> m ()
notify req@(PubSubPayload (t, msg)) = do
    subs <- subscribers' msg t
    responses <-
        mapSetConcurrently
            (\node -> runExceptT $ issueRequest node (notifyRequest req))
            subs
    void $
        traverseSet
            (\case
                 Left _ -> return ()
                 Right (PubSubResponse Ok) -> return ()
                 Right (PubSubResponse Error) -> return ())
            responses

notifyRequest :: msg -> Request ('PubSub 'Notify) msg
notifyRequest = PubSubRequest
