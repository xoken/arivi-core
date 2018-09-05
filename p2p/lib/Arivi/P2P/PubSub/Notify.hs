{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE GADTs                 #-}

module Arivi.P2P.PubSub.Notify
    ( notify
    ) where

import Arivi.P2P.MessageHandler.NodeEndpoint
import Arivi.P2P.P2PEnv
import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Types
import Arivi.P2P.Types
import Arivi.Utils.Set

import Codec.Serialise
import Control.Monad.Except
import Data.Hashable

notify ::
       (HasP2PEnv env m r msg, HasSubscribers m t, Serialise t, Hashable msg)
    => Notify t msg
    -> m ()
notify req@(Notify t msg) = do
    subs <- subscribers' msg t
    responses <-
        mapSetConcurrently
            (\node -> runExceptT $ issueRequest node (PubSubRequest req))
            subs
    void $
        traverseSet
            (\case
                 Left _ -> return ()
                 Right (PubSubResponse Ok) -> return ()
                 Right (PubSubResponse Error) -> return ())
            responses
