{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PartialTypeSignatures #-}
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

import Codec.Serialise
import Control.Concurrent.STM
import Control.Concurrent.Async.Lifted
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Except
import Data.Hashable

notify ::
       forall env m r msg t.
       (HasP2PEnv env m r msg, HasSubscribers m t, Hashable t, Eq t, Serialise t)
    => Notify t msg
    -> m ()
notify req@(Notify t _) = do
    Subscribers subs <- subscribers
    case subs ^. at t of
        Nothing -> return ()
        Just ns -> do
            nodes <- liftIO $ readTVarIO ns
            responses <-
                mapConcurrently
                    (\node -> runExceptT $ issueRequest node (PubSubRequest req))
                    nodes
            forM_
                responses
                (\case
                     Left _ -> return ()
                     Right (PubSubResponse (_ :: Notify t msg)) -> return ())
