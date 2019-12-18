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

module Arivi.P2P.PubSub.Notify
    ( notify
    ) where

<<<<<<< HEAD
=======
import Codec.Serialise

>>>>>>> breaking out arivi-core from arivi
import Arivi.P2P.MessageHandler.NodeEndpoint
import Arivi.P2P.P2PEnv
import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Types
import Arivi.P2P.Types
import Arivi.Utils.Set

<<<<<<< HEAD
import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Monad.Except
import Control.Monad.Reader

notify ::
    ( HasP2PEnv env m r t rmsg msg)
    => PubSubPayload t msg
    -> m ()
notify req@(PubSubPayload (t, msg)) = do
    subs <- asks subscribers
    inboxed <-  (liftIO . readTVarIO) =<< asks inbox
    peers <- liftIO $ notifiersForMessage inboxed subs msg t
    responses <-
        mapSetConcurrently
            (\node -> runExceptT $ issueRequest node (notifyRequest req))
            peers
=======
import Control.Concurrent.STM.TVar ()
import Control.Monad.Except
import Control.Monad.Logger (logDebug)
import Control.Monad.Reader

notify ::
       (Serialise msg, Show t)
    => (HasP2PEnv env m r t rmsg msg) =>
           PubSubPayload t msg -> m ()
notify req@(PubSubPayload (t, _msg)) = do
    $(logDebug) "notify called"
    subs <- asks subscribers
    peers <- liftIO $ subscribersForTopic t subs
    responses <- mapSetConcurrently (\node -> runExceptT $ issueRequest node (notifyRequest req)) peers
>>>>>>> breaking out arivi-core from arivi
    void $
        traverseSet
            (\case
                 Left _ -> return ()
<<<<<<< HEAD
                 Right (PubSubResponse Ok) -> return ()
                 Right (PubSubResponse Error) -> return ())
=======
                 Right (PubSubResponse Ok) -> do
                     $(logDebug) "Notify Successful"
                     return ()
                 Right (PubSubResponse Error) -> do
                     $(logDebug) "Notify failed"
                     return ())
>>>>>>> breaking out arivi-core from arivi
            responses

notifyRequest :: msg -> Request ('PubSub 'Notify) msg
notifyRequest = PubSubRequest
