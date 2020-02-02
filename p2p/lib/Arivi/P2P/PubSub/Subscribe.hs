{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Arivi.P2P.PubSub.Subscribe
    ( subscribe
    ) where

import Arivi.P2P.MessageHandler.HandlerTypes
import Arivi.P2P.MessageHandler.NodeEndpoint
import Arivi.P2P.P2PEnv
import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Types
import Arivi.P2P.Types
import Codec.Serialise
import Control.Monad.Except
import Control.Monad.Logger (logDebug)
import Control.Monad.Reader
import qualified Data.Text as T

subscribe ::
       (Serialise msg, Show t)
    => (HasP2PEnv env m r t rmsg msg) =>
           PubSubPayload t Timer -> NodeId -> m ()
subscribe req@(PubSubPayload (t, _)) nid = do
    $(logDebug) "subscribe called"
    resp <- runExceptT $ issueRequest nid (subscribeRequest req)
    notf <- asks notifiers
    case resp of
        Left _ -> return ()
        Right (PubSubResponse Ok) -> do
            $(logDebug) "Subscribe successful creating new Notifier"
            liftIO $ newNotifier nid notf t
            $(logDebug) $ T.pack ("created new Notifier with NodeId = " ++ show nid)
        Right (PubSubResponse Error) -> do
            $(logDebug) "Subscribe failed"
            return ()

subscribeRequest :: msg -> Request ('PubSub 'Subscribe) msg
subscribeRequest = PubSubRequest
