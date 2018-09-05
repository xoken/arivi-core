{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

module Arivi.P2P.PubSub.Publish
    ( module Arivi.P2P.PubSub.Publish
    ) where

import Arivi.P2P.MessageHandler.NodeEndpoint
import Arivi.P2P.P2PEnv
import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Types
import Arivi.P2P.Types
import Arivi.Utils.Set

import Codec.Serialise
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Except
import Data.Hashable

publish ::
       ( HasP2PEnv env m r msg
       , HasSubscribers m t
       , Hashable t
       , Eq t
       , Serialise t
       )
    => Publish t msg
    -> m ()
publish req@(Publish t _) = do
    Subscribers subs <- subscribers
    case subs ^. at t of
        Nothing -> return ()
        Just ns -> do
            nodes <- liftIO $ readTVarIO ns
            responses <-
                mapSetConcurrently
                   (\node -> runExceptT $ issueRequest node (PubSubRequest req))
                    nodes
            void $ traverseSet
               (\case
                     Left _ -> return ()
                     Right (PubSubResponse Ok) -> return ()
                     Right (PubSubResponse Error) -> return ())
               responses
