module Arivi.P2P.PubSub.Types
    (
    ) where

import           Arivi.P2P.PubSub.Types

-- | Called by each service to register its Topics. Creates entries in TopicMap
-- the TopicHandler passed takes a topicmessage and returns a topic
-- if topicmessage is returned it is sent to all watchers
-- it can also return exception if the block is valid or invalidaccordingly peers are judged
registerTopic :: (HasP2PEnv m) => [(Topic, TopicHandler, Int)] -> m ()
-- | called by a service when some content created by it needs to be broadcasted to the network ie notifiers as well as subscribers
publishTopic :: (HasP2PEnv m) => Topic -> TopicMessage -> m ()
-- | spawns a thread with the parameter TVar TopicMap and checks expired subscribers and deletes entry
maintainWatchers :: (HasP2PEnv m) => m ()
-- | spawns a thread with the parameter TVar TopicMap and checks expired notifiers and possible resubscribe to the peer based on rep. also checks if min number if satisfied and add more peers accordingly by calling send options which takes peer from kad
maintainNotifiers :: (HasP2PEnv m) => m ()-- -- | called by a service to read the TopicMessage TQueue
-- readTopic :: (HasP2PEnv m) => TopicId -> m TopicMessage
-- -- | called by a service after it has verified a previously read TopicMessage to broadcast it further to the subscribers. only sent to Subscribers minus nodes in MessageHashMap
-- notifyTopic :: (HasP2PEnv m) => TopicId -> TopicMessage -> m ()\
-- -- | called in case of a unvalid block and used for peer rep based on MessageHashMap
-- flagMessage :: (HasP2PEnv m) => TopicId -> TopicMessage -> m ()
