-- |
-- Module      :  Arivi.P2P.ServiceRegistry
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- ServiceRegistry is part of Arivi P2P layer, It keeps track of ServiceContext
-- and ConnectionCommand

module Arivi.P2P.ServiceRegistry
(
      ConnectionCommand(..)
    , ContextId
    , ServiceContext(..)
    , genUniqueContextId
    , registerService
) where

import           Arivi.Network.Connection     (Connection, ConnectionId)
import           Arivi.Network.Types          (TransportType (..))
import           Arivi.P2P.PubSub             (Topic)
import           Arivi.P2P.Types              (ServiceCode)
import           Control.Concurrent.MVar      (MVar)
import           Control.Concurrent.STM.TChan (TChan)
import           Data.HashTable.IO
import           Data.Maybe                   (Maybe)
import           Data.UUID                    (UUID)
import           Data.UUID.V1                 (nextUUID)

-- | ContextId is type synonyms for UUID
type ContextId = UUID

-- | ConnectionCommand keeps track of Connection Shared Variable
data ConnectionCommand = ConnectionCommand {
           connectionId   :: ConnectionId   -- ^ Unique Connection Identifier
          ,connectionMVar :: MVar Connection-- ^ Shared Variable  for connection
          }

-- | Keeps information about Services
data ServiceContext = ServiceContext {
       contextId              :: ContextId      -- ^ Context Identifier
                                                --   for services
      ,serviceCode            :: ServiceCode    -- ^ Service Code like
                                                --   KDM,BSY,etc
      ,transportType          :: TransportType  -- ^ Transport Type
                                                --   like `UDP` or
                                                --   `TCP`
      ,outputTChan            :: TChan String   -- ^ This `TChan` is
                                                --   used for receiving
                                                --   message after
                                                --   fragmentation
      , serviceTopicDirectory :: CuckooHashTable ServiceCode [Topic]
  -- ,connectionCommandTChan :: TChan ConnectionCommand -- ^ This `TChan` gives
  --                                                    --   ConnectionCommand
  --                                                    --   for
    }

-- | Generates unique context id
genUniqueContextId :: IO (Maybe Data.UUID.UUID)
genUniqueContextId = Data.UUID.V1.nextUUID


-- TODO serviceRegistry HashMap contextId serviceContex

-- data ServiceRegistry = ServiceRegistry {
--                 hashTable     :: CuckooHashTable
--               , contextId     :: ContextId
--               , serviceContex :: ServiceContext
--             }

-- | Registers service in serviceRegistry HashMap
-- registerService
--   ::
--         Data.UUID.UUID ServiceContext
--      -> Data.UUID.UUID
--      -> ServiceCode
--      -> TransportType
--      -> Control.Concurrent.STM.TChan.TChan String
--      -> CuckooHashTable ServiceCode [Topic]
--      -> IO (CuckooHashTable ServiceCode [Topic])
registerService serviceRegistryHashMap contextId serviceCode transportType
                            outputTChan serviceTopicDirectory = do

          Data.HashTable.IO.insert serviceRegistryHashMap
                                   contextId
                                   (ServiceContext contextId
                                                   serviceCode
                                                   transportType
                                                   outputTChan
                                                   serviceTopicDirectory)
          return serviceRegistryHashMap


-- registerService ariviHandle serviceCode transportType
