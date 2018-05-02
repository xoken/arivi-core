-- |
-- Module      :  Arivi.Network.HashMapManager
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- HashMapManager is responsible for managing connection HashMap concurrently
-- for different threads
-- HashMapManager will run on separate thread and any other threads can request
-- different commands using the OperationRequest TChan

module Arivi.Network.HashMapManager
(
    Command(..)
  , ConnectionHashMap
  , OperationRequest(..)
  , addConnection
  , removeConnection
  , runHashMapManager

) where


import           Arivi.Network.Connection (Connection (..), ConnectionId)
import           Control.Concurrent.STM   (TChan, atomically, newTChan,
                                           readTChan)
import           Control.Monad            (forever)
import           Data.HashTable.IO        (CuckooHashTable, delete, insert, new)

-- | Using these different Commands different threads can access connection
--   `TChan`
data Command  =
                 Delete -- ^ Command for Delete
               | Insert -- ^ Command for Insert
               | Update -- ^ Command for Update
               | Query  -- ^ Command for Read
               deriving (Show,Eq)


-- | Used to pass OperationRequest from different threads to the HashMapManager
data OperationRequest = DeleteRequest{
                          mCommand      :: Command
                        , mConnectionId :: ConnectionId
                      }
                    | InsertRequest {
                          mCommand    :: Command
                        , mConnection :: Connection
                      }
                     deriving (Eq)


-- | ConnectionHashMap is `CuckooHashTable`
type ConnectionHashMap k v = Data.HashTable.IO.CuckooHashTable k v


-- | Adds given connection in the ConnectionHashMap
addConnection :: Connection
              -> ConnectionHashMap ConnectionId Connection
              -> IO()
addConnection connection connectionHashMap =
                          Data.HashTable.IO.insert connectionHashMap
                                                  (connectionId connection)
                                                  connection


-- | Removes connection specified by given `ConnectionId` from the
-- `ConnectionHashMap`
removeConnection :: ConnectionId
                 -> ConnectionHashMap ConnectionId Connection
                 -> IO ()
removeConnection connectionId connectionHashMap =
                         Data.HashTable.IO.delete connectionHashMap
                                                  connectionId

-- | Runs indefinitely and reads from OperationRequest TChan and does operation
--   accordingly
runHashMapManager :: IO (ConnectionHashMap ConnectionId Connection)
runHashMapManager = do

        connectionHashMap <- Data.HashTable.IO.new

        operationRequestTChan <- atomically $ newTChan

        forever $
            do
              operationRequest <- atomically $ readTChan operationRequestTChan

              let operationCommand = mCommand operationRequest

              case operationCommand of
                 Delete -> do
                            let connectionId = mConnectionId operationRequest
                            removeConnection connectionId connectionHashMap
                 Insert -> do
                           let connection = mConnection operationRequest
                           addConnection connection connectionHashMap
