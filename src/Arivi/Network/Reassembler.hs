-- |
-- Module      :  Arivi.Network.Reassembler
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module is used for combining received fragments
--


module Arivi.Network.Reassembler
(
  reassembleFrames
) where

import           Arivi.Network.Types    (ConnectionId, Header (..), MessageId,
                                         Parcel (..), Payload (..))
import           Control.Concurrent.STM (TChan, atomically, readTChan,
                                         writeTChan)
import qualified Data.ByteString.Lazy   as Lazy (ByteString, concat)
import qualified Data.HashMap.Strict    as StrictHashMap (HashMap, delete,
                                                          insert, lookup)
import           Data.Maybe             (Maybe, fromJust)


-- | Extracts `Payload` messages from `DataParcel` and puts in the
--   list of fragmentsHashMap
reassembleFrames:: TChan Parcel
               -> TChan (ConnectionId, Lazy.ByteString)
               -> StrictHashMap.HashMap MessageId Lazy.ByteString
               -> IO ()

reassembleFrames reassemblyTChan p2pMessageTChan fragmentsHashMap = do

    parcel <- atomically $ readTChan reassemblyTChan

    let messageIdNo = messageId (header parcel)

    let payloadMessage = getPayload (encryptedPayload parcel)

    let messages = fromJust (StrictHashMap.lookup messageIdNo fragmentsHashMap)

    let appendedMessage = Lazy.concat [messages, payloadMessage]


    let currentFragmentNo = fragmentNumber (header parcel)

    if currentFragmentNo ==  totalFragements (header parcel)
      then
        do
           let parcelConnectionId = connectionId (header parcel)
           atomically $ writeTChan
                          p2pMessageTChan (parcelConnectionId,appendedMessage)

           let updatedFragmentsHashMap = StrictHashMap.delete messageIdNo
                                                              fragmentsHashMap

           reassembleFrames reassemblyTChan p2pMessageTChan
                                            updatedFragmentsHashMap
    else
       do
        let updatedFragmentsHashMap = StrictHashMap.insert messageIdNo
                                                           appendedMessage
                                                           fragmentsHashMap
        reassembleFrames reassemblyTChan p2pMessageTChan updatedFragmentsHashMap
