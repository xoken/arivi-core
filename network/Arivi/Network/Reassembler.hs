{-# LANGUAGE OverloadedStrings #-}
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

import           Arivi.Crypto.Cipher.ChaChaPoly1305 (getCipherTextAuthPair)
import           Arivi.Crypto.Utils.PublicKey.Utils (decryptMsg)
import           Arivi.Network.Connection           (Connection (..))
import           Arivi.Network.Types                (ConnectionId, Header (..),
                                                     MessageId, Parcel (..),
                                                     Payload (..), serialise)
import           Control.Concurrent.STM             (TChan, atomically,
                                                     readTChan, writeTChan)
import qualified Data.Binary                        as Binary (encode)
import qualified Data.ByteString.Char8              as Char8 (ByteString)
import qualified Data.ByteString.Lazy               as Lazy (ByteString, concat,
                                                             fromStrict,
                                                             toStrict)
import qualified Data.HashMap.Strict                as StrictHashMap (HashMap,
                                                                      delete,
                                                                      insert,
                                                                      lookup)
import           Data.Int                           (Int64)
import           Data.Maybe                         (fromMaybe)
import           Debug.Trace
type AEADNonce = Int64

-- | Extracts `Payload` messages from `DataParcel` and puts in the
--   list of fragmentsHashMap
reassembleFrames::
                  Connection
               -> TChan Parcel
               -> TChan Lazy.ByteString
               -> StrictHashMap.HashMap MessageId Lazy.ByteString
               -> AEADNonce
               -> IO ()

reassembleFrames connection reassemblyTChan p2pMessageTChan
                                            fragmentsHashMap aeadNonce = do

    parcel <- atomically $ readTChan reassemblyTChan

    let messageIdNo = messageId (header parcel)
    let (cipherText,authenticationTag) = getCipherTextAuthPair
                                        (Lazy.toStrict
                                          (getPayload
                                            (encryptedPayload parcel)))

    let parcelHeader = Lazy.toStrict $ serialise (header parcel)
    let ssk = sharedSecret connection
    let payloadMessage =  Lazy.fromStrict $ decryptMsg aeadNonce
                                                    ssk parcelHeader
                                                    authenticationTag
                                                    cipherText

    let messages = fromMaybe  "" (StrictHashMap.lookup messageIdNo
                                                           fragmentsHashMap)

    let appendedMessage = Lazy.concat [messages, payloadMessage]
    traceShow (appendedMessage) (return ())

    let currentFragmentNo = fragmentNumber (header parcel)

    if currentFragmentNo ==  totalFragements (header parcel)
      then
        do
           atomically $ writeTChan p2pMessageTChan appendedMessage

           let updatedFragmentsHashMap = StrictHashMap.delete messageIdNo
                                                              fragmentsHashMap

           reassembleFrames connection reassemblyTChan p2pMessageTChan
                                            updatedFragmentsHashMap
                                            (aeadNonce + 1)
    else
       do
        let updatedFragmentsHashMap = StrictHashMap.insert messageIdNo
                                                           appendedMessage
                                                           fragmentsHashMap
        reassembleFrames connection  reassemblyTChan p2pMessageTChan
                                                     updatedFragmentsHashMap
                                                     (aeadNonce + 1)
