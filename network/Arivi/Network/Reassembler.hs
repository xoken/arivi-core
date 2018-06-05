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
import           Arivi.Network.Types                (Header (..), MessageId,
                                                     Parcel (..), Payload (..),
                                                     serialise)
import           Control.Concurrent.STM             (STM, writeTChan)
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
               -> Parcel
               -> StrictHashMap.HashMap MessageId Lazy.ByteString
               -> STM (StrictHashMap.HashMap MessageId Lazy.ByteString)

reassembleFrames connection parcel fragmentsHashMap = do

    let messageIdNo = messageId (header parcel)
    let (cipherText,authenticationTag) = getCipherTextAuthPair
                                        (Lazy.toStrict
                                          (getPayload
                                            (encryptedPayload parcel)))

    let parcelHeader = Lazy.toStrict $ serialise (header parcel)
    let fragmentAead = aeadNonce (header parcel)
    let ssk = sharedSecret connection
    let payloadMessage =  Lazy.fromStrict $ decryptMsg fragmentAead
                                                    ssk parcelHeader
                                                    authenticationTag
                                                    cipherText

    let messages = fromMaybe  "" (StrictHashMap.lookup messageIdNo
                                                           fragmentsHashMap)

    let appendedMessage = Lazy.concat [messages, payloadMessage]
    traceShow appendedMessage (return ())

    let currentFragmentNo = fragmentNumber (header parcel)

    if currentFragmentNo ==  totalFragements (header parcel)
      then
        do
           writeTChan (p2pMessageTChan connection) appendedMessage
           let updatedFragmentsHashMap = StrictHashMap.delete messageIdNo
                                                              fragmentsHashMap
           return updatedFragmentsHashMap
    else
       do
        let updatedFragmentsHashMap = StrictHashMap.insert messageIdNo
                                                           appendedMessage
                                                           fragmentsHashMap
        return updatedFragmentsHashMap
