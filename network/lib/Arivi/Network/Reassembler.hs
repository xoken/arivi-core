{-# LANGUAGE BangPatterns      #-}
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
    ( reassembleFrames
    ) where

import           Arivi.Crypto.Cipher.ChaChaPoly1305 (getCipherTextAuthPair)
import           Arivi.Crypto.Utils.PublicKey.Utils (decryptMsg)
import           Arivi.Network.Connection           (CompleteConnection,
                                                     p2pMessageTChan,
                                                     sharedSecret)
import           Arivi.Network.Types                (Header (..), MessageId,
                                                     Parcel (..), Payload (..),
                                                     serialise)
import           Arivi.Utils.Exception
import           Control.Concurrent.STM             (STM, writeTChan)
import           Control.Exception                  (throw)
import qualified Data.ByteString.Lazy               as Lazy (ByteString, concat,
                                                             fromStrict,
                                                             toStrict)
import qualified Data.HashMap.Strict                as StrictHashMap (HashMap,
                                                                      delete,
                                                                      insert,
                                                                      lookup)
import           Data.Maybe                         (fromMaybe)
import           Debug.Trace

-- | Extracts `Payload` messages from `DataParcel` and puts in the
--   list of fragmentsHashMap. Returns the hashmap along with a Just p2pMessage in case of a complete message reassembly or Nothing otherwise
reassembleFrames :: CompleteConnection
                 -> Parcel
                 -> StrictHashMap.HashMap MessageId Lazy.ByteString
                 -> (StrictHashMap.HashMap MessageId Lazy.ByteString, Maybe Lazy.ByteString)

reassembleFrames connection parcel fragmentsHashMap = do
    let messageIdNo = messageId (header parcel)
    let (cipherText, authenticationTag) =
            getCipherTextAuthPair
                (Lazy.toStrict (getPayload (encryptedPayload parcel)))
    let parcelHeader = Lazy.toStrict $ serialise (header parcel)
    let fragmentAead = aeadNonce (header parcel)
    let ssk = sharedSecret connection
    let !payloadMessage =  Lazy.fromStrict $ decryptMsg fragmentAead
                                                    ssk parcelHeader
                                                    authenticationTag
                                                    cipherText
    let messages = fromMaybe  "" (StrictHashMap.lookup messageIdNo
                                                           fragmentsHashMap)

    let appendedMessage = Lazy.concat [messages, payloadMessage]
    let currentFragmentNo = fragmentNumber (header parcel)

    if currentFragmentNo ==  totalFragements (header parcel)
      then
        (StrictHashMap.delete messageIdNo fragmentsHashMap, Just appendedMessage)
      else
        (StrictHashMap.insert messageIdNo appendedMessage fragmentsHashMap, Nothing)
