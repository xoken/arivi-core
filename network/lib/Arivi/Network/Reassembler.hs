{-# LANGUAGE OverloadedStrings #-}

<<<<<<< HEAD
-- |
-- Module      :  Arivi.Network.Reassembler
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
=======
>>>>>>> breaking out arivi-core from arivi
--
-- This module is used for combining received fragments
--
module Arivi.Network.Reassembler
    ( reassembleFrames
    , decryptPayload
    ) where

<<<<<<< HEAD
import           Arivi.Crypto.Cipher.ChaChaPoly1305 (getCipherTextAuthPair)
import           Arivi.Crypto.Utils.PublicKey.Utils (decryptMsg)
import           Arivi.Network.Connection           (CompleteConnection,
                                                     sharedSecret)


import           Arivi.Network.Types                (Header (..), MessageId,
                                                     Parcel (..), Payload (..),
                                                     serialise)
import qualified Data.ByteString.Lazy               as Lazy (ByteString, concat,
                                                             fromStrict,
                                                             toStrict)
import qualified Data.HashMap.Strict                as StrictHashMap (HashMap,
                                                                      delete,
                                                                      insert,
                                                                      lookup)
import           Data.Maybe                         (fromMaybe)
=======
import Arivi.Crypto.Cipher.ChaChaPoly1305 (getCipherTextAuthPair)
import Arivi.Crypto.Utils.PublicKey.Utils (decryptMsg)
import Arivi.Network.Connection (CompleteConnection, sharedSecret)

import Arivi.Network.Types (Header(..), MessageId, Parcel(..), Payload(..), serialise)
import qualified Data.ByteString.Lazy as Lazy (ByteString, concat, fromStrict, toStrict)
import qualified Data.HashMap.Strict as StrictHashMap (HashMap, delete, insert, lookup)
import Data.Maybe (fromMaybe)
>>>>>>> breaking out arivi-core from arivi

-- | Extracts `Payload` messages from `DataParcel` and puts in the
--   list of fragmentsHashMap. Returns the hashmap along with a Just p2pMessage in case of a complete message reassembly or Nothing otherwise
reassembleFrames ::
       CompleteConnection
    -> Parcel
    -> StrictHashMap.HashMap MessageId Lazy.ByteString
    -> (StrictHashMap.HashMap MessageId Lazy.ByteString, Maybe Lazy.ByteString)
reassembleFrames connection parcel fragmentsHashMap
    | fragmentNumber (header parcel) == totalFragments (header parcel) =
<<<<<<< HEAD
        ( StrictHashMap.delete messageIdNo fragmentsHashMap
        , Just appendedMessage)
    | otherwise =
        ( StrictHashMap.insert messageIdNo appendedMessage fragmentsHashMap
        , Nothing)
=======
        (StrictHashMap.delete messageIdNo fragmentsHashMap, Just appendedMessage)
    | otherwise = (StrictHashMap.insert messageIdNo appendedMessage fragmentsHashMap, Nothing)
>>>>>>> breaking out arivi-core from arivi
  where
    messageIdNo = messageId (header parcel)
    payloadMessage = decryptPayload connection parcel
    messages = fromMaybe "" (StrictHashMap.lookup messageIdNo fragmentsHashMap)
    appendedMessage = Lazy.concat [messages, payloadMessage]

decryptPayload :: CompleteConnection -> Parcel -> Lazy.ByteString
decryptPayload connection parcel =
<<<<<<< HEAD
    Lazy.fromStrict $
    decryptMsg fragmentAead ssk parcelHeader authenticationTag cipherText
  where
    (cipherText, authenticationTag) =
        getCipherTextAuthPair
            (Lazy.toStrict (getPayload (encryptedPayload parcel)))
=======
    Lazy.fromStrict $ decryptMsg fragmentAead ssk parcelHeader authenticationTag cipherText
  where
    (cipherText, authenticationTag) = getCipherTextAuthPair (Lazy.toStrict (getPayload (encryptedPayload parcel)))
>>>>>>> breaking out arivi-core from arivi
    parcelHeader = Lazy.toStrict $ serialise (header parcel)
    fragmentAead = aeadNonce (header parcel)
    ssk = sharedSecret connection
