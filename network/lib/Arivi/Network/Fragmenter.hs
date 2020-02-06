{-# LANGUAGE ViewPatterns #-}
<<<<<<< HEAD
=======

>>>>>>> breaking out arivi-core from arivi
module Arivi.Network.Fragmenter
    ( processPayload
    ) where

<<<<<<< HEAD
import           Arivi.Crypto.Utils.PublicKey.Utils (encryptMsg)
import           Arivi.Crypto.Utils.Random
import           Arivi.Network.Connection           as Conn (CompleteConnection,
                                                             aeadNonceCounter,
                                                             connectionId,
                                                             egressSeqNum,
                                                             sharedSecret,
                                                             transportType)
import           Arivi.Network.StreamClient
import           Arivi.Network.Types                (FragmentNumber,
                                                     Header (..), MessageId,
                                                     Parcel (..), Payload (..),
                                                     TransportType (..))
import           Codec.Serialise
import           Control.Concurrent.STM             (STM, modifyTVar, readTVar)

import qualified Data.ByteString.Char8              as B
import qualified Data.ByteString.Lazy               as BSL
import           Data.Int                           (Int64)
=======
import Arivi.Crypto.Utils.PublicKey.Utils (encryptMsg)
import Arivi.Crypto.Utils.Random
import Arivi.Network.Connection as Conn
    ( CompleteConnection
    , aeadNonceCounter
    , connectionId
    , egressSeqNum
    , sharedSecret
    , transportType
    )
import Arivi.Network.StreamClient
import Arivi.Network.Types (FragmentNumber, Header(..), MessageId, Parcel(..), Payload(..), TransportType(..))
import Codec.Serialise
import Control.Concurrent.STM (STM, modifyTVar, readTVar)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
>>>>>>> breaking out arivi-core from arivi

-- Fragment size need not be an Int64
getFragmentCount :: Int64 -> Int64 -> Int64
getFragmentCount fragmentSize payloadLength
<<<<<<< HEAD
    | payloadLength `mod` fragmentSize == 0 =
        quot payloadLength fragmentSize
    | otherwise = quot payloadLength fragmentSize + 1

-- | This function is called in a different thread for each message
processPayload ::
       Int64 -> Payload -> Conn.CompleteConnection -> IO [STM BSL.ByteString]
=======
    | payloadLength `mod` fragmentSize == 0 = quot payloadLength fragmentSize
    | otherwise = quot payloadLength fragmentSize + 1

-- | This function is called in a different thread for each message
processPayload :: Int64 -> Payload -> Conn.CompleteConnection -> IO [STM BSL.ByteString]
>>>>>>> breaking out arivi-core from arivi
processPayload fragmentSize payload conn = do
    msgId <- generateMessageId
    let fragmentNum = 1 :: FragmentNumber
    -- Verify that converting payload length to Int16 and dividing won't cause problems
<<<<<<< HEAD
    let fragmentCount =
            getFragmentCount fragmentSize (BSL.length (getPayload payload))
    return $
        fragmentPayload
            fragmentSize
            (getPayload payload)
            conn
            msgId
            fragmentNum
            fragmentCount

-- | Enqueue the msg onto the outboundTChan
processFragment ::
       BSL.ByteString
    -> Conn.CompleteConnection
    -> MessageId
    -> FragmentNumber
    -> FragmentNumber
    -> STM BSL.ByteString
processFragment fragment conn msgId fragmentNum fragmentCount = do
    iv <- readTVar (Conn.aeadNonceCounter conn)
    egressNonce <- readTVar (Conn.egressSeqNum conn)
    let headerData =
            DataHeader
                msgId
                fragmentNum
                fragmentCount
                (Conn.connectionId conn)
                egressNonce
                iv
    let encryptedData =
            encryptMsg
                iv
                (Conn.sharedSecret conn)
                (BSL.toStrict $ serialise headerData)
                (BSL.toStrict fragment)
=======
    let fragmentCount = getFragmentCount fragmentSize (BSL.length (getPayload payload))
    return $ fragmentPayload fragmentSize (getPayload payload) conn msgId fragmentNum fragmentCount

-- | Enqueue the msg onto the outboundTChan
processFragment ::
       BSL.ByteString -> Conn.CompleteConnection -> MessageId -> FragmentNumber -> FragmentNumber -> STM BSL.ByteString
processFragment fragment conn msgId fragmentNum fragmentCount = do
    iv <- readTVar (Conn.aeadNonceCounter conn)
    egressNonce <- readTVar (Conn.egressSeqNum conn)
    let headerData = DataHeader msgId fragmentNum fragmentCount (Conn.connectionId conn) egressNonce iv
    let encryptedData =
            encryptMsg iv (Conn.sharedSecret conn) (BSL.toStrict $ serialise headerData) (BSL.toStrict fragment)
>>>>>>> breaking out arivi-core from arivi
    let parcel = Parcel headerData (Payload $ BSL.fromStrict encryptedData)
    modifyTVar (Conn.aeadNonceCounter conn) (+ 1)
    modifyTVar (Conn.egressSeqNum conn) (+ 1)
    return $
        case transportType conn of
            TCP -> serialiseAndFrame parcel
            UDP -> serialise parcel

serialiseAndFrame :: Parcel -> BSL.ByteString
serialiseAndFrame = createFrame . serialise

-- | Fragments the payload, calls processFragment on the fragment and
--   recursively calls the remaining payload
fragmentPayload ::
       Int64
    -> BSL.ByteString
    -> Conn.CompleteConnection
    -> MessageId
    -> FragmentNumber
    -> FragmentNumber
    -> [STM BSL.ByteString]
fragmentPayload _ (BSL.null -> True) _ _ _ _ = []
fragmentPayload fragmentSize payload conn msgId fragmentNum fragmentCount =
    frame : fragmentPayload fragmentSize rest conn msgId (fragmentNum + 1) fragmentCount
        -- Change hardcoded fragment size
  where
    (fragment, rest) = BSL.splitAt fragmentSize payload
    frame = processFragment fragment conn msgId fragmentNum fragmentCount

-- | Generating a random 8 byte bytestring for msgId
generateMessageId :: IO B.ByteString
generateMessageId = getRandomByteString 8
