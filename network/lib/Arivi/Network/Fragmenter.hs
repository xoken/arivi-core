{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Arivi.Network.Fragmenter
(
    processPayload
) where

import           Arivi.Crypto.Utils.PublicKey.Utils (encryptMsg)
import           Arivi.Crypto.Utils.Random
import           Arivi.Network.Connection           as Conn (CompleteConnection, egressSeqNum, aeadNonceCounter, sharedSecret, connectionId)
import           Arivi.Network.StreamClient
import           Arivi.Network.Types                (FragmentNumber,
                                                     Header (..), MessageId,
                                                     Parcel (..), Payload (..))
import           Codec.Serialise
import           Control.Concurrent.STM             (STM, modifyTVar, readTVar)


import qualified Data.ByteString.Char8              as B
import qualified Data.ByteString.Lazy               as BSL
import           Data.Int                           (Int16, Int64)
import           Debug.Trace
-- | Convert GHC.Int64 to Data.Int.Int16
fromInt64ToInt16 :: Int64 -> Int16
fromInt64ToInt16 = fromIntegral


-- | Hardcoded currently to 256 bytes
getFragmentSize :: Int16
getFragmentSize = 1024

getFragmentCount :: Int16 -> Int16
getFragmentCount payloadLength | payloadLength `mod` getFragmentSize == 0 =
                                   quot payloadLength getFragmentSize
                               | otherwise =
                                   quot payloadLength getFragmentSize + 1


-- | This function is called in a different thread for each message
processPayload :: Payload -> Conn.CompleteConnection -> IO [STM BSL.ByteString]
processPayload payload conn = do
    msgId <- generateMessageId
    let fragmentNum = 1 :: FragmentNumber
    -- Verify that converting payload length to Int16 and dividing won't cause problems
    let fragmentCount = getFragmentCount (fromInt64ToInt16 (BSL.length (getPayload payload)))
    return $ fragmentPayload (getPayload payload) conn msgId fragmentNum fragmentCount


-- | Enqueue the msg onto the outboundTChan
processFragment :: BSL.ByteString -> Conn.CompleteConnection -> MessageId -> FragmentNumber -> FragmentNumber -> STM BSL.ByteString
processFragment fragment conn msgId fragmentNum fragmentCount = do
    aeadNonce <- readTVar (Conn.aeadNonceCounter conn)
    egressNonce <- readTVar (Conn.egressSeqNum conn)
    let headerData = DataHeader msgId fragmentNum fragmentCount (Conn.connectionId conn) egressNonce aeadNonce
    let encryptedData = encryptMsg aeadNonce (Conn.sharedSecret conn) (BSL.toStrict $ traceShow (BSL.length $ serialise headerData) (serialise headerData)) (BSL.toStrict fragment)
    let parcel = Parcel headerData (Payload $ BSL.fromStrict encryptedData)

    modifyTVar (Conn.aeadNonceCounter conn) (+1)
    modifyTVar (Conn.egressSeqNum conn) (+1)
    return $ serialiseAndFrame parcel

serialiseAndFrame :: Parcel -> BSL.ByteString
serialiseAndFrame = createFrame . serialise


-- | Fragments the payload, calls processFragment on the fragment and recursively calls the remaining payload
fragmentPayload :: BSL.ByteString -> Conn.CompleteConnection -> MessageId -> FragmentNumber -> FragmentNumber -> [STM BSL.ByteString]
fragmentPayload (BSL.null -> True) _ _ _ _ = []
fragmentPayload payload conn msgId fragmentNum fragmentCount = frame : fragmentPayload rest conn msgId (fragmentNum + 1) fragmentCount where
        -- Change hardcoded fragment size
        (fragment, rest) = BSL.splitAt 1024 payload
        frame = processFragment fragment conn msgId fragmentNum fragmentCount

-- | Generating a random 8 byte bytestring for msgId
generateMessageId :: IO B.ByteString
generateMessageId = getRandomByteString 8

-- payloadToFrames :: BSL.ByteString -> Int64 -> STM [BSL.ByteString]
-- payloadToFrames (BSL.null -> True) _ = return []
-- payloadToFragments payload n = process fragment : payloadToFragments (BSL.drop n payload) n where
--         fragment = BSL.take n payload
--         process = createFrame.serialise.processFragment
