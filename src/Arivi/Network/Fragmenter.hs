module Arivi.Network.Fragmenter
(
) where

import           Arivi.Crypto.Utils.PublicKey.Utils
import           Arivi.Crypto.Utils.Random
import qualified Arivi.Network.Connection           as Conn
import           Arivi.Network.Types                (FragmentCount,
                                                     FragmentNumber, MessageId,
                                                     Opcode (..), Parcel (..),
                                                     Payload (..),
                                                     makeDataParcel)
import           Control.Concurrent.STM             (TChan, atomically,
                                                     writeTChan)
import qualified Data.ByteString.Char8              as B
import qualified Data.ByteString.Lazy               as BSL
import           Data.Int                           (Int16, Int64)

-- | Convert GHC.Int64 to Data.Int.Int16
fromInt64ToInt16 :: Int64 -> Int16
fromInt64ToInt16 = fromIntegral

-- | Hardcoded currently to 256 bytes
getFragmentSize :: Int16
getFragmentSize = 256


-- | Enqueue the msg onto the outboundTChan
processFragment :: BSL.ByteString -> Conn.Connection -> MessageId -> FragmentNumber -> FragmentCount -> IO()
processFragment chunk conn msgId fragmentNum fragmentCount = do
    -- make outbound msg
    let outboundMsg = (msgId, fragmentNum, fragmentCount, Payload chunk)
    -- put it into TChan
    atomically $ writeTChan (Conn.outboundFragmentTChan conn) outboundMsg


processPayload :: BSL.ByteString -> Conn.Connection ->IO()
processPayload payload conn = do
    msgId <- generateMessageId
    let fragmentNum = 1 :: FragmentNumber
    -- Verify that converting payload length to Int16 and dividing won't cause problems
    let fragmentCount = quot (fromInt64ToInt16 (BSL.length payload)) getFragmentSize
    fragmentPayload payload conn msgId fragmentNum fragmentCount

fragmentPayload :: BSL.ByteString -> Conn.Connection -> MessageId -> FragmentNumber -> FragmentCount -> IO()
fragmentPayload payload conn msgId fragmentNum fragmentCount =
    if BSL.null payload then
        return ()
    else do
        let (chunk, rest) = BSL.splitAt 256 payload
        processFragment chunk conn msgId fragmentNum fragmentCount
        fragmentPayload rest conn msgId (fragmentNum + 1) fragmentCount

    -- | Generating a random 8 byte bytestring for msgId
generateMessageId :: IO B.ByteString
generateMessageId = getRandomByteString 8