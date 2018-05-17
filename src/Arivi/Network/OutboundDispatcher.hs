module Arivi.Network.OutboundDispatcher (
    outboundFrameDispatcher
    ) where

import           Arivi.Crypto.Utils.PublicKey.Utils (encryptMsg)
import           Arivi.Network.Connection           as NetworkConnection
-- import           Arivi.Network.Stream
import           Arivi.Network.Types                (Header (..), Opcode (..),
                                                     OutboundFragment,
                                                     Parcel (..), Payload (..))
import           Arivi.Network.Utils
import           Codec.Serialise
import           Control.Concurrent.STM             (TChan, atomically,
                                                     newTChan, readTChan,
                                                     writeTChan)
import qualified Data.Binary                        as Binary (decode, encode)
import qualified Data.ByteString.Char8              as B (ByteString, empty)
import           Data.Int                           (Int16, Int64)
-- | Take a bytestring, convert to Int, increment it and convert back to Bytestring
incrementAeadNonce :: B.ByteString -> B.ByteString
incrementAeadNonce aeadNonce = lazyToStrict incrementedNonce where
    -- Convert to lazy bytestring
    lazyBSNonce = strictToLazy aeadNonce
    -- convert to integer
    nonceAsInt = Binary.decode lazyBSNonce :: Integer
    -- increment by 1
    -- convert back to bytestring
    incrementedNonce = Binary.encode (nonceAsInt + 1)


-- | This thread is created one per connection.
-- | It is spawned by the FSM just before going into SecureTransportEstablised state with an initial value of the aead nonce. The aead nonce is incremented accordingly each time by the recursive call
outboundFrameDispatcher :: TChan OutboundFragment -> NetworkConnection.Connection -> B.ByteString -> Integer -> IO()
outboundFrameDispatcher outboundTChan conn aeadnonce replayNonce = do
    (msgId, fragmentNum, fragmentCount, fragment) <- atomically $ readTChan outboundTChan
    -- Generate header
    let headerData = DataHeader DATA msgId fragmentNum fragmentCount (NetworkConnection.connectionId conn) replayNonce
    -- Generate encrypted payload
    let encryptedData = encryptMsg aeadnonce (NetworkConnection.sharedSecret conn) headerData (lazyToStrict $ getPayload fragment)
    -- Create parcel
    let parcel = Parcel headerData (Payload $ strictToLazy encryptedData)
    -- Create frame
    -- let frame = createFrame (serialise parcel)
    -- Call the send function on the socket
    -- sendFrame(NetworkConnection.socket conn) frame
    -- Recursively call function with nonces incremented
    outboundFrameDispatcher outboundTChan conn (incrementAeadNonce aeadnonce) (replayNonce + 1)
    return()
