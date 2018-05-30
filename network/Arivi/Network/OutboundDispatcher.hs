module Arivi.Network.OutboundDispatcher (
    outboundFrameDispatcher
    ) where

import           Arivi.Crypto.Utils.PublicKey.Utils (encryptMsg)
import           Arivi.Network.Connection           as Conn (Connection (..))
import           Arivi.Network.StreamClient
import           Arivi.Network.Types                (Header (..),
                                                     OutboundFragment,
                                                     Parcel (..), Payload (..))
import           Arivi.Network.Utils
import           Codec.Serialise
import           Control.Concurrent.STM             (TChan, atomically,
                                                     readTChan)
import qualified Data.Binary                        as Binary (decode, encode)
import qualified Data.ByteString.Char8              as B (ByteString)
import           Data.ByteString.Lazy               as L
import           Data.Int                           (Int64)
-- | This thread is created one per connection.
-- | It is spawned by the FSM just before going into SecureTransportEstablised state with an initial value of the aead nonce. The aead nonce is incremented accordingly each time by the recursive call
outboundFrameDispatcher :: TChan OutboundFragment -> Connection -> Int64 -> Integer -> IO ()
outboundFrameDispatcher outboundTChan conn aeadnonce replayNonce = do
    (msgId, fragmentNum, fragmentCount, fragment) <- atomically $ readTChan outboundTChan
    -- Generate header
    let headerData = DataHeader msgId fragmentNum fragmentCount (Conn.connectionId conn) replayNonce
    -- Generate encrypted payload
    let encryptedData = encryptMsg aeadnonce (sharedSecret conn) (L.toStrict $ serialise headerData) (lazyToStrict $ getPayload fragment)
    -- Create parcel
    let parcel = Parcel headerData (Payload $ strictToLazy encryptedData)
    -- Create frame
    let frame = createFrame (serialise parcel)
    -- Call the send function on the socket
    sendFrame(socket conn) frame
    -- Recursively call function with nonces incremented
    outboundFrameDispatcher outboundTChan conn (aeadnonce+1) (replayNonce + 1)
    return ()
