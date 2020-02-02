module Arivi.Network.Handshake
    ( initiatorHandshake
    , recipientHandshake
    , receiveHandshakeResponse
    ) where

import Arivi.Network.Connection as Conn
    ( CompleteConnection
    , IncompleteConnection
    , connectionId
    , remoteNodeId
    , sharedSecret
    )
import Arivi.Network.Exception
import Arivi.Network.HandshakeUtils
import Arivi.Network.Types (HandshakeInitMasked(..), Parcel(..))
import Codec.Serialise
import Control.Exception (throw)
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString.Lazy as L

-- | Takes the static secret key and connection object and returns a serialized KeyExParcel along with the updated connection object
initiatorHandshake :: Ed25519.SecretKey -> Conn.IncompleteConnection -> IO (EphemeralKeyPair, L.ByteString)
initiatorHandshake sk conn
        -- Generate an ephemeral keypair. Get a new connection with ephemeral keys populated
 = do
    ephemeralKeyPair <- generateEphemeralKeys
        -- Get handshake init message and updated connection object with temp shared secret key
    let (hsInitMsg, updatedConn) = createHandshakeInitMsg sk conn (fst ephemeralKeyPair)
    let hsParcel = generateInitParcel hsInitMsg (snd ephemeralKeyPair) updatedConn
    return (ephemeralKeyPair, serialise hsParcel)

-- | Takes receiver static secret key, connection object and the received msg and returns a serialised handshake response along with the updated connection object
recipientHandshake ::
       Ed25519.SecretKey -> Conn.IncompleteConnection -> Parcel -> IO (L.ByteString, Conn.CompleteConnection)
recipientHandshake sk conn parcel
    | verifySignature sk hsInitMsg
    -- Generate an ephemeral keypair. Get a new connection with ephemeral keys populated
     = do
        (eSKSign, ephemeralPublicKey) <- generateEphemeralKeys
                -- Get updated connection structure with final shared secret key for session
        let updatedConn = extractSecrets conn senderEphemeralPublicKey eSKSign
        let updatedConn' = updatedConn {Conn.remoteNodeId = nodePublicKey hsInitMsg}
                -- NOTE: Need to delete the ephemeral key pair from the connection object as it is not needed once shared secret key is derived
        let hsRespMsg = createHandshakeRespMsg (Conn.connectionId updatedConn')
        let hsRespParcel = generateRespParcel hsRespMsg (sharedSecret updatedConn') ephemeralPublicKey
        return (serialise hsRespParcel, updatedConn')
    | otherwise = throw NetworkSignatureVerificationFailedException
  where
    (hsInitMsg, senderEphemeralPublicKey) = readHandshakeMsg sk parcel

-- | Initiator receives response from remote and returns updated connection object
receiveHandshakeResponse :: Conn.IncompleteConnection -> EphemeralKeyPair -> Parcel -> Conn.CompleteConnection
receiveHandshakeResponse conn (ephemeralPrivateKey, _) parcel = updatedConn
    -- (hsRespMsg, updatedConn) = readHandshakeResp conn parcel
  where
    (_, updatedConn) = readHandshakeResp conn ephemeralPrivateKey parcel
    -- Need to delete ephemeral keypair from updated conn object
