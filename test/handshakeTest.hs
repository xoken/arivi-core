import Arivi.Network.Handshake
import Arivi.Network.Types
import Arivi.Crypto.Utils.PublicKey.Utils

main = do
    (senderSK, senderPK) <- generateSigningKeyPair
    (recvSK, recvPK) <- generateSigningKeyPair
    let senderNodeId = generateNodeId senderSK
    let recvNodeId = generateNodeId recvSK

    sentParcel <- doInitHandshake [V0] 42 senderNodeId senderSK recvNodeId
    receiveHandshake recvNodeId recvSK sentParcel