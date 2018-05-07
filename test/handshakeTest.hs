import Arivi.Network.Handshake
import Arivi.Network.Types
import Arivi.Network.Connection
import Arivi.Crypto.Utils.PublicKey.Utils
import Data.ByteString.Char8 as B

main = do
    (senderSK, senderPK) <- generateSigningKeyPair
    (recvSK, recvPK) <- generateSigningKeyPair
    let senderNodeId = generateNodeId senderSK
    let recvNodeId = generateNodeId recvSK
    let connId = B.pack "1"
    let conn = Connection connId recvNodeId undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined
    sentParcel <- doInitHandshake senderSK conn
    receiveHandshake recvNodeId recvSK sentParcel
