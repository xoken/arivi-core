<<<<<<< HEAD
import           Arivi.Crypto.Cipher.ChaChaPoly1305
import           Arivi.Crypto.Utils.PublicKey.Utils
import           Arivi.Crypto.Utils.Random
import           Arivi.Network.Connection
import           Arivi.Network.Handshake
import           Arivi.Network.Types
import           Crypto.Error
import           Data.ByteString.Char8              as B

main::IO()

=======
import Arivi.Crypto.Cipher.ChaChaPoly1305
import Arivi.Crypto.Utils.PublicKey.Utils
import Arivi.Crypto.Utils.Random
import Arivi.Network.Connection
import Arivi.Network.Handshake
import Arivi.Network.Types
import Crypto.Error
import Data.ByteString.Char8 as B

main :: IO ()
>>>>>>> breaking out arivi-core from arivi
main = do
    (senderSK, senderPK) <- generateSigningKeyPair
    (recvSK, recvPK) <- generateSigningKeyPair
    let senderNodeId = generateNodeId senderSK
    let recvNodeId = generateNodeId recvSK
    let connId = B.pack "1"
<<<<<<< HEAD
    let senderConn = Connection connId recvNodeId undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined
    let recvConn = Connection connId  undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined
    (sentParcel, senderConn1) <- initiatorHandshake senderSK senderConn
    (recvParcel, recvConn1) <- recipientHandshake recvSK recvConn sentParcel
    let senderConn2 = receiveHandshakeResponse senderConn1 recvParcel

    let sSSK = sharedSecret senderConn2
    let rSSK = sharedSecret recvConn1

=======
    let senderConn =
            Connection
                connId
                recvNodeId
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
    let recvConn =
            Connection
                connId
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
                undefined
    (sentParcel, senderConn1) <- initiatorHandshake senderSK senderConn
    (recvParcel, recvConn1) <- recipientHandshake recvSK recvConn sentParcel
    let senderConn2 = receiveHandshakeResponse senderConn1 recvParcel
    let sSSK = sharedSecret senderConn2
    let rSSK = sharedSecret recvConn1
>>>>>>> breaking out arivi-core from arivi
    print (sSSK == rSSK)
