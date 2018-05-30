import           Arivi.Crypto.Cipher.ChaChaPoly1305
import           Arivi.Crypto.Utils.Keys.Encryption
import           Arivi.Crypto.Utils.Random
import           Crypto.Error
import           Data.Binary                        (decode, encode)
import           Data.ByteArray
import           Data.ByteString.Char8              as B
import           Data.ByteString.Lazy               as L
import           Data.Int                           (Int64)

main :: IO()
main = do



    -- | Sender will compute ephemeral key pairs
    (ePhSK,ePhPK) <- generateKeyPair


    -- | Receiver will compute his key pairs

    (resvSK,resvPK) <- generateKeyPair


    -- | Sender will send ephemeral public key to Receiver, then Receiver will compute
    -- shared secret key using received ephemeral public key

    let sharedSecretKey = Data.ByteArray.convert (derivedSharedSecretKey ePhPK resvSK) :: B.ByteString



    -- | sender will compute 12 Bytes Nonce
    -- mNonce <-  getRandomByteString 8

    let mNonce = L.toStrict $ encode (1::Int64)

    let mheader = B.pack "this is header"
    let mPlaintext = B.pack "Hello World"


    -- | sender encrypts plain text using Nonce sharedSecretKey and header

    let cipherTextWithAuthTag = chachaEncrypt mNonce sharedSecretKey mheader mPlaintext


    -- | Receiver separates actual cipher text and message authentication tag (MAC) parts

    let (cipherText,authenticationTag) = getCipherTextAuthPair (throwCryptoError cipherTextWithAuthTag)

    -- | Calculates plain text message if authenticationTag is valid
    print (chachaDecrypt mNonce sharedSecretKey mheader authenticationTag cipherText)


    -- | Message is tampered
    let tamperdCipherTextMessage = B.append cipherText (B.pack "d")

    -- | If message is tampered, gives an error

    print (chachaDecrypt mNonce sharedSecretKey mheader authenticationTag tamperdCipherTextMessage)
