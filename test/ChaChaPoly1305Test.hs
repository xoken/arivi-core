import           Arivi.Crypto.Cipher.ChaChaPoly1305
import           Arivi.Crypto.Utils.Keys.Encryption
import           Arivi.Crypto.Utils.Random
import           Crypto.Error
import           Data.ByteArray
import           Data.ByteString.Char8




main :: IO()
main = do



    -- | Sender will compute ephemeral key pairs
    (ePhSK,ePhPK) <- Crypto.Utils.Keys.Encryption.generateKeyPair


    -- | Receiver will compute his key pairs

    (resvSK,resvPK) <- Crypto.Utils.Keys.Encryption.generateKeyPair


    -- | Sender will send ephemeral public key to Receiver, then Receiver will compute
    -- shared secret key using received ephemeral public key

    let sharedSecretKey = Data.ByteArray.convert (derivedSharedSecreatKey ePhPK resvSK) :: ByteString



    -- | sender will compute 12 Bytes Nonce
    mNonce <-  Crypto.Utils.Random.getRandomByteString 12

    let mheader = Data.ByteString.Char8.pack "this is header"
    let mPlaintext = Data.ByteString.Char8.pack "Hello World"


    -- | sender encrypts plain text using Nonce sharedSecretKey and header

    let cipherTextWithAuthTag = chachaEncrypt mNonce sharedSecretKey mheader mPlaintext


    -- | Receiver separates actual cipher text and message authentication tag (MAC) parts

    let (cipherText,authenticationTag) = getCipherTextAuthPair (throwCryptoError cipherTextWithAuthTag)

    -- | Calculates plain text message if authenticationTag is valid
    print (chachaDecrypt mNonce sharedSecretKey mheader authenticationTag cipherText)


    -- | Message is tampered
    let tamperdCipherTextMessage = Data.ByteString.Char8.append cipherText (Data.ByteString.Char8.pack "d")

    -- | If message is tampered, gives an error

    print (chachaDecrypt mNonce sharedSecretKey mheader authenticationTag tamperdCipherTextMessage)