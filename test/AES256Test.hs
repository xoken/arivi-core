import           Arivi.Crypto.Cipher.AES256
import           Arivi.Crypto.Utils.Keys.Encryption
import           Data.ByteString.Char8              as B


main::IO()
main = do

    -- |Ephemeral Keys generation at sender side
    (esk,epk) <- Arivi.Crypto.Utils.Keys.Encryption.generateKeyPair
    -- | Key generation at REceiver side
    (rsk,rpk) <- Arivi.Crypto.Utils.Keys.Encryption.generateKeyPair


    -- | sharedSecretKey generation
    let sharedSecretKey = Arivi.Crypto.Utils.Keys.Encryption.createSharedSecreatKey esk rpk

    -- | Initialization vector
    let iv0 =Arivi.Crypto.Cipher.AES256.nullIV
    let iv = Arivi.Crypto.Cipher.AES256.ivAdd iv0 44354

    -- | PlainText Message
    let plainTextMsg = B.pack "Hello World"

    -- | Encryption using AES 256
    let cipherText = Arivi.Crypto.Cipher.AES256.aesEncrypt sharedSecretKey iv plainTextMsg

    print cipherText

    -- | Decryption using AES 256

    let decryptedCipherText = Arivi.Crypto.Cipher.AES256.aesDecrypt sharedSecretKey iv cipherText

    print decryptedCipherText
