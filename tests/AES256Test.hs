import Crypto.Utils.Cipher.AES256
import Crypto.Utils.Keys.Encryption
import Data.ByteString.Char8 as B


main::IO()
main = do

    -- |Ephemeral Keys generation at sender side
    (esk,epk) <- Crypto.Utils.Keys.Encryption.generateKeyPair
    -- | Key generation at REceiver side
    (rsk,rpk) <- Crypto.Utils.Keys.Encryption.generateKeyPair


    -- | sharedSecretKey generation 
    let sharedSecretKey = Crypto.Utils.Keys.Encryption.createSharedSecreatKey esk rpk
    
    -- | Initialization vector 
    let iv0 = Crypto.Utils.Cipher.AES256.nullIV
    let iv = Crypto.Utils.Cipher.AES256.ivAdd iv0 44354

    -- | PlainText Message
    let plainTextMsg = B.pack "Hello World"

    -- | Encryption using AES 256
    let cipherText = Crypto.Utils.Cipher.AES256.aesEncrypt sharedSecretKey iv plainTextMsg

    print cipherText

    -- | Decryption using AES 256

    let decryptedCipherText = Crypto.Utils.Cipher.AES256.aesDecrypt sharedSecretKey iv cipherText

    print decryptedCipherText