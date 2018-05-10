import           Data.Proxy

import           Arivi.Crypto.Utils.Keys.Encryption
import           Arivi.Crypto.Utils.Random

main :: IO()
main = do


   -- | Receivers Public Key and Secret Key Generations

   let randomByteString = Crypto.Utils.Random.getRandomByteString 32
   randomSeed <- randomByteString

   let receiversSK = Crypto.Utils.Keys.Encryption.getSecretKey randomSeed
   let receiversPK = Crypto.Utils.Keys.Encryption.getPublicKey receiversSK


   -- | Ephemeral Public Key and Secret Key Generations


   let ephemeralRandomByteString = Crypto.Utils.Random.getRandomByteString 32
   ephemeralRandomSeed <- ephemeralRandomByteString


   let ephemeralSK = Crypto.Utils.Keys.Encryption.getSecretKey ephemeralRandomSeed
   let ephemeralPK = Crypto.Utils.Keys.Encryption.getPublicKey ephemeralSK


   -- | SharedSecret generation at sender side

   let ssksender = createSharedSecreatKey ephemeralSK receiversPK

   -- | SharedSecret generation at receiver side

   let sskReceiver = derivedSharedSecreatKey ephemeralPK receiversSK


   print (ssksender == sskReceiver) -- ^ Proof of Both the SharedSecret keys are equal
