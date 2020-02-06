<<<<<<< HEAD
import           Arivi.Crypto.Utils.Random




main :: IO()
main = do

    let mRandomByteString = getRandomByteString 32

    -- | Converts `IO ByteString` type to `ByteString` type
    r <- mRandomByteString


=======
import Arivi.Crypto.Utils.Random

main :: IO ()
main = do
    let mRandomByteString = getRandomByteString 32
    -- | Converts `IO ByteString` type to `ByteString` type
    r <- mRandomByteString
>>>>>>> breaking out arivi-core from arivi
    print r
