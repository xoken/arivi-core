import Arivi.Crypto.Utils.Random

main :: IO ()
main = do
    let mRandomByteString = getRandomByteString 32
    -- | Converts `IO ByteString` type to `ByteString` type
    r <- mRandomByteString
    print r
