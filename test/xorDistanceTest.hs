<<<<<<< HEAD
import           Arivi.Crypto.Utils.XorDistance


firstKey =  "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
secondKey = "ffffefffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"


main :: IO()
main = do

    print(getRawXor firstKey secondKey)
    print(getXorDistance firstKey secondKey)
    print(getXorDistance firstKey firstKey)
=======
import Arivi.Crypto.Utils.XorDistance

firstKey =
    "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

secondKey =
    "ffffefffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

main :: IO ()
main = do
    print (getRawXor firstKey secondKey)
    print (getXorDistance firstKey secondKey)
    print (getXorDistance firstKey firstKey)
>>>>>>> breaking out arivi-core from arivi
