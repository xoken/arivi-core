{-# LANGUAGE OverloadedStrings #-}

<<<<<<< HEAD
import           Arivi.Crypto.Hashing.Blake2b_512
import           Data.ByteString.Char8            (ByteString)
=======
import Arivi.Crypto.Hashing.Blake2b_512
import Data.ByteString.Char8 (ByteString)
>>>>>>> breaking out arivi-core from arivi

s :: ByteString
s = "hello world"

<<<<<<< HEAD

main :: IO()
main =
    print  (makeHash s)
=======
main :: IO ()
main = print (makeHash s)
>>>>>>> breaking out arivi-core from arivi
