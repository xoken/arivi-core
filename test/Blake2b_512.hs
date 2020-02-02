{-# LANGUAGE OverloadedStrings #-}

import Arivi.Crypto.Hashing.Blake2b_512
import Data.ByteString.Char8 (ByteString)

s :: ByteString
s = "hello world"

main :: IO ()
main = print (makeHash s)
