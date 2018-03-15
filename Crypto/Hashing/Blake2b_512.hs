module Crypto.Hashing.Blake2b_512
(
    makeHash 
) where 



import Crypto.Hash (Blake2b_512,Digest,hash)
import Data.ByteString.Char8 (ByteString,pack)

makeHash  :: ByteString -> Digest Blake2b_512
makeHash  = hash 
