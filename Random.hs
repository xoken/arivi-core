module Crypto.Random
(
    getRandomByteString
)
where 

import Raaz.Random
import Raaz.Core.Types
import Raaz.Core.Memory
import Data.ByteString


-- | Generates RandomByteString of length `len` 

getRandomByteString :: BYTES Int -> IO ByteString
getRandomByteString len = securely (randomByteString (len :: BYTES Int) :: RandM ByteString)