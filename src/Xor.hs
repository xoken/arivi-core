module Xor(
    getXorDistance,
    getXorDistancelog2,
    hexToDecimal,
    hexToDigits,
    hexToDec,
)
where 


import Data.Bits
import Numeric (showHex,showIntAtBase)
import Data.Char (intToDigit,digitToInt)
import Data.ByteString.Char8 (ByteString)



fn str =  (digitToInt str ):: Int
hexToDigits str = map fn str

hexToDec [] _ _  = 0; 
hexToDec (xs:x) index length = hexToDec(x) (index+1) length + (16 ^ index)*xs


hexToDecimal lst = hexToDec (reverse lst) 0 (length lst)



biWisexorOfKeys firstNodeId secondNodeId = zipWith xor (hexToDigits firstNodeId) (hexToDigits secondNodeId) 

getXorDistance firstNodeId secondNodeId = hexToDecimal(map toInteger (biWisexorOfKeys firstNodeId secondNodeId))

getXorDistancelog2 number = logBase 2 (fromIntegral number)
