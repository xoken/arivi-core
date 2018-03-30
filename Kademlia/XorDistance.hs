module Kademlia.XorDistance(

    getXorDistance,
    getRawXor,
)
where


import           Data.Bits
import           Data.ByteString.Char8 (ByteString)
import           Data.Char             (digitToInt, intToDigit)
import           Numeric               (showHex, showIntAtBase)



<<<<<<< HEAD
fn str      =  digitToInt str :: Int
hexToDigits = map fn
=======
fn str =  digitToInt str :: Int
hexToDigits  = map fn
>>>>>>> 8926b49a345627bedf9f05a30f218477f1271cfb

hexToDec [] _ _              = 0;
hexToDec (xs:x) index length = hexToDec x (index+1) length + (16 ^ index)*xs


hexToDecimal lst = hexToDec (reverse lst) 0 (length lst)



<<<<<<< HEAD
biWisexorOfKeys firstNodeId secondNodeId = zipWith xor (hexToDigits firstNodeId)
                                            (hexToDigits secondNodeId)
=======
biWisexorOfKeys firstNodeId secondNodeId = zipWith xor (hexToDigits firstNodeId) (hexToDigits secondNodeId)
>>>>>>> 8926b49a345627bedf9f05a30f218477f1271cfb

getRawXor firstNodeId secondNodeId = hexToDecimal(map toInteger
                                            (biWisexorOfKeys firstNodeId
                                            secondNodeId))

-- getXorDistancelog2 number = logBase 2 (fromIntegral number)

-- | Our notion of distance

getXorDistance firstNodeId secondNodeId = if rawXor == 0
                                            then 0
                                          else logBase 2 (fromIntegral rawXor)
<<<<<<< HEAD
                                          where rawXor = getRawXor firstNodeId
                                                    secondNodeId
=======
                                          where rawXor = getRawXor firstNodeId secondNodeId
>>>>>>> 8926b49a345627bedf9f05a30f218477f1271cfb
