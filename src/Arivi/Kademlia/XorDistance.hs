-- |
-- Module      : Arivi.Kademlia.XorDistance
-- License     :
-- Maintainer  : Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides generation of xor distance between two node Id's
--
module Arivi.Kademlia.XorDistance(

    getXorDistance,
    getRawXor
)
where


import           Data.Bits
import           Data.ByteString.Char8 (ByteString)
import           Data.Char             (digitToInt, intToDigit)
import           Numeric               (showHex, showIntAtBase)


fn :: Char -> Int
fn str =  digitToInt str :: Int

hexToDigits :: String -> [Int]
hexToDigits = map fn

hexToDec :: (Num t1, Integral b) => [t1] -> b -> t -> t1
hexToDec [] _ _              = 0;
hexToDec (xs:x) index length = hexToDec x (index+1) length + (16 ^ index)*xs

hexToDecimal :: Num a => [a] -> a
hexToDecimal lst = hexToDec (reverse lst) 0 (length lst)


bitWisexorOfKeys :: String -> String -> [Int]
bitWisexorOfKeys firstNodeId secondNodeId = zipWith xor (hexToDigits firstNodeId)
                                            (hexToDigits secondNodeId)
-- | Gives the xor between two nodeIds
getRawXor :: String -> String -> Integer
getRawXor firstNodeId secondNodeId = hexToDecimal(map toInteger
                                            (bitWisexorOfKeys firstNodeId
                                            secondNodeId))

-- getXorDistancelog2 number = logBase 2 (fromIntegral number)

-- | Gives log2 of xor between two nodeId's if xor of two nodeId's is 0 then
-- it returns 0
getXorDistance :: Floating t => String -> String -> t
getXorDistance firstNodeId secondNodeId = if rawXor == 0
                                            then 0
                                          else logBase 2 (fromIntegral rawXor)
                                          where rawXor = getRawXor firstNodeId
                                                    secondNodeId
