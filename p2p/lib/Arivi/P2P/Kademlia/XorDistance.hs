-- |
-- Module      : Arivi.Kademlia.XorDistance
-- License     :
-- Maintainer  : Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides generation of xor distance between two node Id's
--
module Arivi.P2P.Kademlia.XorDistance
    ( getXorDistance
    , getRawXor
    , getKbIndex
    ) where

import qualified Arivi.P2P.Kademlia.Types as T
import           Data.Bits
import           Data.ByteString.Base16   (encode)
import qualified Data.ByteString.Char8    as C
import           Data.Char                (digitToInt)

fn :: Char -> Int
fn str = digitToInt str :: Int

hexToDigits :: String -> [Int]
hexToDigits = map fn

hexToDec :: (Num t1, Integral b) => [t1] -> b -> t -> t1
hexToDec [] _ _           = 0
hexToDec (xs:x) index len = hexToDec x (index + 1) len + (16 ^ index) * xs

hexToDecimal :: Num a => [a] -> a
hexToDecimal lst = hexToDec (reverse lst) (0 :: Integer) (length lst)

bitWisexorOfKeys :: String -> String -> [Int]
bitWisexorOfKeys firstNodeId secondNodeId =
    zipWith xor (hexToDigits firstNodeId) (hexToDigits secondNodeId)

-- | Gives the xor between two nodeIds
getRawXor :: String -> String -> Integer
getRawXor firstNodeId secondNodeId =
    hexToDecimal (map toInteger (bitWisexorOfKeys firstNodeId secondNodeId))

-- getXorDistancelog2 number = logBase 2 (fromIntegral number)
-- | Gives log2 of xor between two nodeId's if xor of two nodeId's is 0 then
-- it returns 0
getXorDistance :: Floating t => String -> String -> t
getXorDistance firstNodeId secondNodeId =
    if rawXor == 0
        then 0
        else logBase 2 (fromIntegral rawXor)
  where
    rawXor = getRawXor firstNodeId secondNodeId

-- | Gives the KB-index for two given node ids i.e gives the index at which
--  the node will be stored in the kbucket hash table
getKbIndex :: T.NodeId -> T.NodeId -> Int
getKbIndex node1 node2 = kbi
  where
    node1Str = C.unpack $ Data.ByteString.Base16.encode node1
    node2Str = C.unpack $ Data.ByteString.Base16.encode node2
    kbi = round (getXorDistance node1Str node2Str :: Double)
