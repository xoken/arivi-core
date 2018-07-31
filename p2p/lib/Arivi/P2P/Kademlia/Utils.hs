module Arivi.P2P.Kademlia.Utils
    ( extractFirst
    , extractSecond
    , extractThird
    , stringToHostAddress
    , convertToSockAddr
    , sockAddrToHostAddr
    , sockAddrToPortNumber
    , extractFirst2
    , extractSecond2
    , extractThird2
    , extractFourth
    , isNodeIdElem
    , getSockAddr
    , getRandomSequence
    , getRandomSequence2
    , convToSockAddr
    , randomList
    , count'
    ) where

import           Arivi.Crypto.Utils.Keys.Signature
import qualified Data.ByteString.Char8             as C
import           Data.Int
import qualified Data.List.Split                   as S
import           Data.Word
import           Network.Socket
import qualified Network.Socket.Internal           as M
import           System.Random

-- Helper functions to extract value from 3-tuple
extractFirst :: (a, b, c) -> a
extractFirst (a, _, _) = a

extractSecond :: (a, b, c) -> b
extractSecond (_, b, _) = b

extractThird :: (a, b, c) -> c
extractThird (_, _, c) = c

extractFirst2 :: (a, b, c, d) -> a
extractFirst2 (a, _, _, _) = a

extractSecond2 :: (a, b, c, d) -> b
extractSecond2 (_, b, _, _) = b

extractThird2 :: (a, b, c, d) -> c
extractThird2 (_, _, c, _) = c

extractFourth :: (a, b, c, d) -> d
extractFourth (_, _, _, d) = d

stringToHostAddress :: String -> HostAddress
stringToHostAddress x = remoteIp
  where
    temp = S.splitOn "." x
    temp2 =
        case Prelude.map (read :: String -> Word8) temp of
            [a, b, c, d] -> (a, b, c, d)
            _ ->
                error
                    "stringToHostAddress: Parse failed trying to make a HostAddress."
    remoteIp = tupleToHostAddress temp2

-- converts a given port number and a host address to a sock address
convToSockAddr :: PortNumber -> HostAddress -> SockAddr
convToSockAddr = SockAddrInet

-- covnerts a string of format IP:Port to (PublicKey,SockAddr)
convertToSockAddr :: String -> (PublicKey, SockAddr)
convertToSockAddr x = (nodeId, fSockAddr)
  where
    addrString = S.splitOn ":" x
    remotePort = read $ addrString !! 2 :: M.PortNumber
    remoteIp = stringToHostAddress (addrString !! 1)
    nodeId = hexToPublicKey (C.pack (head addrString))
    fSockAddr = SockAddrInet remotePort remoteIp

getSockAddr :: HostAddress -> PortNumber -> SockAddr
getSockAddr ip udpPort = SockAddrInet udpPort ip

sockAddrToHostAddr :: SockAddr -> HostAddress
sockAddrToHostAddr (SockAddrInet _ b) = b
sockAddrToHostAddr _ =
    error "sockAddrToHostAddr: SockAddr is not of constructor SockAddrInet "

sockAddrToPortNumber :: SockAddr -> PortNumber
sockAddrToPortNumber (SockAddrInet a _) = a
sockAddrToPortNumber _ =
    error "sockAddrToPortNumber: SockAddr is not of constructor SockAddrInet "

-- Helper function to check if a values exist in a list of type [(a,_)]
isNodeIdElem :: Eq t => [(t, b)] -> t -> Bool
isNodeIdElem [] _ = False
isNodeIdElem (x:xs) m
    | fst x == m = True
    | otherwise = isNodeIdElem xs m

randomList :: Int -> IO [Int]
randomList 0 = return []
randomList n = do
    r <- randomRIO (1, 255)
    rs <- randomList (n - 1)
    return (r : rs)

-- Generates a random number of type Word32
getRandomSequence :: IO Word32
getRandomSequence = randomIO

getRandomSequence2 :: IO Int32
getRandomSequence2 = randomIO

count' :: Eq a => a -> [a] -> Int
count' x = length . filter (x ==)
