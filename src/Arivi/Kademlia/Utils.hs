module Arivi.Kademlia.Utils
(
    extractFirst,
    extractSecond,
    extractThird,
    stringToHostAddress,
    convertToSockAddr,
    sockAddrToHostAddr,
    sockAddrToPortNumber,
    extractFirst2,
    extractSecond2,
    extractThird2,
    extractFourth,
    isNodeIdElem,
    getSockAddr
) where

import           Arivi.Kademlia.Signature
import           Data.ByteArray
import qualified Data.ByteString.Char8    as C
import qualified Data.List.Split          as S
import           Data.Word
import           Network.Socket
import qualified Network.Socket.Internal  as M

-- Helper functions to extract value from 3-tuple
extractFirst :: (a, b, c) -> a
extractFirst (a,_,_) = a

extractSecond :: (a, b, c) -> b
extractSecond (_,b,_) = b

extractThird :: (a, b, c) -> c
extractThird (_,_,c) = c


extractFirst2 :: (a,b,c,d) -> a
extractFirst2 (a,_,_,_) = a

extractSecond2 :: (a,b,c,d) -> b
extractSecond2 (_,b,_,_) = b

extractThird2 :: (a,b,c,d) -> c
extractThird2 (_,_,c,_) = c

extractFourth :: (a,b,c,d) -> d
extractFourth (_,_,_,d) = d


stringToHostAddress :: String -> HostAddress
stringToHostAddress x = remoteIp
    where temp     = S.splitOn "." x
          temp2    = case Prelude.map (read :: String -> Word8) temp
                        of [a,b,c,d] -> (a,b,c,d)
          remoteIp = tupleToHostAddress temp2

-- covnerts a string of format IP:Port to SockAddr
convertToSockAddr :: String -> (PublicKey,SockAddr)
convertToSockAddr x  = (nodeId,fSockAddr)
    where addrString = S.splitOn ":" x
          remotePort = read $ addrString !! 2 :: M.PortNumber
          remoteIp   = stringToHostAddress (addrString !! 1)
          nodeId     = hexToPublicKey (C.pack (head addrString))
          fSockAddr  = SockAddrInet remotePort remoteIp

getSockAddr ip udpPort = SockAddrInet udpPort ip

sockAddrToHostAddr :: SockAddr -> HostAddress
sockAddrToHostAddr (SockAddrInet a b) = b

sockAddrToPortNumber :: SockAddr -> PortNumber
sockAddrToPortNumber (SockAddrInet a b) = a

-- Helper function to check if a values exist in a list of type [(a,_)]
isNodeIdElem [] _      = False
isNodeIdElem (x:xs) m
    | fst x == m     = True
    | otherwise        = isNodeIdElem xs m
