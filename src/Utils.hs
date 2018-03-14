module Utils 
(
    extractFirst,
    extractSecond,
    extractThird,
    stringToHostAddress,
    convertToSockAddr 
) where 

import qualified Data.List.Split           as S  
import           Network.Socket    
import qualified Network.Socket.Internal   as M 
import           Data.Word 
                             

-- Helper functions to extract value from 3-tuple
extractFirst :: (a, b, c) -> a
extractFirst (a,_,_) = a

extractSecond :: (a, b, c) -> b
extractSecond (_,b,_) = b

extractThird :: (a, b, c) -> c
extractThird (_,_,c) = c

stringToHostAddress :: [Char] -> HostAddress
stringToHostAddress x = remoteIp
    where temp     = S.splitOn "." x   
          temp2    = case (Prelude.map (read :: String -> Word8) temp) of [a,b,c,d] -> (a,b,c,d)  
          remoteIp = tupleToHostAddress temp2   

-- covnerts a string of format IP:Port to SockAddr  
convertToSockAddr :: [Char] -> SockAddr 
convertToSockAddr x  = fSockAddr
    where addrString = S.splitOn ":" x
          remotePort = read $ addrString !! 1 :: M.PortNumber 
          remoteIp   = stringToHostAddress (addrString !! 0)
          fSockAddr  = SockAddrInet remotePort remoteIp


