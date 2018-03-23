
import           Network.Arivi.UdpServer 
import           Network.Socket 
import           System.Environment
import qualified Network.Socket.ByteString     as N    (recvFrom,sendTo) 
import qualified Data.ByteString.Char8         as C 
import qualified Data.List.Split               as S  
import qualified Network.Socket.Internal       as M 
import           Data.Word 


stringToHostAddress :: [Char] -> HostAddress
stringToHostAddress x = remoteIp
    where temp     = S.splitOn "." x   
          temp2    = case (Prelude.map (read :: String -> Word8) temp) of [a,b,c,d] -> (a,b,c,d)  
          remoteIp = tupleToHostAddress temp2  

convertToSockAddr :: [Char] -> SockAddr
convertToSockAddr x  = fSockAddr
    where addrString = S.splitOn ":" x
          remotePort = read $ addrString !! 1 :: M.PortNumber 
          remoteIp   = stringToHostAddress (addrString !! 0)
          fSockAddr  = SockAddrInet remotePort remoteIp

main = do 
    args <- getArgs 
    let port       = head args 
        ip         = "127.0.0.1"
        sa         = "127.0.0.1:8000"
        tosockAddr = convertToSockAddr sa 
    
    addrinfos <- getAddrInfo Nothing (Just ip) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bind sock (addrAddress serveraddr) 
    
    bs <- N.sendTo (sock) (C.pack (args !! 1)) (tosockAddr) 

    print ("Bytes Sent : " ++ (show bs) )