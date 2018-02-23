module Node
  ( runUDPServerForever,
    messageHandler,
    loadDefaultPeers,
    networkClient,
    addToKbChan,
    convertToSockAddr,
  ) where

import           Control.Concurrent        (forkIO, newChan, newEmptyMVar,
                                            putMVar, readChan, takeMVar,
                                            threadDelay, writeChan,isEmptyChan,Chan,ThreadId)
import           Control.Monad             (forever)
import qualified Data.ByteString.Char8     as C
import           Network.Socket            hiding (recv)
import qualified Network.Socket.ByteString as N (recv, recvFrom, sendAll,
                                                 sendAllTo, sendTo)
import qualified Network.Socket.Internal   as M 
import           Control.Concurrent.Async
import           System.Environment
import           System.Random             (randomRIO)
import qualified Data.Map.Strict           as Map 
import           Data.Maybe 
import           System.Random
import qualified Data.List.Split           as S  
import           Data.Word 
      
-- Helper functions to extract value from 3-tuple
extractFirst :: (a, b, c) -> a
extractFirst (a,_,_) = a

extractSecond :: (a, b, c) -> b
extractSecond (_,b,_) = b

extractThird :: (a, b, c) -> c
extractThird (_,_,c) = c

-- Custom data type to send & recieve message 
data Message = PING
               |PONG
               |FIND_NODE { 
                   nodeId     :: [Char]
                ,  find       :: [Char]
                ,  address    :: [Char] 
                ,  seqNo      :: [Char]
                ,  signature  :: [Char] 
                }
               |FN_RESP {
                    nodeId    :: [Char]
                ,   knodes    :: [(String,String)] 
                ,   address   :: [Char] 
                ,   seqNo     :: [Char]
                ,   signature :: [Char] 
                }            
               deriving (Show)

-- Process all the incoming messages to server and write the response to outboundChan 
-- whenever a findNode message is recieved it write that peer to peerChan  
messageHandler :: Chan (String,SockAddr,Socket) 
               -> Chan (String,SockAddr,Socket) 
               -> Chan (SockAddr,Int)           
               -> Int                           
               -> IO ThreadId                   

messageHandler inboundChan outboundChan peerChan workerId = forkIO $ forever $ do
    msg <- readChan inboundChan

    case msg of
        ("PING",addr,sock) -> writeChan outboundChan ("PONG",addr,sock)
        
        ("FIND_NODE",addr,sock) -> do 
            -- add the asking node to corresponding k-bucket 
            -- Just for testing 
            rnd <- randomRIO (1::Int,10::Int)
            writeChan peerChan (extractSecond msg,rnd)
            -- clientResponse <- findKNodes sock 
            writeChan outboundChan ("FN_RESP",addr,sock)
        
        ("FN_RESP",addr,sock) -> do 
            print "Response recieved"
            -- We have to issue subsequent FIND_NODE to each node recieved here 

-- Sends the message written by outboundChan to remote Client 
networkClient :: Chan (String,SockAddr,Socket) 
              -> Int 
              -> IO ThreadId

networkClient outboundChan workerId = forkIO $ forever $ do 
    msg <- readChan outboundChan
    N.sendTo (extractThird msg) (C.pack $ extractFirst msg) (extractSecond msg)          

-- Runs on a seperate thread & and is responsible for writing to kbChan   
addToKbChan :: Chan (Map.Map Int [SockAddr] ) 
            -> Chan (SockAddr,Int) 
            -> Int 
            -> IO ThreadId 

addToKbChan kbChan peerChan workerId = forkIO $ forever $ do
    msg <- readChan peerChan 
    rl <- isEmptyChan kbChan 
    case rl of 
        True -> do 
            let temp = Map.empty 
                temp2 = Map.insert (snd msg) (fst msg : []) temp  
            writeChan kbChan temp2
            print temp2
        False -> do 
                kb  <- readChan kbChan
                if (Map.lookup (snd msg) kb == Nothing)
                    then do
                        let temp = Map.insert (snd msg) (fst msg:[]) kb 
                        writeChan kbChan temp 
                        print temp
                    else do 
                        let temp    = Map.lookup (snd msg) kb 
                            temp2   = fromMaybe ((fst msg):[]) temp 
                            temp3   = (fst msg) : temp2
                            payLoad = Map.insert (snd msg) (temp3) kb
                        print payLoad     
                        writeChan kbChan payLoad
                                                 
-- UDP server which is constantly listenting for requests
runUDPServerForever :: String 
                    -> String 
                    -> Chan(String,SockAddr,Socket) 
                    -> Chan (Socket) 
                    -> IO ()

runUDPServerForever local_ip local_port inboundChan servChan = do
    addrinfos <- getAddrInfo Nothing (Just local_ip) (Just local_port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bind sock (addrAddress serveraddr)

    writeChan servChan sock 

    print "Server now listening for requests"
    forever $
         do
            (mesg, socaddr2) <- N.recvFrom sock 4096
            print (mesg,socaddr2)
            writeChan inboundChan ((C.unpack mesg),socaddr2,sock)

-- covnerts a string of format IP:Port to SockAddr  
convertToSockAddr :: [Char] -> SockAddr 
convertToSockAddr x  = fSockAddr
    where addrString = S.splitOn ":" x
          remotePort = read $ addrString !! 1 :: M.PortNumber 
          temp       = S.splitOn "." (addrString !! 0)  
          temp2      = case (Prelude.map (read :: String -> Word8) temp) of [a,b,c,d] -> (a,b,c,d)
          remoteIp   = tupleToHostAddress temp2
          fSockAddr  = SockAddrInet remotePort remoteIp

-- Load Default peers into kbChan i.e K-buckets 
loadDefaultPeers :: [SockAddr] 
                 -> Chan (String,SockAddr,Socket) 
                 -> Chan (SockAddr,Int) 
                 -> Chan (Socket) 
                 -> IO ()

loadDefaultPeers peerList outboundChan peerChan servChan = do 
    sock <- readChan servChan 
    mapM_ (writeChan peerChan) (zip peerList (replicate (length peerList) 1))
    mapM_ (writeChan outboundChan) (zip3 (repl "FIND_NODE") peerList (repl (sock)))
    where repl = replicate (length peerList) 