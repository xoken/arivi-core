module Node
  ( runUDPServerForever
  ) where

import           Control.Concurrent        (forkIO, newChan, newEmptyMVar,
                                            putMVar, readChan, takeMVar,
                                            threadDelay, writeChan)
import           Control.Monad             (forever)
import qualified Data.ByteString.Char8     as C
import           Network.Socket            hiding (recv)
-- import qualified Network.Socket.ByteString (recv, sendAll)
import qualified Network.Socket.ByteString as N (recv, recvFrom, sendAll,
                                                 sendAllTo, sendTo)
--import Data.List.Split
import           Control.Concurrent.Async
import           System.Environment
import           System.Random             (randomRIO)

workerCount = 5

-- Helper functions to extract value from 3-tuple
extractFirst :: (a, b, c) -> a
extractFirst (a,_,_) = a

extractSecond :: (a, b, c) -> b
extractSecond (_,b,_) = b

extractThird :: (a, b, c) -> c
extractThird (_,_,c) = c

messageHandler inboundChan outboundChan workerId = forkIO $ forever $ do
    msg <- readChan inboundChan
    case msg of
        ("PING",addr,sock) -> writeChan outboundChan ("PONG",addr,sock)
        ("FIND_NODE",addr,sock) -> writeChan outboundChan ("Uh Ah..",addr,sock)

networkClient outboundChan workerId = forkIO $ forever $ do
    msg <- readChan outboundChan
    print $ show (extractThird msg)
    -- responding back to remote node using local UDP listen port
    do N.sendTo (extractThird msg) (C.pack $ extractFirst msg) (extractSecond msg)

-- This create a UDP server which is constantly listenting for requests
runUDPServerForever :: String -> String -> IO ()
runUDPServerForever local_ip local_port  = do
    addrinfos <- getAddrInfo Nothing (Just local_ip) (Just local_port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bind sock (addrAddress serveraddr)
    print "Server now listening for requests"

    inboundChan <- newChan
    outboundChan <- newChan

    mapM_ (messageHandler inboundChan outboundChan) [1..workerCount]
    mapM_ (networkClient outboundChan) [1..workerCount]

    forever $
         do
            (mesg, socaddr2) <- N.recvFrom sock 4096
            print (mesg,socaddr2)
            writeChan inboundChan ((C.unpack mesg),socaddr2,sock)

-- Create a connection to the server and returns socket
createSocket server_ip server_port = do
    addrinfos <- getAddrInfo Nothing (Just server_ip) (Just server_port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    connect sock (addrAddress serveraddr)
    return (sock)

 -- Connects to server and sends it a message
sendMessage :: String -> Socket -> IO ()
sendMessage s sock = do
    N.sendAll sock $ C.pack s
    (mesg, socaddr2) <- N.recvFrom sock 4096
    print (mesg,socaddr2)

-- This calls sendMessage to send multiple messages to server
run 0 _  = return ()
run n client_soc = do
    sendMessage "PING" client_soc
    run (n-1) client_soc

main :: IO ()
main = do
        args <- getArgs
        done <- newEmptyMVar
        -- Runs these functions on different threads
        forkIO $ runUDPServerForever (args !! 0) (args !! 1) >> putMVar done ()
        threadDelay 100000 -- wait one second
        takeMVar done
