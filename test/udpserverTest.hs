{-# LANGUAGE RankNTypes #-}

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString.Char8   as C
import           Eve
import           Network.Arivi.UdpServer


msgHandler :: Payload -> App ()
msgHandler (Payload msg) = liftIO $ do
    print "Now i Have the message from udp server"
    print C.unpack msg ++ show "I can do wateva i want with it"

main = do
    let ip      = "127.0.0.1"
        udpPort = Just "8000"
        tcpPort = Nothing

    let ac = AriviConfig ip udpPort tcpPort
    temp <- runAriviInstance ac msgHandler

    print temp




