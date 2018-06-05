-- {-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Arivi.Network.Datagram
(
    createUDPSocket
  -- , runUDPServerForever
) where

import           Arivi.Env
import           Arivi.Logging
import           Control.Concurrent          (ThreadId, forkIO, newEmptyMVar,
                                              putMVar, takeMVar)
import           Control.Monad               (forever)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Network.Socket
import           Network.Socket.ByteString   (recvFrom, sendAll)
import qualified Network.Socket.ByteString   as N (recvFrom)


-- runUDPServerForever :: Socket
--                     -> SockAddr
--                     -> IO ()

-- runUDPServerForever sock sockAddr  = do

--     bind sock sockAddr
--     print ("UDP Server now listening for requests at : " ++ show sockAddr)
--     forever $
--                 do
--             (mesg, socaddr2) <- N.recvFrom sock 4096
--             print ""

createUDPSocket :: Show portNumber => HostName -> portNumber -> IO Socket
createUDPSocket ipAddress portNumber = do
    let hint = defaultHints {addrFlags = [AI_PASSIVE],
                             addrSocketType = Datagram}

    selfAddr:_  <- getAddrInfo (Just hint) (Just ipAddress) (Just (show portNumber))
    mSocket <- socket (addrFamily selfAddr) (addrSocketType selfAddr)
                                        (addrProtocol selfAddr)
    bind mSocket (addrAddress selfAddr)
    return mSocket

-- | Lifts the `withSocketDo` to a `MonadBaseControl IO m`
liftWithSocketsDo :: (MonadBaseControl IO m) => m a -> m a
liftWithSocketsDo f = control $ \runInIO -> withSocketsDo (runInIO f)

-- runUDPServer :: (HasAriviNetworkInstance m,HasSecretKey m,HasLogging m)
--              => ServiceName
--              -> m ()
-- runUDPServer port = $(withLoggingTH)  (LogNetworkStatement "UDP server started") LevelInfo $
--    liftWithSocketsDo $ do
--       let hints = defaultHints {  addrFlags = [AI_PASSIVE]
--                                  ,addrSocketType = Datagram}
--       addr:_ <- liftIO $ getAddrInfo (Just hints) Nothing (Just port)
--       return ()






makeSocket :: HostName -> PortNumber -> SocketType -> IO Socket
makeSocket ipAddress portNumber socketType = do
        let hint = defaultHints {addrFlags = [AI_PASSIVE],
                                 addrSocketType = socketType}

        addr:_ <- getAddrInfo (Just hint) (Just ipAddress)
                                        (Just (show portNumber))

        mSocket <- socket (addrFamily addr) (addrSocketType addr)
                                            (addrProtocol addr)
        bind mSocket (addrAddress addr)
        return mSocket


runUDPServerForever sock = forever
    (Network.Socket.ByteString.recvFrom sock 4096 >>= print)

runUDPClientForever peerIpAddress peerPortNumber peerSocketType =   do
        let hint = defaultHints {addrFlags = [AI_PASSIVE],
                                 addrSocketType = peerSocketType}
        addr:_ <- getAddrInfo (Just hint) (Just peerIpAddress)
            (Just (show peerPortNumber))

        addr2:_ <- getAddrInfo (Just hint) (Just peerIpAddress)
            (Just "4500")
        mSocket <- socket (addrFamily addr2) (addrSocketType addr2)
                                            (addrProtocol addr2)
        bind mSocket (addrAddress addr2)

        connect mSocket (addrAddress addr)
        forever (Network.Socket.ByteString.sendAll mSocket "4096")
