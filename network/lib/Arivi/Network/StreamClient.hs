module Arivi.Network.StreamClient
    ( createSocket
    , sendFrame
    , createFrame
    ) where

import           Arivi.Network.Types            (TransportType (..))

import           Control.Concurrent.MVar
import           Control.Monad                  (when)
import           Data.Binary
import qualified Data.ByteString.Lazy           as BSL
import           Data.Int                       (Int16)
import           Network.Socket
import qualified Network.Socket.ByteString.Lazy as N (sendAll)


-- | Eg: createSocket "127.0.0.1" 3000 TCP
createSocket :: String -> Int -> TransportType -> IO Socket
createSocket = createSocketWithOptions []

createSocketWithOptions ::
       [SocketOption] -> String -> Int -> TransportType -> IO Socket
createSocketWithOptions options ip port tt =
    withSocketsDo $ do
        let portNo = Just (show port)
        let transport_type = getAddressType tt
        let hints = defaultHints {addrSocketType = transport_type}
        addr:_ <- getAddrInfo (Just hints) (Just ip) portNo
        sock <- socket AF_INET transport_type (addrProtocol addr)
        mapM_
            (\option ->
                 when
                     (isSupportedSocketOption option)
                     (setSocketOption sock option 1))
            options
        connect sock (addrAddress addr)
        return sock
  where
    getAddressType TCP = Stream
    getAddressType UDP = Datagram

sendFrame :: MVar Int -> Socket -> BSL.ByteString -> IO ()
sendFrame writeLock sock msg = do
    a <- takeMVar writeLock
    N.sendAll sock msg
    putMVar writeLock a

-- | prefixes length to cborg serialised parcel
createFrame :: BSL.ByteString -> BSL.ByteString
createFrame parcelSerialised = BSL.concat [lenSer, parcelSerialised]
  where
    len = BSL.length parcelSerialised
    lenw16 = fromIntegral len :: Int16
    lenSer = encode lenw16
