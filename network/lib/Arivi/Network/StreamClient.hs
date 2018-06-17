module Arivi.Network.StreamClient (
    createSocket,
    sendFrame,
    createFrame
) where

import           Arivi.Network.Types            (TransportType (..))
import           Control.Concurrent.MVar
import           Data.Binary
import qualified Data.ByteString.Lazy           as BSL
import           Data.Int                       (Int16)
import           Network.Socket
import qualified Network.Socket.ByteString.Lazy as N (sendAll)

getAddressType :: TransportType -> SocketType
getAddressType TCP = Stream
getAddressType UDP = Datagram

-- | Eg: createSocket "127.0.0.1" 3000 TCP
createSocket :: String -> Int -> TransportType -> IO Socket
createSocket ipAdd port transportType = withSocketsDo $ do
    let portNo = Just (show port)
    let transport_type = getAddressType transportType
    let hints = defaultHints {addrSocketType = transport_type}
    addr:_ <- getAddrInfo (Just hints) (Just ipAdd) portNo
    sock <- socket AF_INET transport_type (addrProtocol addr)
    connect sock (addrAddress addr)
    return sock

sendFrame :: MVar Int -> Socket -> BSL.ByteString -> IO ()
sendFrame writeLock sock msg = do
    -- let (_, parcelSer) = BSL.splitAt 2 msg
    -- traceShow (Ser.deserialise parcelSer :: Parcel) return()
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
