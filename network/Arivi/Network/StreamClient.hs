{-# LANGUAGE OverloadedStrings #-}
{-#  #-}

module Arivi.Network.StreamClient (
    createSocket,
    sendFrame,
    createFrame
) where

import           Arivi.Network.Types       (TransportType (..))
import           Arivi.Logging
import           Data.Binary
import qualified Data.ByteString.Lazy      as BSL
import           Data.Int                  (Int16)
import           Network.Socket
import qualified Network.Socket.ByteString as N (sendAll)

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

sendFrame :: Socket -> BSL.ByteString -> IO ()
sendFrame sock msg = N.sendAll sock (BSL.toStrict msg)

-- | prefixes length to cborg serialised parcel
createFrame :: BSL.ByteString -> BSL.ByteString
createFrame parcelSerialised = BSL.concat [lenSer, parcelSerialised]
                    where
                      len = BSL.length parcelSerialised
                      lenw16 = fromIntegral len :: Int16
                      lenSer = encode lenw16
