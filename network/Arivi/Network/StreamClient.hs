module Arivi.Network.StreamClient (
    createSocket,
    sendFrame,
    createFrame,
    createSocketUDP
) where

import           Arivi.Network.Types       (TransportType (..))
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
sendFrame sock msg = do
                socketName <- getSocketName sock
                mIpAddress <- inet_ntoa $ getIPAddress socketName
                print ("sendFrame " ++ mIpAddress)
                N.sendAll sock (BSL.toStrict msg)

-- | prefixes length to cborg serialised parcel
createFrame :: BSL.ByteString -> BSL.ByteString
createFrame parcelSerialised =  parcelSerialised -- BSL.concat [lenSer, parcelSerialised]
                    where
                      len = BSL.length parcelSerialised
                      lenw16 = fromIntegral len :: Int16
                      lenSer = encode lenw16

createSocketUDP :: String -> Int -> TransportType -> IO Socket
createSocketUDP ipAddress portNumber socketType = do
        let hint = defaultHints {addrFlags = [AI_PASSIVE],
                                 addrSocketType = Datagram}
        addr:_ <- getAddrInfo (Just hint) (Just ipAddress)
            (Just (show portNumber))

        addr2:_ <- getAddrInfo (Just hint) (Just ipAddress)
            (Just "4509")
        mSocket <- socket (addrFamily addr2) (addrSocketType addr2)
                                            (addrProtocol addr2)
        bind mSocket (addrAddress addr2)

        connect mSocket (addrAddress addr)
        return mSocket


getIPAddress :: SockAddr -> HostAddress
getIPAddress (SockAddrInet _ hostAddress) = hostAddress
getIPAddress _                            = error "getIPAddress: SockAddr is not of constructor SockAddrInet "
