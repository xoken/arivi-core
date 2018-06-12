module Arivi.Network.StreamClient (
    createSocket,
    sendFrame,
    createFrame
) where

import           Arivi.Network.Types       (Parcel (..), TransportType (..))
import qualified Codec.Serialise           as Ser
import           Data.Binary
import qualified Data.ByteString.Lazy      as BSL
import           Data.Int                  (Int16)
import           Debug.Trace
import           Network.Socket
import qualified Network.Socket.ByteString as N (sendAll)

getAddressType :: TransportType -> SocketType
getAddressType TCP = Stream
getAddressType UDP = Datagram

-- | Eg: createSocket "127.0.0.1" 3000 TCP
createSocket :: HostName -> PortNumber -> PortNumber -> TransportType -> IO Socket
createSocket ipAdd port selfPort transportType = withSocketsDo $ do
    let portNo = Just (show port)
    let transport_type = getAddressType transportType

    if transportType == TCP
        then do
            let hints = defaultHints {addrSocketType = transport_type}
            addr:_ <- getAddrInfo (Just hints) (Just ipAdd) portNo
            sock <- socket AF_INET transport_type (addrProtocol addr)
            connect sock (addrAddress addr)
            return sock
        else
            do
            let hint = defaultHints {addrFlags = [AI_PASSIVE],
                                 addrSocketType = transport_type}
            peerAddr:_ <- getAddrInfo (Just hint) (Just ipAdd) portNo
            selfAddr:_ <- getAddrInfo (Just hint) Nothing (Just (show selfPort))

            selfSocket <- socket (addrFamily selfAddr) (addrSocketType selfAddr)
                                            (addrProtocol selfAddr)
            bind selfSocket (addrAddress selfAddr)

            connect selfSocket (addrAddress peerAddr)
            return selfSocket

sendFrame :: Socket -> BSL.ByteString -> IO ()
sendFrame sock msg =
    -- let (_, parcelSer) = BSL.splitAt 2 msg
    -- traceShow (Ser.deserialise parcelSer :: Parcel) return()
    -- traceShow "before sending " (return())
    -- traceShow "##################################################" (return())
    -- traceShow msg (return())
    -- traceShow "##################################################" (return())
    N.sendAll sock (BSL.toStrict msg)

-- | prefixes length to cborg serialised parcel
createFrame :: BSL.ByteString -> TransportType -> BSL.ByteString
createFrame parcelSerialised transportType =
        case transportType of
            TCP ->  BSL.concat [lenSer, parcelSerialised]
                    where
                      len = BSL.length parcelSerialised
                      lenw16 = fromIntegral len :: Int16
                      lenSer = encode lenw16
            UDP -> BSL.concat [parcelSerialised]
