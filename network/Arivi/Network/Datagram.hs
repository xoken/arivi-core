module Arivi.Network.Datagram
(
    -- createUDPSocket
    createUDPFrame
  , getUDPConnectionId
  , makeSocket
  , readFromUDPSocketForever
  , readHandshakeRespFromTChan
  , runUDPServerForever
  , sendUDPFrame
) where

import           Arivi.Crypto.Cipher.ChaChaPoly1305 (getCipherTextAuthPair)
import           Arivi.Crypto.Utils.PublicKey.Utils (decryptMsg)
import           Arivi.Env                          (HasAriviNetworkInstance,
                                                     HasSecretKey,
                                                     ariviNetworkConnectionMap,
                                                     getAriviNetworkInstance,
                                                     getSecretKey)
import           Arivi.Logging                      (HasLogging)
import qualified Arivi.Network.Connection           as Conn (Connection (..), HandshakeStatus (..))
import           Arivi.Network.Handshake            (recipientHandshake)
import           Arivi.Network.Types                (AeadNonce, ConnectionId,
                                                     Header (..), Parcel (..),
                                                     Payload (..),
                                                     PersonalityType (..),
                                                     SequenceNum,
                                                     TransportType (..),
                                                     deserialiseOrFail,
                                                     serialise)
import qualified Arivi.Network.Utils                as Utils (getIPAddress,
                                                              getPortNumber)
import           Arivi.Utils.Exception              (AriviException (..))
import           Control.Concurrent.Async.Lifted    (async)
import           Control.Concurrent.STM             (atomically)
import           Control.Concurrent.STM.TChan       (TChan, newTChan, readTChan,
                                                     writeTChan)
import           Control.Concurrent.STM.TVar        (modifyTVar, newTVarIO,
                                                     readTVarIO, writeTVar)
import           Control.Exception                  (throw)
import           Control.Monad.IO.Class             (liftIO)
import           Crypto.PubKey.Ed25519              (SecretKey)
import qualified Data.ByteString.Char8              as Char8 (ByteString, pack)
import qualified Data.ByteString.Lazy               as Lazy (ByteString, concat,
                                                             fromStrict,
                                                             toStrict)
import           Data.HashMap.Strict                as StrictHashMap
import           Debug.Trace
import           Network.Socket
import qualified Network.Socket.ByteString          as Network (recvFrom,
                                                                sendTo)

-- runUDPServerForever :: Socket
--                     -> SockAddr
--                     -> IO ()

-- runUDPServerForever sock sockAddr  = do

--     bind sock sockAddr
--     print ("UDP Server now listening for requests at : " ++ show sockAddr)
--     forever $
--                 do
--             (mesg, socaddr2) <- Network.recvFrom sock 4096
--             print ""

-- createUDPSocket :: Show portNumber => HostName -> portNumber -> IO Socket
-- createUDPSocket ipAddress portNumber = do
--     let hint = defaultHints {addrFlags = [AI_PASSIVE],
--                              addrSocketType = Datagram}

--     selfAddr:_  <- getAddrInfo (Just hint) (Just ipAddress)
--                                             (Just (show portNumber))

--     mSocket <- socket (addrFamily selfAddr) (addrSocketType selfAddr)
--                                         (addrProtocol selfAddr)
--     bind mSocket (addrAddress selfAddr)
--     return mSocket


makeSocket :: HostName -> PortNumber -> SocketType -> IO Socket
makeSocket ipAddress portNumber socketType = do
        let hint = defaultHints {addrFlags = [AI_PASSIVE],
                                 addrSocketType = socketType}

        selfAddr:_ <- getAddrInfo (Just hint) (Just ipAddress)
                                        (Just (show portNumber))

        selfSocket <- socket (addrFamily selfAddr) (addrSocketType selfAddr)
                                            (addrProtocol selfAddr)
        bind selfSocket (addrAddress selfAddr)
        return selfSocket



-- runUDPServerForever sock = forever
--     $ do
--     (receivedMessage,peerSockAddr) <- Network.recvFrom sock 4096
--     let cid = sockAddrToConnectionId

handleUDPInboundConnection :: (HasAriviNetworkInstance m
                             , HasSecretKey m
                             , HasLogging m)
                           => Conn.Connection
                           -> m ()
handleUDPInboundConnection connection =   do
    handshakeStatus <- liftIO $ readTVarIO (Conn.handshakeComplete connection)

    parcel <- liftIO $ atomically $ readTChan (Conn.reassemblyTChan connection)
    traceShow parcel (return())

    if handshakeStatus /= Conn.HandshakeDone
        then do
          -- doHandshake
              sk <- getSecretKey
              ariviNetworkInstance <- getAriviNetworkInstance
              let hashMapTVar = ariviNetworkConnectionMap ariviNetworkInstance

              (serialisedParcel, updatedConn) <- liftIO $
                                                    recipientHandshake sk
                                                            connection parcel

              traceShow "before sendUDPFrame HandshakeDone" (return())
              _ <- liftIO $ sendUDPFrame (Conn.socket updatedConn)
                                        (Conn.remoteSockAddr updatedConn)
                                        (createUDPFrame serialisedParcel)
              traceShow "after sendUDPFrame HandshakeDone" (return())

              let cid = Conn.connectionId updatedConn


              liftIO $ atomically $ writeTVar
                                      (Conn.handshakeComplete updatedConn)
                                          Conn.HandshakeDone

              liftIO $ atomically $ modifyTVar hashMapTVar
                                                    (StrictHashMap.insert cid
                                                            updatedConn)
              handleUDPInboundConnection updatedConn
        else
            do
            let (cipherText,authenticationTag) = getCipherTextAuthPair
                                                (Lazy.toStrict
                                                  (getPayload
                                                    (encryptedPayload parcel)))
            let parcelHeader = Lazy.toStrict $ serialise (header parcel)
            let fragmentAead = aeadNonce (header parcel)
            let ssk = Conn.sharedSecret connection
            let payloadMessage =  Lazy.fromStrict $ decryptMsg fragmentAead
                                                            ssk parcelHeader
                                                            authenticationTag
                                                            cipherText
            _ <- liftIO $ atomically $ writeTChan (Conn.p2pMessageTChan connection)
                                                      payloadMessage
            handleUDPInboundConnection connection

runUDPServerForever :: (HasAriviNetworkInstance m
                        , HasSecretKey m
                        , HasLogging m)
                    => Socket
                    -> m ()
runUDPServerForever mSocket = do
    -- traceShow "inside runUDPServerForever" (return())
    (receivedMessage,peerSockAddr) <- liftIO $ Network.recvFrom mSocket 4096
    -- traceShow receivedMessage (return())
    -- traceShow "after recvFrom" (return())
    -- traceShow receivedMessage (return())
    ariviNetworkInstance <- getAriviNetworkInstance
    let hashMapTVar = ariviNetworkConnectionMap ariviNetworkInstance
    cid <- liftIO $ getUDPConnectionId peerSockAddr
    hm <- liftIO $ readTVarIO hashMapTVar

    eitherExeptionParcel <- deserialiseParcel receivedMessage
    case eitherExeptionParcel of
        Left deserialiseException -> throw deserialiseException
        Right parcel ->
            case StrictHashMap.lookup cid hm of
                Just conn -> do
                                -- cid <- liftIO $ getUDPConnectionId peerSockAddr
                                _ <- liftIO $ atomically $ writeTChan
                                                            (Conn.reassemblyTChan conn)
                                                            parcel
                                -- traceShow parcel (return())
                                runUDPServerForever mSocket
                Nothing ->  do
                            newConnection <- liftIO $ do
                                mIpAddress <- Utils.getIPAddress peerSockAddr
                                let mPort = Utils.getPortNumber peerSockAddr
                                let mTransportType = UDP
                                -- let mConnectionId = getUDPConnectionId peerSockAddr
                                egressNonce <- liftIO (newTVarIO (2 :: SequenceNum))
                                ingressNonce <- liftIO (newTVarIO (2 :: SequenceNum))
                                -- Need to change this to proper value
                                mAEADNonce <- liftIO (newTVarIO (2^63+1 :: AeadNonce))

                                mReassemblyTChan <- atomically newTChan
                                _ <- atomically $ writeTChan mReassemblyTChan
                                                              parcel

                                p2pMsgTChan <- atomically newTChan
                                hsCompleteTVar <- newTVarIO Conn.HandshakeNotStarted
                                let connection = Conn.Connection
                                               { Conn.connectionId = cid
                                               , Conn.ipAddress = mIpAddress
                                               , Conn.port = mPort
                                               , Conn.transportType = mTransportType
                                               , Conn.personalityType = RECIPIENT
                                               , Conn.socket = mSocket
                                               , Conn.reassemblyTChan = mReassemblyTChan
                                               , Conn.p2pMessageTChan = p2pMsgTChan
                                               , Conn.egressSeqNum = egressNonce
                                               , Conn.ingressSeqNum = ingressNonce
                                               , Conn.aeadNonceCounter = mAEADNonce
                                               , Conn.handshakeComplete = hsCompleteTVar
                                               , Conn.remoteSockAddr = peerSockAddr
                                               }
                                return connection

                            liftIO $ atomically $ modifyTVar hashMapTVar
                                        (StrictHashMap.insert cid newConnection)

                            _ <-  async (handleUDPInboundConnection
                                                            newConnection)
                            -- traceShow parcel (return())
                            runUDPServerForever mSocket



    -- if (isNewConnection)
    --     then

    --         new reassemblyTChan
    --         insert into reassemblyTChan
    --         connection = makeConnection
    --         insert into HashMap
    --         handleUDPInboundConnection
    --     else do
    --         cid <- getUDPConnectionId peerSockAddr

    --         reassemblyTChan = getConnection nobejet using peerSockAddr
    --         insert into reassemblyTChan


readFromUDPSocketForever updatedConn = do
    -- traceShow "inside readFromUDPSocketForever" (return())
    (receivedMessage,peerSockAddr) <- liftIO $ Network.recvFrom
                                                (Conn.socket updatedConn)
                                                4096
    eitherExeptionParcel <- deserialiseParcel receivedMessage
    case eitherExeptionParcel of
        Left deserialiseException -> throw deserialiseException
        Right parcel ->  liftIO $ atomically $ writeTChan (Conn.reassemblyTChan updatedConn)
                                                              parcel
    -- traceShow "after recvFrom readFromUDPSocketForever" (return())
    readFromUDPSocketForever updatedConn

getUDPConnectionId :: SockAddr -> IO ConnectionId
getUDPConnectionId peerSockAddr = do
    ipAddress <- Utils.getIPAddress peerSockAddr
    let portNumber = show (Utils.getPortNumber peerSockAddr)
    return $ Char8.pack $ ipAddress
                        ++ "|"
                        ++ portNumber
                        ++ "|"
                        ++ show UDP

-- checkIsNewConnection :: (HasAriviNetworkInstance m)
--                 => SockAddr
--                 -> m Bool
-- checkIsNewConnection peerSockAddr = do
--     ariviNetworkInstance <- getAriviNetworkInstance
--     let hashMapTVar = ariviNetworkConnectionMap ariviNetworkInstance
--     cid <- liftIO $ getUDPConnectionId peerSockAddr
--     hm <- liftIO $ readTVarIO hashMapTVar
--     case (StrictHashMap.lookup cid hm) of
--         Just _  -> return True
--         Nothing -> return False


deserialiseParcel :: (Monad m) =>
     Char8.ByteString -> m (Either AriviException Parcel)
deserialiseParcel parcelCipher =
     either
       (return . Left . AriviDeserialiseException) (return . Right)
       (deserialiseOrFail (Lazy.fromStrict parcelCipher))



sendUDPFrame :: Socket -> SockAddr -> Lazy.ByteString -> IO Int
sendUDPFrame mSocket peerSockAddr msg =
    Network.sendTo mSocket (Lazy.toStrict msg) peerSockAddr

createUDPFrame :: Lazy.ByteString -> Lazy.ByteString
createUDPFrame parcelSerialised  =  Lazy.concat [parcelSerialised]



readHandshakeRespFromTChan :: TChan Parcel-> SecretKey -> IO Parcel
readHandshakeRespFromTChan reassemblyTChan sk = do

  hsRespParcel <- atomically $ readTChan reassemblyTChan

  case hsRespParcel of
    parcel@(Parcel (HandshakeRespHeader _ _ ) _ ) -> return parcel
    _ -> throw AriviWrongParcelException
