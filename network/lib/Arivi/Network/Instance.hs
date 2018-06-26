{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Arivi.Network.Instance
    ( openConnection
    ) where

import           Arivi.Env
import           Arivi.Logging
import           Arivi.Network.Connection             as Conn
import           Arivi.Network.ConnectionHandler
import           Arivi.Network.Handshake
import           Arivi.Network.StreamClient
import           Arivi.Network.Types                  as ANT ( ConnectionHandle (..)
                                                             , NodeId
                                                             , PersonalityType(..)
                                                             , TransportType (..))
import           Arivi.Utils.Exception
import           Control.Exception                    (try)

import           Control.Monad.Reader
import           Crypto.PubKey.Ed25519                (SecretKey)
import           Data.HashMap.Strict                  as HM
import           Data.IORef
import           Network.Socket


doEncryptedHandshake ::
       Conn.IncompleteConnection -> SecretKey -> IO Conn.CompleteConnection
doEncryptedHandshake connection sk = do
    (ephemeralKeyPair, serialisedParcel) <- initiatorHandshake sk connection
    sendFrame
        (Conn.waitWrite connection)
        (Conn.socket connection)
        (createFrame serialisedParcel)
    hsRespParcel <-
        readHandshakeRespSock
            (Conn.waitWrite connection)
            (Conn.socket connection)
    return $ receiveHandshakeResponse connection ephemeralKeyPair hsRespParcel

openConnection ::
       forall m. (HasLogging m, HasSecretKey m)
    => HostName
    -> PortNumber
    -> TransportType
    -> NodeId
    -> m (Either AriviException ConnectionHandle)
openConnection host portNum tt rnId = do
    let cId = makeConnectionId host portNum tt
    sock <- liftIO $ createSocket host (read (show portNum)) tt
    conn <-
        liftIO $
        mkIncompleteConnection cId rnId host portNum tt INITIATOR sock 2
    case tt of
        TCP -> openTCPConnection conn
        UDP -> undefined

openTCPConnection ::
       forall m. (MonadIO m, HasLogging m, HasSecretKey m)
    => Conn.IncompleteConnection
    -> m (Either AriviException ConnectionHandle)
openTCPConnection conn = do
    sk <- getSecretKey
    res <- liftIO $ try $ doEncryptedHandshake conn sk
    fragmentsHM <- liftIO $ newIORef HM.empty
    case res of
        Left e -> return $ Left e
        Right updatedConn ->
            return . Right $
            ConnectionHandle
            { ANT.send = sendMessage updatedConn
            , ANT.recv = readSock updatedConn fragmentsHM
            , ANT.close = closeConnection (Conn.socket updatedConn)
            }


-- openUDPConnection ::
--        ( MonadIO m
--        , HasLogging m
--        , HasSecretKey m
--        )
--     => Conn.IncompleteConnection
--     -> m (Either AriviException ANT.ConnectionId)
-- openUDPConnection conn = do
--   traceShow "before Handshake" (return ())
--   res <- doEncryptedHandshakeForUDP connection pType
--   traceShow "after Handshake" (return ())
--   case res of
--       Left e -> return $ Left e
--       Right updatedConn -> return $ Right cId

-- lookupCId ::
--        (MonadIO m, HasAriviNetworkInstance m)
--     => ANT.ConnectionId
--     -> m (Maybe CompleteConnection)
-- lookupCId cId = do
--     ariviInstance' <- getAriviNetworkInstance
--     let tv = connectionMap ariviInstance'
--     hmap <- liftIO $ readTVarIO tv
--     return $ HM.lookup cId hmap
