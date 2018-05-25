{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module      :  Arivi.P2P.Instance
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- PubSub is module for Publish Subscribe architecture of Arivi P2P Layer
--

module Arivi.P2P.Instance
(
    openConnection
  , sendMessage
) where

import Arivi.Crypto.Utils.PublicKey.Signature
import Arivi.Network.Connection
import Arivi.Network.FSM
import Arivi.Network.Instance
import Arivi.Network.StreamClient
import Arivi.Network.Types as ANT
import Arivi.P2P.Types
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Data.Functor.Identity
import Data.ByteString.Lazy
import Data.HashMap.Lazy as HM
import GHC.Generics
import Network.Socket
import Arivi.Env

openConnection :: (HasAriviNetworkInstance m) => HostAddress -> PortNumber -> TransportType -> NodeId -> PersonalityType -> m ANT.ConnectionId
openConnection addr port tt rnid pType = do
  ariviInstance <- getAriviNetworkInstance
  tv <- liftIO $ atomically $ connectionMap ariviInstance
  eventChan <- liftIO $ (newTChanIO :: IO (TChan Event))
  socket <- liftIO $ createSocket (show addr) (read (show port)) tt
  outboundChan <- liftIO $ (newTChanIO :: IO (TChan OutboundFragment))
  reassemblyChan <- liftIO $ (newTChanIO :: IO (TChan Parcel))
  let cId = makeConnectionId addr port tt
      connection = Connection {connectionId = cId, remoteNodeId = rnid, ipAddress = addr, port = port, transportType = tt, personalityType = pType, socket = socket, eventTChan = eventChan, outboundFragmentTChan = outboundChan, reassemblyTChan = reassemblyChan}
  liftIO $ atomically $  modifyTVar tv (HM.insert cId connection)
  liftIO $ withAsync (initFSM connection) (\_ -> atomically $ modifyTVar tv (HM.delete cId))
  return cId

sendMessage :: (HasAriviNetworkInstance m) => ANT.ConnectionId -> ByteString -> m ()
sendMessage cId msg = do
  ariviInstance <- getAriviNetworkInstance
  tv <- liftIO $ atomically $ connectionMap ariviInstance
  hmap <- liftIO $ readTVarIO tv
  let conn = case (HM.lookup cId hmap) of
        Just c -> c
        Nothing -> error "Something terrible happened! You have been warned not to enter the forbidden lands"
  liftIO $ atomically $ writeTChan (eventTChan conn) (SendDataEvent (Payload msg))

closeConnection :: (HasAriviNetworkInstance m) => ANT.ConnectionId -> m ()
closeConnection cId = do
  ariviInstance <- getAriviNetworkInstance
  tv <- liftIO $ atomically $ connectionMap ariviInstance
  hmap <- liftIO $ readTVarIO tv
  let conn = case (HM.lookup cId hmap) of
        Just c -> c
        Nothing -> error "Something terrible happened! You have been warned not to enter the forbidden lands"
  liftIO $ atomically $ modifyTVar tv (HM.delete cId)
