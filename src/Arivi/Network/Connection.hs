module Arivi.Network.Connection
(
    ConnectionState (..),
    IDLE_STATE (..),
    INVITED_STATE (..),
    KEY_EXCHNAGED_STATE (..),
    SERVICES_QUERIED_STATE (..),
    SERVICES_NEGOTIATED_STATE (..),
    TERMINATED_STATE (..)
) where

import           Arivi.Network.Datagram
import qualified Arivi.Network.Multiplexer as MP
import           Arivi.Network.Stream
import           Arivi.Network.Types       as T
import qualified Data.Map.Strict           as Map


type FrameHeader = String

class ConnectionState s where
    doHandShakeInitNE     :: s -> FrameHeader -> IO ()
    doHandShakeAckNE      :: s -> FrameHeader -> IO ()
    doInitiateHandShake   :: s -> FrameHeader -> IO ()
    doTerminateConnection :: s -> FrameHeader -> IO ()
    doOfferNE             :: s -> FrameHeader -> IO ()
    doCloseNE             :: s -> FrameHeader -> IO ()
    doErrorNE             :: s -> FrameHeader -> IO ()
    doPingNE              :: s -> FrameHeader -> IO ()
    doPongNE              :: s -> FrameHeader -> IO ()
    doSendMessage         :: s -> T.PayLoad     -> IO ()

data IDLE_STATE = IDLE
                     deriving (Show,Eq)

data INVITED_STATE = INVITED
                     deriving (Show,Eq)

data KEY_EXCHNAGED_STATE = KEY_EXCHNAGED
                     deriving (Show,Eq)

data SERVICES_QUERIED_STATE = SERVICES_QUERIED
                     deriving (Show,Eq)

data SERVICES_NEGOTIATED_STATE = SERVICES_NEGOTIATED
                     deriving (Show,Eq)

data TERMINATED_STATE = TERMINATED
                     deriving (Show,Eq)



