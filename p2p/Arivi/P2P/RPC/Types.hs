{-# LANGUAGE DeriveGeneric #-}

module Arivi.P2P.RPC.Types
    ( ServiceId
    , ResourceHandlerList
    , ResourceToPeerMap
    , ResourceId
    , MessageTypeRPC(..)
    , NodeId
    , P2PUUID
    , ServicePayload(..)
    , P2Pinfo(..)
    , ServiceMessage
    , ResourceHandler
    , ResponseCode(..)
    ) where

import Arivi.P2P.MessageHandler.HandlerTypes
    ( ConnectionHandle
    , IP
    , NodeId
    , P2PMessage
    , Port
    )

-- import           Arivi.P2P.MessageHandler.HandlerTypes (Peer (..))
import Codec.Serialise (Serialise)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TQueue

import Control.Concurrent.STM.TVar

import Data.ByteString

import Data.ByteString.Char8 as Char8 (ByteString)

import Data.HashMap.Strict as HM

import GHC.Generics (Generic)

--import              Data.Hashable
type ResourceId = String

type ServiceId = String

type ServiceMessage = ByteString

type ResourceHandlerList = [(ResourceId, ResourceHandler)]

type ResourceToPeerMap = HM.HashMap ResourceId (ResourceHandler, TQueue NodeId)

type ResourceHandler = (ServiceMessage -> ServiceMessage)

data MessageTypeRPC
    = Options { to :: NodeId
              , from :: NodeId }
    | Support { to :: NodeId
              , from :: NodeId
              , supportedResources :: [ResourceId] }
    | RequestResource { to :: NodeId
                      , from :: NodeId
                      , rid :: ResourceId
                      , serviceMessage :: ServiceMessage }
    | ReplyResource { to :: NodeId
                    , from :: NodeId
                    , rid :: ResourceId
                    , serviceMessage :: ServiceMessage }
    | Response { to :: NodeId
               , from :: NodeId
               , responseCode :: ResponseCode }
    deriving (Eq, Ord, Show, Generic)

instance Serialise MessageTypeRPC

data ResponseCode
    = Busy
    | Error
    deriving (Eq, Ord, Show, Generic)

instance Serialise ResponseCode

-- any other responses can be added here
--instance Hashable MessageTypeRPC
{-
data MessageRC = RequestRC{
  to ::NodeId
  ,from ::NodeId
  ,serviceMessage ::String
}| ReplyRC {
  to :: NodeId
  ,from :: NodeId
  ,serviceMessage :: String
}deriving(Eq,Ord,Show,Generic)
-}
type P2PUUID = String

-- assort the extra things in a tuple
data P2Pinfo = P2Pinfo
    { uuid :: P2PUUID
    , node :: NodeId
    } deriving (Eq, Show)

data ServicePayload = ServicePayload
    { resid :: ResourceId
    , message :: ByteString
    , extra :: Maybe P2Pinfo
    } deriving (Eq, Show)
