{-# LANGUAGE DeriveGeneric #-}

module Arivi.P2P.RPC.Types
    ( ResourceHandlerList
    , ArchivedResourceToPeerMap
    , ResourceId
    , MessageTypeRPC(..)
    , NodeId
    , P2PUUID
    , ServicePayload(..)
    , P2Pinfo(..)
    , ServiceMessage(..)
    , ResourceHandler
    , ResponseCode(..)
    , TransientResourceToPeerMap
    , ResourceType(..)
    ) where

import           Arivi.P2P.MessageHandler.HandlerTypes (NodeId, P2PUUID)
import           Codec.Serialise                       (Serialise)
import           Control.Concurrent.STM.TVar
import           Data.ByteString
import qualified Data.ByteString.Lazy                  as Lazy (ByteString)
import           Data.Hashable
import           Data.HashMap.Strict                   as HM
import           GHC.Generics                          (Generic)

newtype ResourceId =
    ResourceId String
    deriving (Eq, Ord, Show, Generic)

instance Serialise ResourceId

instance Hashable ResourceId

newtype ServiceMessage =
    ServiceMessage Lazy.ByteString
    deriving (Eq, Ord, Show, Generic)

instance Serialise ServiceMessage

type ResourceHandlerList = [(ResourceId, ResourceHandler)]

type ArchivedResourceToPeerMap
     = HM.HashMap ResourceId (ResourceHandler, TVar [NodeId])

type ResourceHandler = (ServiceMessage -> ServiceMessage)

data MessageTypeRPC
    = Options { to   :: NodeId
              , from :: NodeId }
    | Support { to                 :: NodeId
              , from               :: NodeId
              , supportedResources :: [ResourceId] }
    | RequestResource { to             :: NodeId
                      , from           :: NodeId
                      , rid            :: ResourceId
                      , serviceMessage :: ServiceMessage }
    | ReplyResource { to             :: NodeId
                    , from           :: NodeId
                    , rid            :: ResourceId
                    , serviceMessage :: ServiceMessage }
    | Response { to           :: NodeId
               , from         :: NodeId
               , responseCode :: ResponseCode }
    deriving (Eq, Ord, Show, Generic)

instance Serialise MessageTypeRPC

-- Error here is a placeholder proper errors will be defined later
data ResponseCode
    = Busy
    | Error
    | DeserialiseError
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
-- assort the extra things in a tuple
data P2Pinfo = P2Pinfo
    { uuid :: P2PUUID
    , node :: NodeId
    } deriving (Eq, Show)

data ServicePayload = ServicePayload
    { resid   :: ResourceId
    , message :: ByteString
    , extra   :: Maybe P2Pinfo
    } deriving (Eq, Show)

type TransientResourceToPeerMap
     = HM.HashMap ResourceId (ResourceHandler, TVar [NodeId])

data ResourceType
    = Archived
    | Transient
    deriving (Eq, Ord, Show, Generic)

instance Serialise ResourceType
