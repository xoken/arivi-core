{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE Rank2Types #-}

module Arivi.P2P.RPC.Types
    ( ServiceId
    , ResourceHandlerList
    , ArchivedResourceToPeerMap
    , ResourceId
    , NodeId
    , P2PUUID
    , ServicePayload(..)
    , P2Pinfo(..)
    , ServiceMessage
    , ResourceHandler(..)
    , ResponseCode(..)
    , TransientResourceToPeerMap
    , ResourceType(..)
    , Options(..)
    , Supported(..)
    ) where

import           Arivi.P2P.MessageHandler.HandlerTypes (NodeId)
import           Codec.Serialise                       (Serialise)
import           Control.Concurrent.STM.TVar
import           Data.ByteString
import qualified Data.ByteString.Lazy                  as Lazy (ByteString)
import           Data.HashMap.Strict                   as HM
import           GHC.Generics                          (Generic)

import           Arivi.P2P.Types (RpcPayload(..))

type ResourceId = String

type ServiceId = String

type ServiceMessage = Lazy.ByteString

type ResourceHandlerList = [(ResourceId, ResourceHandler)]

type ArchivedResourceToPeerMap
     = HM.HashMap ResourceId (ResourceHandler, TVar [NodeId])

newtype ResourceHandler = ResourceHandler (forall r m . RpcPayload r m -> RpcPayload r m)
{-
data MessageTypeRPC
    = RequestResource { to :: NodeId
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
-}
-- Error here is a placeholder proper errors will be defined later
data ResponseCode
    = Busy
    | Error
    | DeserialiseError
    deriving (Eq, Ord, Show, Generic)

instance Serialise ResponseCode

data Options = Options deriving (Eq, Ord, Show, Generic, Serialise)

data Supported r = Supported r deriving(Eq, Ord, Generic, Serialise)



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
