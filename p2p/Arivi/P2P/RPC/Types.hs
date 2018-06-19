{-# LANGUAGE DeriveGeneric #-}

module Arivi.P2P.RPC.Types
    ( ServiceId
    , ResourceList
    , ResourceToPeerMap
    , ResourceId
    , MessageTypeRPC(..)
    , NodeId
    , P2PUUID
    , ReadResourceWrapper(..)
    ) where

import           Arivi.P2P.MessageHandler.HandlerTypes (Peer (..))
import           Codec.Serialise                       (Serialise)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.TVar
import           Data.ByteString
import           Data.ByteString.Char8                 as Char8 (ByteString)
import           Data.HashMap.Strict                   as HM
import           GHC.Generics                          (Generic)

--import              Data.Hashable
type IP = String

type Port = Int

type ResourceId = String

type NodeId = String

type ServiceId = String

type ResourceList = [ResourceId]

type ResourceToPeerMap = HM.HashMap ResourceId (ServiceId, TQueue Peer)

data MessageTypeRPC
    = Options { to   :: NodeId
              , from :: NodeId }
    | Support { to                 :: NodeId
              , from               :: NodeId
              , supportedResources :: [ResourceId] }
    | RequestRC { to             :: NodeId
                , from           :: NodeId
                , rid            :: ResourceId
                , serviceMessage :: ByteString }
    | ReplyRC { to             :: NodeId
              , from           :: NodeId
              , rid            :: ResourceId
              , serviceMessage :: ByteString }
    deriving (Eq, Ord, Show, Generic)

instance Serialise MessageTypeRPC

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
data ReadResourceWrapper = ReadResourceWrapper
    { uuid    :: P2PUUID
    , resid   :: ResourceId
    , message :: ByteString
    , node    :: NodeId
    } deriving (Eq, Show)
