{-# LANGUAGE DeriveGeneric #-}

module Service.Types
    ( ResourceType(..)
    ) where

import Codec.Serialise
import Data.ByteString.Lazy as Lazy
import GHC.Generics

data ResourceType
    = Archived
    | Transient
    deriving (Eq, Ord, Show, Generic)

instance Serialise ResourceType
