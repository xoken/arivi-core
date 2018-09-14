{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Codec.Serialise
import Data.ByteString (ByteString)
import GHC.Generics

data Cmd = GetBlockC
         | GetBlockChainInfoC
         | GetBlockCountC
         | GetBlockHeaderC
         | GetTransactionC
         deriving (Eq, Ord, Show, Generic)

instance Serialise Cmd

newtype GetBlock = GetBlock
    { getBlockHeader :: ByteString
      -- ^hash of the block header
    } deriving (Eq, Ord, Show, Generic, Serialise)

data GetBlockChainInfo =
    GetBlockChainInfo
    deriving (Eq, Ord, Show, Generic)

instance Serialise GetBlockChainInfo

data GetBlockCount =
    GetBlockCount
    deriving (Eq, Ord, Show, Generic)

instance Serialise GetBlockCount

newtype GetBlockHeader = GetBlockHeader
    { getBlockHeaderHeader :: ByteString
      -- ^hash of the block header
    } deriving (Eq, Ord, Show, Generic)

newtype GetTransaction = GetTransaction
    { getTransactionTransactionId :: ByteString
      -- ^Transaction id
    } deriving (Eq, Ord, Show, Generic)
