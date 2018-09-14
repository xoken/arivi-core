{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Transaction.Types where

import Ledger.Types

import Codec.Serialise
import Control.Monad (unless, foldM)
import Control.Monad.State.Strict (State, modify, runState)
import Crypto.Hash (hash, Digest)
import Crypto.Hash.Algorithms
import Crypto.PubKey.Ed25519 (sign, PublicKey, SecretKey, Signature, toPublic, verify)
import Data.ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Text hiding (map)
import GHC.Generics

data Transaction = Transaction
     { header :: !TransactionHeader
     , signature :: !Signature
     } deriving (Show, Eq, Generic)

data TransactionHeader = Transfer
     { sender :: !PublicKey
     , receiver :: !PublicKey
     , amount :: !Amount
     } deriving (Show, Eq, Ord, Generic)

instance Serialise Transaction
instance Serialise TransactionHeader

data InvalidTxField = InvalidTxSignature !Text
                    | InvalidTransfer !Ledger.Types.TransferError
                    deriving (Show, Eq, Ord)

data InvalidTx = InvalidTx Transaction InvalidTxField
               deriving (Show, Eq)
