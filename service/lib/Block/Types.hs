{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Block.Types where

import Transaction.Types

import Codec.Serialise (Serialise, serialise)
import Control.Monad (unless)
import MerkleTree
import Crypto.PubKey.Ed25519 (PublicKey, SecretKey, Signature, sign, verify)
import Data.ByteString as BS (ByteString, take, isPrefixOf)
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 (pack)
import Data.Text (Text)
import GHC.Generics

type BlockChain = [Block]

data BlockHeader = BlockHeader
     { origin :: PublicKey
     , previousHash :: ByteString
     , merkleRoot :: ByteString
     , nonce :: Int
     } deriving (Show, Eq, Generic)

data Block = Block
     { index :: Int
     , header :: BlockHeader
     , transactions :: [Transaction]
     , signature :: Signature
     } deriving (Show, Eq, Generic)

instance Serialise Block
instance Serialise BlockHeader

data InvalidBlock
   = InvalidBlockSignature Text
   | InvalidBlockIndex Int
   | InvalidPrevBlockHash
   | InvalidBlockNonce
   | InvalidBlockNumTxs
   | InvalidBlockMerkleRoot ByteString
   | InvalidBlockTx [InvalidTx]
   deriving (Show, Eq)
