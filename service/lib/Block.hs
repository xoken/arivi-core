{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Block where

import Block.Types
import Transaction.Types hiding (header, signature)
import Transaction
import Ledger
import Ledger.Types

import Codec.Serialise (Serialise, serialise)
import Control.Monad (unless)
import MerkleTree
import Crypto.PubKey.Ed25519 (PublicKey, SecretKey, Signature, sign, verify)
import Data.ByteString as BS (ByteString, take, isPrefixOf)
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 (pack)
import Data.Text (Text)
import GHC.Generics

type KeyPair = (PublicKey, SecretKey)

-- Keep this same from KHAAAN!
genesisBlock :: KeyPair -> Block
genesisBlock (pk, sk) =
  Block { index = 0
        , header = genesisBlockHeader
        , transactions = []
        , signature = sign sk pk (BSL.toStrict $ serialise genesisBlockHeader)
        }
  where
    genesisBlockHeader =
      BlockHeader { origin = pk
                  , previousHash = "0"
                  , merkleRoot = getMerkleRoot emptyHash
                  , nonce = 0
                  }

verifyBlockSignature :: Block -> Either InvalidBlock ()
verifyBlockSignature block@Block{..} = do
  let verified = verify (origin header) (BSL.toStrict $ serialise block) signature
  unless verified $ Left $ InvalidBlockSignature "Unable to verify the signature of the block."

hashBlockHeader :: BlockHeader -> ByteString
hashBlockHeader = mkHash . BSL.toStrict . serialise

hashBlock :: Block -> ByteString
hashBlock = mkHash . BSL.toStrict . serialise

mineBlock :: Block -> KeyPair -> [Transaction] -> Block
mineBlock prevBlock (pk, sk) txs =
  Block { index = index'
        , header = proofOfWork index' blockHeader
        , transactions = txs
        , signature = sign sk pk (BSL.toStrict $ serialise blockHeader)
        }
  where
    index' = index prevBlock + 1
    blockHeader = BlockHeader { origin = pk
                              , previousHash = hashBlockHeader $ header prevBlock
                              , merkleRoot = getMerkleTxsRoot txs
                              , nonce = 0
                              }

proofOfWork :: Int -> BlockHeader -> BlockHeader
proofOfWork idx blockHeader = blockHeader { nonce = calcNonce 0}
  where
    difficulty = calcDifficulty idx
    prefix = pack $ replicate difficulty '0'

    calcNonce n
      | prefix' == prefix = n
      | otherwise = calcNonce $ n + 1
      where
        headerHash = hashBlockHeader (blockHeader { nonce = n })
        prefix' = BS.take difficulty headerHash


validateProofOfWork :: Block -> Bool
validateProofOfWork block =
    BS.isPrefixOf prefix $ hashBlockHeader (header block)
  where
    difficulty = calcDifficulty $ index block
    prefix = pack $ replicate difficulty '0'

calcDifficulty :: Int -> Int
calcDifficulty = round . logBase 2.0 . fromIntegral

validateBlock :: Ledger -> Block -> Block -> Either InvalidBlock ()
validateBlock ledger prevBlock block
  | index block /= index prevBlock + 1 = Left $ InvalidBlockIndex (index block)
  | hashBlockHeader (header prevBlock) /= previousHash (header block) = Left InvalidPrevBlockHash
  | not (validateProofOfWork block) = Left InvalidBlockNonce
  | null (transactions block) = Left InvalidBlockNumTxs
  | mRoot /= mRoot' = Left $ InvalidBlockMerkleRoot mRoot'
  | otherwise = do
      -- Verify signature of block
      verifyBlockSignature block
      -- Validate all transactions w/ respect to world state
      first InvalidBlockTx $
        validateTransactions ledger txs
  where
    txs = transactions block
    mRoot  = merkleRoot $ header block      -- given root
    mRoot' = getMerkleTxsRoot txs

first :: (a -> b) -> Either a c -> Either b c
first f (Left a) = Left (f a)
first _ (Right c) = Right c


validateAndApplyBlock :: Ledger -> Block -> Block -> Either InvalidBlock (Ledger, [InvalidTx])
validateAndApplyBlock ledger prevBlock block =
  do
    validateBlock ledger prevBlock block
    Right $ applyBlock ledger block

applyBlock :: Ledger -> Block -> (Ledger, [InvalidTx])
applyBlock ledger = applyTransactions ledger . transactions
