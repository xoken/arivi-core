{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Transaction where

import Transaction.Types
import Ledger.Types
import Ledger
import MerkleTree
import Node.Types

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

type ApplyM = State [InvalidTx]

throwErr :: InvalidTx -> ApplyM ()
throwErr itx = modify (itx:)

transaction :: SecretKey  -> TransactionHeader  -> Transaction
transaction sk txHdr = Transaction txHdr $ sign sk (toPublic sk) (BSL.toStrict $ serialise txHdr)

transferTransaction :: SecretKey -> PublicKey -> Amount -> Transaction
transferTransaction sk pk amount = transaction sk $ Transfer (toPublic sk) pk amount

verifyTxSignature :: Transaction -> Either InvalidTx ()
verifyTxSignature tx@Transaction {..} = do
  let verified = verify (sender header) (BSL.toStrict $ serialise tx) signature
  unless verified $ Left $ InvalidTx tx $ InvalidTxSignature "Unable to verify signature of the transaction."

applyTransaction :: Ledger -> Transaction -> ApplyM Ledger
applyTransaction ledger tx@(Transaction Transfer {..} _) = do
    case verifyTxSignature tx of
        Left err -> throwErr err
        Right _ -> pure ()
    case Ledger.transfer sender receiver amount ledger of
        Left err -> do
            throwErr $ InvalidTx tx $ InvalidTransfer err
            pure ledger
        Right ledger' -> pure ledger'

applyTransactions :: Ledger -> [Transaction] -> (Ledger, [InvalidTx])
applyTransactions ledger txs = runApplyM $ foldM applyTransaction ledger txs

validateTransactions :: Ledger -> [Transaction] -> Either [InvalidTx] ()
validateTransactions ledger txs =
  case snd $ applyTransactions ledger txs of
    [] -> Right ()
    x -> Left x

runApplyM :: ApplyM Ledger -> (Ledger, [InvalidTx])
runApplyM = flip runState []

hashTransaction :: Transaction -> ByteString
hashTransaction = mkHash . BSL.toStrict . serialise

getMerkleProof :: [Transaction] -> Transaction -> ByteString
getMerkleProof txs tx = BSL.toStrict . serialise $ merkleProof tree leaf
  where
    tree = mkMerkleTree $ map hashTransaction txs
    leaf = mkLeafRootHash $ hashTransaction tx

merkleProved :: ByteString -> ByteString -> Transaction -> Maybe Bool
merkleProved proof root tx =
  case deserialiseOrFail (BSL.fromStrict proof) of
    Left err -> Nothing
    Right p -> Just $ validateMerkleProof p (mkLeafRootHash root) (mkLeafRootHash $ hashTransaction tx)

getMerkleTxsRoot :: [Transaction] -> ByteString
getMerkleTxsRoot = mtHash . mkMerkleTree . map hashTransaction

----------------------------------------------------------------------------------------------------
-- Hash helpers
----------------------------------------------------------------------------------------------------

mkHash :: ByteString  -> ByteString
mkHash = convert . (hash :: ByteString -> Digest SHA256)
