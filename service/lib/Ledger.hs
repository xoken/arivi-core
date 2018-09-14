{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger
    ( module Ledger
    ) where

import Ledger.Types

import Codec.Serialise
import Codec.Serialise.Encoding
import Codec.Serialise.Decoding
import Control.Monad (when)
import Crypto.PubKey.Ed25519 (PublicKey, Signature, publicKey, signature)
import Crypto.Error
import Numeric.Natural (Natural)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Monoid

initLedger :: Ledger
initLedger = Ledger HM.empty

getBalance :: PublicKey -> Ledger -> Maybe Amount
getBalance pk = HM.lookup pk . unLedger

addBalance :: PublicKey -> Amount -> Ledger -> Ledger
addBalance pk amount =
  Ledger . HM.adjust (+amount) pk . unLedger

addAddress :: PublicKey -> Ledger -> Ledger
addAddress pk ledger =
    case getBalance pk ledger of
        Nothing -> Ledger $ HM.insert pk 0 ledger'
        Just _ -> ledger
  where
    ledger' = unLedger ledger

transfer ::
       PublicKey -> PublicKey -> Amount -> Ledger -> Either TransferError Ledger
transfer from to amount ledger = do
    let ledger' = foldr addAddress ledger [from, to]
    senderBal <-
        case getBalance from ledger' of
            Nothing -> Left $ KeyDoesNotExist from
            Just amnt -> Right amnt
    recvBal <-
        case getBalance to ledger' of
            Nothing -> Left $ KeyDoesNotExist to
            Just amnt -> Right amnt
    when (amount < 1) (Left $ InvalidTransferAmount amount)
    when (senderBal < amount) (Left $ InsufficientBalance from senderBal)
    Right $ addBalance to amount $ addBalance from (-amount) ledger'

-- -- | Add transaction to local node state.
-- applyTx :: Tx -> NodeStorage -> NodeStorage
-- applyTx tx storage = storage { nodeStorageTxs = Set.insert tx $ nodeStorageTxs storage }

-- -- | Checks whether transaction belongs to local node state.p
-- memberTx :: Tx -> NodeStorage -> Bool

-- memberTx tx = Set.member tx . nodeStorageTxs
