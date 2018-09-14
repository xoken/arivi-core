module Ledger.Types where

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

type Amount = Natural

newtype Ledger = Ledger
  { unLedger :: HashMap PublicKey Amount
  } deriving (Show, Eq)

data TransferError
  = KeyDoesNotExist PublicKey
  | InvalidTransferAmount Amount
  | InsufficientBalance PublicKey Amount
  deriving (Show, Eq, Ord)


instance Ord PublicKey where
  compare p1 p2 = compare (bs p1) (bs p2)
      where
          bs :: PublicKey -> ByteString
          bs = convert

instance Hashable PublicKey where
    hashWithSalt s p = hashWithSalt s (bs p)
        where
            bs :: PublicKey -> ByteString
            bs = convert

instance Serialise PublicKey where
    encode = encodePublicKey
    decode = decodePublicKey

encodePublicKey :: PublicKey -> Encoding
encodePublicKey bytes = do
    let temp = convert bytes :: Data.ByteString.ByteString
    encodeListLen 2 <> encodeWord 0 <> encode temp

decodePublicKey :: Decoder s PublicKey
decodePublicKey = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
        (2, 0) ->
            throwCryptoError . publicKey <$>
            (decode :: Decoder s Data.ByteString.ByteString)
        _ -> fail "invalid PublicKey encoding"

instance Serialise Signature where
    encode = encodeSignature
    decode = decodeSignature

encodeSignature :: Signature -> Encoding
encodeSignature sig = do
    let temp = convert sig :: ByteString
    encodeListLen 2 <> encodeWord 0 <> encode temp

decodeSignature :: Decoder s Signature
decodeSignature = do
    len <- decodeListLen
    tag <- decodeWord
    case (len, tag) of
        (2, 0) ->
            throwCryptoError . signature <$>
            (decode :: Decoder s ByteString)
        _ -> fail "invalid Signature encoding"
