{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The identity of objects.
module Arivi.ID
      ( ID, Identifiable(..), Distance, distance
      -- * Some Unsafe conversion functions.
      , unsafeToHash, unsafeToID
      ) where

import           Data.Bits
import qualified Data.ByteString as BS
import           Raaz
import           Raaz.Hash.Blake2


-- | The ID of an object.
newtype ID a = ID { unID :: BLAKE2b } deriving (Show, Eq)

-- | Recover the hash from the ID. This function essentially erases
-- the phantom type @a@ and hence should be considered unsafe.
unsafeToHash :: ID a -> BLAKE2b
unsafeToHash = unID

-- | An unsafe function to convert a blake2 hash to an ID of an
-- object. This is an unsafe function as it allows us to cook up ID's
-- for which there really are no objects.
unsafeToID :: BLAKE2b -> ID a
unsafeToID = ID

-- | Types for which one can compute the ID from a value.
class Identifiable value where
  -- | Compute the ID of a value.
  identity :: value -> ID value

-- | The distance values
newtype Distance = Distance BS.ByteString

-- TODO: Performance does it makes sense to use ShortBytestring
-- instead of ByteString.

-- | Compute the distance.
distance :: ID a -> ID b -> Distance
distance ia = Distance . BS.pack . BS.zipWith xor (toBS ia) . toBS
  where toBS = toByteString . unsafeToHash
