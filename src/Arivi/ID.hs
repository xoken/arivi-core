{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The identity of objects.
module Arivi.ID
      ( ID
      -- * Some Unsafe conversion functions.
      , unsafeToHash, unsafeToID
      ) where

import Raaz
import Raaz.Hash.Blake2

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
