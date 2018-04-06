-- |
-- Module      : Crypto.Hashing.Blake2b_512
-- License     :
-- Maintainer  : Mahesh Uligade <maheshsuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides generation of hash using Blake2b_512
--


module Crypto.Hashing.Blake2b_512
(
    makeHash
) where



import           Crypto.Hash           (Blake2b_512, Digest, hash)
import           Data.ByteString.Char8 (ByteString, pack)

-- | Generates Blake2b_512 hash
makeHash  :: ByteString -> Digest Blake2b_512
makeHash  = hash
