-- |
-- Module      : Arivi.Crypto.Hashing.Blake2b_512
-- License     :
-- Maintainer  : Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides generation of hash using Blake2b_512
--


module Arivi.Crypto.Hashing.Blake2b_512
(
    makeHash
) where



import           Crypto.Hash           (Blake2b_512, Digest, hash)
import           Data.ByteString.Char8 (ByteString)

-- | Generates Blake2b_512 hash
makeHash  :: ByteString -> Digest Blake2b_512
makeHash  = hash
