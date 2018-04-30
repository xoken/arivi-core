-- |
-- Module      : Arivi.Crypto.Utils.Random
-- License     :
-- Maintainer  : Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides random generation using Raaz's random no generation
-- with `securely`
--
-- securely :
-- Run a memory action with the internal memory allocated from a
-- locked memory buffer. This memory buffer will never be swapped out by the
-- operating system and will be wiped clean before releasing.
-- Memory locking is an expensive operation and usually there would be a limit to
-- how much locked memory can be allocated. Nonetheless, actions that work with
-- sensitive information like passwords should use this to run an memory action.[1]
-- 1.<https://hackage.haskell.org/package/raaz-0.2.0/docs/Raaz-Core-Memory.html#v:securely>
--

module Arivi.Crypto.Utils.Random
(
    getRandomByteString
)
where

import           Data.ByteString
import           Raaz.Core.Memory
import           Raaz.Core.Types
import           Raaz.Random


-- | Generates RandomByteString of length `len`

getRandomByteString :: BYTES Int -> IO ByteString
getRandomByteString len = securely (randomByteString (len :: BYTES Int) :: RandM ByteString)
