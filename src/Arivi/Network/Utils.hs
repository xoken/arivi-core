-- |
-- Module      :  Arivi.Network.Utils
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides useful utility functions for the Arivi Network Layer
--
module Arivi.Network.Utils
(
    lazyToStrict
  , strictToLazy
) where


import qualified Data.ByteString.Char8 as Char8 (ByteString,pack)
import qualified Data.ByteString.Internal as Internal (unpackBytes)
import qualified Data.ByteString.Lazy as Lazy (ByteString,pack,toStrict)
import qualified GHC.Word (Word8)


-- | Converts lazy ByteString to Strict ByteString
lazyToStrict :: Lazy.ByteString -> Char8.ByteString
lazyToStrict =  Lazy.toStrict


-- | Converts strict ByteString to lazy ByteString
strictToLazy :: Char8.ByteString -> Lazy.ByteString
strictToLazy strictByteString = Lazy.pack
                                     (Internal.unpackBytes strictByteString)
