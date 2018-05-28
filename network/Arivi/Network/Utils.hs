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


import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString, fromStrict, toStrict)


-- | Converts lazy ByteString to Strict ByteString
-- TODO: Find usage and depreacate this function
lazyToStrict :: Lazy.ByteString -> ByteString
lazyToStrict =  Lazy.toStrict


-- | Converts strict ByteString to lazy ByteString
-- TODO: Find usage and depreacate this function
strictToLazy :: ByteString -> Lazy.ByteString
strictToLazy = Lazy.fromStrict
