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
    ( lazyToStrict
    , strictToLazy
    , getIPAddress
    , getPortNumber
    ) where

import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString, fromStrict, toStrict)
import           Network.Socket       (HostName, PortNumber, SockAddr (..),
                                       inet_ntoa)

-- | Converts lazy ByteString to Strict ByteString
-- TODO: Find usage and depreacate this function
lazyToStrict :: Lazy.ByteString -> ByteString
lazyToStrict = Lazy.toStrict

-- | Converts strict ByteString to lazy ByteString
-- TODO: Find usage and depreacate this function
strictToLazy :: ByteString -> Lazy.ByteString
strictToLazy = Lazy.fromStrict

-- | Given `SockAddr` retrieves `HostAddress`
getIPAddress :: SockAddr -> IO HostName
getIPAddress (SockAddrInet _ hostAddress) = inet_ntoa hostAddress
getIPAddress (SockAddrInet6 _ _ (_, _, _, hA6) _) = inet_ntoa hA6
getIPAddress _ =
    error "getIPAddress: SockAddr is not of constructor SockAddrInet "

-- | Given `SockAddr` retrieves `PortNumber`
getPortNumber :: SockAddr -> PortNumber
getPortNumber (SockAddrInet portNumber _) = portNumber
getPortNumber (SockAddrInet6 portNumber _ _ _) = portNumber
getPortNumber _ =
    error "getPortNumber: SockAddr is not of constructor SockAddrInet "
