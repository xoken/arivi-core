-- |
-- Module      : Arivi.P2P.PRT.Types
-- License     :
-- Maintainer  : Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides different data types that are used in the Peer
-- Reputation management
--
module Arivi.P2P.PRT.Types
    ( Config(..)
    , PRTScore
    ) where

-- import           GHC.Generics
-- import           Data.Yaml
-- | `PRTScore` is a unit to count Peer Reputations
newtype PRTScore =
    PRTScore Integer
    deriving (Show, Eq)

-- | These are different types of status of Peer Reputations depending on the
--  response of the peer
data Config = Config
    { goodNotification   :: PRTScore -- ^ Award this score to Peer when given
                                     --   good notification is sent
    , badNotification    :: PRTScore -- ^ Award this score to Peer when given
                                     --   bad notification is sent
    , firstNotification  :: PRTScore -- ^ Award this score to Peer when it has
                                     --   given first good notification
    , goodBlockSender    :: PRTScore -- ^ Award this score to Peer when given
                                     --   good block
    , badBlockSender     :: PRTScore -- ^ Award this score to Peer when given
                                     --   bad blockPRTScore
    , signatureMismatch  :: PRTScore -- ^ Award this score to Peer when
                                     --   signature is Mismatched
    , deserialiseFaliure :: PRTScore -- ^ Award this score to Peer when
                                     --   deserialiseFaliure happens to it's
                                     --   given packet
    } deriving (Show, Eq)
