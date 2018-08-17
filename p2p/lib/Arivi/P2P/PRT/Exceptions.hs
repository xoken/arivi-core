module Arivi.P2P.PRT.Exceptions
    ( PRTExecption(..)
    ) where

import           Control.Exception

data PRTExecption =
    PeerDeedNotFound
    deriving (Show)

instance Exception PRTExecption
