{-# LANGUAGE FlexibleContexts #-}

module Arivi.Utils.Set
    ( module Arivi.Utils.Set
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Applicative
import Control.Concurrent.Async.Lifted
import Control.Monad.Trans.Control

traverseSet :: (Applicative f, Ord b) => (a -> f b) -> Set a -> f (Set b)
traverseSet f = foldr (\x ys -> liftA2 Set.insert (f x) ys) (pure Set.empty)

mapSetConcurrently ::
       (MonadBaseControl IO m, Ord b) => (a -> m b) -> Set a -> m (Set b)
mapSetConcurrently f = runConcurrently . traverseSet (Concurrently . f)
