{-# LANGUAGE FlexibleContexts #-}

<<<<<<< HEAD
-- |
-- Module      :  Arivi.Network.Replay
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
=======
>>>>>>> breaking out arivi-core from arivi
--
-- This module provides useful checking and preventing replay attack
--
module Arivi.Network.Replay
    ( isReplayAttack
    , noOfPendings
    ) where

<<<<<<< HEAD
import           Data.List (genericSplitAt)
=======
import Data.List (genericSplitAt)
>>>>>>> breaking out arivi-core from arivi

-- | Checks if given nonce is present in the pending list
isPresent :: Integer -> [(Integer, Integer)] -> Bool
isPresent _ [] = False
isPresent n list
    | (&&) (leftPair <= n) (n <= rightPair) = True
    | leftPair > n = isPresent n leftPart
    | otherwise = isPresent n rightPart
  where
    middleIndex = length list `quot` 2
    (leftPart, middleElmt:rightPart) = splitAt middleIndex list
    (leftPair, rightPair) = middleElmt

-- | Removes the received nonce value from the pending list pair
updateElement :: Integer -> (Integer, Integer) -> [(Integer, Integer)]
updateElement n (leftPair, rightPair)
    | (&&) (leftPair == n) (n == rightPair) = []
    | (&&) (leftPair == n) (n < rightPair) = [(leftPair + 1, rightPair)]
    | (&&) (leftPair < n) (n == rightPair) = [(leftPair, rightPair - 1)]
<<<<<<< HEAD
    | (&&) (leftPair < n) (n < rightPair) =
        [(leftPair, n - 1), (n + 1, rightPair)]
=======
    | (&&) (leftPair < n) (n < rightPair) = [(leftPair, n - 1), (n + 1, rightPair)]
>>>>>>> breaking out arivi-core from arivi
    | otherwise = error "Number is out of range"

-- | Gives the index of the range pair where the nonce is present
getRangeIndex :: Integer -> [(Integer, Integer)] -> Maybe Integer
getRangeIndex _ [] = Nothing
getRangeIndex n pendingList
    | (&&) (leftPair <= n) (n <= rightPair) = Just middleIndex
    | leftPair > n = getRangeIndex n leftPart
    | otherwise = (+ middleIndex) . (+ 1) <$> getRangeIndex n rightPart
  where
    middleIndex = toInteger (length pendingList) `quot` 2
    (leftPart, middleElmt:rightPart) = genericSplitAt middleIndex pendingList
    (leftPair, rightPair) = middleElmt

-- | Removes gives replayNonce and gives updated pending list
updatePendingList :: Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
updatePendingList replayNonce pendingList =
    case getRangeIndex replayNonce pendingList of
        Nothing -> pendingList
        Just index -> do
<<<<<<< HEAD
            let (leftPart, middleElmt:rightPart) =
                    genericSplitAt index pendingList
            let updatedMiddle = updateElement replayNonce middleElmt
            let updatedPendingList =
                    (++) ((++) leftPart updatedMiddle) rightPart
=======
            let (leftPart, middleElmt:rightPart) = genericSplitAt index pendingList
            let updatedMiddle = updateElement replayNonce middleElmt
            let updatedPendingList = (++) ((++) leftPart updatedMiddle) rightPart
>>>>>>> breaking out arivi-core from arivi
            updatedPendingList

-- | Checks if the given replayNonce is already received or not. If it is there
-- then removes it from pending list and gives updated pending list and missing
-- count
<<<<<<< HEAD
isReplayAttack ::
       Monad m
    => Integer
    -> [(Integer, Integer)]
    -> m (Bool, [(Integer, Integer)])
=======
isReplayAttack :: Monad m => Integer -> [(Integer, Integer)] -> m (Bool, [(Integer, Integer)])
>>>>>>> breaking out arivi-core from arivi
isReplayAttack replayNonce pendingList =
    if isPresent replayNonce pendingList
        then return (False, updatePendingList replayNonce pendingList)
        else return (True, pendingList)

-- | Counts the no of pending nonces in pending list
noOfPendings :: Monad m => [(Integer, Integer)] -> m Integer
noOfPendings [_] = return 0
noOfPendings pendingList = do
    let (leftPair, rightPair) = head pendingList
    pendings <- noOfPendings $ tail pendingList
    let toatalDifference = (rightPair - leftPair + 1) + pendings
    return toatalDifference
