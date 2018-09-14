{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Arivi.Network.Replay
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides useful checking and preventing replay attack
--
module Arivi.Network.Replay
    ( isReplayAttack
    , noOfPendings
    ) where

import           Data.List (genericSplitAt)

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
    | (&&) (leftPair < n) (n < rightPair) =
        [(leftPair, n - 1), (n + 1, rightPair)]
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
            let (leftPart, middleElmt:rightPart) =
                    genericSplitAt index pendingList
            let updatedMiddle = updateElement replayNonce middleElmt
            let updatedPendingList =
                    (++) ((++) leftPart updatedMiddle) rightPart
            updatedPendingList

-- | Checks if the given replayNonce is already received or not. If it is there
-- then removes it from pending list and gives updated pending list and missing
-- count
isReplayAttack ::
       Monad m
    => Integer
    -> [(Integer, Integer)]
    -> m (Bool, [(Integer, Integer)])
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
