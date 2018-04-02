-- |
-- Module      :  Arivi.Network.Session
-- Copyright   :
-- License     :
-- Maintainer  :  Mahesh Uligade <maheshsuligade@gmail.com>
-- Stability   :
-- Portability :
--
-- This module provides useful functions for managing sessions in Arivi
-- communication
module Arivi.Network.Session
(
    getUniqueSessionId,
    genSessionId,
    SessionTuple (..)
) where


import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Base16 (encode)
import Data.HashMap.Strict (HashMap,member,empty)

import Arivi.Crypto.Utils.Random
import Arivi.Crypto.Utils.Keys.Encryption as Encryption
import Arivi.Network.Types (Version,EncodingType)



-- | (sessionId,SessionTuple) are (key,value) pair in HashMap that stores
-- information about all the session uniquely
data SessionTuple = SessionTuple {
                          version         :: Version
                        , sharedSecret    :: Encryption.SharedSecret
                        , remotePublicKey :: Encryption.PublicKey
                        , encodingType    :: EncodingType
                        } deriving (Eq)



-- | Generates a random 4 Byte SessionId using Raaz's random ByteString
-- generation
genSessionId :: IO ByteString
genSessionId = getRandomByteString 4 >>=
                                    \byteString -> return (encode byteString)


-- | Generates unique SessionId by checking it is already present in given
-- HashMap
getUniqueSessionId :: HashMap ByteString SessionTuple -> IO ByteString
getUniqueSessionId hashmap = do
                                sessionId <- genSessionId

                                if member sessionId hashmap
                                    then  getUniqueSessionId hashmap
                                    else
                                        return sessionId
