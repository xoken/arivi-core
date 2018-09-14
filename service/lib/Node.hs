{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Node where

import Transaction
import Ledger
import Node.Types
import Block.Types
import qualified Block as Block

import Control.Concurrent.STM.TVar
import Control.Monad (void, forM, replicateM)
import Control.Monad.Reader
import Crypto.PubKey.Ed25519
import Data.ByteString.Char8 (pack, unpack, ByteString)
import Data.Semigroup ((<>))


{-
initNode :: NodeEnvironment -> IO ()
initNode NodeEnvironment {..} = do
  loop
    where
      loop :: IO void
      loop = do
        input <- getLine
        case words input of
          ["0", from, to, amount] -> do
            let sk = secretKey $ pack from
                pk = publicKey $ pack to
                tx = transferTransaction sk pk (read amount)
            loop
          ["1", txId] -> do
            -- let tx = createTx nodeEnvId to amount
            print $ txId
            loop
          ["2"] -> do
              mineBlock
          _ -> do
            putStrLn $ "Invalid command: " <> input
            loop
      initNodeState :: NodeState
      initNodeState = NodeState { nodeChain = [genesisBlock keyPair]
                                , nodeLedger = initLedger
                                , nodeMemPool = []
                                }
-}

mineBlock :: Node Block
mineBlock = do
    nodeState <- asks nodeEnvState >>= liftIO . readTVarIO
    when (null $ nodeChain nodeState) (error "No genesis block")
    let prevBlock = last $ nodeChain nodeState
        memPooledtxs = nodeMemPool nodeState
        keyPair = (undefined, undefined) -- get our secretkey
    pure $ Block.mineBlock prevBlock keyPair memPooledtxs
