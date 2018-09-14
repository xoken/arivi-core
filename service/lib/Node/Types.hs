{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Node.Types where

import Arivi.P2P.P2PEnv
import Types
import Block.Types
import Ledger.Types
import Transaction.Types

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Logger
import Data.ByteString (ByteString)

data NodeEnv = NodeEnv
    { nodeEnvState :: TVar NodeState
    , p2pEnv :: P2PEnv Cmd Cmd ByteString ByteString
    }

data NodeState =
  NodeState { nodeChain :: BlockChain
            , nodeLedger :: Ledger
            , nodeMemPool :: [Transaction]
            } deriving (Show, Eq)

newtype Node a = Node
    { unNode :: ReaderT NodeEnv (LoggingT IO) a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader NodeEnv)
