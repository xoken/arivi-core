{-# language DeriveGeneric, DeriveAnyClass, Rank2Types, ScopedTypeVariables  #-}
{-# language GADTs, DataKinds, KindSignatures, TypeFamilies #-}
{-# language PartialTypeSignatures #-}

module Arivi.P2P where


import Arivi.P2P.Types

import Data.Hashable
import qualified Data.HashMap.Strict as Map
import Data.ByteString.Lazy (ByteString)
import Codec.Serialise
import Control.Concurrent.STM
import Control.Concurrent.Async

import GHC.Generics

type NodeId = String
type PeerDetails = String

{-
init :: Map k (Handler msg IO) -> IO ()
init handlers = do
  let peers = Map.empty
  -- loadDefaultPeers
  -- sendOptions (Map.keys handlers)
  return ()
-}


runHandlerConcurrently :: Handler i o IO -> [i] -> IO [o]
runHandlerConcurrently h is = mapConcurrently (runHandler h) is

blockRequest :: Request 'Rpc
blockRequest = RpcRequestG (RpcRequest "hey" BlockResource)

blockResponse :: Response 'Rpc
blockResponse = RpcResponseG (RpcResponse "hey" BlockResource)

mainOutgoing :: IO ()
mainOutgoing = do
  a <- runHandler (issueRequest coolMap) ("key1", blockRequest)
  print (fst a)

coolMap :: Map NodeId PeerDetails
coolMap = Map.insert "key1" "value1" (Map.insert "key2" "value2" Map.empty)

issueRequest :: forall i.
       Map NodeId PeerDetails
    -> Handler (NodeId, Request i) (NodeId, Response i) IO
issueRequest _ =
    Handler $ \(nId, req) -> do
        case req of
          RpcRequestG (RpcRequest msg r) -> do
            print (serialise msg)
            print (serialise r)
            return (nId, RpcResponseG (RpcResponse msg BlockResource))
          OptionsRequestG SRequest -> do
            return (nId, OptionsResponseG (SResponse BlockResource))

optionsHandler :: [k] -> Handler SRequest (SResponse [k]) IO
optionsHandler resources = Handler (const $ return (SResponse resources))

{-
-- This is assumed to be running in a thread
requestOptions ::
       TVar (Map NodeId PeerDetails)
    -> [NodeId]
    -> Map k (TVar [NodeId])
    -> IO ()
requestOptions peers nodes resourcers = do
    bs <-
        runHandlerConcurrently
            (issueRequest peers Option)
            (zip nodes (repeat SRequest)) :: IO [(NodeId, SResponse Int)]
    return ()

fetchResource ::
       (Serialise i, Serialise o)
    => TVar (Map NodeId PeerDetails)
    -> Map k (TVar [NodeId])
    -> k
    -> Handler i (NodeId, o) IO
fetchResource peers resourcers resource =
    Handler $ \x -> (runHandler (issueRequest peers RPC) (nId, x))
  where
    nId = undefined -- lookup in resourcers

rpcHandler ::
       (Serialise i, Serialise o)
    => Map k (Handler (RpcRequest i k) (RpcResponse o k) IO)
    -> Handler (RpcRequest i k) (RpcResponse o k) IO
rpcHandler m = undefined

notify ::
       (Serialise i, Serialise t)
    => TVar (Map NodeId PeerDetails)
    -> NodeId
    -> t
    -> Handler i () IO
notify peers node t = undefined --handleit

notifyAll :: (Serialise i, Serialise t) => TVar (Map NodeId PeerDetails) -> Map t (TVar [NodeId]) -> t -> Handler i () IO
notifyAll peers nodes t = Handler $ \i -> do return ()
-}

data Block = Block BlockHeader Int deriving (Eq, Ord, Show, Generic, Serialise)
data BlockHeader = BlockHeader Int deriving (Eq, Ord, Show, Generic, Serialise)

serviceMessage :: String
serviceMessage = "GetBlocks"

data Resource = BlockResource
              | BlockHeaderResource
              deriving (Eq, Ord, Show, Generic, Serialise, Hashable)

blockResourceHandler ::
       Handler (RpcRequest ByteString Resource) (RpcResponse ByteString Resource) IO
blockResourceHandler =
    Handler $ \b@(RpcRequest m r) -> do
        print "In blockResourceHandler" >> print b
        return (RpcResponse m r)

blockHeaderResourceHandler ::
       Handler (RpcRequest ByteString Resource) (RpcResponse ByteString Resource) IO
blockHeaderResourceHandler =
    Handler $ \b@(RpcRequest m r) -> do
        print "In blockHeaderResourceHandler" >> print b
        return (RpcResponse m r)

handlerMap :: Map Resource (Handler (RpcRequest ByteString Resource) (RpcResponse ByteString Resource) IO)
handlerMap =
    Map.insert
        BlockResource
        blockResourceHandler
        (Map.insert BlockHeaderResource blockHeaderResourceHandler Map.empty)

resourcers :: Map Resource (TVar [NodeId])
resourcers =
    Map.insert
        BlockResource
        undefined
        (Map.insert BlockHeaderResource undefined Map.empty)

{-
getBlock :: IO (NodeId, Block)
getBlock =
    runHandler (fetchResource undefined resourcers BlockResource) serviceMessage
-}

invoke :: forall k o i.
          (Serialise i, Serialise k, Eq k, Show k, Hashable k, Show i)
       => ByteString
       -> Map k (Handler (RpcRequest i k) (RpcResponse o k) IO)
       -> IO (RpcResponse o k)
invoke bs m = do
    let rr@(RpcRequest _ k) = deserialise bs :: (RpcRequest i k)
    print "In invoke "
    print rr
    case Map.lookup k m of
        Just h -> runHandler h rr
        Nothing -> error "Cover has blown"

mainIncoming :: IO ()
mainIncoming = do
  let rpcMessage = serialise (RpcRequest (serialise "abc") BlockResource)
  a <- invoke rpcMessage handlerMap :: IO (RpcResponse ByteString Resource)
  print "In main"
  print a
