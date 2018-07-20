{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

import qualified Arivi.Crypto.Utils.Keys.Signature   as S
import qualified Arivi.P2P.Kademlia.Kbucket          as K
import qualified Arivi.P2P.Kademlia.Types            as T
import qualified Arivi.P2P.Kademlia.Utils            as U
import qualified Arivi.P2P.Kademlia.XorDistance      as X
import           Control.Monad.IO.Class

import qualified Arivi.Crypto.Utils.PublicKey.Utils  as U
import qualified Arivi.P2P.Kademlia.LoadDefaultPeers as LD
import           Arivi.P2P.P2PEnv
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.STM
import           Control.Monad.Trans.Control
import qualified Network.Socket                      as N
import           STMContainers.Map                   as M

-- tom :: Reader String String
-- tom = do
--     env <- ask -- gives you the environment which in this case is a String
--     return (env ++ " This is Tom.")
-- jerry :: Reader String String
-- jerry = do
--   env <- ask
--   return (env ++ " This is Jerry.")
-- tomAndJerry :: Reader String String
-- tomAndJerry = do
--     t <- tom
--     j <- jerry
--     return (t ++ "\n" ++ j)
-- runJerryRun :: String
-- runJerryRun = runReader tomAndJerry "Who is this?"
getNodeId :: IO T.NodeId
getNodeId = do
    (sk, pk) <- S.generateKeyPair
    let pk = U.generateNodeId sk
    let nid = pk :: T.NodeId
    return nid

getNep :: String -> N.PortNumber -> N.PortNumber -> T.NodeEndPoint
getNep hostAddr port1 port2 = nep
  where
    ha = hostAddr
    p1 = port1
    p2 = port2
    nep = T.NodeEndPoint ha p1 p2

getRPeer :: IO K.Peer
getRPeer = do
    nid <- getNodeId
    let nep = getNep "127.0.0.1" 7000 8000
    return $ K.Peer (nid, nep)

-- data Dummy = Dummy {
--     nid :: Int
-- } deriving (Show)
-- foo :: Dummy -> String
-- foo x = if nid x == 0 then "0" else "not zero"
-- isZero :: Dummy -> Bool
-- isZero x = foo x == "0"
-- data MyError = ODD
--                | FAILED
--                deriving (Show)
-- getDouble :: Int -> Either MyError Int
-- getDouble num = if num `mod` 2 == 0
--               then Right (num * 2)
--               else Left ODD
-- demo :: Int -> IO Either MyError Int
-- deom num = do
--   either
-- main :: IO ()
-- main = do
--   let a = getDouble 111
--   case a of
--     Left _  -> print "failed"
--     Right x -> print x
type AppM = ReaderT (T.Kbucket Int [T.Peer]) IO

instance T.HasKbucket AppM where
    getKb = ask

demo :: (HasP2PEnv m) => Int -> m ()
demo k = do
    peers <- K.getKRandomPeers k
    return ()

demo2 =
    atomically $ do
        let a = "12"
        M.new
