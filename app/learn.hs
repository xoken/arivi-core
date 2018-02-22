import qualified Data.List.Split    as S 
import Network.Socket 
import qualified Network.Socket.Internal   as M
import Data.Word
import Data.Binary 

data Message = PING
               |PONG
               |FIND_NODE { 
                   nodeId     :: [Char]
                ,  find       :: [Char]
                ,  address    :: [Char] 
                ,  seqNo      :: [Char]
                ,  signature  :: [Char] 
                }
               |FN_RESP {
                    nodeId    :: [Char]
                ,   knodes    :: [(String,String)] 
                ,   address   :: [Char] 
                ,   seqNo     :: [Char]
                ,   signature :: [Char] 
                }            
               deriving (Show)

data Exp = IntE Int
         | OpE  String Exp Exp
   deriving Show

instance Binary Exp where
      put (IntE i)      = do put (0 :: Word8)
                             put i
      put (OpE s e1 e2) = do put (1 :: Word8)
                             put s
                             put e1
                             put e2

      get = do t <- get :: Get Word8
               case t of
                    0 -> do i <- get
                            return (IntE i)
                    1 -> do s  <- get
                            e1 <- get
                            e2 <- get
                            return (OpE s e1 e2)

