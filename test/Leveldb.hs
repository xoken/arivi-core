{-# LANGUAGE OverloadedStrings #-}

import qualified CAS.DB.LevelDB as LevelDB

import Control.Monad.IO.Class (liftIO)

main :: IO ()
main =
    runResourceT $ do
        bloom <- bloomFilter 10
        db <- open "/tmp/lvlbloomtest" defaultOptions {createIfMissing = True, filterPolicy = Just . Left $ bloom}
        putValue "zzz" "zzz" db
        putValue "yyy" "yyy" db
        putValue "xxx" "xxx" db
        getValue "yyy" db >>= liftIO . print
        getValue "xxx" db >>= liftIO . print
        deleteValue "xxx" db
        getValue "yyy" db >>= liftIO . print
        getValue "xxx" db >>= liftIO . print
        getValue "zzz" db >>= liftIO . print
        putValue "zzz" "1234" db
        getValue "zzz" db >>= liftIO . print
        return ()
