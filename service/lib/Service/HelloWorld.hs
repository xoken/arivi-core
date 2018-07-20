{-# LANGUAGE OverloadedStrings #-}

module Service.HelloWorld where

import Arivi.P2P.P2PEnv
import Arivi.P2P.RPC.Functions
import Arivi.Utils.Logging
import Control.Monad.IO.Class
import Data.ByteString.Lazy as Lazy

-- import           Service.Types                    (ResourceType (Archived))
import Arivi.P2P.RPC.Types (ResourceType(..))
import Control.Concurrent

type ServiceMsg = Lazy.ByteString

handler :: Lazy.ByteString -> Lazy.ByteString
handler serviceMsg = Lazy.concat [serviceMsg, "Praise Jesus"]

getResourceId :: String
getResourceId = "HelloWorld"

registerHelloWorld :: (HasP2PEnv m, HasLogging m) => m ()
registerHelloWorld =
    registerResource getResourceId handler Archived >>
    liftIO (threadDelay 5000000) >>
    updatePeerInResourceMap

getHelloWorld :: (HasP2PEnv m, HasLogging m) => m ()
getHelloWorld = do
    resource <- getResource getResourceId "HelloWorld"
    liftIO $ print "here"
    liftIO $ print resource
