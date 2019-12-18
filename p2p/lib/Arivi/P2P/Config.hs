<<<<<<< HEAD
{-# LANGUAGE DeriveGeneric     #-}
=======
{-# LANGUAGE DeriveGeneric #-}
>>>>>>> breaking out arivi-core from arivi
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arivi.P2P.Config
    ( module Arivi.P2P.Config
    ) where

<<<<<<< HEAD
import           Arivi.P2P.Kademlia.Types (NodeEndPoint, Peer (..))
import           Control.Exception
import           Crypto.Error             (throwCryptoError)
import           Crypto.PubKey.Ed25519
import qualified Data.ByteArray           as BA
import           Data.ByteString.Char8
import           Data.Text                as T
import           Data.Yaml
import           GHC.Generics
import           Network.Socket

data Config = Config
    { tcpPort                      :: PortNumber
    , udpPort                      :: PortNumber
    , secretKey                    :: SecretKey
    , trustedPeers                 :: [Peer]
    , myNodeId                     :: ByteString
    , myIp                         :: String
    , logFile                      :: T.Text
    , sbound                       :: Int
    , pingThreshold                :: Int
    , kademliaConcurrencyFactor    :: Int
    } deriving (Show, Generic)

instance FromJSON ByteString where
    parseJSON =
        withText "ByteString" $ \t ->
            pure $ Data.ByteString.Char8.pack (T.unpack t)

instance FromJSON Peer

instance FromJSON NodeEndPoint
=======
import Arivi.P2P.Kademlia.Types
import Control.Exception
import Crypto.Error (throwCryptoError)
import Crypto.PubKey.Ed25519
import qualified Data.ByteArray as BA
import Data.ByteString.Char8
import Data.Text as T
import Data.Yaml
import GHC.Generics
import Network.Socket

data Config =
    Config
        { tcpPort :: PortNumber
        , udpPort :: PortNumber
        , secretKey :: SecretKey
        , trustedPeers :: [Peer]
        , myNodeId :: ByteString
        , myIp :: String
        , logFile :: T.Text
        , sbound :: Int
        , pingThreshold :: Int
        , kademliaConcurrencyFactor :: Int
        , ipcListenPort :: PortNumber
        , ipcRemotePort :: PortNumber
        }
    deriving (Show, Generic)

instance FromJSON ByteString where
    parseJSON = withText "ByteString" $ \t -> pure $ Data.ByteString.Char8.pack (T.unpack t)

instance FromJSON NodeEndPoint where
    parseJSON (Object v) = NodeEndPoint <$> v .: "nodeIp" <*> v .: "udpPort" <*> v .: "tcpPort"
    parseJSON _ = error "Can't parse NodeEndPoint from YAML"

instance FromJSON Peer where
    parseJSON (Object v) = Peer <$> v .: "nodeID" <*> v .: "endPoint"
    parseJSON _ = error "Can't parse Peer"
>>>>>>> breaking out arivi-core from arivi

instance FromJSON PortNumber where
    parseJSON v = fromInteger <$> parseJSON v

instance FromJSON SecretKey where
<<<<<<< HEAD
    parseJSON v =
        throwCryptoError . Crypto.PubKey.Ed25519.secretKey <$>
        (parseJSON v :: Parser ByteString)
=======
    parseJSON v = throwCryptoError . Crypto.PubKey.Ed25519.secretKey <$> (parseJSON v :: Parser ByteString)
>>>>>>> breaking out arivi-core from arivi

instance FromJSON Config

instance ToJSON ByteString where
    toJSON a = String $ T.pack (Data.ByteString.Char8.unpack a)

instance ToJSON Peer

instance ToJSON NodeEndPoint

instance ToJSON PortNumber where
    toJSON = Number . fromInteger . toInteger

instance ToJSON SecretKey where
    toJSON sk = toJSON (BA.convert sk :: ByteString)

instance ToJSON Config

makeConfig :: Config -> FilePath -> IO ()
makeConfig config configPath = encodeFile configPath config

readConfig :: FilePath -> IO Config
readConfig path = do
    config <- decodeFileEither path :: IO (Either ParseException Config)
    case config of
<<<<<<< HEAD
        Left e    -> throw e
=======
        Left e -> throw e
>>>>>>> breaking out arivi-core from arivi
        Right con -> return con
