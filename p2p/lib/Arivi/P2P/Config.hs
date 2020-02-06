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
import Control.Monad (guard)
import Crypto.Error (throwCryptoError)
import Crypto.PubKey.Ed25519
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Char8 as C
import Data.Maybe
import Data.Text as T
import qualified Data.Text.Encoding as E
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
        , listenIP :: String
        , logFile :: T.Text
        , sbound :: Int
        , pingThreshold :: Int
        , kademliaConcurrencyFactor :: Int
        , endPointListenIP :: String
        , endPointListenPort :: PortNumber
        }
    deriving (Show, Generic)

instance FromJSON ByteString where
    parseJSON = withText "ByteString" $ \t -> pure $ fromJust (decodeHex t)

instance FromJSON NodeEndPoint where
    parseJSON (Object v) = NodeEndPoint <$> v .: "nodeIp" <*> v .: "udpPort" <*> v .: "tcpPort"
    parseJSON _ = error "Can't parse NodeEndPoint from YAML"

instance FromJSON Peer where
    parseJSON (Object v) = Peer <$> v .: "nodeID" <*> v .: "nodeEndPoint"
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
    toJSON a = String $ encodeHex a

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

-- | Encode as string of human-readable hex characters.
encodeHex :: ByteString -> Text
encodeHex = E.decodeUtf8 . B16.encode

-- | Decode string of human-readable hex characters.
decodeHex :: Text -> Maybe ByteString
decodeHex text =
    let (x, b) = B16.decode (E.encodeUtf8 text)
     in guard (b == BS.empty) >> return x
