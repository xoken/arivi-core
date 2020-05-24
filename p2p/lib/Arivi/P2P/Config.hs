{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arivi.P2P.Config
    ( module Arivi.P2P.Config
    ) where

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
        , ariviLogFile :: T.Text
        , sbound :: Int
        , pingThreshold :: Int
        , kademliaConcurrencyFactor :: Int
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

instance FromJSON PortNumber where
    parseJSON v = fromInteger <$> parseJSON v

instance FromJSON SecretKey where
    parseJSON v = throwCryptoError . Crypto.PubKey.Ed25519.secretKey <$> (parseJSON v :: Parser ByteString)

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
        Left e -> throw e
        Right con -> return con

-- | Encode as string of human-readable hex characters.
encodeHex :: ByteString -> Text
encodeHex = E.decodeUtf8 . B16.encode

-- | Decode string of human-readable hex characters.
decodeHex :: Text -> Maybe ByteString
decodeHex text =
    let (x, b) = B16.decode (E.encodeUtf8 text)
     in guard (b == BS.empty) >> return x
