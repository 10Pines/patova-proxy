{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Config where

import qualified Conferer as C
import           Conferer.FromConfig.Hedis ()
import           Conferer.FromConfig.Warp ()
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Database.Redis as Redis
import           GHC.Generics
import           Network.Wai.Handler.Warp as Warp

instance Show Settings where
  show _ = "Settings {}"

data AppConfig = AppConfig
  { appConfigSessionDurationSeconds :: Integer
  , appConfigProxy :: ProxyConfig
  , appConfigServer :: Settings
  , appConfigOidc :: OidcConfig
  , appConfigRedis :: Redis.ConnectInfo
  , appConfigKeyPrefix :: ByteString
  , appConfigExternalUrl :: ByteString
  , appConfigAllowedUsers :: Text
  } deriving (Generic, Show)

instance C.DefaultConfig AppConfig where
  configDef = AppConfig
    { appConfigSessionDurationSeconds = 60 * 60 * 24
    , appConfigProxy = C.configDef
    , appConfigKeyPrefix = "patova/"
    , appConfigOidc = C.configDef
    , appConfigServer = setPort 3334 defaultSettings
    , appConfigRedis = C.configDef
    , appConfigExternalUrl = "http://localhost:3334/__/oauth2/callback"
    , appConfigAllowedUsers = ""
    }
instance C.FromConfig AppConfig

data ProxyConfig = ProxyConfig 
  { proxyConfigPort :: Int
  , proxyConfigHost :: ByteString
  } deriving (Generic, Show)

instance C.DefaultConfig ProxyConfig where
  configDef = ProxyConfig 
    { proxyConfigPort = 4567
    , proxyConfigHost = "127.0.0.1"
    }
instance C.FromConfig ProxyConfig

data OidcConfig = OidcConfig
  { oidcConfigClientBaseUrl :: Text
  , oidcConfigClientId :: ByteString
  , oidcConfigClientSecret :: ByteString
  } deriving (Generic, Show)

instance C.DefaultConfig OidcConfig where
  configDef = OidcConfig
    { oidcConfigClientBaseUrl = "https://accounts.google.com/"
    , oidcConfigClientId = error "Missing client id"
    , oidcConfigClientSecret = error "Missing client secret"
    }
instance C.FromConfig OidcConfig