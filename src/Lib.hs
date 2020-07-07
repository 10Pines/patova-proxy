{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
module Lib where

import qualified Conferer as C
import           Config
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Database.Redis as Redis
import           LoginApp (makeLoginApp)
import qualified Network.HTTP.Client as Client
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.ReverseProxy
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as WaiLogger
import qualified TokenValidation as User
import qualified Web.OIDC.Client as OIDC

startPatovaProxy :: IO ()
startPatovaProxy = do
  manager <- Client.newManager $ Client.defaultManagerSettings { Client.managerConnCount = 0, Client.managerIdleConnectionCount = 0 }
  conf <- C.defaultConfigWithDefaults "patova" []
  appConfig <- C.getFromConfig @AppConfig "" conf
  putStrLn $ "started proxy on " ++ show (getPort $ appConfigServer $ appConfig)

  Redis.withCheckedConnect (appConfigRedis appConfig) $ \conn -> do
    authServerManager <- Client.newManager tlsManagerSettings
    prov <- OIDC.discover (oidcConfigClientBaseUrl $ appConfigOidc appConfig) authServerManager
    let oidc = OIDC.setCredentials
                (oidcConfigClientId $ appConfigOidc appConfig)
                (oidcConfigClientSecret $ appConfigOidc appConfig)
                (appConfigExternalUrl appConfig) $ OIDC.newOIDC prov
    loginApp <- makeLoginApp conn appConfig oidc authServerManager
    runSettings (appConfigServer appConfig) $
      WaiLogger.logStdout $ waiProxyTo (proxyOrHandleRequest appConfig conn loginApp) defaultOnExc manager

proxyOrHandleRequest :: AppConfig -> Redis.Connection -> Application -> Request -> IO WaiProxyResponse
proxyOrHandleRequest appConfig conn loginApp req = do
  if rawPathInfo req == "/__/logout"
    then return $ WPRApplication loginApp
    else
      User.getUserFromToken appConfig conn req
      >>= \case
        Left e -> do
          print e
          return $ WPRApplication loginApp
        Right user -> do
          let
            newHeaders = ("X-Jaimdal-Auth", LBS.toStrict $ JSON.encode user) : requestHeaders req
            outgoingRequest = req { requestHeaders = newHeaders }
            validUsers = filter (/= "") $ Text.split (== ',') $ appConfigAllowedUsers appConfig
          if (appConfigAllowedUsers appConfig == "*" || (User.email user `elem` validUsers && User.email_verified user))
            then return $ WPRModifiedRequest outgoingRequest $
                   ProxyDest (proxyConfigHost $ appConfigProxy appConfig) (proxyConfigPort $ appConfigProxy appConfig)
            else return $ WPRApplication loginApp
