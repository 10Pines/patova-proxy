{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
module Lib where

import qualified Conferer as C
import           Config
import qualified Data.Aeson as JSON
import           Data.Aeson ((.=))
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe (fromJust)
import           Data.String.Conversions
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.UUID as UUID
import           Data.UUID (UUID)
import qualified Database.Redis as Redis
import           LoginApp (makeLoginApp)
import qualified Network.HTTP.Client as Client
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.ReverseProxy
import           Network.HTTP.Types
import           Network.HTTP.Types (status200)
import           Network.HTTP.Types.Header
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as WaiLogger
import           System.Random (randomIO)
import qualified Text.Mustache as Mustache
import qualified TokenValidation as User
import           Web.Cookie
import qualified Web.OIDC.Client as OIDC

startPatovaProxy :: IO ()
startPatovaProxy = do
  manager <- Client.newManager $ Client.defaultManagerSettings { Client.managerConnCount = 0, Client.managerIdleConnectionCount = 0 }
  conf <- C.defaultConfigWithDefaults "patova" []
  appConfig <- C.getFromConfig @AppConfig "" conf
  putStrLn $ "started proxy on " ++ show (getPort $ appConfigServer $ appConfig)

  Redis.withCheckedConnect (appConfigRedis appConfig) $ \conn -> do
    authServerManager <- Client.newManager tlsManagerSettings
    let mkOidc = do
          prov <- OIDC.discover (oidcConfigClientBaseUrl $ appConfigOidc appConfig) authServerManager
          pure $ OIDC.setCredentials
                      (oidcConfigClientId $ appConfigOidc appConfig)
                      (oidcConfigClientSecret $ appConfigOidc appConfig)
                      (appConfigExternalUrl appConfig) $ OIDC.newOIDC prov
    loginApp <- makeLoginApp conn appConfig mkOidc authServerManager
    runSettings (appConfigServer appConfig) $
      WaiLogger.logStdout $ waiProxyTo (proxyOrHandleRequest appConfig conn mkOidc authServerManager) defaultOnExc manager

proxyOrHandleRequest :: AppConfig -> Redis.Connection -> IO OIDC.OIDC -> Client.Manager -> Request -> IO WaiProxyResponse
proxyOrHandleRequest appConfig conn mkOidc authServerManager req = do
  case pathInfo req of
    ["__", "logout"] -> do
        let cookie = toLazyByteString $ renderSetCookie $ defaultSetCookie
                      { setCookieName = "__patova"
                      , setCookieValue = ""
                      , setCookiePath = Just "/"
                      }
        return $ WPRResponse $
          responseLBS status200 [(hSetCookie, cs cookie)] "<html><head><meta http-equiv=\"refresh\" content=\"0;URL='/'\"></head><body>Redirecting</body></html>"

    ["__", "oauth2", "callback"] -> do
        let code = fromJust $ snd $ head $ filter ((== "code") . fst) $ queryString req
        oidc <- mkOidc
        tokens <- OIDC.requestTokens @JSON.Value oidc Nothing code authServerManager

        token <- randomIO @UUID
        _ <- Redis.runRedis conn $ do
          _ <- Redis.setex
            (appConfigKeyPrefix appConfig <> UUID.toASCIIBytes token)
            (appConfigSessionDurationSeconds appConfig)
            (cs $ JSON.encode $ OIDC.otherClaims $ OIDC.idToken tokens)
          return ()
        let cookie = toLazyByteString $ renderSetCookie $ defaultSetCookie
                      { setCookieName = "__patova"
                      , setCookieValue = cs $ show token
                      , setCookiePath = Just "/"
                      }
        return $ WPRResponse $
          responseLBS status200 [(hSetCookie, cs cookie)] "<html><head><meta http-equiv=\"refresh\" content=\"0;URL='/'\"></head><body>Redirecting</body></html>"

    [ "__", "auth", "redirect"] -> do
        oidc <- mkOidc
        r <- OIDC.getAuthenticationRequestUrl oidc [OIDC.profile, OIDC.email] Nothing []
        return $ WPRResponse $
          responseLBS status302 [(hLocation, cs $ show r)] ""

    _ -> do
      User.getUserFromToken appConfig conn req
      >>= \case
        Left e -> do
          print e
          template <- Mustache.compileMustacheFile "login.mustache"
          let renderedTemplate =
                Mustache.renderMustache template $ JSON.object
                  (if e == "" then [] else [ "error" .= (cs e :: Text) ])
          return $ WPRResponse $
            responseLBS status200 [] $ cs renderedTemplate
        Right user -> do
          let
            newHeaders = ("X-Jaimdal-Auth", LBS.toStrict $ JSON.encode user) : requestHeaders req
            outgoingRequest = req { requestHeaders = newHeaders }
          return $ WPRModifiedRequest outgoingRequest $
              ProxyDest (proxyConfigHost $ appConfigProxy appConfig) (proxyConfigPort $ appConfigProxy appConfig)
