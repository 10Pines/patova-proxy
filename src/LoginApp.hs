{-# LANGUAGE DeriveGeneric #-}
module LoginApp (makeLoginApp) where

import           Conferer.FromConfig.Warp ()
import           Config
import           Control.Monad.Except
import qualified Data.Aeson as JSON
import           Data.ByteString (ByteString)
import           Data.String.Conversions
import qualified Data.UUID as UUID
import           Data.UUID (UUID)
import qualified Database.Redis as Redis
import qualified Network.HTTP.Client as Client
import           Network.HTTP.Types
import           Network.Wai
import           System.Random (randomIO)
import           Web.Cookie
import qualified Web.OIDC.Client as O
import           Web.Scotty
import           Web.Scotty.Cookie

makeLoginApp :: Redis.Connection -> AppConfig -> O.OIDC -> Client.Manager -> IO Application
makeLoginApp conn appConfig oidc authServerManager = do
  scottyApp $ do
    get "/__/logout" $ do
        setCookie $ defaultSetCookie
          { setCookieName = "__patova"
          , setCookieValue = ""
          , setCookiePath = Just "/"
          }
        redirect "/"

    get "/__/oauth2/callback" $ do
        code <- param @ByteString "code"
        liftIO $ print code
        tokens <- liftIO $ O.requestTokens @JSON.Value oidc Nothing code authServerManager
        liftIO $ putStrLn $ cs $ JSON.encode $ O.otherClaims $ O.idToken $ tokens

        token <- liftIO $ randomIO @UUID
        _ <- liftIO $ Redis.runRedis conn $ do
          _ <- Redis.setex 
            (UUID.toASCIIBytes token) 
            (appConfigSessionDurationSeconds appConfig) 
            (cs $ JSON.encode $ O.otherClaims $ O.idToken tokens)
          return ()
        setCookie $ defaultSetCookie
          { setCookieName = "__patova"
          , setCookieValue = (UUID.toASCIIBytes token)
          , setCookiePath = Just "/"
          }
        redirect "/"

    get "/__/auth/redirect" $ do
        r <- O.getAuthenticationRequestUrl oidc [O.profile, O.email] Nothing [] 
        redirect $ cs $ show r

    matchAny (regex ".*") $ do
      status status403
      setHeader "Content-Type" "text/html"
      file "./login.html"