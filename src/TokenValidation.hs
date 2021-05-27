{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module TokenValidation (getUserFromToken, payloadEsRoot, email, email_verified) where

import           Conferer.FromConfig.Warp ()
import           Config
import           Control.Monad.Except
import qualified Data.Aeson as JSON
import           Data.Either.Combinators
import           Data.List (find)
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Database.Redis as Redis
import           GHC.Generics
import           Network.HTTP.Types
import           Network.Wai
import           Web.Cookie

data BackofficePayload =
  BackofficePayload
  { payloadUid :: Int
  , payloadEmail :: Text
  , payloadUsername :: Text
  , payloadFullName :: Text
  , payloadEsRoot :: Bool
  } deriving (Show, Eq, Generic)

instance JSON.ToJSON BackofficePayload
instance JSON.FromJSON BackofficePayload

data GoogleAuthPayload = GoogleAuthPayload
  { hd :: Maybe Text
  , email :: Text
  , email_verified :: Bool
  , given_name :: Text
  , family_name :: Text
  , name :: Text
  } deriving (Generic, Show, JSON.ToJSON, JSON.FromJSON)

getUserFromToken :: AppConfig -> Redis.Connection -> Request -> IO (Either String GoogleAuthPayload)
getUserFromToken appConfig conn req = runExceptT $ do
  (_headerName, headerContent) <- liftEither $ maybeToRight "" $
    find (\(headerName, _headerContent) -> headerName == hCookie) $ requestHeaders req
  (_cookieName, cookieContent) <- liftEither $ maybeToRight "No hay cookie de autorization" $
    find (\(cookieName, _cookieContent) -> cookieName == "__patova") $ parseCookies headerContent
  contenidoDeRedis <- ExceptT $ fmap ((maybeToRight "Authorization not valid (probably expired)" =<<) . mapLeft (const "Connection with redis failed, try again later")) $
    Redis.runRedis conn $ do
      Redis.get $ appConfigKeyPrefix appConfig <> cookieContent
  user <- ExceptT $ return $ mapLeft (const "Failed to decode user from database, try logging in again") $ JSON.eitherDecodeStrict contenidoDeRedis
  let validUsers = filter (/= "") $ Text.split (== ',') $ appConfigAllowedUsers appConfig
  if appConfigAllowedUsers appConfig == "*"
    then do
      return user
    else do
      unless (email_verified user) $
        liftEither $ Left "User didn't validate their email, it must be validate before continuing"
      unless (email user `elem` validUsers) $
        liftEither $ Left "You are not allowed to see this page, you must log in as a user who has access."
      return user
