{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module TokenValidation (getUserFromToken, payloadEsRoot, email, email_verified) where

import           Conferer.FromConfig.Warp ()
import           Config
import           Control.Monad.Except
import qualified Data.Aeson as JSON
import           Data.Either.Combinators
import           Data.List (find)
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
  { hd :: Text
  , email :: Text
  , email_verified :: Bool
  , given_name :: Text
  , family_name :: Text
  , name :: Text
  } deriving (Generic, Show, JSON.ToJSON, JSON.FromJSON)

getUserFromToken :: AppConfig -> Redis.Connection -> Request -> IO (Either String GoogleAuthPayload)
getUserFromToken appConfig conn req = runExceptT $ do
  (headerName, headerContent) <- liftEither $ maybeToRight "No hay header de cookies" $
    find (\(headerName, headerContent) -> headerName == hCookie) $ requestHeaders req
  (cookieName, cookieContent) <- liftEither $ maybeToRight "No hay cookie de autorization" $
    find (\(cookieName, cookieContent) -> cookieName == "__patova") $ parseCookies headerContent
  contenidoDeRedis <- ExceptT $ fmap ((maybeToRight "No estaba la key" =<<) . mapLeft (const "Falló redis")) $ Redis.runRedis conn $ do
    Redis.get $ appConfigKeyPrefix appConfig <> cookieContent
  ExceptT $ return $ mapLeft (const "Fallo parsear el usuario") $ JSON.eitherDecodeStrict contenidoDeRedis
