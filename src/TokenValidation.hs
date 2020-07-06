{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module TokenValidation (getUserFromToken, payloadEsRoot, email, email_verified) where

import           Blaze.ByteString.Builder (fromByteString, fromLazyByteString)
import qualified Conferer as C
import           Conferer.FromConfig.Warp ()
import           Control.Monad
import           Control.Monad.Except
import qualified Crypto.Hash as Crypto
import qualified Crypto.MAC.HMAC as Crypto
import qualified Data.Aeson as JSON
import           Data.Binary.Builder (toLazyByteString)
import qualified Data.ByteArray.Encoding as Mem
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BS
import           Data.Either.Combinators
import           Data.List (find)
import           Data.Maybe (fromJust, fromMaybe)
import           Data.String.Conversions
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Text.Lazy as Text (toStrict)
import qualified Data.UUID as UUID
import           Data.UUID (UUID)
import qualified Database.Redis as Redis
import           GHC.Generics
import           LoginApp (makeLoginApp)
import qualified Network.HTTP.Client as Client
import           Network.HTTP.ReverseProxy
import           Network.HTTP.Types
import           Network.HTTP.Types.Header (hCookie, hSetCookie)
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp
import           System.Random (randomIO)
import           Web.Cookie
import           Web.Scotty
import           Web.Scotty.Cookie

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

getUserFromToken :: Redis.Connection -> Request -> IO (Either String GoogleAuthPayload)
getUserFromToken conn req = runExceptT $ do
  (headerName, headerContent) <- liftEither $ maybeToRight "No hay header de cookies" $
    find (\(headerName, headerContent) -> headerName == hCookie) $ requestHeaders req
  (cookieName, cookieContent) <- liftEither $ maybeToRight "No hay cookie de autorization" $
    find (\(cookieName, cookieContent) -> cookieName == "__patova") $ parseCookies headerContent
  contenidoDeRedis <- ExceptT $ fmap ((maybeToRight "No estaba la key" =<<) . mapLeft (const "Falló redis")) $ Redis.runRedis conn $ do
    Redis.get cookieContent
  ExceptT $ return $ mapLeft (const "Fallo parsear el usuario") $ JSON.eitherDecodeStrict contenidoDeRedis
