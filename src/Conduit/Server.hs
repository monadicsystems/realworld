{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Conduit.Server where

import Conduit.App
import Conduit.Database
import qualified Conduit.Model as Model
import Conduit.Resource as Resource
import qualified Conduit.Resource as View
import Conduit.Validate (registerForm)
import Conduit.Validate as Validate
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Function ((&))
import Data.Map.Strict (toList)
import Data.Proxy
import Data.Text
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Hasql.Connection as Connection
import Lucid
import Network.Wai.Handler.Warp (run)
import Servant hiding (Server)
import Servant.API
import Servant.Auth
import Servant.Auth.Server
import Servant.Htmx
import Servant.Server hiding (Server)
import Web.Forma (FormResult (..), runForm, showFieldName)

type Server api = ServerT api App

server :: CookieSettings -> JWTSettings -> Server Routes
server cookieSettings jwtSettings =
  homeHandler
  :<|> articleHandler
  :<|> followHandler
  :<|> unfollowHandler
  :<|> publishFormHandler
  :<|> editArticleFormHandler
  :<|> settingsHandler
  :<|> profileHandler
  :<|> logoutHandler
  :<|> registerFormHandler
  :<|> loginFormHandler
  :<|> registerHandler cookieSettings jwtSettings
  :<|> loginHandler cookieSettings jwtSettings
  :<|> publishHandler
  :<|> editArticleHandler
  :<|> globalFeedHandler
  :<|> tagFeedHandler
  :<|> authorFeedHandler
  :<|> favoritesFeedHandler

context :: CookieSettings -> JWTSettings -> Context '[CookieSettings, JWTSettings]
context cookieConfig jwtConfig = cookieConfig :. jwtConfig :. EmptyContext

getJwtConfig :: IO JWTSettings
getJwtConfig = defaultJWTSettings <$> generateKey

getAppConfig :: IO AppConfig
getAppConfig = do
  dbSecret <- BS.readFile "secret.txt"
  print dbSecret
  let dbConnSettings = Connection.settings "localhost" 5432 "realworld" dbSecret "realworld"

  connResult <- Connection.acquire dbConnSettings

  case connResult of
    Left err -> error $ show err
    Right conn -> pure $ AppConfig conn

runAppAsHandler :: forall a. AppConfig -> App a -> Handler a
runAppAsHandler c x = liftIO $ runReaderT (unApp x) c

runApp :: Int -> IO ()
runApp port = do
  let api = Proxy :: Proxy Routes
      cookieConfig :: CookieSettings
      cookieConfig = defaultCookieSettings
        { cookieIsSecure = NotSecure
        , cookieSameSite = SameSiteStrict
        , cookieXsrfSetting = Nothing
        }

  jwtConfig <- getJwtConfig
  appConfig <- getAppConfig

  let (AppConfig dbConn) = appConfig

  -- DB SETUP START --
  -- DROP TABLES IF THEY EXIST
  -- runUncheckedSqlIO dbConn dropCommentsSession
  -- runUncheckedSqlIO dbConn dropArticlesTagsSession
  -- runUncheckedSqlIO dbConn dropArticlesSession
  -- runUncheckedSqlIO dbConn dropFollowsSession
  -- runUncheckedSqlIO dbConn dropUsersSession

  -- CREATE TABLES
  runUncheckedSqlIO dbConn createUsersSession
  runUncheckedSqlIO dbConn createArticlesSession
  runUncheckedSqlIO dbConn createCommentsSession
  runUncheckedSqlIO dbConn createArticlesTagsSession
  runUncheckedSqlIO dbConn createFollowsSession

  -- DB SETUP END --

  let app :: AppConfig -> Application
      app c =
        serveWithContext api (context cookieConfig jwtConfig) $
          hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings]) (runAppAsHandler c) (server cookieConfig jwtConfig)
  run port $ app appConfig
