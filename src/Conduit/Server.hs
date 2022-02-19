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
import Conduit.Validate (signUpForm)
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

unprotectedServer :: CookieSettings -> JWTSettings -> Server UnprotectedRoutes
unprotectedServer cookieSettings jwtSettings =
  signUpFormHandler
    :<|> signInFormHandler
    :<|> authorizeSignUp cookieSettings jwtSettings
    :<|> authorizeSignIn cookieSettings jwtSettings
  where
    signUpFormHandler :: Maybe Text -> App (Resource.Partial Resource.SignUpForm)
    signUpFormHandler hxReq = pure $ case hxReq of
      Just "true" -> Resource.NotWrapped $ Resource.SignUpForm Nothing []
      _ -> Resource.Wrapped Nothing $ Resource.SignUpForm Nothing []

    signInFormHandler :: Maybe Text -> App (Resource.Partial Resource.SignInForm)
    signInFormHandler hxReq = pure $ case hxReq of
      Just "true" -> Resource.NotWrapped $ Resource.SignInForm Nothing []
      _ -> Resource.Wrapped Nothing $ Resource.SignInForm Nothing []

    authorizeSignUp ::
      CookieSettings ->
      JWTSettings ->
      Model.SignUpForm ->
      App (Headers '[HXPush, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Resource.SignUpResponse)
    authorizeSignUp
      cookieSettings
      jwtSettings
      signUpForm = do
        -- Check that submission is valid
        formResult <- runForm Validate.signUpForm (toJSON signUpForm)
        case formResult of
          ParsingFailed _ parseErr ->
            [parseErr]
              & SignUpFailure signUpForm
              & noHeader
              & noHeader
              & noHeader
              & pure
          ValidationFailed errMap ->
            errMap
              & toList
              & Prelude.map (\(field, f) -> f $ showFieldName field)
              & SignUpFailure signUpForm
              & noHeader
              & noHeader
              & noHeader
              & pure
          Succeeded signUpForm -> do
            -- If valid form, write user to DB and return creds
            -- TODO: Write user info to DB and getCreds
            result <- insertUser signUpForm
            case result of
              Left err ->
                SignUpFailure signUpForm [pack $ show err]
                  & noHeader
                  & noHeader
                  & noHeader
                  & pure
              Right user -> do
                mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
                case mApplyCookies of
                  Nothing -> throwError err401
                  Just applyCookies ->
                    Resource.SignUpSuccess user
                      & applyCookies
                      & addHeader (toUrl homeLink)
                      & pure

    authorizeSignIn ::
      CookieSettings ->
      JWTSettings ->
      Model.SignInForm ->
      App (Headers '[HXPush, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Resource.SignInResponse)
    authorizeSignIn
      cookieSettings
      jwtSettings
      signInForm = do
        -- Check that submission is valid
        formResult <- runForm Validate.signInForm (toJSON signInForm)
        liftIO $ print signInForm
        case formResult of
          ParsingFailed _ parseErr ->
            [parseErr]
              & SignInFailure signInForm
              & noHeader
              & noHeader
              & noHeader
              & pure
          ValidationFailed errMap ->
            errMap
              & toList
              & Prelude.map (\(field, f) -> f $ showFieldName field)
              & SignInFailure signInForm
              & noHeader
              & noHeader
              & noHeader
              & pure
          Succeeded signInForm -> do
            -- If valid form, fetch user from DB and return creds
            -- TODO: Fetch user info from DB and getCreds
            result <- verifyUser signInForm
            case result of
              Left _ ->
                SignInFailure signInForm ["Incorrect username or password"]
                  & noHeader
                  & noHeader
                  & noHeader
                  & pure
              Right user -> do
                mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
                case mApplyCookies of
                  Nothing -> throwError err401
                  Just applyCookies ->
                    applyCookies (Resource.SignInSuccess user)
                      & addHeader (toUrl homeLink)
                      & pure

protectedServer :: AuthResult Model.User -> ServerT ProtectedRoutes App
protectedServer (Authenticated user) = authHandler user -- if authenticated go to authed routes
protectedServer _ = noAuthHandler -- if not auth is there then redirect accordingly

authHandler :: Model.User -> ServerT ProtectedRoutes App
authHandler user =
  homeHandler
    :<|> followHandler
    :<|> unfollowHandler
    :<|> editorHandler
    :<|> settingsHandler
    :<|> profileHandler
    :<|> logoutHandler
  where
    homeHandler :: Maybe Text -> App (Partial Home)
    homeHandler hxReq =
      case hxReq of
        Just "true" -> pure $ View.NotWrapped $ View.Home (Just user)
        _ -> pure $ View.Wrapped (Just user) $ View.Home (Just user)

    followHandler :: Model.FollowForm -> App UnfollowButton
    followHandler = undefined

    unfollowHandler :: Model.UnfollowForm -> App FollowButton
    unfollowHandler = undefined

    editorHandler :: Maybe Text -> App (Partial Editor)
    editorHandler hxReq =
      case hxReq of
        Just "true" -> pure $ View.NotWrapped View.Editor
        _ -> pure $ View.Wrapped (Just user) View.Editor

    settingsHandler :: Maybe Text -> App (Partial Settings)
    settingsHandler hxReq =
      case hxReq of
        Just "true" -> pure $ View.NotWrapped $ View.Settings user
        _ -> pure $ View.Wrapped (Just user) $ View.Settings user

    profileHandler :: Maybe Text -> Text -> App (Partial Profile)
    profileHandler hxReq atUsername = do
      let username = T.drop 1 atUsername
      userResult <- getUserByUsername username
      case userResult of
        Left _ -> throwError err401
        Right user' -> do
          if Model.userID user == Model.userID user'
            then
              case hxReq of
                Just "true" -> pure $ View.NotWrapped $ View.PrivateProfile user
                _ -> pure $ View.Wrapped (Just user) $ View.PrivateProfile user
            else do
              -- See if user is following the other
              isFollowingResult <- doesFollowExist user user'
              case isFollowingResult of
                Left _ -> throwError err401
                Right isFollowing ->
                  case hxReq of
                    Just "true" -> pure $ View.NotWrapped $ View.PublicProfile user' isFollowing
                    _ -> pure $ View.Wrapped (Just user) $ View.PublicProfile user' isFollowing

    logoutHandler :: App (Headers '[HXPush, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Resource.SignOutResponse)
    logoutHandler = do
      SignOutResponse
      & noHeader
      & noHeader
      & addHeader (toUrl homeLink)
      & pure

redirectFor :: forall a. App a
redirectFor = throwError $ err303
  { errHeaders = [("Location", encodeUtf8 $ toUrl signUpFormLink)]
  }

noAuthHandler :: Server ProtectedRoutes
noAuthHandler =
  homeHandler
    :<|> (\_ -> redirectFor @UnfollowButton)
    :<|> (\_ -> redirectFor @FollowButton)
    :<|> (\_ -> redirectFor @(Partial Editor))
    :<|> (\_ -> redirectFor @(Partial Settings))
    :<|> profileHandler
    :<|> redirectFor @(Headers '[HXPush, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] SignOutResponse)
  where
    homeHandler :: Maybe Text -> App (Partial Home)
    homeHandler hxReq = case hxReq of
      Just "true" -> pure $ View.NotWrapped $ View.Home Nothing
      _ -> pure $ View.Wrapped Nothing $ View.Home Nothing

    profileHandler :: Maybe Text -> Text -> App (Partial Profile)
    profileHandler hxReq atUsername = do
      let username = T.drop 1 atUsername
      result <- getUserByUsername username
      case result of
        Left _ -> throwError err401
        Right user -> do
          case hxReq of
            Just "true" -> pure $ View.NotWrapped $ View.PublicProfile user False
            _ -> pure $ View.Wrapped Nothing $ View.PublicProfile user False

server :: CookieSettings -> JWTSettings -> Server Routes
server cookieSettings jwtSettings = protectedServer :<|> unprotectedServer cookieSettings jwtSettings

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
  -- runUncheckedSqlIO dbConn dropTagsSession

  -- CREATE TABLES
  -- runUncheckedSqlIO dbConn createUsersSession
  -- runUncheckedSqlIO dbConn createArticlesSession
  -- runUncheckedSqlIO dbConn createTagsSession
  -- runUncheckedSqlIO dbConn createCommentsSession
  -- runUncheckedSqlIO dbConn createArticlesTagsSession
  -- runUncheckedSqlIO dbConn createFollowsSession

  -- DB SETUP END --

  let app :: AppConfig -> Application
      app c =
        serveWithContext api (context cookieConfig jwtConfig) $
          hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings]) (runAppAsHandler c) (server cookieConfig jwtConfig)
  run port $ app appConfig
