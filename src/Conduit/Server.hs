{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Conduit.Server where

import Conduit.Database
import qualified Conduit.Model as Model
import Conduit.Resource as Resource
import qualified Conduit.Resource as View
import Conduit.Validate (signUpForm)
import Conduit.Validate as Validate
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Function ((&))
import Data.Map.Strict (toList)
import Data.Proxy
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Lucid
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API
import Servant.Auth
import Servant.Auth.Server
import Servant.Htmx
import Servant.Server
import Web.Forma (FormResult (..), runForm, showFieldName)

unprotectedServer :: CookieSettings -> JWTSettings -> Server UnprotectedRoutes
unprotectedServer cookieSettings jwtSettings =
  signUpFormHandler
    :<|> signInFormHandler
    :<|> authorizeSignUp cookieSettings jwtSettings
    :<|> authorizeSignIn cookieSettings jwtSettings
  where
    -- homeHandler :: Maybe Text -> Handler (Resource.Partial Resource.Home)
    -- homeHandler hxReq = pure $ case hxReq of
    --   Just "true" -> Resource.NotWrapped $ Resource.Home Nothing
    --   _ -> Resource.Wrapped Nothing $ Resource.Home Nothing

    signUpFormHandler :: Maybe Text -> Handler (Resource.Partial Resource.SignUpForm)
    signUpFormHandler hxReq = pure $ case hxReq of
      Just "true" -> Resource.NotWrapped $ Resource.SignUpForm Nothing []
      _ -> Resource.Wrapped Nothing $ Resource.SignUpForm Nothing []

    signInFormHandler :: Maybe Text -> Handler (Resource.Partial Resource.SignInForm)
    signInFormHandler hxReq = pure $ case hxReq of
      Just "true" -> Resource.NotWrapped $ Resource.SignInForm Nothing []
      _ -> Resource.Wrapped Nothing $ Resource.SignInForm Nothing []

    authorizeSignUp ::
      CookieSettings ->
      JWTSettings ->
      Model.SignUpForm ->
      Handler (Headers '[HXPush, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Resource.SignUpResponse)
    authorizeSignUp
      cookieSettings
      jwtSettings
      signUpForm = do
        -- Check that submission is valid
        formResult <- runForm Validate.signUpForm (toJSON signUpForm)
        liftIO $ print signUpForm
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
          Succeeded (Model.SignUpForm email password username) -> do
            -- If valid form, write user to DB and return creds
            -- TODO: Write user info to DB and getCreds
            let user = Model.User "" email "https://api.realworld.io/images/smiley-cyrus.jpeg" username -- userCreds
            mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
            liftIO $ print email
            case mApplyCookies of
              Nothing -> throwError err401
              Just applyCookies ->
                applyCookies (Resource.SignUpSuccess user)
                  & addHeader (toUrl homeLink)
                  & pure

    authorizeSignIn ::
      CookieSettings ->
      JWTSettings ->
      Model.SignInForm ->
      Handler (Headers '[HXPush, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Resource.SignInResponse)
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
          Succeeded (Model.SignInForm email password) -> do
            -- If valid form, fetch user from DB and return creds
            -- TODO: Fetch user info from DB and getCreds
            let user = Model.User "" email "https://api.realworld.io/images/smiley-cyrus.jpeg" "rashad" -- userCreds
            mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
            liftIO $ print email
            case mApplyCookies of
              Nothing -> throwError err401
              Just applyCookies ->
                applyCookies (Resource.SignInSuccess user)
                  & addHeader (toUrl homeLink)
                  & pure

protectedServer :: AuthResult Model.User -> Server ProtectedRoutes
protectedServer (Authenticated user) = authHandler user -- if authenticated go to authed routes
protectedServer _ = noAuthHandler -- if not auth is there then redirect accordingly

authHandler :: Model.User -> Server ProtectedRoutes
authHandler user = homeHandler :<|> editorHandler :<|> settingsHandler
  where
    homeHandler :: Maybe Text -> Handler (Partial Home)
    homeHandler hxReq =
      case hxReq of
        Just "true" -> pure $ View.NotWrapped $ View.Home (Just user)
        _ -> pure $ View.Wrapped (Just user) $ View.Home (Just user)

    editorHandler :: Maybe Text -> Handler (Partial Editor)
    editorHandler hxReq = do
      case hxReq of
        Just "true" -> pure $ View.NotWrapped View.Editor
        _ -> pure $ View.Wrapped (Just user) View.Editor

    settingsHandler :: Maybe Text -> Handler (Partial Settings)
    settingsHandler hxReq = do
      case hxReq of
        Just "true" -> pure $ View.NotWrapped $ View.Settings user
        _ -> pure $ View.Wrapped (Just user) $ View.Settings user

noAuthHandler :: Server ProtectedRoutes
noAuthHandler =
  homeHandler
    :<|> redirectFor @(Partial Editor)
    :<|> redirectFor @(Partial Settings)
  where
    homeHandler :: Maybe Text -> Handler (Partial Home)
    homeHandler hxReq = case hxReq of
      Just "true" -> pure $ View.NotWrapped $ View.Home Nothing
      _ -> pure $ View.Wrapped Nothing $ View.Home Nothing

    redirectFor :: forall a. ToHtml a => Maybe Text -> Handler a
    redirectFor _ = throwError $ err303 {errHeaders = [("Location", encodeUtf8 $ toUrl signUpFormLink)]}

server :: CookieSettings -> JWTSettings -> Server Routes
server cookieSettings jwtSettings = protectedServer :<|> unprotectedServer cookieSettings jwtSettings

cookieConfig :: CookieSettings
cookieConfig = defaultCookieSettings {cookieIsSecure = NotSecure, cookieSameSite = SameSiteStrict, cookieXsrfSetting = Nothing}

context :: CookieSettings -> JWTSettings -> Context '[CookieSettings, JWTSettings]
context cookieConfig jwtConfig = cookieConfig :. jwtConfig :. EmptyContext

getJwtConfig :: IO JWTSettings
getJwtConfig = defaultJWTSettings <$> generateKey

runApp :: Int -> IO ()
runApp port = do
  jwtConfig <- getJwtConfig
  let api = Proxy :: Proxy Routes
  run port $ serveWithContext api (context cookieConfig jwtConfig) (server cookieConfig jwtConfig)
