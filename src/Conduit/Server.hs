{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Conduit.Server where

import Conduit.Database
import Conduit.Model (SignInForm (SignInForm), blankSignInForm, blankSignUpForm)
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
import Lucid
import Network.Wai.Handler.Warp (run)
import Servant
import Servant (addHeader)
import Servant.API
import Servant.Auth
import Servant.Auth.Server
import Servant.Htmx
import Servant.Server
import Web.Forma (FormResult (..), runForm, showFieldName)

-- wrapIfHXRequest :: ToHtml a => Maybe Model.User -> a -> Maybe Text -> Handler (Resource.Partial a)
-- wrapIfHXRequest mbUser partial hxRequestHeader = pure $ case hxRequestHeader of
--   Just "true" -> Resource.NotWrapped partial
--   _ -> Resource.Wrapped mbUser partial

unprotectedServer :: CookieSettings -> JWTSettings -> Server UnprotectedRoutes
unprotectedServer cookieSettings jwtSettings =
  homeHandler
    :<|> signUpFormHandler
    :<|> signInFormHandler
    :<|> authorizeSignUp cookieSettings jwtSettings
    :<|> authorizeSignIn cookieSettings jwtSettings
  where
    homeHandler :: Maybe Text -> Handler (Resource.Partial Resource.Home)
    homeHandler hxReq = pure $ case hxReq of
      Just "true" -> Resource.NotWrapped Resource.Home
      _ -> Resource.Wrapped Nothing Resource.Home

    signUpFormHandler :: Maybe Text -> Handler (Resource.Partial Resource.SignUpForm)
    signUpFormHandler hxReq = pure $ case hxReq of
      Just "true" -> Resource.NotWrapped $ Resource.SignUpForm blankSignUpForm []
      _ -> Resource.Wrapped Nothing $ Resource.SignUpForm blankSignUpForm []

    signInFormHandler :: Maybe Text -> Handler (Resource.Partial Resource.SignInForm)
    signInFormHandler hxReq = pure $ case hxReq of
      Just "true" -> Resource.NotWrapped $ Resource.SignInForm blankSignInForm []
      _ -> Resource.Wrapped Nothing $ Resource.SignInForm blankSignInForm []

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
protectedServer (Authenticated user) = handler user
protectedServer Indefinite = throwAll err404
protectedServer _ = throwAll err401

handler :: Model.User -> Maybe Text -> Handler (Partial Home)
handler user hxReq = do
  liftIO $ print "Protected endpoint hit"
  case hxReq of
    Just "true" -> pure $ View.NotWrapped $ View.LoggedInHome user
    _ -> pure $ View.Wrapped (Just user) $ View.LoggedInHome user

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
