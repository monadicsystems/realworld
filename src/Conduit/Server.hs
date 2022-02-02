{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Conduit.Server where

import Conduit.Database
import Conduit.Model (SignInForm (SignInForm), blankSignUpForm)
import qualified Conduit.Model as Model
import Conduit.Resource as Resource
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
import Conduit.Validate (signUpForm)

wrapIfHXRequest :: ToHtml a => a -> Maybe Text -> Handler (Resource.Partial a)
wrapIfHXRequest partial hxRequestHeader = pure $ case hxRequestHeader of
  Just "true" -> Resource.NotWrapped partial
  _ -> Resource.Wrapped partial

unprotectedServer :: CookieSettings -> JWTSettings -> Server UnprotectedRoutes
unprotectedServer cookieSettings jwtSettings =
  homeHandler
    :<|> signUpFormHandler
    :<|> signInFormHandler
    :<|> authorizeSignUp cookieSettings jwtSettings
    :<|> authorizeSignIn cookieSettings jwtSettings
  where
    homeHandler :: Maybe Text -> Handler (Resource.Partial Resource.Home)
    homeHandler = wrapIfHXRequest Resource.Home

    signUpFormHandler :: Maybe Text -> Handler (Resource.Partial Resource.SignUpForm)
    signUpFormHandler = wrapIfHXRequest $ Resource.SignUpForm blankSignUpForm []

    signInFormHandler :: Maybe Text -> Handler (Resource.Partial Resource.SignInForm)
    signInFormHandler = wrapIfHXRequest Resource.SignInForm

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
            let user = Model.User "" email "" username -- userCreds
            mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
            liftIO $ print email
            case mApplyCookies of
              Nothing -> throwError err401
              Just applyCookies ->
                applyCookies Resource.SignUpSuccess
                  & addHeader (toUrl homeLink)
                  & pure

    authorizeSignIn ::
      CookieSettings ->
      JWTSettings ->
      Model.SignInForm ->
      Handler (Headers '[HXPush, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Resource.SignInResponse)
    authorizeSignIn cookieSettings jwtSettings (Model.SignInForm email password) = undefined

protectedServer :: AuthResult Model.User -> Server ProtectedRoutes
protectedServer (Authenticated user) = wrapIfHXRequest $ Resource.Profile user
protectedServer _ = throwAll err401

server :: CookieSettings -> JWTSettings -> Server Routes
server cookieSettings jwtSettings = unprotectedServer cookieSettings jwtSettings :<|> protectedServer

runApp :: Int -> IO ()
runApp port = do
  -- We *also* need a key to sign the cookies
  myKey <- generateKey
  -- Adding some configurations. 'Cookie' requires, in addition to
  -- CookieSettings, JWTSettings (for signing), so everything is just as before
  let jwtCfg = defaultJWTSettings myKey
      cfg = defaultCookieSettings {cookieIsSecure = NotSecure} :. jwtCfg :. EmptyContext
      --- Here is the actual change
      api = Proxy :: Proxy Routes
  run port $ serveWithContext api cfg (server defaultCookieSettings jwtCfg)
