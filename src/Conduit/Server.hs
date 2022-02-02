{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Conduit.Server where

import Conduit.Database
import Conduit.Model (SignInForm (SignInForm))
import qualified Conduit.Model as Model
import Conduit.Resource as Resource
import Control.Monad.IO.Class (liftIO)
import Data.Proxy
import Data.Text
import Lucid
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API
import Servant.Auth
import Servant.Auth.Server
import Servant.Server

unprotectedServer :: CookieSettings -> JWTSettings -> Server UnprotectedRoutes
unprotectedServer cookieSettings jwtSettings =
  homeHandler
    :<|> signUpFormHandler
    :<|> signInFormHandler
    :<|> authorizeSignUp cookieSettings jwtSettings
    :<|> authorizeSignIn cookieSettings jwtSettings
  where
    wrapIfHXRequest :: ToHtml a => a -> Maybe Text -> Handler (Resource.Partial a)
    wrapIfHXRequest partial hxRequestHeader = pure $ case hxRequestHeader of
      Just "true" -> Resource.NotWrapped partial
      _ -> Resource.Wrapped partial

    homeHandler :: Maybe Text -> Handler (Resource.Partial Resource.Home)
    homeHandler = wrapIfHXRequest Resource.Home

    signUpFormHandler :: Maybe Text -> Handler (Resource.Partial Resource.SignUpForm)
    signUpFormHandler = wrapIfHXRequest Resource.SignUpForm

    signInFormHandler :: Maybe Text -> Handler (Resource.Partial Resource.SignInForm)
    signInFormHandler = wrapIfHXRequest Resource.SignInForm

    authorizeSignUp ::
      CookieSettings ->
      JWTSettings ->
      Model.SignUpForm ->
      Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Resource.Profile)
    authorizeSignUp cookieSettings jwtSettings (Model.SignUpForm email password username) = do
      -- TODO: ADD actual stuff
      let usr = Model.User "" email "" username
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
      case mApplyCookies of
        Nothing -> throwError err401
        Just applyCookies -> return $ applyCookies Resource.Profile
    authorizeSignIn cookieSettings jwtSettings Model.SignInForm = undefined

protectedServer :: AuthResult Model.User -> Server ProtectedRoutes
protectedServer (Authenticated user) =
  pure Resource.Profile
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
