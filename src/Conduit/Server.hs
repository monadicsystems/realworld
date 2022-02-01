{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Conduit.Server where

import Conduit.Database
import Conduit.Model (SignInForm (SignInForm))
import qualified Conduit.Model as Model
import qualified Conduit.View as View
import Control.Monad.IO.Class (liftIO)
import Data.Text
import Lucid
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server
import Servant.HTML.Lucid (HTML)
import Servant.Htmx
import Servant.Server

type HomeRoute = Get '[HTML] View.Home

type SignUpFormRoute = "register" :> Get '[HTML] View.SignUpForm

type SignInFormRoute = "login" :> Get '[HTML] View.SignInForm

type SignUpFormSubmitRoute =
  "sign-up"
    :> ReqBody '[JSON] Model.SignUpForm
    :> Post '[HTML] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] View.Profile)

type SignInFormSubmitRoute =
  "sign-in"
    :> ReqBody '[JSON] Model.SignInForm
    :> Post '[HTML] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] View.Profile)

type UnprotectedRoutes =
  HomeRoute
    :<|> SignUpFormRoute
    :<|> SignInFormRoute
    :<|> SignUpFormSubmitRoute
    :<|> SignInFormSubmitRoute

unprotectedServer :: CookieSettings -> JWTSettings -> Server UnprotectedRoutes
unprotectedServer cookieSettings jwtSettings =
  pure View.Home
    :<|> pure View.SignUpForm
    :<|> pure View.SignInForm
    :<|> authorizeSignUp cookieSettings jwtSettings
    :<|> authorizeSignIn cookieSettings jwtSettings
  where
    authorizeSignUp ::
      CookieSettings ->
      JWTSettings ->
      Model.SignUpForm ->
      Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] View.Profile)
    authorizeSignUp cookieSettings jwtSettings Model.SignUpForm = do
      let usr = Model.User "Ali Baba" "ali@email.com" "" ""
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
      case mApplyCookies of
        Nothing -> throwError err401
        Just applyCookies -> return $ applyCookies View.Profile
    authorizeSignIn cookieSettings jwtSettings Model.SignInForm = undefined

type ProtectedRoutes = Get '[HTML] (Html ())

protectedServer :: AuthResult Model.User -> Server ProtectedRoutes
protectedServer (Authenticated user) =
  pure $ h1_ [] "Authorized"
protectedServer _ = throwAll err401

type Routes auths =
  UnprotectedRoutes
    :<|> Auth auths Model.User :> ProtectedRoutes

server :: CookieSettings -> JWTSettings -> Server (Routes auths)
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
      api = Proxy :: Proxy (Routes '[Cookie])
  run port $ serveWithContext api cfg (server defaultCookieSettings jwtCfg)
