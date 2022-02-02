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
import Network.Wai.Handler.Warp (run)
import Servant.Auth
import Servant.Auth.Server
import Servant.Server
import Servant.API
import Servant
import Lucid

unprotectedServer :: CookieSettings -> JWTSettings -> Server UnprotectedRoutes
unprotectedServer cookieSettings jwtSettings =
  pure (Resource.Wrapper Resource.Home)
    :<|> pure Resource.SignUpForm
    :<|> pure Resource.SignInForm
    :<|> authorizeSignUp cookieSettings jwtSettings
    :<|> authorizeSignIn cookieSettings jwtSettings
  where
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
  pure $ h1_ [] "Authorized"
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
