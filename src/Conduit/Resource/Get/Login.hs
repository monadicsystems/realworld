{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Conduit.Resource.Get.Login where

import Conduit.App
import Conduit.Core
import Conduit.Link
import qualified Conduit.Model as Model
import qualified Conduit.Template as Template
import Control.Monad (forM_)
import Data.Proxy
import Data.Text
import Lucid
import Lucid.Htmx
import Lucid.Htmx.Servant
import Lucid.Hyperscript (useHyperscript, __, _hs)
import Servant
import Servant.Auth
import Servant.Auth.Server
import Servant.HTML.Lucid (HTML)
import Servant.Htmx
import Servant.Links
import Servant.Server

data View = View (Maybe Model.LoginForm) [Text]

instance ToHtml View where
  toHtml (View mbLoginForm errors) =
    div_ [class_ "auth-page"] $
      div_ [class_ "container page"] $
        div_ [class_ "row"] $
          div_ [class_ "col-md-6 offset-md-3 col-xs-12"] $ do
            h1_ [class_ "text-xs-center"] "Sign in"
            p_ [class_ "text-xs-center"] $ a_ [hxBoost_ "true", hxTarget_ "#content-slot", hxPushUrlSafe_ (Left True), href_ registerFormLink] "Need an account?"
            case errors of
              [] -> ""
              errors' -> ul_ [class_ "error-messages"] $ mapM_ (li_ [] . toHtml) errors'
            form_ [hxPost_ loginLink, hxTarget_ "#content-slot"] $ do
              fieldset_ [class_ "form-group"] $
                input_
                  [ class_ "form-control form-control-lg",
                    type_ "text",
                    name_ "loginFormEmail",
                    placeholder_ "Email",
                    value_ $ maybe "" Model.loginFormEmail mbLoginForm
                  ]
              fieldset_ [class_ "form-group"] $
                input_
                  [ class_ "form-control form-control-lg",
                    type_ "password",
                    name_ "loginFormPassword",
                    placeholder_ "Password",
                    value_ $ maybe "" Model.loginFormPassword mbLoginForm
                  ]
              button_ [class_ "btn btn-lg btn-primary pull-xs-right"] "Sign in"
  toHtmlRaw = toHtml

type Route = "login" :> HXRequest :> Get '[HTML] (Template.Partial View)

handler :: Maybe Text -> App (Template.Partial View)
handler hxReq = pure $ case hxReq of
  Just "true" -> Template.NotWrapped $ View Nothing []
  _ -> Template.Wrapped Nothing $ View Nothing []
