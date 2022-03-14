{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Conduit.Resource.Get.Editor where

import Conduit.App
import Conduit.Core
import Conduit.Link
import qualified Conduit.Model as Model
import qualified Conduit.Template as Template
import qualified Conduit.Database as Database
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

type Route = "editor" :> Auth '[Cookie] Model.User :> HXRequest :> Get '[HTML] (Template.Partial View)

data View = View

instance ToHtml View where
  toHtml _ =
    div_ [class_ "editor-page"] $
      div_ [class_ "container page"] $
        div_ [class_ "row"] $
          div_ [class_ "col-md-10 offset-md-1 col-xs-12"] $
            form_ $
              fieldset_ $ do
                fieldset_ [class_ "form-group"] $ input_ [type_ "text", class_ "form-control form-control-lg", placeholder_ "Article Title"]
                fieldset_ [class_ "form-group"] $ input_ [type_ "text", class_ "form-control", placeholder_ "What's this article about?"]
                fieldset_ [class_ "form-group"] $ textarea_ [class_ "form-control", rows_ "8", placeholder_ "Write your article (in markdown)"] ""
                fieldset_ [class_ "form-group"] $ do
                  input_ [type_ "text", class_ "form-control", placeholder_ "Enter tags"]
                  div_ [class_ "tag-list"] ""
                button_ [class_ "btn btn-lg pull-xs-right btn-primary", type_ "button"] "Publish Article"
  toHtmlRaw = toHtml

handler :: AuthResult Model.User -> Maybe Text -> App (Template.Partial View)
handler (Authenticated user) hxReq =
  case hxReq of
    Just "true" -> pure $ Template.NotWrapped View
    _ -> pure $ Template.Wrapped (Just user) View
handler _ hxReq =
  case hxReq of
    Just "true" -> pure $ Template.NotWrapped View
    _ -> pure $ Template.Wrapped Nothing View
