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

module Conduit.Resource.Get.Form.Article where

import qualified Conduit.Model as Model
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

data Editor = Editor

instance ToHtml Editor where
  toHtml Editor =
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

