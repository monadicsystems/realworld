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

module Conduit.Resource.Get.GlobalFeed where

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

type Route = "feed" :> "global" :> Get '[HTML] Template.Feed

handler :: App Template.Feed
handler = do
  articlesResult <- Database.getAllArticles
  case articlesResult of
    Left queryErr -> throwError err401
    Right articles -> do
      articleInfos <- Database.getArticleInfos articles
      pure $ Template.Feed
        False
        [("Global Feed", globalFeedLink, True)]
        articleInfos
