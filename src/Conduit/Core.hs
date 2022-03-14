{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Conduit.Core where

-- import Conduit.App
-- import Control.Monad (forM_)
import Data.Proxy
import Data.Text
-- import Lucid
-- import Lucid.Htmx
-- import Lucid.Htmx.Servant
-- import Lucid.Hyperscript (useHyperscript, __, _hs)
import Servant
-- import Servant.Auth
-- import Servant.Auth.Server
import Servant.HTML.Lucid (HTML)
-- import Servant.Htmx
-- import Servant.Links
-- import Servant.Server

type MakeRoute method path resource = path :> method '[HTML] resource

proxy :: forall a. Proxy a
proxy = Proxy

textShow :: Show a => a -> Text
textShow = pack . show
