{-# LANGUAGE DataKinds #-}

module Conduit.Server where

import Conduit.Database
import Conduit.View
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Server

type Routes = Get '[HTML] Home

server :: Server Routes
server = return Home
