{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}

module Conduit.App where

import Control.Monad.Trans.Reader hiding (asks)
import Control.Exception (catch, throwIO, try)
import Control.Monad.Error
import Control.Monad.Reader
import Hasql.Connection (Connection)
import Servant.Server
import Servant.Server.Generic (AsServerT)

data AppConfig = AppConfig
    { appConfigDBConnection :: Connection
    }

newtype App a = App
  { unApp :: ReaderT AppConfig IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppConfig)

-- TODO: Fix instance
instance MonadError ServerError App where
    throwError :: ServerError -> App a
    throwError = liftIO . throwIO
    {-# INLINE throwError #-}

    catchError :: App a -> (ServerError -> App a) -> App a
    catchError action handler = App $ ReaderT $ \env -> do
        let ioAction = runApp env action
        ioAction `catch` \e -> runApp env $ handler e
    {-# INLINE catchError #-}

class Has field env where
    obtain :: env -> field

instance Has Connection AppConfig where
    obtain = appConfigDBConnection

grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field

runApp :: AppConfig -> App a -> IO a
runApp env = (flip runReaderT env) . unApp
