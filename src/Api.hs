{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts  #-}

module Api
  ( queryApi
  , inputApi
  , Config (..)
  , HandlerM
  , API
  , api
  ) where

import Servant
import Data.Text
import Database.Persist.Sqlite as DB 
import Control.Monad.Reader 

import Model

data Config = Config
  { getPool :: ConnectionPool
  }

type HandlerM = ReaderT Config Handler

type API = 
       "input" :> ReqBody '[PlainText] Text :> Post '[PlainText] NoContent
  :<|> "query" :> QueryParam "key" Text :> Get '[PlainText] Text

api :: Proxy API
api = Proxy

runDb :: (MonadReader Config m, MonadIO m) => ReaderT SqlBackend IO b -> m b
runDb query = do
  pool <- asks getPool
  liftIO $ runSqlPool query pool

queryApi :: Maybe Text -> HandlerM Text
queryApi mkey = case mkey of
  Nothing -> throwError err400
  Just key -> do
    num <- runDb $ DB.count [KeyKey ==. key]
    _ <- runDb $ insert (Key key)
    return $ pack $ show num

inputApi :: Text -> HandlerM NoContent
inputApi str = do 
  _ <- runDb $ insert (Str str)
  return NoContent
