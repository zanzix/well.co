{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE OverloadedStrings   #-}

module Server
  ( runServer
  ) where

import Network.Wai
import Servant
import Database.Persist.Sqlite as DB 
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader

import Model 
import Api 

server :: ServerT API HandlerM
server = inputApi :<|> queryApi 

nt :: Config -> HandlerM a -> Handler a
nt s x = runReaderT x s

app :: Config -> Application
app conf = serve api $ hoistServer api (nt conf) server

runServer :: IO Application
runServer  = do
  pool <- runStderrLoggingT $ do
    createSqlitePool "well.db3" 50
  runSqlPool (runMigration migrateAll) pool
  return $ app (Config pool)