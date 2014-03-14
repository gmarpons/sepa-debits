{-# LANGUAGE
  FlexibleContexts,
  NoImplicitPrelude,
  OverloadedStrings
  #-}

module Sepa.MongoUtils where

import           ClassyPrelude
import qualified Control.Monad.Trans.Resource                                   as R
  (MonadBaseControl, ResourceT,
   runResourceT)
import qualified Data.Time.Clock                                                as C
  (NominalDiffTime)
import qualified Database.Persist.MongoDB                                       as DB
  (Action, Database, HostName,
   master, withMongoDBConn, runMongoDBPool)
import qualified Network                                                        as N
  (PortID(PortNumber))

runDb :: (MonadIO m, R.MonadBaseControl IO m) => DB.Action m a -> m a
runDb action = DB.withMongoDBConn dbname hostname port Nothing time runAction
  where
    runAction = DB.runMongoDBPool DB.master action
    dbname = "test" :: DB.Database
    hostname = "localhost" :: DB.HostName
    port = N.PortNumber 27017 :: N.PortID
    time = 2 :: C.NominalDiffTime -- 1 hour before close connection

runResourceDbT :: (MonadIO m, R.MonadBaseControl IO m) =>
                  R.ResourceT (DB.Action m) a -> m a
runResourceDbT = runDb . R.runResourceT
