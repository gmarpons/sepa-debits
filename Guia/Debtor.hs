{-# LANGUAGE
  FlexibleContexts,
  GADTs,
  GeneralizedNewtypeDeriving,
  NoImplicitPrelude,
  OverloadedStrings,
  QuasiQuotes,
  TemplateHaskell,
  TypeFamilies
  #-}

module Guia.Debtor where

import ClassyPrelude
import Data.Conduit
  (MonadBaseControl)
import Data.Time.Clock
  (NominalDiffTime)
import qualified Database.Persist.MongoDB as DB
  (Action, Database, HostName, Key, MongoBackend,
   insert, master, runMongoDBPool, withMongoDBConn)
import Database.Persist.TH
  (persistLowerCase)
import qualified Database.Persist.TH as DB
  (mkPersist, mkPersistSettings, mpsGeneric, share)
import Language.Haskell.TH.Syntax
  (Type(ConT))
import qualified Network as N
  (PortID(PortNumber))

let mongoSettings = (DB.mkPersistSettings (ConT ''DB.MongoBackend)) {DB.mpsGeneric = False}
  in DB.share [DB.mkPersist mongoSettings] [persistLowerCase|
Debtor
  name Text
  deriving Show Eq Read
|]

main :: IO ()
main = DB.withMongoDBConn dbname hostname port Nothing time $ DB.runMongoDBPool DB.master $ do
  _ <- DB.insert $ Debtor "Guillem"
  return ()
    where
      dbname = "test" :: DB.Database
      hostname = "localhost" :: DB.HostName
      port = N.PortNumber 27017 :: N.PortID
      time = 3600 :: NominalDiffTime -- 1 hour before close connection

withDB :: (MonadIO m, MonadBaseControl IO m) => DB.Action m a -> m a
withDB action = DB.withMongoDBConn dbname hostname port Nothing time runAction
  where
    runAction = DB.runMongoDBPool DB.master action
    dbname = "test" :: DB.Database
    hostname = "localhost" :: DB.HostName
    port = N.PortNumber 27017 :: N.PortID
    time = 3600 :: NominalDiffTime -- 1 hour before close connection

insertDebtor :: Debtor -> IO (DB.Key Debtor)
insertDebtor debtor = withDB $ do
  DB.insert debtor
