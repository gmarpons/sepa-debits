{-# LANGUAGE
  FlexibleContexts,
  FlexibleInstances,
  GADTs,
  GeneralizedNewtypeDeriving,
  NoImplicitPrelude,
  OverloadedStrings,
  TemplateHaskell,
  TypeFamilies
  #-}

module Guia.Debtor
       ( Debtor,
         mkSpanishBank,
         SpanishBank,
         validSpanishBank,
         validSpanishBankBic,

         runDB
       ) where

import ClassyPrelude
import qualified Data.Char as C
  (isDigit, isUpper)
import Data.Conduit
  (MonadBaseControl, runResourceT)
import Data.Conduit.Binary
import qualified Data.Csv as CSV
import Data.Csv
  ((.:))
import Data.Time.Calendar
  (Day)
import Data.Time.Clock
  (NominalDiffTime)
import qualified Database.Persist.MongoDB as DB
  (Action, Database, Filter, HostName, Key, PersistEntityBackend,
   PersistMonadBackend, PersistQuery,
   deleteWhere, insert, master, runMongoDBPool, withMongoDBConn)
import qualified Database.Persist.Quasi as DB
  (lowerCaseSettings)
import qualified Database.Persist.TH as DB
  (mkPersist, persistFileWith, share)
import Guia.MongoSettings
import qualified Network as N
  (PortID(PortNumber))

DB.share [DB.mkPersist mongoSettings]
  $(DB.persistFileWith DB.lowerCaseSettings "Guia/Debtor.persistent")

mkSpanishBank :: Text -> Text -> Text -> SpanishBank
mkSpanishBank fourDigitsCode bic name =
  assert (validSpanishBank fourDigitsCode bic name) (SpanishBank fourDigitsCode bic name)

validSpanishBank :: Text -> Text -> Text -> Bool
validSpanishBank fourDigitsCode bic name =
     length fourDigitsCode == 4
  && all C.isDigit fourDigitsCode
  && validSpanishBankBic bic
  && length name <= maxSpanishBankName
  where maxSpanishBankName = 100

validSpanishBankBic :: Text -> Bool
validSpanishBankBic bic =    length bic == 11
                          && all C.isUpper institution
                          && country == "ES"
                          && all isDigitOrUpper location
                          && all isDigitOrUpper branch
  where (institution, suffix) = splitAt 4 bic
        (country, suffix')    = splitAt 2 suffix
        (location, branch)    = splitAt 2 suffix'
        isDigitOrUpper c      = C.isDigit c || C.isUpper c

instance CSV.FromNamedRecord (Maybe SpanishBank) where
  parseNamedRecord r = maybeSpanishBank <$> r .: "spanishBankFourDigitsCode"
                                        <*> r .: "spanishBankBic"
                                        <*> r .: "spanishBankName"
    where maybeSpanishBank fourDigitsCode bic name
            | validSpanishBank fourDigitsCode bic name
                = Just $ SpanishBank fourDigitsCode bic name
            | otherwise = Nothing

-- spanishBanksFromCsvFile :: (MonadBaseControl IO m)
--                            => FilePath -> m (Either String (CSV.Header, Vector SpanishBank))
-- spanishBanksFromCsvFile filePath = runResourceT $ sourceFile filePath $$
          --

runDB :: (MonadIO m, MonadBaseControl IO m) => DB.Action m a -> m a
runDB action = DB.withMongoDBConn dbname hostname port Nothing time runAction
  where
    runAction = DB.runMongoDBPool DB.master action
    dbname = "test" :: DB.Database
    hostname = "localhost" :: DB.HostName
    port = N.PortNumber 27017 :: N.PortID
    time = 3600 :: NominalDiffTime -- 1 hour before close connection

insertDebtor :: Debtor -> IO (DB.Key Debtor)
insertDebtor debtor = runDB $ DB.insert debtor

cleanDebtors :: (DB.PersistQuery m,
                 DB.PersistEntityBackend Debtor ~ DB.PersistMonadBackend m) => m ()
cleanDebtors = DB.deleteWhere ([] :: [DB.Filter Debtor])
