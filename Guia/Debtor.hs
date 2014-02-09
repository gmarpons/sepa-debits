{-# LANGUAGE
  DeriveDataTypeable,
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
       ( -- Debtor
         Debtor, DebtorId,

         -- Mandate
         Mandate, MandateId,

         -- SpanishBank
         mkSpanishBank,
         SpanishBank, SpanishBankId,
         validSpanishBank,
         validSpanishBankBic,

         -- SpanishBankAccount
         SpanishBankAccount, SpanishBankAccountId,

         runDB,
         insertDebtor,
         cleanDebtors
       ) where

import ClassyPrelude
import qualified Data.Char                                                      as CH
  (isDigit, isUpper)
import qualified Control.Monad.Trans.Resource                                   as R
  (MonadBaseControl)
import Data.Time.Calendar
  (Day)
import Data.Time.Clock
  (NominalDiffTime)
import qualified Database.Persist.MongoDB                                       as DB
  (Action, Database, Filter, HostName, Key, PersistEntityBackend,
   PersistMonadBackend, PersistQuery,
   deleteWhere, insert, master, runMongoDBPool, withMongoDBConn)
import qualified Database.Persist.Quasi                                         as DB
  (lowerCaseSettings)
import qualified Database.Persist.TH                                            as DB
  (mkPersist, persistFileWith, share)
import Guia.MongoSettings
import qualified Network                                                        as N
  (PortID(PortNumber))

DB.share [DB.mkPersist mongoSettings]
  $(DB.persistFileWith DB.lowerCaseSettings "Guia/Debtor.persistent")

mkSpanishBank :: Text -> Text -> Text -> SpanishBank
mkSpanishBank fourDigitsCode bic name =
  assert (validSpanishBank fourDigitsCode bic name) (SpanishBank fourDigitsCode bic name)

validSpanishBank :: Text -> Text -> Text -> Bool
validSpanishBank fourDigitsCode bic name =
     length fourDigitsCode == 4
  && all CH.isDigit fourDigitsCode
  && validSpanishBankBic bic
  && length name <= maxSpanishBankName
  where maxSpanishBankName = 100

validSpanishBankBic :: Text -> Bool
validSpanishBankBic bic =    length bic == 11
                          && all CH.isUpper institution
                          && country == "ES"
                          && all isDigitOrUpper location
                          && all isDigitOrUpper branch
  where (institution, suffix) = splitAt 4 bic
        (country, suffix')    = splitAt 2 suffix
        (location, branch)    = splitAt 2 suffix'
        isDigitOrUpper c      = CH.isDigit c || CH.isUpper c

runDB :: (MonadIO m, R.MonadBaseControl IO m) => DB.Action m a -> m a
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
