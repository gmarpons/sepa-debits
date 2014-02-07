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

module Guia.Debtor
       (Debtor,
        mkSpanishBank,
        SpanishBank,
        validSpanishBank,
        validSpanishBankBic)
       where

import ClassyPrelude
import qualified Data.Char as C
  (isDigit, isUpper)
import Data.Conduit
  (MonadBaseControl)
import Data.Time.Calendar
  (Day)
import Data.Time.Clock
  (NominalDiffTime)
import qualified Database.Persist.MongoDB as DB
  (Action, Database, Filter, HostName, Key, MongoBackend, PersistEntityBackend,
   PersistMonadBackend, PersistQuery,
   deleteWhere, insert, master, runMongoDBPool, withMongoDBConn)
import Database.Persist.TH
  (persistLowerCase)
import qualified Database.Persist.TH as DB
  (mkPersist, mkPersistSettings, mpsGeneric)
import Language.Haskell.TH.Syntax
  (Type(ConT))
import qualified Network as N
  (PortID(PortNumber))

let mongoSettings = (DB.mkPersistSettings (ConT ''DB.MongoBackend)) {DB.mpsGeneric = False}
  in DB.mkPersist mongoSettings [persistLowerCase|

Debtor sql=debtors
  firstName        Text
  lastName         Text
  activeMandate    Mandate Maybe
  oldMandates      [Mandate]
  registrationDate Day
  deriving Show Eq Read Ord

Mandate sql=mandates
  reference     Text
  account       SpanishBankAccount
  signatureDate Day
  -- Unique constraints
  UniqueMandateReference reference
  deriving Show Eq Read Ord

SpanishBankAccount sql=spanishBankAccounts
  iban   Text
  bankId SpanishBankId
  deriving Show Eq Read Ord

SpanishBank sql=spanishBanks
  fourDigitsCode Text
  bic            Text       -- ISO 9362 (also known as SWIFT-BIC, SWIFT ID or SWIFT code)
  name           Text
  -- Unique constraints
  UniqueSpanishBankFourDigitsCode fourDigitsCode
  UniqueSpanishBankBic            bic
  deriving Show Eq Read Ord
|]

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

runDB :: (MonadIO m, MonadBaseControl IO m) => DB.Action m a -> m a
runDB action = DB.withMongoDBConn dbname hostname port Nothing time runAction
  where
    runAction = DB.runMongoDBPool DB.master action
    dbname = "test" :: DB.Database
    hostname = "localhost" :: DB.HostName
    port = N.PortNumber 27017 :: N.PortID
    time = 3600 :: NominalDiffTime -- 1 hour before close connection

insertDebtor :: Debtor -> IO (DB.Key Debtor)
insertDebtor debtor = runDB $ do
  DB.insert debtor

cleanDebtors :: (DB.PersistQuery m,
                 DB.PersistEntityBackend Debtor ~ DB.PersistMonadBackend m) => m ()
cleanDebtors = do
  DB.deleteWhere ([] :: [DB.Filter Debtor])
