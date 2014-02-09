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

         insertDebtor,
         cleanDebtors
       ) where

import           ClassyPrelude
import qualified Data.Char                                                      as CH
  (isDigit, isUpper)
import qualified Data.Time.Calendar                                             as C
  (Day)
import qualified Database.Persist.MongoDB                                       as DB
  (Filter, Key, PersistEntityBackend,
   PersistMonadBackend, PersistQuery,
   deleteWhere, insert)
import qualified Database.Persist.Quasi                                         as DB
  (lowerCaseSettings)
import qualified Database.Persist.TH                                            as DB
  (mkPersist, persistFileWith, share)
import           Guia.MongoSettings
import           Guia.MongoUtils

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

insertDebtor :: Debtor -> IO (DB.Key Debtor)
insertDebtor debtor = runDb $ DB.insert debtor

cleanDebtors :: (DB.PersistQuery m,
                 DB.PersistEntityBackend Debtor ~ DB.PersistMonadBackend m) => m ()
cleanDebtors = DB.deleteWhere ([] :: [DB.Filter Debtor])
