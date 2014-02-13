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
         mkDebtor,
         Debtor, DebtorId,
         firstName,
         lastName,
         mandates,
         registrationDate,
         validDebtor,
         validDebtorName,

         -- Mandate
         mkMandate,
         Mandate, MandateId,
         mandateRef,
         iban,
         signatureDate,
         lastTimeActive,
         validMandate,

         -- SpanishBank
         mkSpanishBank,
         SpanishBank, SpanishBankId,
         fourDigitsCode,
         bic,
         bankName,
         validSpanishBank,
         validSpanishBankBic,

         insertDebtor,
         cleanDebtors
       ) where

import           ClassyPrelude
import           Control.Lens
  ((^.))
import qualified Data.Char                                                      as CH
  (isDigit, isUpper)
import qualified Data.List                                                      as LT
import qualified Data.Time.Calendar                                             as T
import qualified Database.Persist.MongoDB                                       as DB
  (Filter, Key, PersistEntityBackend,
   PersistMonadBackend, PersistQuery,
   deleteWhere, insert)
import qualified Database.Persist.Quasi                                         as DB
  (lowerCaseSettings)
import qualified Database.Persist.TH                                            as DB
  (mkPersist, mpsGenerateLenses, mpsPrefixFields, persistFileWith, share)
import           Guia.MongoSettings
import           Guia.MongoUtils
import           Guia.SpanishIban


-- WARNING: the use of lenses (setters) can violate the invariants of the Abstract Data
-- Types in this module.
DB.share [DB.mkPersist mongoSettings { DB.mpsGenerateLenses = True
                                     , DB.mpsPrefixFields   = False }]
  $(DB.persistFileWith DB.lowerCaseSettings "Guia/Debtor.persistent")


-- Debtors

mkDebtor :: Text -> Text -> [Mandate] -> T.Day -> Debtor
mkDebtor firstName_ lastName_ mandates_ registrationDate_ =
  assert (validDebtor firstName_ lastName_ mandates_ registrationDate_)
  $ Debtor firstName_ lastName_ mandates_ registrationDate_

validDebtor :: Text -> Text -> [Mandate] -> T.Day -> Bool
validDebtor firstName_ lastName_ mandates_ _registrationDate_ =
     validDebtorName firstName_ lastName_
  && validMandateList mandates_

validDebtorName :: Text -> Text -> Bool
validDebtorName firstName_ lastName_ =
     not (null firstName_) && length firstName_ <= maxLengthFirstName
  && not (null lastName_)  && length lastName_  <= maxLengthLastName
  && length lastName_  + length firstName_      <= maxLengthFullName
  where maxLengthFirstName = 40
        maxLengthLastName  = 60
        maxLengthFullName  = 70 -- SEPA constraint (2.72)


-- | We want mandates ordered from more recent to oldest.
validMandateList :: [Mandate] -> Bool
validMandateList mandates_ = LT.and lastTimePairs
  where
    lastTimeActiveList   = map (^. lastTimeActive) mandates_
    lastTimeActiveList'  = Nothing : Nothing : lastTimeActiveList
    lastTimeActiveList'' = Nothing : lastTimeActiveList'
    lastTimePairs        = LT.zipWith newerThan lastTimeActiveList'' lastTimeActiveList'
    newerThan Nothing _           = True
    newerThan (Just _) Nothing    = False
    newerThan (Just d1) (Just d2) = T.diffDays d1 d2 >= 0


-- Mandates

mkMandate :: Text -> Text -> T.Day -> Maybe T.Day -> Mandate
mkMandate ref iban_ signatureDate_ lastTimeActive_ =
  assert (validMandate ref iban_ signatureDate_ lastTimeActive_)
  $ Mandate ref iban_ signatureDate_ lastTimeActive_

-- In fact more characters are allowed in mandate reference code (but not all), but for
  -- simplicity we constraint us to digits + space.
validMandate :: Text -> Text -> T.Day -> Maybe T.Day -> Bool
validMandate ref iban_ _signatureDate_ _lastTimeActive_ =
     length ref == 35 && all (\c -> CH.isDigit c || c == ' ') ref
  && validSpanishIban iban_


-- Spanish banks

mkSpanishBank :: Text -> Text -> Text -> SpanishBank
mkSpanishBank fourDigitsCode_ bic_ bankName_ =
  assert (validSpanishBank fourDigitsCode_ bic_ bankName_)
  $ SpanishBank fourDigitsCode_ bic_ bankName_

validSpanishBank :: Text -> Text -> Text -> Bool
validSpanishBank fourDigitsCode_ bic_ bankName_ =
     length fourDigitsCode_ == 4 && all CH.isDigit fourDigitsCode_
  && validSpanishBankBic bic_
  && length bankName_ <= maxLengthBankName
  where maxLengthBankName = 100

validSpanishBankBic :: Text -> Bool
validSpanishBankBic bic_ =   length bic_ == 11
                          && all CH.isUpper institution
                          && country == "ES"
                          && all isDigitOrUpper location
                          && all isDigitOrUpper branch
  where (institution, suffix) = splitAt 4 bic_
        (country, suffix')    = splitAt 2 suffix
        (location, branch)    = splitAt 2 suffix'
        isDigitOrUpper c      = CH.isDigit c || CH.isUpper c

insertDebtor :: Debtor -> IO (DB.Key Debtor)
insertDebtor debtor = runDb $ DB.insert debtor

cleanDebtors :: (DB.PersistQuery m,
                 DB.PersistEntityBackend Debtor ~ DB.PersistMonadBackend m) => m ()
cleanDebtors = DB.deleteWhere ([] :: [DB.Filter Debtor])
