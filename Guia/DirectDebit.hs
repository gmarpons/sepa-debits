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

module Guia.DirectDebit
       ( -- Direct debit instructions set
         mkDirectDebitSet,
         DirectDebitSet, DirectDebitSetId,
         description,
         creationTime,
         creditor,
         debits,
         creationDay,
         creationTimeOfDay,
         validDirectDebitSet,
         storableDirectDebitSet,
         storableDebits,

         -- Direct debit instruction
         mkDirectDebit,
         DirectDebit, DirectDebitId,
         debtorFirstName,
         debtorLastName,
         mandate,
         items,
         validDirectDebit,
         storableDirectDebit
       ) where

import           ClassyPrelude
import           Control.Lens
import qualified Data.List                                                      as LT
import qualified Data.Time.Calendar                                             as T
import qualified Data.Time.LocalTime                                            as T
import qualified Database.Persist.Quasi                                         as DB
  (upperCaseSettings)
import qualified Database.Persist.TH                                            as DB
  (mkPersist, mpsGenerateLenses, mpsPrefixFields, persistFileWith, share)
import           Guia.BillingConcept
import           Guia.Creditor
import           Guia.Debtor
import           Guia.MongoSettings


-- WARNING: the use of lenses (setters) can violate the invariants of the Abstract Data
-- Types in this module.
DB.share [DB.mkPersist mongoSettings { DB.mpsGenerateLenses = True
                                     , DB.mpsPrefixFields   = False }]
  $(DB.persistFileWith DB.upperCaseSettings "Guia/DirectDebit.persistent")


-- Direct debit instruction set

mkDirectDebitSet :: Text -> T.ZonedTime -> Creditor -> [DirectDebit] -> DirectDebitSet
mkDirectDebitSet descr creation_ creditor_ debits_ =
  assert (validDirectDebitSet descr creation_ creditor_ debits_)
  $ DirectDebitSet descr creation_ creditor_ debits_

-- Cannot check meaningful creation date outside IO
validDirectDebitSet :: Text -> T.ZonedTime -> Creditor -> [DirectDebit] -> Bool
validDirectDebitSet  descr _creation_ _creditor_ debits_ =
     not (null descr) && length descr <= maxLengthDescr
  -- && length (mkMessageId creation_ creditor_) == lengthMessageId
  && validDebits debits_
  where maxLengthDescr  = 40
        -- lengthMessageId = 35    -- SEPA constraint

validDebits :: [DirectDebit] -> Bool
validDebits _debits_ = True

-- | Check things that are not part of the invariant of the data type, but are necessary
-- both to store in the database and to generate message file.
storableDirectDebitSet :: DirectDebitSet -> IO Bool
storableDirectDebitSet col =
  if validDirectDebitSet descr creation_ creditor_ debits_
  then do
    zonedTime <- T.getZonedTime
    let currentLocalTime  = T.zonedTimeToLocalTime zonedTime
        creationLocalTime = T.zonedTimeToLocalTime creation_
    -- FIXME: problem if creation and validation in different time zones
    return $ creationLocalTime <= currentLocalTime && storableDebits debits_
  else return False             -- storable implies valid
  where descr      = col ^. description
        creation_  = col ^. creationTime
        creditor_  = col ^. creditor
        debits_    = col ^. debits

storableDebits :: [DirectDebit] -> Bool
storableDebits debits_ =
     not (null debits_)
     -- All mandates are different
  && length (LT.nub allMandateRefs) == length allMandateRefs
     -- All debit instructions have items
  && all (not . null) (debits_ ^.. traversed.items)
  where allMandateRefs = debits_ ^.. traversed.mandate.mandateRef

creationDay :: (Gettable f, Conjoined p) =>
               p T.Day (f T.Day) -> p DirectDebitSet (f DirectDebitSet)
creationDay = to _creationDay

_creationDay :: DirectDebitSet -> T.Day
_creationDay col = T.localDay (T.zonedTimeToLocalTime (col ^. creationTime))

creationTimeOfDay :: (Gettable f, Conjoined p) =>
                     p T.TimeOfDay (f T.TimeOfDay) -> p DirectDebitSet (f DirectDebitSet)
creationTimeOfDay = to _creationTimeOfDay

_creationTimeOfDay :: DirectDebitSet -> T.TimeOfDay
_creationTimeOfDay col = T.localTimeOfDay (T.zonedTimeToLocalTime (col ^. creationTime))


-- Direct debit instructions

mkDirectDebit :: Text -> Text -> Mandate -> [BillingConcept] -> DirectDebit
mkDirectDebit first_ last_ mandate_ items_ =
  assert (validDirectDebit first_ last_ mandate_ items_)
  $ DirectDebit first_ last_ mandate_ items_

validDirectDebit :: Text -> Text -> Mandate -> [BillingConcept] -> Bool
validDirectDebit firstName_ lastName_ _mandate_ _items_ =
     validDebtorName firstName_ lastName_

-- | Check things that are not part of the invariant of the data type, but are necessary
-- both to store in the database and to generate message file.
storableDirectDebit :: Text -> Text -> Mandate -> [BillingConcept] -> Bool
storableDirectDebit = validDirectDebit
