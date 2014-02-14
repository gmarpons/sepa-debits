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
       ( -- Direct debit instructions collection
         mkDirectDebitCollection,
         DirectDebitCollection, DirectDebitCollectionId,
         description,
         messageId,
         creationTime,
         creditor,
         debits,
         validDirectDebitCollection,
         storableDirectDebitCollection,
         storableDebits,
         mkMessageId,

         -- Direct debit instruction
         mkDirectDebit,
         DirectDebit, DirectDebitId,
         debtorFirstName,
         debtorLastName,
         mandate,
         items,
         unstructured,
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
import qualified Text.Printf                                                    as PF
  (printf)

-- WARNING: the use of lenses (setters) can violate the invariants of the Abstract Data
-- Types in this module.
DB.share [DB.mkPersist mongoSettings { DB.mpsGenerateLenses = True
                                     , DB.mpsPrefixFields   = False }]
  $(DB.persistFileWith DB.upperCaseSettings "Guia/DirectDebit.persistent")


-- Direct debit instruction collection

mkDirectDebitCollection :: Text -> T.ZonedTime -> Creditor -> [DirectDebit] ->
                           DirectDebitCollection
mkDirectDebitCollection descr creation_ creditor_ debits_ =
  assert (validDirectDebitCollection descr creation_ creditor_ debits_)
  $ DirectDebitCollection descr messageId_ creation_ creditor_ debits_
  where messageId_ = mkMessageId creation_ creditor_

-- | Creates a SEPA message Id following Spanish Q19.14 instructions. Its construction
-- guarantees the SEPA constraint of exactly 35 alphanumeric characters.
mkMessageId :: T.ZonedTime -> Creditor -> Text
mkMessageId creation_ creditor_ = "PRE" ++ yyyymmdd ++ hhmmss ++ milis ++ counter
  where
    yyyymmdd          = pack $ filter (/= '-') $ T.showGregorian (T.localDay localTime)
    (hhmmss', milis') = break (== '.') $ show (T.localTimeOfDay localTime)
    hhmmss            = pack $ filter (/= ':') hhmmss'
    milis             = pack $ take 5 $ dropWhile (== '.') (milis' ++ repeat '0')
                        -- messageCount is updated elsewhere
    counter           = pack $ PF.printf "%013d" (creditor_ ^. messageCount)
    localTime = T.zonedTimeToLocalTime creation_

-- Cannot check meaningful creation date outside IO
validDirectDebitCollection :: Text -> T.ZonedTime -> Creditor -> [DirectDebit] -> Bool
validDirectDebitCollection  descr creation_ creditor_ debits_ =
     not (null descr) && length descr <= maxLengthDescr
  && length (mkMessageId creation_ creditor_) == lengthMessageId
  && validDebits debits_
  where maxLengthDescr  = 40
        lengthMessageId = 35    -- SEPA constraint

validDebits :: [DirectDebit] -> Bool
validDebits _debits_ = True

-- | Check things that are not part of the invariant of the data type, but are necessary
-- both to store in the database and to generate message file.
storableDirectDebitCollection :: DirectDebitCollection -> IO Bool
storableDirectDebitCollection col =
  if validDirectDebitCollection descr creation_ creditor_ debits_
  then -- do
    -- zonedTime <- T.getZonedTime
    -- let currentLocalTime  = T.zonedTimeToLocalTime zonedTime
    --     creationLocalTime = T.zonedTimeToLocalTime creation_
    return $ storableDebits debits_
             {- && creationLocalTime `before` currentLocalTime-}
  else return False             -- storable implies valid
  where descr      = col ^. description
        creation_  = col ^. creationTime
        creditor_  = col ^. creditor
        debits_    = col ^. debits
        -- today             = T.localDay currentLocalTime
        -- now               = T.localTimeOfDay currentLocalTime
        -- creationDay       = T.localDay creationLocalTime
        -- creationTimeOfDay = T.localTimeOfDay creationLocalTime
        -- before t1 t2 =

storableDebits :: [DirectDebit] -> Bool
storableDebits debits_ =
     not (null debits_)
     -- All mandates are different
  && length (LT.nub allMandateRefs) == length allMandateRefs
     -- All debit instructions have items
  && all (not . null) (debits_ ^.. traversed.items)
  where allMandateRefs = debits_ ^.. traversed.mandate.mandateRef


-- Direct debit instructions

mkDirectDebit :: Text -> Text -> Mandate -> [BillingConcept] -> Text -> DirectDebit
mkDirectDebit first_ last_ mandate_ items_ unstructured_ =
  assert (validDirectDebit first_ last_ mandate_ items_ unstructured_)
  $ DirectDebit first_ last_ mandate_ items_ unstructured_

validDirectDebit :: Text -> Text -> Mandate -> [BillingConcept] -> Text -> Bool
validDirectDebit firstName_ lastName_ _mandate_ _items_ unstructured_ =
     validDebtorName firstName_ lastName_
  && length unstructured_ <= maxLengthUnstructured
  where maxLengthUnstructured = 140 -- SEPA constraint (2.89), WARNING: min length is 1,
                                    -- but checked in storableDirectDebit

-- | Check things that are not part of the invariant of the data type, but are necessary
-- both to store in the database and to generate message file.
storableDirectDebit :: Text -> Text -> Mandate -> [BillingConcept] -> Text -> Bool
storableDirectDebit firstName_ lastName_ _mandate_ _items_ unstructured_ =
     validDirectDebit firstName_ lastName_ _mandate_ _items_ unstructured_
  && not (null unstructured_)   -- SEPA constraint
