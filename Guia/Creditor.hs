{-# LANGUAGE
  GADTs,
  NoImplicitPrelude,
  OverloadedStrings,
  TemplateHaskell,
  TypeFamilies
  #-}

module Guia.Creditor
       ( -- Creditor
         mkCreditor,
         Creditor, CreditorId,
         sepaId,
         fullName,
         creditorIban,
         messageCount,
         mandateCount,
         activity,
         validCreditor
       ) where

import           ClassyPrelude
import qualified Database.Persist.Quasi                                         as DB
  (upperCaseSettings)
import qualified Database.Persist.TH                                            as DB
  (mkPersist, mpsGenerateLenses, mpsPrefixFields, persistFileWith, share)
import           Guia.MongoSettings
import           Guia.SpanishIban


-- WARNING: the use of lenses (setters) can violate the invariants of the Abstract Data
-- Types in this module.
DB.share [DB.mkPersist mongoSettings { DB.mpsGenerateLenses = True
                                     , DB.mpsPrefixFields   = False }]
  $(DB.persistFileWith DB.upperCaseSettings "Guia/Creditor.persistent")


-- Creditors

mkCreditor :: Text -> Text -> IBAN -> Int -> Int -> Text -> Creditor
mkCreditor id_ name iban messageCount_ mandateCount_ activity_ =
  assert (validCreditor id_ name iban messageCount_ mandateCount_ activity_)
  $ Creditor id_ name iban messageCount_ mandateCount_ activity_

validCreditor :: Text -> Text -> IBAN -> Int -> Int -> Text -> Bool
validCreditor id_ name iban messageCount_ mandateCount_ _activity =
     length id_ == 16
  && not (null name) && length name <= maxLengthFullName -- SEPA constraints
  && validSpanishIban iban
  && messageCount_ >= 0
  && mandateCount_ >= 0
  where
    maxLengthFullName = 70      -- SEPA constraint
