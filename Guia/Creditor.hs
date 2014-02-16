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

module Guia.Creditor
       ( -- Creditor
         mkCreditor,
         Creditor, CreditorId,
         sepaId,
         fullName,
         creditorIban,
         messageCount,
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

mkCreditor :: Text -> Text -> Text -> Int -> Creditor
mkCreditor id_ name iban count =
  assert (validCreditor id_ name iban count)
  $ Creditor id_ name iban count

validCreditor :: Text -> Text -> Text -> Int -> Bool
validCreditor id_ name iban count =
     length id_ == 16
  && not (null name) && length name <= maxLengthFullName -- SEPA constraints
  && validSpanishIban iban
  && count >= 0
  where
    maxLengthFullName = 70      -- SEPA constraint
