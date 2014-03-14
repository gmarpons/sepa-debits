{-# LANGUAGE
  NoImplicitPrelude,
  TemplateHaskell
  #-}

module Sepa.MongoSettings
       ( mongoSettings
       ) where

import           ClassyPrelude
import qualified Database.Persist.MongoDB                                       as DB
  (MongoBackend)
import qualified Database.Persist.TH                                            as DB
  (MkPersistSettings,
   mkPersistSettings, mpsGeneric)
import qualified Language.Haskell.TH.Syntax                                     as DB
  (Type(ConT))

mongoSettings :: DB.MkPersistSettings
mongoSettings = (DB.mkPersistSettings (DB.ConT ''DB.MongoBackend)) {DB.mpsGeneric = False}
