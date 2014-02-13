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

module Guia.BillingConcept
       ( -- BillingConcept
         mkBillingConcept,
         BillingConcept, BillingConceptId,
         shortName,
         longName,
         basePrice,
         vatRatio,
         finalPrice,
         validBillingConcept
       ) where

import           ClassyPrelude
import           Control.Lens
  ((^.))
import qualified Control.Lens                                                   as L
  (Conjoined, Contravariant)
import qualified Control.Lens.Getter                                            as L
  (to)
import qualified Data.List                                                      as LT
import qualified Data.Text.Read                                                 as TXT
  (decimal)
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


-- WARNING: the use of lenses (setters) can violate the invariants of the Abstract Data
-- Types in this module.
DB.share [DB.mkPersist mongoSettings { DB.mpsGenerateLenses = True
                                     , DB.mpsPrefixFields   = False }]
  $(DB.persistFileWith DB.lowerCaseSettings "Guia/BillingConcept.persistent")


-- Billing concepts

mkBillingConcept :: Text -> Maybe Text -> Maybe Double -> Maybe Double -> BillingConcept
mkBillingConcept short long price vat =
  assert (validBillingConcept short long price vat)
  -- We set defaults for basePrice and vatRatio, if not given
  $ BillingConcept short long (fromMaybe 0 price) (fromMaybe 0.21 vat)

validBillingConcept :: Text -> Maybe Text -> Maybe Double -> Maybe Double -> Bool
validBillingConcept short long _price _vat =
     not (null short) && length short <= maxShortNameLength
  && length long <= maxLongNameLength
  where maxShortNameLength = 8  -- To be able to concat many of them in a line
        maxLongNameLength  = 70 -- GUI constraint

-- | Getter for basePrice * vatRatio.
finalPrice :: (Functor f, L.Contravariant f, L.Conjoined p) =>
              p Double (f Double) -> p BillingConcept (f BillingConcept)
finalPrice = L.to _finalPrice

_finalPrice :: BillingConcept -> Double
_finalPrice concept = concept ^. basePrice * concept ^. vatRatio
