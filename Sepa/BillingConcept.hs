{-# LANGUAGE
  GADTs,
  NoImplicitPrelude,
  OverloadedStrings,
  TemplateHaskell,
  TypeFamilies
  #-}

module Sepa.BillingConcept
       ( -- BillingConcept
         mkBillingConcept,
         BillingConcept, BillingConceptId,
         shortName,
         longName,
         basePrice,
         vatRatio,
         finalPrice,
         validBillingConcept,
         priceToText
       ) where

import           ClassyPrelude
import           Control.Lens
  ((^.))
import qualified Control.Lens                                                   as L
  (Conjoined, Contravariant)
import qualified Control.Lens.Getter                                            as L
  (to)
import qualified Database.Persist.Quasi                                         as DB
  (upperCaseSettings)
import qualified Database.Persist.TH                                            as DB
  (mkPersist, mpsGenerateLenses, mpsPrefixFields, persistFileWith, share)
import           Sepa.MongoSettings


-- WARNING: the use of lenses (setters) can violate the invariants of the Abstract Data
-- Types in this module.
DB.share [DB.mkPersist mongoSettings { DB.mpsGenerateLenses = True
                                     , DB.mpsPrefixFields   = False }]
  $(DB.persistFileWith DB.upperCaseSettings "Sepa/BillingConcept.persistent")


-- Billing concepts

mkBillingConcept :: Text -> Text -> Maybe Int -> Maybe Int -> BillingConcept
mkBillingConcept short long price vat =
  assert (validBillingConcept short long price vat)
  -- We set defaults for basePrice and vatRatio, if not given
  $ BillingConcept short long (fromMaybe 0 price) (fromMaybe 21 vat)

validBillingConcept :: Text -> Text -> Maybe Int -> Maybe Int -> Bool
validBillingConcept short long price vat =
     not (null short) && length short <= maxLengthShortName
  && length long <= maxLengthLongName
  && fromMaybe 0 price >= 0
  && fromMaybe 0 vat   >= 0
  where maxLengthShortName = 10 -- To be able to concat many of them in a line
        maxLengthLongName  = 70 -- GUI constraint

-- | Getter for basePrice * vatRatio.
finalPrice :: (Functor f, L.Contravariant f, L.Conjoined p) =>
              p Int (f Int) -> p BillingConcept (f BillingConcept)
finalPrice = L.to _finalPrice

_finalPrice :: BillingConcept -> Int
_finalPrice concept = concept ^. basePrice + vat
  where vat = round $ toRational (concept ^. basePrice * concept ^. vatRatio) / 100

-- | Helper function to convert any non-negative Int representing an amount * 100 (all
-- across this module money amounts are stored in this way to avoid fractional digits) to
-- text with two fractional digits.
priceToText :: Int -> Text
priceToText amount = assert (amount >= 0) $ pack amountWithFractional
  where amountReversed = (reverse . show) amount
        amountWithFractional = case amountReversed of
          x : []         ->           "0.0" ++      [x]
          x : y : []     ->           "0."  ++ (y : [x])
          x : y : z : [] ->           z : '.' : y : [x]
          x : y : l      -> reverse l ++ ('.' : y : [x])
          _              -> error "priceToText: show has returned empty list for a number"
