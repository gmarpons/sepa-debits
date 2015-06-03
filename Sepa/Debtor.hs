{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Sepa.Debtor
       ( -- Debtor
         mkDebtor,
         Debtor, DebtorId,
         firstName,
         lastName,
         mandates,
         registrationDate,
         validDebtor,
         validDebtorName,
         getActiveMandate,

         -- Mandate
         mkMandate,
         Mandate, MandateId,
         mandateRef,
         iban,
         signatureDate,
         lastTimeActive,
         isNew,
         validMandate,
         updateMandatesLastTimeActive,

         -- SpanishBank
         mkSpanishBank,
         SpanishBank, SpanishBankId,
         fourDigitsCode,
         bic,
         bankName,
         validSpanishBank,
         validSpanishBankBic
       ) where

import           ClassyPrelude
import           Control.Lens
import qualified Data.Char                as CH (isDigit, isUpper)
import qualified Data.List                as LT
import qualified Data.Map                 as M
import qualified Data.Time.Calendar       as T
import           Database.Persist         ((=.))
import           Database.Persist.MongoDB (nestEq, (->.))
import qualified Database.Persist.MongoDB as DB
import qualified Database.Persist.Quasi   as DB (upperCaseSettings)
import qualified Database.Persist.TH      as DB (mkPersist, mpsGenerateLenses,
                                                 mpsPrefixFields,
                                                 persistFileWith, share)
import           Sepa.MongoSettings
import           Sepa.SpanishIban


-- WARNING: the use of lenses (setters) can violate the invariants of the Abstract Data
-- Types in this module.
DB.share [DB.mkPersist mongoSettings { DB.mpsGenerateLenses = True
                                     , DB.mpsPrefixFields   = False }]
  $(DB.persistFileWith DB.upperCaseSettings "Sepa/Debtor.persistent")


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
        maxLengthFullName  = 68 -- SEPA constraint (2.72) is 70, keep room for ", "

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

getActiveMandate :: T.Day -> Debtor -> Maybe Mandate
getActiveMandate today debtor =
  -- Better pass today as an argument, to not make a lot of system calls.
  -- zonedTime <- T.getZonedTime
  -- let today  = T.localDay (T.zonedTimeToLocalTime zonedTime)

  -- Mandates are ordered, with most recently active first
  case debtor ^. mandates of
    (mandate : _) -> case mandate ^. lastTimeActive of
      -- 36 months == 3 years
      Just lta -> if T.addDays (3 * 365) lta > today then Just mandate else Nothing
      Nothing  -> Just mandate
    []            -> Nothing    -- No mandates yet


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

isNew :: (Contravariant f, Functor f, Conjoined p) => p Bool (f Bool) -> p Mandate (f Mandate)
isNew = to _isNew

_isNew :: Mandate -> Bool
_isNew = isNothing . (^. lastTimeActive)

--updateMandateLastTimeActive :: DB.ConnectionPool -> Text -> T.Day -> IO ()
updateMandatesLastTimeActive mandateRefs newDay = do
  debtors <- DB.selectList ([] :: [DB.Filter Debtor]) []
  let mandateAL = concatMap (\(DB.Entity k d) -> fmap (\m -> (m ^. mandateRef, (k, d))) (d ^. mandates)) debtors
  let debtorsMap = M.fromList mandateAL
  forM_ mandateRefs $ \ref ->
    case M.lookup ref debtorsMap of
      Nothing     -> error "updateMandatesLastTimeActive: no debtor"
      Just (k, d) -> do
        let updateMandate m = if   m ^. mandateRef == ref
                              then m & lastTimeActive .~ Just newDay
                              else m
        let mandates' = map updateMandate (d ^. mandates)
        DB.update k [Mandates =. mandates']

  -- forM_ mandateRefs $ \ref -> do
  --   mDebtorE <- DB.selectFirst [Mandates ->. MandateRef `nestEq` ref] []
  --   case mDebtorE of
  --     Nothing                           -> error "updateMandateLastTimeActive: no debtor"
  --     Just (DB.Entity debtorKey debtor) -> do
  --       let mandates' = case debtor ^. mandates of
  --             (m : ms) -> (m & lastTimeActive .~ Just newDay) : ms
  --             []       -> error "updateMandateLastTimeActive: debtor with no mandates"
  --       DB.update debtorKey [Mandates =. mandates']


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
