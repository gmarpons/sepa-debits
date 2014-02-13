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

         -- Mandate
         mkMandate,
         Mandate, MandateId,
         mandateRef,
         iban,
         signatureDate,
         lastTimeActive,
         bankId,
         validMandate,
         spanishIbanPrefixFromCcc,
         cccControlDigits,

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
import qualified Control.Lens                                                   as L
  (Conjoined, Contravariant)
import qualified Control.Lens.Getter                                            as L
  (to)
import qualified Data.Char                                                      as CH
  (digitToInt, intToDigit, isDigit, isUpper)
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
import           Guia.MongoUtils
import qualified Text.Printf                                                    as PF
  (printf)


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
     not (null firstName_) && length firstName_ <= maxFirstNameLength
  && not (null lastName_)  && length lastName_  <= maxLastNameLength
  && length lastName_  + length firstName_ <= maxNameLength
  && validMandateList mandates_
  where maxFirstNameLength = 40
        maxLastNameLength  = 40
        maxNameLength      = 70

-- | We want mandates ordered from more recent to oldest.
validMandateList :: [Mandate] -> Bool
validMandateList mandates_ = LT.and lastTimePairs
  where
    lastTimeActiveList   = map (^. lastTimeActive) mandates_
    lastTimeActiveList'  = Nothing : Nothing : lastTimeActiveList
    lastTimeActiveList'' = Nothing : lastTimeActiveList'
    lastTimePairs        = LT.zipWith newerThan lastTimeActiveList' lastTimeActiveList''
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


-- | Getter for a unique key to look for a SpanishBank.
bankId :: (Functor f, L.Contravariant f, L.Conjoined p) =>
          p Text (f Text) -> p Mandate (f Mandate)
bankId = L.to _bankId

_bankId :: Mandate -> Text
_bankId mandate = fourDigitsCode_
  where (_ibanPrefix, ccc)   = splitAt 4 (mandate ^. iban)
        (fourDigitsCode_, _) = splitAt 4 ccc

validSpanishIban :: Text -> Bool
validSpanishIban iban_ =
  -- That the last 22 chars are digits is already guaranteed by asserts in
  -- spanishIbanPrefixFromCcc and cccControlDigits
     length iban_ == 24
  && ibanPrefix == spanishIbanPrefixFromCcc ccc
  && controlDigits == cccControlDigits fourDigitsCode_ office num
  -- && DB.fromPersistValueText (DB.unKey bankId_) == Right fourDigitsCode_
  where (ibanPrefix, ccc)          = splitAt 4 iban_
        (fourDigitsCode_, suffix)  = splitAt 4 ccc
        (office, suffix')          = splitAt 4 suffix
        (controlDigits, num)       = splitAt 2 suffix'

-- | Pre: 'ccc' contains exactly 20 decimal digits. Returned value contains a 4-character
-- text, with "ES" prefix followed by 2 decimal digits.
spanishIbanPrefixFromCcc :: Text -> Text
spanishIbanPrefixFromCcc ccc =
  assert (all CH.isDigit ccc && length ccc == 20)
  $ "ES" ++ checkDigits
  where
    checkDigits = pack $ PF.printf "%02d" (98 - (extendedCccAsNum `mod` 97))
    extendedCccAsNum :: Integer                          -- We need unbound precision
    extendedCccAsNum = case extendedCccAsEither of
      (Left msg)                            -> error $ "spanishIbanPrefix: " ++ msg
      (Right (num, text)) | not (null text) -> error "spanishIbanPrefix: bad decimal"
                          | otherwise       -> num
    -- 14 is the code for "E", and "28" is the code for "S", see
    -- http://en.wikipedia.org/wiki/International_Bank_Account_Number#Generating_IBAN_check_digits
    extendedCccAsEither = TXT.decimal $ ccc ++ "14" ++ "28" ++ "00"

-- | Pre: 'bankId' contains exactly 4 decimal digits.
-- Pre: 'office' contains exactly 4 decimal digits.
-- Pre: 'num' contains exactly 10 decimal digits.
-- Returned value contains exactly 2 decimal digits.
cccControlDigits :: Text -> Text -> Text -> Text
cccControlDigits bankCode office num =
  assert (    all CH.isDigit bankCode && length bankCode ==  4
           && all CH.isDigit office && length office ==  4
           && all CH.isDigit num    && length num    == 10 )
  $ pack [firstDigit, secondDigit]
  where
    -- CH.digitToInt fails if not isHexDigit
    firstDigit                        = cccDigit (bankCode ++ office)
    secondDigit                       = cccDigit num
    cccDigit                          = intsToDigit . stringToInts . unpack
    stringToInts str                  = reverse $ map CH.digitToInt str
    intsToDigit                       = intToDigit' . intsToInt
    intsToInt ints                    =  11 - (sum (zipWith (*) weights ints) `mod` 11)
    weights                           = [6, 3, 7, 9, 10, 5, 8, 4, 2, 1] :: [Int]
    intToDigit'                       :: Int -> Char
    intToDigit' 10                    = '1'
    intToDigit' 11                    = '0'
    intToDigit' x | x >= 0 && x <= 9  = CH.intToDigit x -- would fail with x >= 16
    intToDigit' _x                    = error "cccControlDigits: Int >= 11"


-- Spanish banks

mkSpanishBank :: Text -> Text -> Text -> SpanishBank
mkSpanishBank fourDigitsCode_ bic_ bankName_ =
  assert (validSpanishBank fourDigitsCode_ bic_ bankName_)
  $ SpanishBank fourDigitsCode_ bic_ bankName_

validSpanishBank :: Text -> Text -> Text -> Bool
validSpanishBank fourDigitsCode_ bic_ bankName_ =
     length fourDigitsCode_ == 4 && all CH.isDigit fourDigitsCode_
  && validSpanishBankBic bic_
  && length bankName_ <= maxBankName
  where maxBankName = 100

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
