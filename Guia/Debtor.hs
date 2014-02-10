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
         Debtor, DebtorId,
         firstName,
         lastName,
         activeMandate,
         oldMandates,
         registrationDate,

         -- Mandate
         Mandate, MandateId,
         mandateReference,
         account,
         signatureDate,

         -- SpanishBankAccount
         SpanishBankAccount, SpanishBankAccountId,
         iban,
         bankId,
         validSpanishBankAccount,
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
import qualified Data.Char                                                      as CH
  (digitToInt, intToDigit, isDigit, isUpper)
import qualified Data.Text.Read                                                 as T
  (decimal)
import qualified Data.Time.Calendar                                             as C
  (Day)
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

DB.share [DB.mkPersist mongoSettings { DB.mpsGenerateLenses = True
                                     , DB.mpsPrefixFields   = False }]
  $(DB.persistFileWith DB.lowerCaseSettings "Guia/Debtor.persistent")

validSpanishBankAccount :: Text -> SpanishBank -> Bool
validSpanishBankAccount iban_ bank =
     all CH.isDigit iban_ && length iban_ == 24
  && ibanPrefix == spanishIbanPrefixFromCcc ccc
  && controlDigits == cccControlDigits bankCode office num
  && bank ^. fourDigitsCode == bankCode
  where (ibanPrefix, ccc)    = splitAt 4 iban_
        (bankCode, suffix)   = splitAt 4 ccc
        (office, suffix')    = splitAt 4 suffix
        (controlDigits, num) = splitAt 2 suffix'

-- | Pre: 'ccc' contains exactly 20 decimal digits. Returned value contains a 4-character
-- text, with "ES" prefix followed by 2 decimal digits.
spanishIbanPrefixFromCcc :: Text -> Text
spanishIbanPrefixFromCcc ccc = assert (all CH.isDigit ccc && length ccc == 20)
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
    extendedCccAsEither = T.decimal $ ccc ++ "14" ++ "28" ++ "00"

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

mkSpanishBank :: Text -> Text -> Text -> SpanishBank
mkSpanishBank fourDigitsCode_ bic_ bankName_ =
  assert (validSpanishBank fourDigitsCode_ bic_ bankName_)
  $ SpanishBank fourDigitsCode_ bic_ bankName_

validSpanishBank :: Text -> Text -> Text -> Bool
validSpanishBank fourDigitsCode_ bic_ bankName_ =
     length fourDigitsCode_ == 4
  && all CH.isDigit fourDigitsCode_
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
