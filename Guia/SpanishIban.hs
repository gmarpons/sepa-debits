{-# LANGUAGE
  NoImplicitPrelude,
  OverloadedStrings
  #-}

module Guia.SpanishIban
       ( -- Spanish iban
         IBAN,
         bankDigits,
         officeDigits,
         validSpanishIban,
         spanishIbanPrefixFromCcc,
         cccControlDigits
       ) where

import           ClassyPrelude
import qualified Control.Lens                                                   as L
  (Conjoined, Gettable,
   to)
import qualified Data.Char                                                      as CH
  (digitToInt, intToDigit, isDigit)
import qualified Data.Text.Read                                                 as TXT
  (decimal)
import qualified Text.Printf                                                    as PF
  (printf)


type IBAN = Text

-- | Getter for the four digits code for banks.
bankDigits :: (L.Gettable f, L.Conjoined p) => p Text (f Text) -> p Text (f Text)
bankDigits = L.to _bankDigits

_bankDigits :: Text -> Text
_bankDigits iban = assert (validSpanishIban iban) bankDigits_
  where (_ibanPrefix, ccc)      = splitAt 4 iban
        (bankDigits_, _)        = splitAt 4 ccc

-- | Getter for the four digits code for bank offices.
officeDigits :: (L.Gettable f, L.Conjoined p) => p Text (f Text) -> p Text (f Text)
officeDigits = L.to _officeDigits

_officeDigits :: Text -> Text
_officeDigits iban = assert (validSpanishIban iban) officeDigits_
  where (_ibanPrefix, office_dc_num)   = splitAt 8 iban
        (officeDigits_, _)              = splitAt 4 office_dc_num

validSpanishIban :: IBAN -> Bool
validSpanishIban iban_ =
  -- That the last 22 chars are digits is already guaranteed by asserts in
  -- spanishIbanPrefixFromCcc and cccControlDigits
     length iban_ == 24
  && ibanPrefix == spanishIbanPrefixFromCcc ccc
  && controlDigits == cccControlDigits fourDigitsCode_ office_ num
  -- && DB.fromPersistValueText (DB.unKey bankId_) == Right fourDigitsCode_
  where (ibanPrefix, ccc)          = splitAt 4 iban_
        (fourDigitsCode_, suffix)  = splitAt 4 ccc
        (office_, suffix')         = splitAt 4 suffix
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
cccControlDigits bank office_ num =
  assert (    all CH.isDigit bank     && length bank ==  4
           && all CH.isDigit office_  && length office_ ==  4
           && all CH.isDigit num      && length num    == 10 )
  $ pack [firstDigit, secondDigit]
  where
    -- CH.digitToInt fails if not isHexDigit
    firstDigit                        = cccDigit (bank ++ office_)
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
