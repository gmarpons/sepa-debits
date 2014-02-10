{-# LANGUAGE
  DeriveDataTypeable,
  FlexibleContexts,
  FlexibleInstances,
  GADTs,
  NoImplicitPrelude,
  OverloadedStrings,
  ScopedTypeVariables,
  TypeFamilies
  #-}

module Guia.CsvImport where

import           ClassyPrelude
import qualified Control.Monad.Trans.Error                                      as E
  (Error(strMsg))
import qualified Control.Monad.Trans.State                                      as MS
  (evalStateT, get, modify)
import qualified Control.Monad.Trans.Resource                                   as R
  (MonadResource,
   monadThrow, runResourceT)
import qualified Data.ByteString.Lazy                                           as LBS
  (hGetContents)
import           Data.Conduit
  ((=$), ($$))
import qualified Data.Conduit                                                   as C
  (Source,
   bracketP, yield)
import qualified Data.Conduit.List                                              as CL
  (consume, iterM)
import           Data.Csv
  ((.:))
import qualified Data.Csv                                                       as CSV
  (FromNamedRecord(..))
import qualified Data.Csv.Streaming                                             as CSV
  (Records(Cons, Nil),
   decodeByName)
import qualified Database.Persist.MongoDB                                       as DB
  (insert_)
import           Guia.Debtor
import           Guia.MongoUtils
import qualified System.IO                                                      as IO
  (FilePath, IOMode(ReadMode),
   openFile)

data CsvException
  = CsvError                   String -- ^ Generic exception
  | CsvParsingException        String
  | CsvTypeConversionException String
  | CsvHeaderException         String
  | CsvDataValidationException String
  deriving (Show, Typeable)

instance Exception CsvException
instance E.Error CsvException where
  strMsg = CsvError

-- | Conduit's 'C.Source' for decoding CSV files. Every line/record is decoded into a
-- value of type @o@. Data type @o@ needs to have an @instance FromNamedRecord (Maybe o)@
-- that returns 'Nothing' if the data parsed has correct type but it's not valid input for
-- constructing a value of type @o@. CSV file needs to have a header line giving to every
-- used column the same name that is used in 'CSV.parseNamedRecord' for the data type 'o'.
-- Both @IOException@'s and exceptions related to parsing and converting CSV contents into
-- values can be thrown.
sourceCsvFile :: (R.MonadResource m, CSV.FromNamedRecord (Maybe o))
                 => IO.FilePath -> C.Source m o
-- bracketP to assure correct resource deallocation. We use package cassava to decode CSV
-- files, converting all the exceptional situations codified as Either/Maybe into true
-- exceptions. We use monad State to keep track of the record number we are analyzing and
-- use this information in exception reports.
sourceCsvFile filePath =
  C.bracketP (IO.openFile filePath IO.ReadMode) hClose $ \h -> do
    byteString <- liftIO $ LBS.hGetContents h
    let eRecords = CSV.decodeByName byteString
    case eRecords of
      Left msg                 -> R.monadThrow (CsvHeaderException msg)
      Right (_header, records) -> MS.evalStateT (yield' records) (2 :: Integer)
  where
    yield' (CSV.Cons (Left  msg)      _ ) = throw' CsvTypeConversionException msg
    yield' (CSV.Cons (Right (Just r)) rs) = lift (C.yield r) >> MS.modify (+1) >> yield' rs
    yield' (CSV.Cons (Right Nothing)  _ ) = throw' CsvDataValidationException ""
    yield' (CSV.Nil  (Just msg)       _ ) = throw' CsvParsingException (take 100 msg)
    yield' (CSV.Nil  Nothing          _ ) = return ()
    throw' e msg = do
      recordNum <- MS.get
      R.monadThrow $ e ("Line " ++ show recordNum ++ ": " ++ msg)


-- @OldBankAccount@'s

readOldBankAccounts :: IO.FilePath -> IO [OldBankAccount]
readOldBankAccounts filePath =
  R.runResourceT $ sourceCsvFile filePath $$ CL.consume

data OldBankAccount
  = OldBankAccount
    { id   :: Int
    , iban :: Text }
  deriving (Eq, Show, Read)

instance CSV.FromNamedRecord (Maybe OldBankAccount) where
  parseNamedRecord r = maybeOldBankAccount <$> r .: "id"
                                           <*> r .: "bank_id"
                                           <*> r .: "office"
                                           <*> r .: "control_digits"
                                           <*> r .: "num"
    where maybeOldBankAccount id_ b o c n
            | cccControlDigits b o n == c = Just $ OldBankAccount id_ (iban_ b o c n)
            | otherwise                   = Nothing
          iban_ b o c n = spanishIbanPrefixFromCcc (ccc b o c n) ++ ccc b o c n
          ccc   b o c n = concat [b, o, c, n]

-- @SpanishBank@'s

importSpanishBanks :: IO.FilePath -> IO ()
importSpanishBanks filePath = do
  (_ :: [SpanishBank]) <-
    runResourceDbT $ sourceCsvFile filePath $$ CL.iterM DB.insert_ =$ CL.consume
  return ()

instance CSV.FromNamedRecord (Maybe SpanishBank) where
  parseNamedRecord r = maybeSpanishBank <$> r .: "four_digits_code"
                                        <*> r .: "bic"
                                        <*> r .: "name"
    where maybeSpanishBank fourDigitsCode_ bic_ name
            | validSpanishBank fourDigitsCode_ bic_ name
                = Just $ mkSpanishBank fourDigitsCode_ bic_ name
            | otherwise = Nothing
