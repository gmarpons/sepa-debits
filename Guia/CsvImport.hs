{-# LANGUAGE
  DeriveDataTypeable,
  FlexibleContexts,
  FlexibleInstances,
  GADTs,
  NoImplicitPrelude,
  OverloadedStrings,
  ScopedTypeVariables,
  TemplateHaskell,
  TypeFamilies
  #-}

module Guia.CsvImport where

import qualified Prelude        -- ClassyPrelude.unzip doesn't work
  (unzip)

import           ClassyPrelude
import qualified Control.Applicative                                            as A
  (Alternative(..))
import           Control.Lens
  ((^.) {-, }(&), (.~) -})
import qualified Control.Lens                                                   as L
  (makeLenses)
import qualified Control.Monad.Trans.Maybe                                      as M
import qualified Control.Monad.Trans.Error                                      as E
  (Error(strMsg))
import qualified Control.Monad.Trans.State                                      as MS
  (evalStateT, get, modify)
import qualified Control.Monad.Trans.Resource                                   as R
  (MonadResource, ResourceT,
   monadThrow, runResourceT)
import qualified Data.ByteString.Lazy                                           as LBS
  (hGetContents)
import qualified Data.ByteString.Char8                                          as C8
  (unpack)
import           Data.Conduit
  (($$))
import qualified Data.Conduit                                                   as C
  (Source,
   bracketP, yield)
import qualified Data.Conduit.List                                              as CL
  (consume, mapM_)
import           Data.Csv
  ((.:))
import qualified Data.Csv                                                       as CSV
  (FromField(..), FromNamedRecord(..))
import qualified Data.Csv.Streaming                                             as CSV
  (Records(Cons, Nil),
   decodeByName)
import qualified Data.IntMap                                                    as IM
import qualified Data.Time.Calendar                                             as T
import qualified Data.Time.Clock                                                as T
import           Database.MongoDB
  ((=:))
import qualified Database.MongoDB                                               as DB
  (genObjectId)
import qualified Database.MongoDB.Admin                                         as DB
  (Index(..),
   createIndex, dropCollection)
import qualified Database.Persist.MongoDB                                       as DB
import qualified Database.Persist.Quasi                                         as DB
  (lowerCaseSettings)
import qualified Database.Persist.TH                                            as DB
  (mkPersist, mpsGenerateLenses, mpsPrefixFields, persistFileWith, share)
import           Guia.Debtor
import           Guia.MongoSettings
import           Guia.MongoUtils
import qualified System.IO                                                      as IO
  (IOMode(ReadMode),
   openFile)
import qualified Text.Printf                                                    as PF

-- Persistent collections

DB.share [DB.mkPersist mongoSettings { DB.mpsGenerateLenses = True
                                     , DB.mpsPrefixFields   = True }]
  $(DB.persistFileWith DB.lowerCaseSettings "Guia/CsvImport.persistent")


-- Datatypes for CSV data

data CsvPayer
  = CsvPayer
    { _csvPayerId                :: Int
    , _csvPayerRegistrationDate  :: T.Day
    , _csvPayerNameId            :: Int
    , _csvPayerBankAccountId     :: Maybe Int }

L.makeLenses ''CsvPayer

data CsvPersonName
  = CsvPersonName
    { _csvPersonNameId            :: Int
    , _csvPersonNameFirst         :: Text
    , _csvPersonNameLast          :: Text }

L.makeLenses ''CsvPersonName

data CsvBankAccount
  = CsvBankAccount
    { _csvBankAccountId   :: Int
    , _csvBankAccountIban :: Text }
  deriving (Eq, Show, Read)

L.makeLenses ''CsvBankAccount


-- Debtors / payers

instance CSV.FromNamedRecord (Maybe CsvPayer) where
  parseNamedRecord r = maybeCsvPayer <$> r .: "id"
                                     <*> r .: "registration_date"
                                     <*> r .: "name_id"
                                     <*> r .: "bank_account_id"
    where maybeCsvPayer i d n a = Just $ CsvPayer i d n a

instance CSV.FromField (T.Day) where
  parseField f = case readMay (C8.unpack f) :: Maybe T.Day of
    Just day    -> pure day
    Nothing     -> A.empty

readCsvPayers :: FilePath -> IO [CsvPayer]
readCsvPayers filePath =
  R.runResourceT $ sourceCsvFile filePath $$ CL.consume

instance CSV.FromNamedRecord (Maybe CsvPersonName) where
  parseNamedRecord r = maybeCsvPersonName <$> r .: "id"
                                          <*> r .: "first"
                                          <*> r .: "last"
    where maybeCsvPersonName i f l = Just $ CsvPersonName i f l

readCsvPersonNames :: FilePath -> IO [CsvPersonName]
readCsvPersonNames filePath =
  R.runResourceT $ sourceCsvFile filePath $$ CL.consume

importDebtors :: FilePath -> IO ()
importDebtors csvDirecory  = do
  now <- T.getCurrentTime
  dummyId <- DB.genObjectId
  putStrLn "Importing payers"
  payers <- readCsvPayers (csvDirecory </> "payers.csv")
  putStrLn "Importing person names"
  personNames <- readCsvPersonNames (csvDirecory </> "person_names.csv")
  liftIO $ putStrLn "Importing bank accounts"
  csvAccounts <- liftIO (readCsvBankAccounts (csvDirecory </> "bank_accounts.csv"))
  let namesMap =    IM.fromList $ map (\pn -> (pn ^. csvPersonNameId, pn)) personNames
      accountsMap = IM.fromList $ map (\a  -> (a  ^. csvBankAccountId ,a)) csvAccounts
      today          = T.utctDay now
      dummyDebtor    = mkDebtor "A" "A" Nothing [] today
      dummyMapDebtor = MapDebtor 1 (DB.oidToKey dummyId)
      dummyMandate   = mkMandate (replicate 35 '0') dummyAccount today
      dummyAccount   = mkSpanishBankAccount "ES8200000000000000000000"
      doInsert payer = do
        let ref :: Text
            ref = pack $ PF.printf "%012d" (payer ^. csvPayerId) ++ replicate (35-12) ' '
            (first_, last_) = case lookup (payer ^. csvPayerNameId) namesMap of
              Just pn -> (pn ^. csvPersonNameFirst, pn ^. csvPersonNameLast)
              Nothing -> error "importDebtors: missing CsvPersonName"
            mCsvAccountId = payer ^. csvPayerBankAccountId
        mMandate <- M.runMaybeT $ do
          csvAccountId <- M.MaybeT $ return mCsvAccountId
          csvAccount <- M.MaybeT $ return $ lookup csvAccountId accountsMap
          let account_ = mkSpanishBankAccount (csvAccount ^. csvBankAccountIban)
          -- Caution with fromGregorianValid: dates are clipped if numbers out of range!
          return $ mkMandate ref account_ (T.fromGregorian 2009 10 31)
        case (mCsvAccountId, mMandate) of
          (Just _, Nothing) -> error "importDebtors: missing CsvBankAccount"
          _                 -> return ()
        let debtor = mkDebtor first_ last_ mMandate [] (payer ^. csvPayerRegistrationDate)
        mongoId <- DB.insert debtor
        DB.insert_ $ MapDebtor (payer ^. csvPayerId) mongoId
        return ()
  runResourceDbT $ do
    liftIO $ putStrLn "Creating debtors collection"
    lift $ mkCollection dummyDebtor
    liftIO $ putStrLn "Creating map_debtors collection"
    lift $ mkCollection dummyMapDebtor
    liftIO $ putStrLn "Creating mandates collection"
    lift $ mkCollection dummyMandate
    liftIO $ putStrLn "Creating spanish_bank_accounts collection"
    lift $ mkCollection dummyAccount
    liftIO $ putStrLn "Inserting"
    mapM_ doInsert payers
    return ()


-- Bank accounts

instance CSV.FromNamedRecord (Maybe CsvBankAccount) where
  parseNamedRecord r = maybeCsvBankAccount <$> r .: "id"
                                           <*> r .: "bank_id"
                                           <*> r .: "office"
                                           <*> r .: "control_digits"
                                           <*> r .: "num"
    where maybeCsvBankAccount i b o c n
            | cccControlDigits b o n == c = Just $ CsvBankAccount i (iban_ b o c n)
            | otherwise                   = Nothing
          iban_ b o c n = spanishIbanPrefixFromCcc (ccc b o c n) ++ ccc b o c n
          ccc   b o c n = concat [b, o, c, n]

readCsvBankAccounts :: FilePath -> IO [CsvBankAccount]
readCsvBankAccounts filePath =
  R.runResourceT $ sourceCsvFile filePath $$ CL.consume

importSpanishBankAccounts :: FilePath -> IO ()
importSpanishBankAccounts filePath = do
  dummyId <- DB.genObjectId
  let dummyAccount = mkSpanishBankAccount "ES8200000000000000000000"
      dummyMapBankAccount = MapBankAccount 0 (DB.oidToKey dummyId)
      doInsert csv = do
        mongoId <- DB.insert $ mkSpanishBankAccount (csv ^. csvBankAccountIban)
        DB.insert_ $ MapBankAccount (csv ^. csvBankAccountId) mongoId
  runResourceDbT $ do
    liftIO $ putStrLn "Creating spanish_bank_accounts collection"
    lift $ mkCollection dummyAccount
    liftIO $ putStrLn "Creating map_bank_accounts collection"
    lift $ mkCollection dummyMapBankAccount
    liftIO $ putStrLn "Importing"
    csvAccounts <- liftIO (readCsvBankAccounts filePath)
    liftIO $ putStrLn "Inserting"
    mapM_ doInsert csvAccounts


-- Spanish banks

importSpanishBanks :: FilePath -> IO ()
importSpanishBanks filePath = do
  let --banks :: C.Source (R.ResourceT (DB.Action IO)) (DB.Entity SpanishBank)
      banks :: C.Source (R.ResourceT (DB.Action IO)) SpanishBank
      banks = sourceCsvFile filePath
      --doImport = banks $$ CL.mapM_ (\e -> DB.insertKey (DB.entityKey e) (DB.entityVal e))
      doImport = banks $$ CL.mapM_ DB.insert_
      dummySpanishBank = mkSpanishBank "0000" "AAAAESAAXXX" ""
  runResourceDbT $ do
    liftIO $ putStrLn "Creating collection"
    lift $ mkCollection dummySpanishBank
    liftIO $ putStrLn "Importing and inserting"
    doImport

instance CSV.FromNamedRecord (Maybe SpanishBank) where
  parseNamedRecord r = maybeSpanishBank <$> r .: "four_digits_code"
                                        <*> r .: "bic"
                                        <*> r .: "bank_name"
    where maybeSpanishBank c b n
            | validSpanishBank c b n = Just $ mkSpanishBank c b n
            | otherwise              = Nothing


-- Helper functions

mkCollection :: DB.PersistEntity record => record -> DB.Action IO ()
mkCollection record = do
  let collectionName = DB.collectionName record
      persistUniqueKeys = DB.persistUniqueKeys record
      indexFieldLists = map (map DB.unDBName . uniqueKey2fieldList) persistUniqueKeys
      uniqueKey2fieldList = snd . Prelude.unzip . DB.persistUniqueToFieldNames
      fieldList2indexName []     = error "Unique key with no field names"
      fieldList2indexName (f:fs) = f ++ foldl' (\x y -> x ++ "_1_" ++ y) "" fs ++ "_1"
      mkIndex fieldList = DB.Index { DB.iColl = collectionName
                                   , DB.iKey = map (=: (1 :: Int)) fieldList -- 1 is asc.
                                   , DB.iName = fieldList2indexName fieldList
                                   , DB.iUnique = True
                                   , DB.iDropDups = False }
  liftIO $ putStrLn "  Dropping collection"
  _ <- DB.dropCollection collectionName
  liftIO $ putStrLn "  Creating unique indexes"
  mapM_ (DB.createIndex . mkIndex) indexFieldLists
  return ()

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
sourceCsvFile :: (R.MonadResource m, CSV.FromNamedRecord (Maybe o)) =>
                 FilePath -> C.Source m o
-- bracketP to assure correct resource deallocation. We use package cassava to decode CSV
-- files, converting all the exceptional situations codified as Either/Maybe into true
-- exceptions. We use monad State to keep track of the record number we are analyzing and
-- use this information in exception reports.
sourceCsvFile filePath =
  C.bracketP (IO.openFile (fpToString filePath) IO.ReadMode) hClose $ \h -> do
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
