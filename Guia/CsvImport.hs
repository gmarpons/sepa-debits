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

import qualified Prelude
  (unzip)

import           ClassyPrelude
import           Control.Lens
  ((^.), (&), (.~))
import qualified Control.Lens                                                   as L
  (makeLenses)
import qualified Control.Monad.Trans.Error                                      as E
  (Error(strMsg))
import qualified Control.Monad.Trans.State                                      as MS
  (evalStateT, get, modify)
import qualified Control.Monad.Trans.Resource                                   as R
  (MonadResource, ResourceT,
   monadThrow, runResourceT)
import qualified Data.ByteString.Lazy                                           as LBS
  (hGetContents)
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
  (FromNamedRecord(..))
import qualified Data.Csv.Streaming                                             as CSV
  (Records(Cons, Nil),
   decodeByName)
import           Database.MongoDB
  ((=:))
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
sourceCsvFile :: (R.MonadResource m, CSV.FromNamedRecord (Maybe o)) =>
                 IO.FilePath -> C.Source m o
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

DB.share [DB.mkPersist mongoSettings { DB.mpsGenerateLenses = True
                                     , DB.mpsPrefixFields   = True }]
  $(DB.persistFileWith DB.lowerCaseSettings "Guia/CsvImport.persistent")


-- @OldBankAccount@'s

data OldBankAccount
  = OldBankAccount
    { _oldBankAccountId   :: Int
    , _oldBankAccountIban :: Text }
  deriving (Eq, Show, Read)

L.makeLenses ''OldBankAccount

readOldBankAccounts :: IO.FilePath -> IO [OldBankAccount]
readOldBankAccounts filePath =
  R.runResourceT $ sourceCsvFile filePath $$ CL.consume

importSpanishBankAccounts :: IO.FilePath -> IO ()
importSpanishBankAccounts filePath = do
  let dummyAccount = mkSpanishBankAccount "ES8200000000000000000000"
      doInsert o = do
        newId <- DB.insert $ mkSpanishBankAccount (o ^. oldBankAccountIban)
        DB.insert_ $ MapBankAccount (o ^. oldBankAccountId) newId
  runResourceDbT $ do
    liftIO $ putStrLn "Creating collection"
    lift $ createCollection dummyAccount
    liftIO $ putStrLn "Importing"
    oldAccounts <- liftIO (readOldBankAccounts filePath)
    mapM_ doInsert oldAccounts

instance CSV.FromNamedRecord (Maybe OldBankAccount) where
  parseNamedRecord r = maybeOldBankAccount <$> r .: "id"
                                           <*> r .: "bank_id"
                                           <*> r .: "office"
                                           <*> r .: "control_digits"
                                           <*> r .: "num"
    where maybeOldBankAccount oldId_ b o c n
            | cccControlDigits b o n == c = Just $ OldBankAccount oldId_ (iban_ b o c n)
            | otherwise                   = Nothing
          iban_ b o c n = spanishIbanPrefixFromCcc (ccc b o c n) ++ ccc b o c n
          ccc   b o c n = concat [b, o, c, n]


-- @SpanishBank@'s

importSpanishBanks :: IO.FilePath -> IO ()
importSpanishBanks filePath = do
  let --banks :: C.Source (R.ResourceT (DB.Action IO)) (DB.Entity SpanishBank)
      banks :: C.Source (R.ResourceT (DB.Action IO)) SpanishBank
      banks = sourceCsvFile filePath
      --doImport = banks $$ CL.mapM_ (\e -> DB.insertKey (DB.entityKey e) (DB.entityVal e))
      doImport = banks $$ CL.mapM_ DB.insert_
      dummySpanishBank = mkSpanishBank "0000" "AAAAESAAXXX" ""
  runResourceDbT $ do
    liftIO $ putStrLn "Creating collection"
    lift $ createCollection dummySpanishBank
    liftIO $ putStrLn "Importing"
    doImport

createCollection :: DB.PersistEntity record => record -> DB.Action IO ()
createCollection record = do
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
  liftIO $ putStrLn "Dropping collection"
  _ <- DB.dropCollection collectionName
  liftIO $ putStrLn "Creating unique indexes"
  mapM_ (DB.createIndex . mkIndex) indexFieldLists
  return ()

instance CSV.FromNamedRecord (Maybe SpanishBank) where
  parseNamedRecord r = maybeSpanishBank <$> r .: "four_digits_code"
                                        <*> r .: "bic"
                                        <*> r .: "bank_name"
    where maybeSpanishBank c b n
            | validSpanishBank c b n = Just $ mkSpanishBank c b n
            | otherwise              = Nothing

-- instance CSV.FromNamedRecord (Maybe (DB.Entity SpanishBank)) where
--   parseNamedRecord r = maybeSpanishBankAndKey <$> r .: "four_digits_code"
--                                               <*> r .: "bic"
--                                               <*> r .: "bank_name"
--     where maybeSpanishBankAndKey key bic_ name
--             | validSpanishBank bic_ name = Just $ DB.Entity (textToKey key)
--                                                             (mkSpanishBank  bic_ name)
--             | otherwise                  = Nothing
--           textToKey text = DB.Key (DB.PersistText text)
