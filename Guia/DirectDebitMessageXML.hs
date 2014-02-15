{-# LANGUAGE
  FlexibleContexts,
  FlexibleInstances,
  GADTs,
  NoImplicitPrelude,
  OverloadedStrings,
  TypeFamilies
  #-}

module Guia.DirectDebitMessageXML where

import           ClassyPrelude    --      hiding (Text)
import           Control.Lens
import qualified Data.List                                                      as L
  (genericDrop)
import qualified Data.Map                                                       as M
import qualified Data.Text.Lazy                                                 as LT
  (Text)
import qualified Data.Time.Calendar                                             as T
import qualified Data.Time.Calendar.Easter                                      as T
import           Guia.BillingConcept
import           Guia.Creditor
import           Guia.Debtor
import           Guia.DirectDebit
-- import qualified Text.Printf                                                    as PF
--  (printf)
import           Text.XML               hiding (writeFile)
import qualified Text.XML.Light.Input                                           as LXML
import qualified Text.XML.Light.Output                                          as LXML

-- For testing only
import qualified Database.Persist.MongoDB as DB
import qualified Guia.MongoUtils as DB
import qualified Data.Time.LocalTime as T


-- Recursive transformation DirectDebitSet -> Document

message :: DirectDebitSet -> Document                                 -- *
message dds = Document prologue root epilogue
  where
    prologue = Prologue [] Nothing []
    root     = Element "CstmrDrctDbtInitn" M.empty subnodes
    epilogue = []
    subnodes = grpHdr dds : pmtInf_L dds

grpHdr :: DirectDebitSet -> Node                                      -- +
grpHdr dds = nodeElem "GrpHdr" subnodes
  where
    subnodes = [ msgId dds, creDtTm dds, nbOfTxs_1_6 dds
               , ctrlSum_1_7 (dds ^. debits), initgPty dds]

msgId :: DirectDebitSet -> Node                                       -- ++
msgId dds = nodeContent "MsgId" (dds ^. messageId)

creDtTm :: DirectDebitSet -> Node                                     -- ++
creDtTm dds = nodeContent "CreDtTm" (isoDate ++ "T" ++ isoTime)
  where
    isoDate      = T.showGregorian (dds ^. creationDay)
    (isoTime, _) = break (=='.') $ show (dds ^. creationTimeOfDay)

nbOfTxs_1_6 :: DirectDebitSet -> Node                                 -- ++
nbOfTxs_1_6 dds = nodeContent "NbOfTxs" (length (dds ^. debits))

ctrlSum_1_7 :: [DirectDebit] -> Node                                 -- ++
ctrlSum_1_7 =
  nodeContent "CtrlSum" . priceToText . sumOf (traverse.items.traverse.finalPrice)

initgPty :: DirectDebitSet -> Node                                    -- ++
initgPty dds = nodeElem "InitgPty" [nm (dds ^. creditor)]

nm :: Creditor -> Node                                                -- +++
nm c = nodeContent "Nm" (c ^. fullName)

-- | Returns one or two nodes of type "PmtInf", one for debits with new mandates and
-- another for old ones.
pmtInf_L :: DirectDebitSet -> [Node]                                  -- *
pmtInf_L dds =
  concat $ (pmtInf' True new, pmtInf' False old) ^.. both
  where
    -- Use of lenses and list comprehensions
    (new, old) = span (^. mandate.isNew) (dds ^.. debits.traverse)
    pmtInf' areNew ddL = [pmtInf areNew ddL dds | not (null ddL)]

pmtInf :: Bool -> [DirectDebit] -> DirectDebitSet -> Node             -- +
pmtInf areNew ddL dds = nodeElem "pmtInf" subnodes
  where
    subnodes = [ pmtInfId areNew dds, pmtMtd, btchBookg, nbOfTxs_2_4 ddL
               , ctrlSum_2_5 ddL, pmtTpInf areNew, reqdColltnDt dds]

pmtInfId :: Bool -> DirectDebitSet -> Node                            -- ++
pmtInfId areNew dds = nodeContent "MsgId" paymentId
  where
    paymentId = prefix ++ drop 3 (dds ^. messageId)
    prefix    = if areNew then "FST" else "REC"

pmtMtd :: Node                                                        -- ++
pmtMtd = nodeContent "PmtMtd" ("DD" :: Text)

btchBookg :: Node                                                     -- ++
btchBookg = nodeContent "BtchBookg" ("TRUE" :: Text)

nbOfTxs_2_4 :: [DirectDebit] -> Node                                  -- ++
nbOfTxs_2_4 = nodeContent "NbOfTxs" . length

ctrlSum_2_5 :: [DirectDebit] -> Node                                  -- ++
ctrlSum_2_5 =
  nodeContent "CtrlSum" . priceToText . sumOf (traverse.items.traverse.finalPrice)

pmtTpInf :: Bool -> Node                                              -- ++
pmtTpInf areNew = nodeElem "PmtTpInf" subnodes
  where
    subnodes = [svcLvl, lclInstrm, seqTp areNew]

svcLvl :: Node                                                        -- +++
svcLvl = nodeElem "SvcLvl" [cd_2_9]

cd_2_9 :: Node                                                        -- ++++
cd_2_9 = nodeContent "Cd" ("SEPA" :: Text)

lclInstrm :: Node                                                     -- +++
lclInstrm = nodeElem "LclInstrm" [cd_2_12]

cd_2_12 :: Node                                                       -- ++++
cd_2_12 = nodeContent "Cd" ("CORE" :: Text)

seqTp :: Bool -> Node                                                 -- +++
seqTp areNew =
  nodeContent "SeqTp" $ if areNew then ("FRST" :: Text) else "RCUR"

-- | Requests a payment in 7 days for all direct debits, even if recurrent mandates.
reqdColltnDt :: DirectDebitSet -> Node
reqdColltnDt dds =
  nodeContent "ReqdColltnDt" $ T.showGregorian (addWorkingDays 7 (dds ^. creationDay))


-- Helper functions for nodes without attributes

nodeElem :: Name -> [Node] -> Node
nodeElem name subnodes  = NodeElement $ Element name M.empty subnodes

nodeContent :: Content c => Name -> c -> Node
nodeContent name content
  = NodeElement $ Element name M.empty [NodeContent (toContent content)]

class Show c => Content c where
  toContent :: c -> Text
  toContent = pack . show

instance Content Text where
  toContent = id

instance Content String where
  toContent = pack

instance Content Int


-- Non-working days for banking (in Spain): all saturdays, all sundays, New Year's Day,
-- Friday before Easter, Monday after Easter, May Day, 25 and 26 December
-- (http://www.lavanguardia.com/economia/20130704/54377221106/cuanto-tarda-en-hacerse-efectiva-una-transferencia-bancaria.html)
-- Very inefficient, but simple, works for i small.
addWorkingDays :: Integer -> T.Day -> T.Day
addWorkingDays i d =
  assert (i >= 0)
  $ case nextWorkDays of { n : _ -> n; [] -> error "addWorkingDays" }
  where
    nextWorkDays    = L.genericDrop (if (isWorkDay d) then i else i - 1) allWorkDays
    allWorkDays     = filter isWorkDay (allDaysSince d)
    isWorkDay    d_ = not $ any ($ d_) [isWeekend, isFest 1 1, isFest 5 1, isFest 12 25
                                 , isFest 12 26, isEasterFOrM]
    allDaysSince d_ = d_ : map (T.addDays 1) (allDaysSince d_)
                      -- sundayAfter is "strictly" after, so we can only substract
    isWeekend    d_ = any (\f -> f (T.sundayAfter d_) == d_) [T.addDays (-7), T.addDays (-1)]
    isFest mm dd d_ = d_ == T.fromGregorian (yearOf d_) mm dd
                      -- gregorianEaster gives Easter day in year of d (can be before d)
    isEasterFOrM d_ = any (\f -> f (T.gregorianEaster (yearOf d_)) == d_) [T.addDays (-2), T.addDays 1]
    yearOf       d_ = let (yy, _, _) = T.toGregorian d_ in yy

    -- -- Infinite lists of next (strictly after 'd') non-working days
    -- sundays         = T.sundayAfter d : map (T.addDays 7) sundays
    -- saturdays       = map (T.addDays (-1)) sundays -- Yes! it's 'sundays'!
    -- newYears        = yearlyOf 1  1
    -- mayDays         = yearlyOf 5  1
    -- christmas       = yearlyOf 12 25
    -- stStevens       = yearlyOf 12 26
    -- fridaysBEaster  = dropWhile (<= d) $ map (T.addDays (-2) . T.gregorianEaster) [yy..]
    -- mondaysAEaster  = dropWhile (<= d) $ map (T.addDays 1 .    T.gregorianEaster) [yy..]
    -- yearlyOf  mm dd = dropWhile (<= d) (yearlyOf' mm dd)
    -- yearlyOf' mm dd = T.fromGregorian yy mm dd : map (T.addGregorianYearsClip 1) (yearlyOf' mm dd)
    -- (yy, _, _)      = T.toGregorian d
    -- l = L.transpose [ sundays, saturdays, newYears, mayDays, christmas, stStevens, fridaysBEaster, mondaysAEaster]


-- Rendering and writing of messages

renderMessage :: DirectDebitSet -> LT.Text
renderMessage dds = renderText settings (message dds)
  where
    settings = def { rsPretty = False }

-- | Write direct debits instructions message to XML file, with a decent pretty-printer
-- (the one coming with Text.XML puts significant whitespace in content nodes).
writeMessageToFile :: DirectDebitSet -> IO ()
writeMessageToFile dds = do
  -- TODO: handle possible error
  let (Just xmlParsedLight) = LXML.parseXMLDoc (renderMessage dds)
  writeFile "Test.xml" (LXML.ppTopElement xmlParsedLight)


-- Test data

dds_ :: IO DirectDebitSet
dds_ = do
  (Just ddsE) <- DB.runDb $ DB.selectFirst ([] :: [DB.Filter DirectDebitSet]) []
  return $ DB.entityVal ddsE

insertDDS :: IO ()
insertDDS = DB.runDb $ do
  now <- liftIO T.getZonedTime
  liftIO $ putStrLn "Get creditor"
  (Just cE)  <- DB.selectFirst ([] :: [DB.Filter Creditor])       []
  liftIO $ putStrLn "Get debtor"
  dEL  <- DB.selectList ([] :: [DB.Filter Debtor])         []
  liftIO $ putStrLn "Get billing concept"
  bcEL       <- DB.selectList  ([] :: [DB.Filter BillingConcept]) []
  let c = DB.entityVal cE
      (d1 : d2 : _) = map DB.entityVal dEL
      (m1 : _) = d1 ^.. mandates.traversed
      (m2 : _) = d2 ^.. mandates.traversed
      (bc1 : bc2 : _) = map DB.entityVal bcEL
      dd1 = mkDirectDebit (d1 ^. firstName) (d1 ^. lastName) m1 [bc1, bc2] ""
      dd2 = mkDirectDebit (d2 ^. firstName) (d2 ^. lastName) m2 [bc1] ""
      dds = mkDirectDebitSet "New DdSet" now c [dd1, dd2]
  liftIO $ putStrLn "Insert"
  DB.deleteWhere ([] :: [DB.Filter DirectDebitSet])
  DB.insert_ dds
  return ()
