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
import           Control.Arrow
import           Control.Lens
-- import qualified Data.List                                                      as L
import qualified Data.Map                                                       as M
import qualified Data.Text.Lazy                                                 as LT
  (Text)
import qualified Data.Time.Calendar                                             as T
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

message :: DirectDebitSet -> Document
message dds = Document prologue root epilogue
  where
    prologue = Prologue [] Nothing []
    root = Element "CstmrDrctDbtInitn" M.empty content
    epilogue = []
    content = grpHdr dds : pmtInfL dds

grpHdr :: DirectDebitSet -> Node                                      -- +
grpHdr dds = nodeElem "GrpHdr" subnodes
  where
    subnodes = [msgId dds, creDtTm dds, nbOfTxs dds, ctrlSum dds, initgPty dds]

msgId :: DirectDebitSet -> Node                                       -- ++
msgId dds = nodeContent "MsgId" (dds ^. messageId)

creDtTm :: DirectDebitSet -> Node                                     -- ++
creDtTm dds = nodeContent "CreDtTm" (isoDate ++ "T" ++ isoTime)
  where
    isoDate      = T.showGregorian (dds ^. creationDay)
    (isoTime, _) = break (=='.') $ show (dds ^. creationTimeOfDay)

nbOfTxs :: DirectDebitSet -> Node                                     -- ++
nbOfTxs dds = nodeContent "NbOfTxs" (length (dds ^. debits))

ctrlSum :: DirectDebitSet -> Node                                     -- ++
ctrlSum dds = nodeContent "CtrlSum" content
  where content = priceToText $ sumOf (debits.traverse.items.traverse.finalPrice) dds

initgPty :: DirectDebitSet -> Node                                    -- ++
initgPty dds = nodeElem "InitgPty" [nm (dds ^. creditor)]

nm :: Creditor -> Node                                                -- +++
nm creditor_ = nodeContent "Nm" (creditor_ ^. fullName)

-- | Returns one or two nodes of type "PmtInf", one for debits with new mandates and
-- another for old ones.
pmtInfL :: DirectDebitSet -> [Node]                                   -- +
pmtInfL dds =
  concat $ (pmtInf' True new, pmtInf' False old) ^.. both
  where
    -- Use of lenses and list comprehensions
    (new, old) = span (^. mandate.isNew) (dds ^.. debits.traverse)
    pmtInf' areNew ddL = [pmtInf areNew ddL dds | not (null ddL)]

pmtInf :: Bool -> [DirectDebit] -> DirectDebitSet -> Node
pmtInf areNew ddL dds = nodeElem "pmtInf" subnodes
  where
    subnodes = [pmtInfId areNew dds]

pmtInfId :: Bool -> DirectDebitSet -> Node
pmtInfId areNew dds = nodeContent "MsgId" paymentId
  where
    paymentId = prefix ++ drop 3 (dds ^. messageId)
    prefix    = if areNew then "FST" else "REC"


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

-- instance Content c => Content [c] where
--   toContent = concatMap toContent


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
