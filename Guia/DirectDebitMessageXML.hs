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
import qualified Data.Map                                                       as M
import qualified Data.Text.Lazy                                                 as LT
  (Text)
import qualified Data.Time.Calendar                                             as T
import           Guia.BillingConcept
import           Guia.Creditor
import           Guia.Debtor
import           Guia.DirectDebit
import qualified Text.Printf                                                    as PF
  (printf)
import           Text.XML               hiding (writeFile)
import qualified Text.XML.Light.Input                                           as LXML
import qualified Text.XML.Light.Output                                          as LXML

-- For testing only
import qualified Database.Persist.MongoDB as DB
import qualified Guia.MongoUtils as DB
import qualified Data.Time.LocalTime as T

message :: DirectDebitCollection -> Document
message col = Document prologue root epilogue
  where
    prologue = Prologue [] Nothing []
    root = Element "CstmrDrctDbtInitn" M.empty [grpHdr col]
    epilogue = []

grpHdr :: DirectDebitCollection -> Node
grpHdr col = nodeElem "GrpHdr" (map ($ col) [msgId, creDtTm, nbOfTxs, ctrlSum])

msgId :: DirectDebitCollection -> Node
msgId col = nodeContent "MsgId" (col ^. messageId)

creDtTm :: DirectDebitCollection -> Node
creDtTm col = nodeElem "CreDtTm" [nodeDate, nodeT, nodeTime]
  where
    nodeDate     = NodeContent $ pack $ T.showGregorian (col ^. creationDay)
    nodeT        = NodeContent "T"
    nodeTime     = NodeContent $ pack isoTime
    (isoTime, _) = break (=='.') $ show (col ^. creationTimeOfDay)

nbOfTxs :: DirectDebitCollection -> Node
nbOfTxs col = nodeContent "NbOfTxs" (length (col ^. debits))

ctrlSum :: DirectDebitCollection -> Node
ctrlSum col = nodeContent "CtrlSum" content
  where content = priceToText $ sumOf (debits.traverse.items.traverse.finalPrice) col


-- Helper functions for nodes without attributes

nodeElem :: Name -> [Node] -> Node
nodeElem name nodes = NodeElement $ Element name M.empty nodes

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


-- Rendering and writing of messages

renderMessage :: DirectDebitCollection -> LT.Text
renderMessage col = renderText settings (message col)
  where
    settings = def { rsPretty = False }

-- | Write direct debits instructions message to XML file, with a decent pretty-printer
-- (the one coming with Text.XML puts significant whitespace in content nodes).
writeMessageToFile :: DirectDebitCollection -> IO ()
writeMessageToFile col = do
  -- TODO: handle possible error
  let (Just xmlParsedLight) = LXML.parseXMLDoc (renderMessage col)
  writeFile "Test.xml" (LXML.ppTopElement xmlParsedLight)


-- Test data

ddc :: IO DirectDebitCollection
ddc = do
  (Just ddcE) <- DB.runDb $ DB.selectFirst ([] :: [DB.Filter DirectDebitCollection]) []
  return $ DB.entityVal ddcE

insertDDC :: IO ()
insertDDC = DB.runDb $ do
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
      ddc_ = mkDirectDebitCollection "New collection" now c [dd1, dd2]
  liftIO $ putStrLn "Insert"
  DB.deleteWhere ([] :: [DB.Filter DirectDebitCollection])
  DB.insert_ ddc_
  return ()
