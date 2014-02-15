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
message col = Document prologue root epilogue
  where
    prologue = Prologue [] Nothing []
    root = Element "CstmrDrctDbtInitn" M.empty (map ($ col) content)
    epilogue = []
    content = grpHdr : []

grpHdr :: DirectDebitSet -> Node                                      -- +
grpHdr = nodeElem "GrpHdr" content
  where
    content = [msgId, creDtTm, nbOfTxs, ctrlSum, initgPty]

msgId :: DirectDebitSet -> Node                                       -- ++
msgId col = nodeContent "MsgId" (col ^. messageId)

creDtTm :: DirectDebitSet -> Node                                     -- ++
creDtTm col = nodeContent "CreDtTm" (isoDate ++ "T" ++ isoTime)
  where
    isoDate      = T.showGregorian (col ^. creationDay)
    (isoTime, _) = break (=='.') $ show (col ^. creationTimeOfDay)

nbOfTxs :: DirectDebitSet -> Node                                     -- ++
nbOfTxs col = nodeContent "NbOfTxs" (length (col ^. debits))

ctrlSum :: DirectDebitSet -> Node                                     -- ++
ctrlSum col = nodeContent "CtrlSum" content
  where content = priceToText $ sumOf (debits.traverse.items.traverse.finalPrice) col

initgPty :: DirectDebitSet -> Node                                    -- ++
initgPty col = nodeElem "InitgPty" content (col ^. creditor)
  where
    content = [nm]

nm :: Creditor -> Node                                                -- +++
nm creditor_ = nodeContent "Nm" (creditor_ ^. fullName)

-- pmtInf_L :: DirectDebitSet -> [Node]                               -- +
-- pmtInf_L col = 



-- Helper functions for nodes without attributes

nodeElem :: Name -> [a -> Node] -> a -> Node
nodeElem name funcs parent = NodeElement $ Element name M.empty (map ($ parent) funcs)

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
renderMessage col = renderText settings (message col)
  where
    settings = def { rsPretty = False }

-- | Write direct debits instructions message to XML file, with a decent pretty-printer
-- (the one coming with Text.XML puts significant whitespace in content nodes).
writeMessageToFile :: DirectDebitSet -> IO ()
writeMessageToFile col = do
  -- TODO: handle possible error
  let (Just xmlParsedLight) = LXML.parseXMLDoc (renderMessage col)
  writeFile "Test.xml" (LXML.ppTopElement xmlParsedLight)


-- Test data

ddc :: IO DirectDebitSet
ddc = do
  (Just ddcE) <- DB.runDb $ DB.selectFirst ([] :: [DB.Filter DirectDebitSet]) []
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
      ddc_ = mkDirectDebitSet "New collection" now c [dd1, dd2]
  liftIO $ putStrLn "Insert"
  DB.deleteWhere ([] :: [DB.Filter DirectDebitSet])
  DB.insert_ ddc_
  return ()
