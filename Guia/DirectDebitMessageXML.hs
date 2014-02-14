{-# LANGUAGE
  FlexibleContexts,
  FlexibleInstances,
  GADTs,
  NoImplicitPrelude,
  OverloadedStrings,
  TypeFamilies
  #-}

module Guia.DirectDebitMessageXML where

import           ClassyPrelude          hiding (Element, Text)
import           Control.Lens
import qualified Data.List                                                      as L
import qualified Data.Map                                                       as M
import           Data.Text.Lazy
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
grpHdr col =
  NodeElement $ Element "GrpHdr" M.empty (map ($ col) [msgId, creDtTm, nbOfTxs])

msgId :: DirectDebitCollection -> Node
msgId col = NodeElement $ Element "MsgId" M.empty [NodeContent (col ^. messageId)]

creDtTm :: DirectDebitCollection -> Node
creDtTm col = NodeElement $ Element "CreDtTm" M.empty [nodeDate, nodeT, nodeTime]
  where
    nodeDate     = NodeContent $ pack $ T.showGregorian (col ^. creationDay)
    nodeT        = NodeContent "T"
    nodeTime     = NodeContent $ pack isoTime
    (isoTime, _) = break (=='.') $ show (col ^. creationTimeOfDay)

nbOfTxs :: DirectDebitCollection -> Node
nbOfTxs col = NodeElement $ Element "NbOfTxs" M.empty []

renderMessage :: DirectDebitCollection -> Text
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

insertDDC :: IO ()
insertDDC = DB.runDb $ do
  now <- liftIO T.getZonedTime
  liftIO $ putStrLn "Get creditor"
  (Just cE)  <- DB.selectFirst ([] :: [DB.Filter Creditor])       []
  liftIO $ putStrLn "Get debtor"
  (Just dE)  <- DB.selectFirst ([] :: [DB.Filter Debtor])         []
  liftIO $ putStrLn "Get billing concept"
  bcEL       <- DB.selectList  ([] :: [DB.Filter BillingConcept]) []
  let c = DB.entityVal cE
      d = DB.entityVal dE
      (m1 : _) = d ^.. mandates.traversed
      bcL = map DB.entityVal bcEL
      dd = mkDirectDebit (d ^. firstName) (d ^. lastName) m1 bcL ""
      ddc = mkDirectDebitCollection "New collection" now c [dd]
  liftIO $ putStrLn "Insert"
  DB.insert_ ddc
  return ()
