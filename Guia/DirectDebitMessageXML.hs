{-# LANGUAGE
  FlexibleContexts,
  FlexibleInstances,
  GADTs,
  NoImplicitPrelude,
  OverloadedStrings,
  TypeFamilies
  #-}

module Guia.DirectDebitMessageXML where

import           ClassyPrelude           hiding (Text)
import           Control.Lens
import qualified Data.List                                                      as L
import qualified Data.Map                                                       as M
import           Data.Text.Lazy          hiding (map)
import           Guia.BillingConcept
import           Guia.Creditor
import           Guia.Debtor
import           Guia.DirectDebit
import qualified Text.Printf                                                    as PF
  (printf)
import           Text.XML

-- For testing only
import qualified Database.Persist.MongoDB as DB
import qualified Guia.MongoUtils as DB
import qualified Data.Time.LocalTime as T

message :: DirectDebitCollection -> Document
message col = Document prologue root epilogue
  where
    prologue = Prologue [] Nothing []
    root = Element "my-name" M.empty []
    epilogue = []

renderMessage :: DirectDebitCollection -> Text
renderMessage col = renderText settings (message col)
  where
    settings = def { rsPretty = True }


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
