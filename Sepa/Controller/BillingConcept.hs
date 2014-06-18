{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Sepa.Controller.BillingConcept where

import           Control.Lens             hiding (element, elements, index, set, view)
import           Data.List
import qualified Data.Text                as T (Text, pack, replace, strip, unpack)
import qualified Data.Text.Lazy           as TL (unpack)
import qualified Database.Persist.MongoDB as DB
import           Formatting               hiding (builder)
import           Graphics.UI.Gtk
import           Sepa.Controller.Class
import           Sepa.Controller.TreeView
import           Sepa.BillingConcept

data BillingConceptsController = BC PanelId Builder

instance Controller BillingConceptsController where

  type E BillingConceptsController = BillingConcept

  type S BillingConceptsController = TreeView

  data D BillingConceptsController =
    DBC { shortNameD   :: T.Text
        , longNameD    :: T.Text
        , basePriceD   :: Maybe Int
        , vatRatioD    :: Maybe Int }

  builder  (BC _        builder_) = builder_

  panelId  (BC panelId_ _       ) = panelId_

  selector = getGladeObject castToTreeView "_Tv"

  setSelectorModel s m _c = treeViewSetModel s m

  setSelectorRenderers s m c = do
    renderFuncs <- renderers c
    setTreeViewRenderers s m renderFuncs

  setSelectorSorting s ls sm c = do
    let orderings = repeat compare -- TODO: catalan collation
    renderFuncs <- renderers c
    setTreeViewSorting s ls Nothing sm orderings renderFuncs

  setSelectorSearching s ls sm c = do
    renderFuncs <- renderers c
    let tx `isPartOf` txs = any (tx `isInfixOf`) txs -- TODO: better searching
    setTreeViewSearching s ls sm isPartOf renderFuncs

  renderers _ = return [ T.unpack      . (^. longName)   . DB.entityVal
                       , T.unpack      . (^. shortName)  . DB.entityVal
                       , priceToString . (^. basePrice)  . DB.entityVal
                       , priceToString . (^. vatRatio)   . DB.entityVal
                       , priceToString . (^. finalPrice) . DB.entityVal
                       ]

  editEntries c =  do e1 <- getGladeObject castToEntry "_EE_longNameEn"   c
                      e2 <- getGladeObject castToEntry "_EE_shortNameEn"  c
                      e3 <- getGladeObject castToEntry "_EE_basePriceEn"  c
                      e4 <- getGladeObject castToEntry "_EE_vatRatioEn"   c
                      e5 <- getGladeObject castToEntry "_EE_finalPriceEn" c
                      return [e1, e2, e3, e4, e5]

  priceEntries c = do e3 <- getGladeObject castToEntry "_EE_basePriceEn"  c
                      e4 <- getGladeObject castToEntry "_EE_vatRatioEn"   c
                      return [e3, e4]

  readData [longNameEn, shortNameEn, basePriceEn, vatRatioEn, _] _ = do
    longName_    <- get longNameEn    entryText
    shortName_   <- get shortNameEn   entryText
    basePrice_   <- get basePriceEn   entryText
    vatRatio_    <- get vatRatioEn    entryText
    return DBC { shortNameD  = T.strip (T.pack shortName_)
               , longNameD   = T.strip (T.pack longName_)
               , basePriceD  = Just (stringToPrice basePrice_)
               , vatRatioD   = Just (stringToPrice vatRatio_)
               }
  readData _ _ = error "readData (BC): wrong number of entries"

  validData d _ =
    return $ validBillingConcept (shortNameD d) (longNameD d) (basePriceD d) (vatRatioD d)

  createFromData d _db _c =
    return $ mkBillingConcept (shortNameD d) (longNameD d) (basePriceD d) (vatRatioD d)

  updateFromData d _old = createFromData d

  selectElement it s sm _c = selectTreeViewElement it s sm

  connectSelector s sm st _c = connectTreeView s sm st

-- TODO: Merge this function with BC.priceToText
-- | We pad with spaces, to correct show prices in right-aligned columns.
priceToString :: Int -> String
priceToString num = TL.unpack $ format (left padding ' ') (toText num)
  where
    toText    = T.replace "." (T.pack separator) . priceToText
    separator = ","             -- FIXME: Take separator from locale
    padding   = 10

-- | Pre: @str@ contains a decimal number with a maximum of two digits fractional parts
-- (possibly surrounded by blanks, that @read@ ignores).
-- Post: the result is the number represented by @str@ multiplied by 100.
stringToPrice :: String -> Int
stringToPrice str =
  let (integer, fractional') = break (== separator) str
      separator = ','           -- FIXME: Take separator from locale
      fractional = case fractional' of
        []             -> "00"  -- Case no separator is used, no fractional part
        _ : []         -> "00"  -- Void fractional part
        _ : x : []     -> x : "0"
        _ : x : y : [] -> x : [y]
        _              -> error "stringToPrice: Too long fractional part"
  in read $ integer ++ fractional
