{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Sepa.Controller.DirectDebit where

import           Control.Lens             hiding (element, elements, index, set, view)
import           Data.List
import qualified Data.Text                as T (Text, pack, replace, strip, unpack)
import qualified Data.Text.Lazy           as TL (unpack)
import qualified Data.Time.Calendar       as C
import qualified Data.Time.LocalTime      as C
import qualified Database.Persist.MongoDB as DB
import           Formatting               hiding (builder)
import           Graphics.UI.Gtk
import           Sepa.Controller.Class
import           Sepa.Controller.BillingConcept -- TODO: Drop this dependency (price funcs)
import           Sepa.DirectDebit

data DirectDebitsController = DD PanelId Builder

instance Controller DirectDebitsController where

  type E DirectDebitsController = DirectDebitSet

  type S DirectDebitsController = ComboBox

  data D DirectDebitsController = DDD

  builder  (DD _        builder_) = builder_

  panelId  (DD panelId_ _       ) = panelId_

  selector = getGladeObject castToComboBox "_Cb"

  setSelectorModel s m _c = comboBoxSetModel s (Just m)

  setSelectorRenderers s m c = do
    renderer <- cellRendererTextNew
    cellLayoutPackStart s renderer False
    let renderFunc = T.unpack . (^. description) . DB.entityVal
    cellLayoutSetAttributes s renderer m (\row -> [cellText := renderFunc row])

  -- setSelectorSorting   s ls sm = setTreeViewSorting   s ls sm orderings
  --   where orderings = repeat compare -- TODO: catalan collation

  -- setSelectorSearching s ls sm = setTreeViewSearching s ls sm isPartOf
  --   where tx `isPartOf` txs = any (tx `isInfixOf`) txs -- TODO: better searching

  renderers _ = return [ C.showGregorian . C.localDay . C.zonedTimeToLocalTime . (^. creationTime) . DB.entityVal
                       ]

  editEntries c = do
    e1 <- getGladeObject castToEntry "_EE_editionDateEn" c
    return [e1]

  readData [] _ = return DDD
  readData _ _  = error "readData (DD): wrong number of entries"

  -- validData d _ =
  --   return $ validBillingConcept (longNameD d) (shortNameD d) (basePriceD d) (vatRatioD d)

  -- createFromData d _c =
  --   return $ mkBillingConcept (longNameD d) (shortNameD d) (basePriceD d) (vatRatioD d)

  updateFromData d _old = createFromData d

  selectElement iter comboBox _sortedModel _c = comboBoxSetActiveIter comboBox iter

  connectSelector comboBox sortedModel setState _c = do
    let onSelectionChangedAction = do
          mIter <- comboBoxGetActiveIter comboBox
          case mIter of
            (Just iter) -> do
              childIter <- treeModelSortConvertIterToChildIter sortedModel iter
              setState (Sel childIter)
            Nothing   -> return ()
    _ <- on comboBox changed onSelectionChangedAction
    return onSelectionChangedAction

