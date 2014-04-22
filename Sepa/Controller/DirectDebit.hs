{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Sepa.Controller.DirectDebit where

import           Control.Lens             hiding (element, elements, index, set, view)
import           Control.Monad
import           Data.Maybe
import qualified Data.Text                as T (Text, unpack)
import qualified Data.Time.Calendar       as C
import qualified Data.Time.LocalTime      as C
import qualified Database.Persist.MongoDB as DB
import           Graphics.UI.Gtk
import           Sepa.BillingConcept
import           Sepa.Controller.BillingConcept -- TODO: Drop this dependency (price funcs)
import           Sepa.Controller.Class
import           Sepa.Controller.TreeView
import           Sepa.Debtor
import           Sepa.DirectDebit

data DirectDebitsController =
  DD
  { panelId_ :: PanelId
  , builder_ :: Builder
  , itemsTv  :: TreeView
  , itemsLs  :: ListStore Item
  }

instance Controller DirectDebitsController where

  type E DirectDebitsController = DirectDebitSet

  type S DirectDebitsController = ComboBox

  data D DirectDebitsController = DDD

  builder = builder_

  panelId = panelId_

  selector = getGladeObject castToComboBox "_Cb"

  setSelectorModel s m _c = comboBoxSetModel s (Just m)

  setSelectorRenderers comboBox listStore _c = do
    renderer <- cellRendererTextNew
    cellLayoutPackStart comboBox renderer False
    let renderFunc = T.unpack . (^. description) . DB.entityVal
    cellLayoutSetAttributes comboBox renderer listStore (\row -> [cellText := renderFunc row])

  setSelectorSorting _comboBox listStore sortedModel _c = do
    let renderFunc = T.unpack . (^. description) . DB.entityVal
    treeSortableSetSortFunc sortedModel 0 $ \xIter yIter -> do
      xRow <- customStoreGetRow listStore xIter
      yRow <- customStoreGetRow listStore yIter
      return $ compare (renderFunc xRow) (renderFunc yRow)

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

  putElement' iter ls c = do
    ddsE <- treeModelGetRow ls iter
    let dds = DB.entityVal ddsE
    listStoreClear (itemsLs c)
    forM_ (dds ^.. debits.traverse) $ \dd ->
      forM_ (dd ^.. items.traverse) $ \bc -> do
        let item = Item { itemLastName    = dd ^. debtorLastName
                        , itemFirstName   = dd ^. debtorFirstName
                        , itemShortName   = bc ^. shortName
                        , itemActualPrice = bc ^. basePrice}
        listStoreAppend (itemsLs c) item



data Item =
  Item
  { itemLastName    :: T.Text
  , itemFirstName   :: T.Text
  , itemShortName   :: T.Text
  , itemActualPrice :: Int
  }


-- | Calls Controller::mkController and then adds special functionality for direct debit
-- sets.
mkController' :: (TreeModelClass (bcModel (DB.Entity Sepa.BillingConcept.BillingConcept)),
                  TreeModelClass (deModel (DB.Entity Sepa.Debtor.Debtor)),
                  TypedTreeModelClass bcModel, TypedTreeModelClass deModel) =>
                 DB.ConnectionPool
              -> (MainWindowState -> IO ())
              -> DirectDebitsController
              -> bcModel (DB.Entity Sepa.BillingConcept.BillingConcept)
              -> deModel (DB.Entity Sepa.Debtor.Debtor)
              -> IO ()
mkController' db setMainState c bcLs deLs = do
  _ <- mkController db setMainState c
  let orderings = repeat compare -- TODO: catalan collation

  -- FIXME: use treeModelFilterRefilter every time deLs and bcLs could have changed

  -- billing concepts TreeView
  bcTv   <- getGladeObject castToTreeView "_billingConceptsTv" c
  bcSm   <- treeModelSortNewWithModel bcLs
  let bcRf = [ T.unpack      . (^. longName)   . DB.entityVal
             , priceToString . (^. basePrice)  . DB.entityVal
             , priceToString . (^. finalPrice) . DB.entityVal
             ]
  treeViewSetModel          bcTv              bcSm
  setTreeViewRenderers      bcTv bcLs                        bcRf
  setTreeViewSorting        bcTv bcLs Nothing bcSm orderings bcRf

  -- debtors TreeView
  deTv   <- getGladeObject castToTreeView "_debtorsTv" c
  let deRf = [ T.unpack      . (^. lastName)   . DB.entityVal
             , T.unpack      . (^. firstName)  . DB.entityVal
             ]
  deFm   <- treeModelFilterNew deLs []
  zonedTime <- C.getZonedTime
  let today  = C.localDay (C.zonedTimeToLocalTime zonedTime)
  treeModelFilterSetVisibleFunc deFm $ \iter -> do
    entity <- treeModelGetRow deLs iter
    return $ isJust (getActiveMandate today (DB.entityVal entity))
  deSm  <- treeModelSortNewWithModel deFm
  treeViewSetModel           deTv                  deSm
  setTreeViewRenderers       deTv deLs                            deRf
  setTreeViewSorting         deTv deLs (Just deFm) deSm orderings deRf

  return ()
