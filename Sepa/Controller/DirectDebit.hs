{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Sepa.Controller.DirectDebit where

import           Control.Lens                   hiding (element, elements,
                                                 index, set, view)
import           Control.Arrow
import           Control.Monad
import           Data.List                      (groupBy, sortBy)
import           Data.Maybe
import           Data.Ord
import qualified Data.Text                      as T
import qualified Data.Time.Calendar             as C
import qualified Data.Time.LocalTime            as C
import qualified Database.Persist.MongoDB       as DB
import           Graphics.UI.Gtk
import           Sepa.BillingConcept
import           Sepa.Controller.BillingConcept
import           Sepa.Controller.Class
import           Sepa.Controller.TreeView
import           Sepa.Creditor
import           Sepa.Debtor
import           Sepa.DirectDebit
import qualified Text.Printf                    as PF (printf)

data DirectDebitsController =
  DD
  { panelId_ :: PanelId
  , builder_ :: Builder
  , itemsTv  :: TreeView
  , itemsLs  :: ListStore Item
  , itemsSm  :: TypedTreeModelSort Item
  }

data Item =
  Item
  { itemLastName  :: T.Text
  , itemFirstName :: T.Text
  , itemMandate   :: Mandate
  , item          :: BillingConcept
  }

instance Controller DirectDebitsController where

  type E DirectDebitsController = DirectDebitSet

  type S DirectDebitsController = ComboBox

  data D DirectDebitsController =
    DDD
    { descriptionD  :: T.Text
    , creationTimeD :: C.ZonedTime
    , debitsD       :: [DirectDebit]
    }

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
    let firstColumnId = 0
    treeSortableSetSortFunc sortedModel firstColumnId $ \xIter yIter -> do
      xRow <- customStoreGetRow listStore xIter
      yRow <- customStoreGetRow listStore yIter
      return $ compare (renderFunc xRow) (renderFunc yRow)
    -- Force initial sorting: descending (newest dds first)
    treeSortableSetSortColumnId sortedModel firstColumnId SortDescending

  renderers _ = do
    let zonedTimeToGregorian = C.showGregorian . C.localDay . C.zonedTimeToLocalTime
    return [ T.unpack             . (^. description)  . DB.entityVal
           , zonedTimeToGregorian . (^. creationTime) . DB.entityVal
           ]

  editEntries c = do
    e1 <- getGladeObject castToEntry "_EE_descriptionEn" c
    e2 <- getGladeObject castToEntry "_EE_editionDateEn" c
    return [e1, e2]

  editWidgets c = do
    w1 <- getGladeObject castToTreeView "_debtorsTv"         c
    w2 <- getGladeObject castToTreeView "_billingConceptsTv" c
    w3 <- getGladeObject castToTreeView "_itemsTv"           c
    return [toWidget w1, toWidget w2, toWidget w3]

  selectWidgets c = do
    w1 <- getGladeObject castToButton "_cloneBt" c
    w2 <- getGladeObject castToButton "_printBt" c
    return [toWidget w1, toWidget w2]

  readData [descriptionEn, _] c = do
    description_ <- get descriptionEn entryText
    today <- C.getZonedTime
    debits_ <- readItems c 
    return DDD { descriptionD  = T.pack description_
               , creationTimeD = today
               , debitsD       = debits_ }

  readData _ _  = error "readData (DD): wrong number of entries"

  -- Impossible to create invalid direct debit set with the GUI interface.
  validData _ _ = return True

  createFromData (DDD description_ creation_ debits_) db _c = do
    mCreditor <- flip DB.runMongoDBPoolDef db $ DB.selectFirst ([] :: [DB.Filter Creditor]) []
    case mCreditor of
      Nothing        -> error "DirectDebitsController::createFromData: no creditor"
      Just creditor_ ->
        return $ mkDirectDebitSet description_ creation_ (DB.entityVal creditor_) debits_

  updateFromData d _old = createFromData d

  selectElement iter comboBox sortedModel _c = do
    sortedIter <- treeModelSortConvertChildIterToIter sortedModel iter
    comboBoxSetActiveIter comboBox sortedIter

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
        let item_ = Item { itemLastName    = dd ^. debtorLastName
                         , itemFirstName   = dd ^. debtorFirstName
                         , itemMandate     = dd ^. mandate
                         , item            = bc }
        listStoreAppend (itemsLs c) item_
    let debits_ = dds ^. debits
    computeAndShowTotals debits_ c

-- | Calls Controller::mkController and then adds special functionality for direct debit
-- sets.
mkController' :: (TreeModelClass (bcModel (DB.Entity Sepa.BillingConcept.BillingConcept)),
                  TreeModelClass (deModel (DB.Entity Sepa.Debtor.Debtor)),
                  TypedTreeModelClass bcModel, TypedTreeModelClass deModel,
                  deModel ~ ListStore, bcModel ~ ListStore) =>
                 DB.ConnectionPool
              -> (MainWindowState -> IO ())
              -> DirectDebitsController
              -> bcModel (DB.Entity Sepa.BillingConcept.BillingConcept)
              -> deModel (DB.Entity Sepa.Debtor.Debtor)
              -> IO ()
mkController' db setMainState c bcLs deLs = do
  (setState, ls, sm) <- mkController db setMainState c
  let orderings = repeat compare -- TODO: catalan collation

  -- FIXME: use treeModelFilterRefilter every time deLs and bcLs could have changed
  -- TODO: Insert an empty dds in selector (not in the database)

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

  -- Visibility of extra buttons

  actualBasePriceEn <- getGladeObject castToEntry  "_actualBasePriceEn" c
  addItemBt         <- getGladeObject castToButton "_addItemBt"         c
  deleteItemBt      <- getGladeObject castToButton "_deleteItemBt"      c
  set actualBasePriceEn [widgetSensitive := False]
  set addItemBt         [widgetSensitive := False]
  set deleteItemBt      [widgetSensitive := False]

  -- Connect buttons

  cloneBt <- getGladeObject castToButton "_cloneBt" c

  _ <- on cloneBt buttonActivated $ do
    selector_ <- selector c
    incrementCreditorMessageCount db
    mCreditor <- flip DB.runMongoDBPoolDef db $ DB.selectFirst ([] :: [DB.Filter Creditor]) []
    creditor_ <- case mCreditor of
      Nothing        -> error "DirectDebitsController::mkController': no creditor"
      Just creditorE -> return $ DB.entityVal creditorE
    let description_ = T.concat [ T.filter (/= ' ') (creditor_ ^. fullName)
                                , "_"
                                , T.pack (C.showGregorian today)
                                , "_"
                                , T.pack (PF.printf "%04d" (creditor_ ^. messageCount))
                                ]
    mIter <- comboBoxGetActiveIter selector_
    debits_ <- case mIter of
      Nothing     -> error "DirectDebitsController::mkController': no combobox iter"
      (Just iter) -> do
        childIter <- treeModelSortConvertIterToChildIter sm iter
        ddsE      <- treeModelGetRow ls childIter
        return $ DB.entityVal ddsE ^. debits
    let newDdsV = mkDirectDebitSet description_ zonedTime creditor_ debits_
    newDdsK <- flip DB.runMongoDBPoolDef db $ DB.insert newDdsV
    listStorePrepend ls (DB.Entity newDdsK newDdsV)
    mNewIter <- treeModelGetIterFirst ls
    newIter <- case mNewIter of
      Nothing      -> error "DirectDebitsController::mkController': no new iter to cloned"
      Just iter    -> return iter
    sortedIter <- treeModelSortConvertChildIterToIter sm newIter
    comboBoxSetActiveIter selector_ sortedIter
    setState (Sel newIter)

  deSel <- treeViewGetSelection deTv
  bcSel <- treeViewGetSelection bcTv

  let onSelChangedAction = do
        countDeSel <- treeSelectionCountSelectedRows deSel
        countBcSel <- treeSelectionCountSelectedRows bcSel
        forM_ [toWidget addItemBt, toWidget actualBasePriceEn] $
          if countDeSel > 0 && countBcSel > 0
          then (`set` [widgetSensitive := True])
          else (`set` [widgetSensitive := False])
        -- When countBcSel > 0 there should be exactly 1 selected billing concept
        if countBcSel > 0 then treeSelectionSelectedForeach bcSel $ \iter -> do
          childIter <- treeModelSortConvertIterToChildIter bcSm iter
          bcE <- treeModelGetRow bcLs childIter
          set actualBasePriceEn [entryText := priceToString (DB.entityVal bcE ^. basePrice)]
        else
          set actualBasePriceEn [entryText := ""]

  _ <- on deSel treeSelectionSelectionChanged onSelChangedAction
  _ <- on bcSel treeSelectionSelectionChanged onSelChangedAction

  itemsSel <- treeViewGetSelection (itemsTv c)

  _ <- on itemsSel treeSelectionSelectionChanged $ do
    countItemsSel <- treeSelectionCountSelectedRows itemsSel
    set deleteItemBt $
      if countItemsSel > 0 then [widgetSensitive := True] else [widgetSensitive := False]

  _ <- on addItemBt buttonActivated $ do
    priceAsString <- get actualBasePriceEn entryText
    mDeIter <- treeSelectionGetSelected deSel
    mBcIter <- treeSelectionGetSelected bcSel
    case (mDeIter, mBcIter) of
      (Just deIter, Just bcIter) -> do
        deFilterIter <- treeModelSortConvertIterToChildIter deSm deIter
        deChildIter  <- treeModelFilterConvertIterToChildIter deFm deFilterIter
        bcChildIter  <- treeModelSortConvertIterToChildIter bcSm bcIter
        deE          <- listStoreGetValue deLs (listStoreIterToIndex deChildIter)
        bcE          <- listStoreGetValue bcLs (listStoreIterToIndex bcChildIter)
        let bc_   = setBasePrice (stringToPrice priceAsString) (DB.entityVal bcE)
            item_ = Item { itemLastName    = DB.entityVal deE ^. lastName
                         , itemFirstName   = DB.entityVal deE ^. firstName
                         -- WARNING: use of head
                         -- TRUST: There is at least one mandate, and is active
                         , itemMandate     = head (DB.entityVal deE ^. mandates)
                         , item            = bc_ }
        _index <- listStoreAppend (itemsLs c) item_
        -- TODO: select appended item and make selection visible
        -- TODO: cheaper totals calculation (readItems needs to sort)
        debits_ <- readItems c
        computeAndShowTotals debits_ c
      _ -> error "DirectDebitsController::mkController': de or bc iter missing"
    return ()

  _ <- on deleteItemBt buttonActivated $ do
    mIter <- treeSelectionGetSelected itemsSel
    case mIter of
      Nothing   -> error "DirectDebitsController::mkController': no items iter on deletion"
      Just iter -> do
        childIter <- treeModelSortConvertIterToChildIter (itemsSm c) iter
        listStoreRemove (itemsLs c) (listStoreIterToIndex childIter)
        -- TODO: cheaper totals calculation (readItems needs to sort)
        debits_ <- readItems c
        computeAndShowTotals debits_ c

  return ()

readItems :: DirectDebitsController -> IO [DirectDebit]
readItems c = do
  let nameEq  (Item last1 fst1 _ _) (Item last2 fst2 _ _) = last1 == last2 && fst1 == fst2
      -- Lexicographic order, no need of a specific collation
      nameOrd = comparing (itemLastName &&& itemFirstName)
                -- can be
  items_  <- listStoreToList (itemsLs c)
  let items' = (groupBy nameEq . sortBy nameOrd) items_
  forM items' $ \itL -> do
    -- TRUST: the use of goupBy ensures that length itL > 0
    let it = head itL
    -- TRUST: no invalid debits can be created (so we don't call validDirectDebit)
    return $ mkDirectDebit (itemFirstName it) (itemLastName it) (itemMandate it) (map item itL)

computeAndShowTotals :: [DirectDebit] -> DirectDebitsController -> IO ()
computeAndShowTotals debits_ c = do
  basePriceEn      <- getGladeObject castToEntry "_basePriceEn"      c
  finalPriceEn     <- getGladeObject castToEntry "_finalPriceEn"     c
  numberOfDebitsEn <- getGladeObject castToEntry "_numberOfDebitsEn" c
  set basePriceEn  [entryText := T.unpack . priceToText $ sumOf (traverse.items.traverse.basePrice)  debits_]
  set finalPriceEn [entryText := T.unpack . priceToText $ sumOf (traverse.items.traverse.finalPrice) debits_]
  set numberOfDebitsEn [entryText := show (length debits_)]
