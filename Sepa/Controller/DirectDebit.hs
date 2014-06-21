{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Sepa.Controller.DirectDebit where

import qualified ClassyPrelude
import           Control.Arrow
import           Control.Lens                      hiding (element, elements,
                                                    index, set, view)
import           Control.Monad
import           Data.IORef
import           Data.List                         (groupBy, sortBy)
import qualified Data.Map                          as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Text                         as T
import qualified Data.Time.Calendar                as C
import qualified Data.Time.LocalTime               as C
import qualified Database.Persist.MongoDB          as DB
import           Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.Internal as Cairo.Internal
import           Graphics.Rendering.Pango
import           Graphics.UI.Gtk
import           Sepa.BillingConcept
import           Sepa.Controller.BillingConcept
import           Sepa.Controller.Class
import           Sepa.Controller.TreeView
import           Sepa.Creditor
import           Sepa.Debtor
import           Sepa.DirectDebit
import           Sepa.DirectDebitMessageXML
import qualified Text.Printf                       as PF (printf)

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
    w2 <- getGladeObject castToButton "_fileBt"  c
    w3 <- getGladeObject castToButton "_printBt" c
    return [toWidget w1, toWidget w2, toWidget w3]

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
  (setState, stRef, ls, sm) <- mkController db setMainState c
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

  priceEntryIdRef <- mkPriceEntry actualBasePriceEn

  let getDirectDebitSet selector_ = do
        mIter <- comboBoxGetActiveIter selector_
        case mIter of
          Nothing     -> error "DirectDebitsController::mkController': no combobox iter"
          (Just iter) -> do
            childIter <- treeModelSortConvertIterToChildIter sm iter
            ddsE      <- treeModelGetRow ls childIter
            return $ DB.entityVal ddsE

  cloneBt <- getGladeObject castToButton "_cloneBt" c

  -- Creditor's message count is incremented on dds cloning

  _ <- on cloneBt buttonActivated $ do
    selector_ <- selector c
    incrementCreditorMessageCount db
    -- mCreditor <- flip DB.runMongoDBPoolDef db $ DB.selectFirst ([] :: [DB.Filter Creditor]) []
    -- creditor_ <- case mCreditor of
    --   Nothing        -> error "DirectDebitsController::mkController': no creditor"
    --   Just creditorE -> return $ DB.entityVal creditorE
    -- let description_ = T.concat [      T.filter (/= ' ') (creditor_ ^. fullName)
    --                             , "_", T.pack (C.showGregorian today)
    --                             , "_", T.pack (PF.printf "%04d" (creditor_ ^. messageCount))
    --                             ]
    -- dds <- getDirectDebitSet selector_
    -- let debits_ = dds ^. debits
    -- let newDdsV = mkDirectDebitSet description_ zonedTime creditor_ debits_
    -- newDdsK <- flip DB.runMongoDBPoolDef db $ DB.insert newDdsV
    dds <- getDirectDebitSet selector_
    (newDdsK, newDdsV) <- DB.runMongoDBPoolDef (cloneDdsAction dds zonedTime) db
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
          do { priceEntryId <- readIORef priceEntryIdRef; signalBlock priceEntryId }
          set actualBasePriceEn [entryText := priceToString (DB.entityVal bcE ^. basePrice)]
          do { priceEntryId <- readIORef priceEntryIdRef; signalUnblock priceEntryId }
        else do
          do { priceEntryId <- readIORef priceEntryIdRef; signalBlock priceEntryId }
          set actualBasePriceEn [entryText := ""]
          do { priceEntryId <- readIORef priceEntryIdRef; signalUnblock priceEntryId }

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

  fileBt <- getGladeObject castToButton "_fileBt" c

  _ <- on fileBt buttonActivated $ do
    Sel iter <- readIORef stRef -- FIXME: unsafe pattern
    ddsE <- treeModelGetRow ls iter
    let dds = DB.entityVal ddsE
    banks <- flip DB.runMongoDBPoolDef db $ DB.selectList ([] :: [DB.Filter SpanishBank]) []
    let banksMap = M.fromList $ map (\e -> (DB.entityVal e ^. fourDigitsCode, DB.entityVal e)) banks
    fileChsDg <- getGladeObject castToFileChooserDialog "_fileChooserDg" c
    isSentDg  <- getGladeObject castToDialog            "_isSentDg"      c
    fileChooserSetCurrentName fileChsDg $ T.unpack (dds ^. description) ++ ".xml"
    widgetShow fileChsDg
    fileChsResp <- dialogRun fileChsDg
    widgetHide fileChsDg
    case fileChsResp of
      ResponseCancel       -> return ()
      ResponseOk           -> do
        mFilePath <- fileChooserGetFilename fileChsDg
        case mFilePath of
          Nothing   -> error "No file name"
          Just filePath -> do
            -- FIXME: handle file writing IO error
            writeMessageToFile dds banksMap (ClassyPrelude.fromString filePath)
            widgetShow isSentDg
            isSentResp <- dialogRun isSentDg
            widgetHide isSentDg
            case isSentResp of
              ResponseYes -> do
                -- Update lastTimeActive for all mandates used in dds
                -- TODO: update mandate info in debtors panel
                let ddsDay = C.localDay (C.zonedTimeToLocalTime (dds ^. creationTime))
                forM_ (dds ^. debits) $ \debit ->
                  updateMandateLastTimeActive db (debit ^. mandate ^. mandateRef) ddsDay
              ResponseNo  -> return ()
              _           -> error "Bad response on isSentDg"
      ResponseDeleteEvent  -> return ()
      _                    -> error "Bad response on file chooser"

  -- Printing

  mainWd  <- builderGetObject (builder c) castToWindow "mainWd"
  printBt <- getGladeObject castToButton "_printBt" c

  _ <- on printBt buttonActivated $ do
    printOp <- printOperationNew
    a4 <- paperSizeNew (Just "iso_a4_210x297mm")
    pageSetup <- pageSetupNew
    pageSetupSetPaperSizeAndDefaultMargins pageSetup a4
    pageSetupSetTopMargin    pageSetup 15 UnitMm
    pageSetupSetBottomMargin pageSetup 12 UnitMm
    pageSetupSetLeftMargin   pageSetup 12 UnitMm
    pageSetupSetRightMargin  pageSetup 50 UnitMm
    -- FIXME: The following commented line doesn't work
    -- set pageSetup [ pageSetupOrientation := PageOrientationLandscape ]
    set printOp   [ printOperationDefaultPageSetup := pageSetup ]
    set printOp   [ printOperationUseFullPage := True ]

    _ <- on printOp printOptBeginPrint $ \printCtxt ->
      set printOp [ printOperationNPages := 1 ]

    _ <- on printOp printOptDrawPage $ \printCtxt n -> do
      putStrLn $ "I'm printing page " ++ show n
      pageSetup_ <- printContextGetPageSetup printCtxt
      selector_  <- selector c
      dds        <- getDirectDebitSet selector_
      cairoCtxt  <- printContextGetCairoContext printCtxt
      pangoCtxt  <- printContextCreatePangoContext printCtxt
      surface    <- Cairo.Internal.getTarget cairoCtxt
      renderWith surface $ renderDirectDebitSet pangoCtxt pageSetup_ dds

    result <- printOperationRun printOp PrintOperationActionPrintDialog mainWd
    return ()

  return ()

renderDirectDebitSet :: PangoContext -> PageSetup -> DirectDebitSet -> Render ()
renderDirectDebitSet pangoCtxt pageSetup dds = do
  topMargin     <- liftIO $ pageSetupGetTopMargin     pageSetup UnitPoints
  bottomMargin  <- liftIO $ pageSetupGetBottomMargin  pageSetup UnitPoints
  leftMargin    <- liftIO $ pageSetupGetLeftMargin    pageSetup UnitPoints
  rightMargin   <- liftIO $ pageSetupGetRightMargin   pageSetup UnitPoints
  paperWidth    <- liftIO $ pageSetupGetPaperWidth    pageSetup UnitPoints
  paperHeight   <- liftIO $ pageSetupGetPaperHeight   pageSetup UnitPoints
  let printableWidth  = paperWidth - leftMargin - rightMargin
      maxYPos         = paperHeight - bottomMargin
      nameWidth       = 0.45 * printableWidth
      itemWidth       = 0.30 * printableWidth
      basePriceWidth  = 0.13 * printableWidth
      finalPriceWidth = 0.13 * printableWidth
      xItem           = leftMargin + nameWidth
  let tableFont    = ("<span size=\"9000\">",     "</span>")
      titleFont    = ("<span size=\"11000\"><b>", "</b></span>")
      tableHdrFont = (fst tableFont ++ "<i>",     "</i>" ++ snd tableFont)
  stub <- liftIO $ layoutEmpty pangoCtxt
  _ <- liftIO $ layoutSetMarkup stub $ fst tableFont ++ "" ++ snd tableFont
  (_ink, PangoRectangle _ _ _ tableFontHeight) <- liftIO $ layoutGetExtents stub

  -- Listing title
  title <- liftIO $ layoutEmpty pangoCtxt
  _ <- liftIO $ layoutSetMarkup title $ fst titleFont ++ T.unpack (dds ^. description) ++ snd titleFont
  moveTo leftMargin topMargin
  showLayout title
  (_ink, PangoRectangle _ _ _ titleHeight) <- liftIO $ layoutGetExtents title

  -- Show totals
  relMoveTo 0 (2 * titleHeight)
  totalLayout <- liftIO $ layoutEmpty pangoCtxt
  let debits_  = dds ^. debits
      total    = sumOf (traverse.items.traverse.finalPrice) debits_
      totalStr = fst tableFont ++ "Total: " ++ priceToString total ++ snd tableFont
  _ <- liftIO $ layoutSetMarkup totalLayout totalStr
  showLayout totalLayout
  relMoveTo 0 tableFontHeight
  numLayout <- liftIO $ layoutEmpty pangoCtxt
  let numStr = fst tableFont ++ "Nombre d'alumnes: " ++ show (length debits_) ++ snd tableFont
  _ <- liftIO $ layoutSetMarkup numLayout numStr
  showLayout numLayout

  -- Table headers
  relMoveTo 0 (2 * titleHeight)
  forM_ [ ("Nom",        nameWidth,       AlignLeft) -- FIXME: take labels from Glade
        , ("Item",       itemWidth,       AlignLeft)
        , ("Preu base",  basePriceWidth,  AlignRight)
        , ("Preu final", finalPriceWidth, AlignRight)] $ \(text, width_, align) -> do
    layout <- liftIO $ layoutEmpty pangoCtxt
    _ <- liftIO $ layoutSetMarkup layout $ fst tableHdrFont ++ text ++ snd tableHdrFont
    liftIO $ layoutSetWidth layout (Just width_)
    liftIO $ layoutSetAlignment layout align
    showLayout layout
    relMoveTo width_ 0
  (_, yPos) <- getCurrentPoint
  moveTo leftMargin (yPos + tableFontHeight)
  relLineTo printableWidth 0
  stroke

  -- Table contents
  let renderDirectDebit :: Double -> Double -> [DirectDebit] -> Render ()
      renderDirectDebit _    _    []         = return ()
      renderDirectDebit xPos yPos (debit:ds) = do
        let lineSep = 3

        -- Debit layout preparation and extent computation

        nameLayout <- liftIO $ layoutEmpty pangoCtxt
        let name = T.unpack $ T.concat [debit ^. debtorLastName, ", ", debit ^. debtorFirstName]
        _ <- liftIO $ layoutSetMarkup nameLayout $ fst tableFont ++ name ++ snd tableFont
        liftIO $ layoutSetWidth nameLayout (Just nameWidth)
        liftIO $ layoutSetAlignment nameLayout AlignLeft
        (_ink, PangoRectangle _ _ _ nameHeight) <- liftIO $ layoutGetExtents nameLayout
        debitLayouts <- forM (debit ^.. items.traverse) $ \item_ -> do
          itemLayouts <- forM [ (T.unpack      (item_ ^. longName),   itemWidth,       AlignLeft )
                              , (priceToString (item_ ^. basePrice),  basePriceWidth,  AlignRight)
                              , (priceToString (item_ ^. finalPrice), finalPriceWidth, AlignRight)]
                         $ \(text, width_, align) -> do
            layout <- liftIO $ layoutEmpty pangoCtxt
            _ <- liftIO $ layoutSetMarkup layout $ fst tableFont ++ text ++ snd tableFont
            liftIO $ layoutSetWidth layout (Just width_)
            liftIO $ layoutSetAlignment layout align
            (_ink, PangoRectangle _ _ _ layoutHeight_) <- liftIO $ layoutGetExtents layout
            return (layout, (width_, layoutHeight_ + lineSep))
          let itemHeight = maximum $ map (snd . snd) itemLayouts
          return (itemLayouts, itemHeight)

        -- Debit layout showing

        let debitHeight = max (nameHeight + lineSep) (sum (snd (unzip debitLayouts)))
        (xPos, yPos) <- if yPos + debitHeight > maxYPos
                        then do showPage
                                return (leftMargin, topMargin)
                        else    return (xPos, yPos)
        moveTo xPos yPos
        -- rectangle xPos yPos 10 debitHeight
        -- setSourceRGBA 0.8 0.8 0.8 0.5
        -- fill
        -- setSourceRGB 0 0 0
        showLayout nameLayout
        relMoveTo nameWidth 0
        forM_ debitLayouts $ \(itemLayouts, itemHeight) -> do
          forM_ itemLayouts $ \(layout, (width_, _)) -> do
            showLayout layout
            relMoveTo width_ 0
          (_, yPos) <- getCurrentPoint
          moveTo xItem (yPos + itemHeight)
        renderDirectDebit leftMargin (yPos + debitHeight) ds

  renderDirectDebit leftMargin (yPos + tableFontHeight + 4) (dds ^. debits)

readItems :: DirectDebitsController -> IO [DirectDebit]
readItems c = do
  let nameEq  (Item last1 fst1 _ _) (Item last2 fst2 _ _) = last1 == last2 && fst1 == fst2
      -- Lexicographic order, no need of a specific collation
      nameOrd = comparing (itemLastName &&& itemFirstName)
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

cloneDdsAction :: (DB.PersistQuery m, DB.PersistMonadBackend m ~ DB.MongoBackend) =>
                  DirectDebitSet
               -> C.ZonedTime
               -- -> [DB.Entity BillingConcept]
               -- -> [DB.Entity Debtor]
               -> m (DB.Key DirectDebitSet, DirectDebitSet)
cloneDdsAction dds zonedTime = do
  let today  = C.localDay (C.zonedTimeToLocalTime zonedTime)
  mCreditor <- DB.selectFirst ([] :: [DB.Filter Creditor]) []
  creditor_ <- case mCreditor of
    Nothing        -> error "DirectDebitsController::mkController': no creditor"
    Just creditorE -> return $ DB.entityVal creditorE
  let description_ = T.concat [      T.filter (/= ' ') (creditor_ ^. fullName)
                              , "_", T.pack (C.showGregorian today)
                              , "_", T.pack (PF.printf "%04d" (creditor_ ^. messageCount))
                              ]
  debtors <- DB.selectList ([] :: [DB.Filter Debtor]) []
  -- WARNING: we don't update item prices (commented out) because it would reset items
  -- with manually modified prices.
  -- We index maps with unique keys for billing concepts and debtors
  -- let bcsMap = M.fromList $ map (\(DB.Entity _ e) -> (e ^. longName, e)) bcs
  let desMap = M.fromList $ map (\(DB.Entity _ e) -> ((e ^. firstName, e ^. lastName), e)) debtors
  let oldDebits  = dds ^. debits
  -- let oldItem2newMItem i = do   -- Maybe monad
  --       mNewItem  <- M.lookup (i ^. longName) bcsMap
  --       return $ i & basePrice .~ (mNewItem ^. basePrice) & vatRatio .~ (mNewItem ^. vatRatio)
  let oldDebit2newMDebit d = do -- Maybe monad
        mNewDebtor  <- M.lookup (d ^. debtorFirstName, d ^. debtorLastName) desMap
        mNewMandate <- getActiveMandate today mNewDebtor
        -- mNewItems <- case mapMaybe oldItem2newMItem (d ^. items) of
        --   []      -> Nothing
        --   items_  -> Just items_
        -- return $ d & mandate .~ mNewMandate & items .~ mNewItems
        return $ d & mandate .~ mNewMandate
  let newDebits = mapMaybe oldDebit2newMDebit oldDebits
  let newDdsV = mkDirectDebitSet description_ zonedTime creditor_ newDebits
  newDdsK <- DB.insert newDdsV
  return (newDdsK, newDdsV)
