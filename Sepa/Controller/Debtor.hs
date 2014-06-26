{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Sepa.Controller.Debtor where

import           Control.Lens             hiding (element, elements, index, set,
                                           view)
import           Control.Monad
import           Data.IORef
import           Data.List
import qualified Data.Text                as T (Text, pack, unpack)
import qualified Data.Text.ICU            as T
import qualified Data.Time.Calendar       as C
import qualified Data.Time.LocalTime      as C
import qualified Database.Persist.MongoDB as DB
import           Graphics.UI.Gtk
import           Sepa.Controller.Class
import           Sepa.Controller.TreeView
import           Sepa.Creditor
import           Sepa.Debtor
import           Sepa.SpanishIban
import qualified Text.Printf              as PF (printf)

data DebtorsController =
  DE
  { panelId_      :: PanelId
  , builder_      :: Builder
  , oldMandatesTv :: TreeView
  , oldMandatesLs :: ListStore Mandate
  , oldMandatesSm :: TypedTreeModelSort Mandate
  }

instance Controller DebtorsController where

  type E DebtorsController = Debtor

  type S DebtorsController = TreeView

  data D DebtorsController =
    DDE { lastNameD  :: T.Text
        , firstNameD :: T.Text }

  builder = builder_

  panelId = panelId_

  selector = getGladeObject castToTreeView "_Tv"

  setSelectorModel s m _ = treeViewSetModel s m

  setSelectorRenderers s m c = do
    renderFuncs <- renderers c
    setTreeViewRenderers s m renderFuncs

  setSelectorSorting s ls sm c = do
    let comp x y = T.collate (T.collator T.Current) (T.pack x) (T.pack y)
    let orderings = repeat comp
    renderFuncs <- renderers c
    setTreeViewSorting s ls Nothing sm orderings renderFuncs

  setSelectorSearching s ls sm c = do
    renderFuncs <- renderers c
    let tx `isPartOf` txs = any (tx `isInfixOf`) txs -- TODO: better searching
    setTreeViewSearching s ls sm isPartOf renderFuncs

  renderers _ = return [ T.unpack        . (^. lastName)          . DB.entityVal
                       , T.unpack        . (^. firstName)         . DB.entityVal
                       , C.showGregorian . (^. registrationDate)  . DB.entityVal
                       ]

  editEntries c = do e1 <- getGladeObject castToEntry "_EE_lastNameEn"  c
                     e2 <- getGladeObject castToEntry "_EE_firstNameEn" c
                     return [e1, e2]

  subElemButtons c = do
    s7  <- getGladeObject castToToggleButton "_mandateNewTb" c
    return [s7]

  readData [lastNameEn, firstNameEn] _ = do
    lastName_    <- get lastNameEn    entryText
    firstName_   <- get firstNameEn   entryText
    return DDE { lastNameD   = T.pack lastName_
               , firstNameD  = T.pack firstName_
               }
  readData _ _ = error "readData (DE): wrong number of entries"

  validData d _c = return $ validDebtorName (firstNameD d) (lastNameD d)

  createFromData d _db _c = do
    zonedTime <- C.getZonedTime
    let today  = C.localDay (C.zonedTimeToLocalTime zonedTime)
    return $ mkDebtor (firstNameD d) (lastNameD d) [] today

  updateFromData d old _db _c =
    return $ old & firstName .~ firstNameD d & lastName .~ lastNameD d

  selectElement it s sm _c = selectTreeViewElement it s sm

  putElement' iter ls c = do
    debtorE <- treeModelGetRow ls iter
    listStoreClear (oldMandatesLs c)
    let debtor = DB.entityVal debtorE
    case debtor ^. mandates of
      []              -> return ()
      (_:oldMandates) -> mapM_ (listStoreAppend (oldMandatesLs c)) oldMandates

  putSubElement = putMandate

  mkSubElemController = mkMandateController

  connectSelector s sm st _c = connectTreeView s sm st

  setSubElemState = setMandatePanelState

putMandate :: TreeIter -> LS DebtorsController -> DebtorsController -> IO ()
putMandate iter ls c = do
  iban1En          <- builderGetObject (builder c) castToEntry "DE_iban1En"
  iban2En          <- builderGetObject (builder c) castToEntry "DE_iban2En"
  iban3En          <- builderGetObject (builder c) castToEntry "DE_iban3En"
  iban4En          <- builderGetObject (builder c) castToEntry "DE_iban4En"
  iban5En          <- builderGetObject (builder c) castToEntry "DE_iban5En"
  iban6En          <- builderGetObject (builder c) castToEntry "DE_iban6En"
  refEn            <- builderGetObject (builder c) castToEntry "DE_mandateRefEn"
  signatureEn      <- builderGetObject (builder c) castToEntry "DE_mandateSignatureEn"
  lastTimeActiveEn <- builderGetObject (builder c) castToEntry "DE_mandateLastTimeActiveEn"
  mapM_ (`set` [entryText := ""])
    [iban1En, iban2En, iban3En, iban4En, iban5En, iban6En, refEn, signatureEn, lastTimeActiveEn]
  (DB.Entity _key debtor)  <- treeModelGetRow ls iter
  print $ length (debtor ^. mandates)
  zonedTime <- C.getZonedTime
  let today  = C.localDay (C.zonedTimeToLocalTime zonedTime)
  case getActiveMandate today debtor of
    Nothing -> putStrLn "Rien"
    Just mandate -> do
      set refEn            [entryText := T.unpack (mandate ^. mandateRef)]
      set signatureEn      [entryText := C.showGregorian (mandate ^. signatureDate)]
      set lastTimeActiveEn [entryText := maybe "" C.showGregorian (mandate ^. lastTimeActive)]
      let (iban1, iban'    ) = splitAt 4 (T.unpack (mandate ^. iban))
          (iban2, iban''   ) = splitAt 4 iban'
          (iban3, iban'''  ) = splitAt 4 iban''
          (iban4, iban'''' ) = splitAt 4 iban'''
          (iban5, iban6    ) = splitAt 4 iban''''
      set iban1En [entryText := iban1]
      set iban2En [entryText := iban2]
      set iban3En [entryText := iban3]
      set iban4En [entryText := iban4]
      set iban5En [entryText := iban5]
      set iban6En [entryText := iban6]

mkMandateController  :: (TreeModelSortClass sm) =>
                        LS DebtorsController
                     -> sm
                     -> DB.ConnectionPool
                     -> IORef (PanelState c)
                     -> (PanelState c -> IO ())
                     -> DebtorsController
                     -> IO ()
mkMandateController ls sm db stRef setPanelState c = do
  iban1En          <- builderGetObject (builder c) castToEntry        "DE_iban1En"
  iban2En          <- builderGetObject (builder c) castToEntry        "DE_iban2En"
  iban3En          <- builderGetObject (builder c) castToEntry        "DE_iban3En"
  iban4En          <- builderGetObject (builder c) castToEntry        "DE_iban4En"
  iban5En          <- builderGetObject (builder c) castToEntry        "DE_iban5En"
  iban6En          <- builderGetObject (builder c) castToEntry        "DE_iban6En"
  newTb_           <- builderGetObject (builder c) castToToggleButton "DE_mandateNewTb"
  saveBt_          <- builderGetObject (builder c) castToButton       "DE_saveMandateBt"
  cancelBt_        <- builderGetObject (builder c) castToButton       "DE_cancelMandateBt"
  refEn            <- builderGetObject (builder c) castToEntry        "DE_mandateRefEn"
  signatureEn      <- builderGetObject (builder c) castToEntry        "DE_mandateSignatureEn"
  lastTimeActiveEn <- builderGetObject (builder c) castToEntry        "DE_mandateLastTimeActiveEn"
  zonedTime        <- C.getZonedTime
  let today        = C.localDay (C.zonedTimeToLocalTime zonedTime)

  handlers <- forM [iban1En, iban2En, iban3En, iban4En, iban5En, iban6En] $ \entry ->
    on entry editableChanged $ do
      st <- readIORef stRef
      case st of
        (EditSub iter _valid) -> do iban1 <- get iban1En entryText
                                    iban2 <- get iban2En entryText
                                    iban3 <- get iban3En entryText
                                    iban4 <- get iban4En entryText
                                    iban5 <- get iban5En entryText
                                    iban6 <- get iban6En entryText
                                    let iban_ = concat [iban1, iban2, iban3, iban4, iban5, iban6]
                                    setPanelState (EditSub iter (validSpanishIban (T.pack iban_)))
        _ -> return ()

  forM_ handlers signalBlock

  _ <- on newTb_ toggled $ do
    -- Widget sensitivity set in Class.hs
    mapM_ (`set` [entryText := ""]) [ iban1En, iban2En, iban3En, iban4En, iban5En, iban6En
                                    , signatureEn, lastTimeActiveEn ]
    mCreditor <- flip DB.runMongoDBPoolDef db $ DB.selectFirst ([] :: [DB.Filter Creditor]) []
    case mCreditor of
      Nothing                      -> error "DirectDebitsController::createFromData: no creditor"
      Just (DB.Entity _ creditor_) -> do
        let newRef = PF.printf "%012d" (creditor_ ^. mandateCount) ++ "                       "
        refEn `set` [entryText := newRef]
    forM_ handlers signalUnblock
    -- Change of state performed in Class.hs

  _ <- on saveBt_ buttonActivated $ do
    forM_ handlers signalBlock
    (EditSub iter True) <- readIORef stRef -- FIXME: unsafe pattern
    (DB.Entity key oldDebtor)  <- treeModelGetRow ls iter
    iban1 <- get iban1En entryText
    iban2 <- get iban2En entryText
    iban3 <- get iban3En entryText
    iban4 <- get iban4En entryText
    iban5 <- get iban5En entryText
    iban6 <- get iban6En entryText
    ref   <- get refEn   entryText -- entryText filled on newTb_ toggled
    let iban_     = concat [iban1, iban2, iban3, iban4, iban5, iban6]
        mandate   = mkMandate (T.pack ref) (T.pack iban_) today Nothing
        mandates_ = oldDebtor ^. mandates
        newDebtor = oldDebtor & mandates .~ (mandate : mandates_)
    incrementCreditorMandateCount db
    flip DB.runMongoDBPoolDef db $ DB.replace key newDebtor
    let index = listStoreIterToIndex iter
    listStoreSetValue ls index (DB.Entity key newDebtor)
    debtorsTreeView  <- selector c
    selection <- treeViewGetSelection debtorsTreeView
    childPath <- treeModelGetPath ls iter
    path      <- treeModelSortConvertChildPathToPath sm childPath
    treeSelectionUnselectPath selection path
    treeSelectionSelectPath   selection path -- Indirectly changes state

  _ <- on cancelBt_ buttonActivated $ do
    forM_ handlers signalBlock
    (EditSub iter _valid) <- readIORef stRef -- FIXME: unsafe pattern
    debtorsTreeView  <- selector c
    selection <- treeViewGetSelection debtorsTreeView
    childPath <- treeModelGetPath ls iter
    path      <- treeModelSortConvertChildPathToPath sm childPath
    treeSelectionUnselectPath selection path
    treeSelectionSelectPath   selection path -- Indirectly changes state

  return ()

setMandatePanelState :: PanelState DebtorsController -> DebtorsController -> IO ()
setMandatePanelState st c = do
  iban1En          <- builderGetObject (builder c) castToEntry        "DE_iban1En"
  iban2En          <- builderGetObject (builder c) castToEntry        "DE_iban2En"
  iban3En          <- builderGetObject (builder c) castToEntry        "DE_iban3En"
  iban4En          <- builderGetObject (builder c) castToEntry        "DE_iban4En"
  iban5En          <- builderGetObject (builder c) castToEntry        "DE_iban5En"
  iban6En          <- builderGetObject (builder c) castToEntry        "DE_iban6En"
  saveBt_          <- builderGetObject (builder c) castToButton       "DE_saveMandateBt"
  cancelBt_        <- builderGetObject (builder c) castToButton       "DE_cancelMandateBt"
  refEn            <- builderGetObject (builder c) castToEntry        "DE_mandateRefEn"
  signatureEn      <- builderGetObject (builder c) castToEntry        "DE_mandateSignatureEn"
  lastTimeActiveEn <- builderGetObject (builder c) castToEntry        "DE_mandateLastTimeActiveEn"
  mandatesTv       <- getGladeObject               castToWidget       "_mandatesTv" c
  let dataWidgets = [iban1En, iban2En, iban3En, iban4En, iban5En, iban6En, refEn,
                     signatureEn, lastTimeActiveEn]

  -- newBt is handled in Class.hs
  case st of
    NoSel       ->       do forM_ [saveBt_, cancelBt_]            (`set` [widgetSensitive    := False])
                            forM_ dataWidgets                     (`set` [widgetSensitive    := False])
                            forM_ [mandatesTv]                    (`set` [widgetSensitive    := False])
    Sel _iter   ->       do forM_ [saveBt_, cancelBt_]            (`set` [widgetSensitive    := False])
                            forM_ dataWidgets                     (`set` [widgetSensitive    := False])
                            forM_ [mandatesTv]                    (`set` [widgetSensitive    := False])
    EditNew _ _ ->       do forM_ [saveBt_, cancelBt_]            (`set` [widgetSensitive    := False])
                            forM_ dataWidgets                     (`set` [widgetSensitive    := False])
                            forM_ [mandatesTv]                    (`set` [widgetSensitive    := False])
    EditOld _ _ ->       do forM_ [saveBt_, cancelBt_]            (`set` [widgetSensitive    := False])
                            forM_ dataWidgets                     (`set` [widgetSensitive    := False])
                            forM_ [mandatesTv]                    (`set` [widgetSensitive    := False])
    EditSub _it valid -> do forM_ [saveBt_]                       (`set` [widgetSensitive    := valid])
                            forM_ [cancelBt_]                     (`set` [widgetSensitive    := True ])
                            forM_ dataWidgets                     (`set` [widgetSensitive    := True ])
                            forM_ [mandatesTv]                    (`set` [widgetSensitive    := True ])

