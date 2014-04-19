{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Sepa.Controller.Class where

import           Control.Monad
import           Data.IORef
import           Data.List
import qualified Database.Persist.MongoDB as DB
import           Graphics.UI.Gtk

type PanelId = String

data MainWindowState
  = View { _choosedPanelId :: PanelId }
  | Edit { _choosedPanelId :: PanelId }

--makeLenses ''MainWindowState

data PanelState c
  = NoSel
  | Sel     { _iter :: TreeIter }
  | EditNew { _data :: D c, _isValid :: Bool }
  | EditOld { _iter :: TreeIter, _isValid :: Bool }

--makeLenses ''PanelState

type PS c = DB.Entity (E c)     -- ^ Used to simplify type signatures in Controller.

type LS c = ListStore (PS c)    -- ^ Used to simplify type signatures in Controller.

-- | Generic panel controller, used to factorize implementation of debtors, billing
-- concepts and direct debits panels (an VBox with all its contained widgets). It's main
-- entry point is template method @mkController@. In general, functions in this class are
-- non-pure, as they use state stored by Gtk in the IO monad. The class is parametrized
-- (through associated types) with an entity type, a @selector@ (e.g. TreeView) type, and
-- type for raw data (as read from the GUI) associated to an entity.
class (DB.PersistEntity (E c), DB.PersistEntityBackend (E c) ~ DB.MongoBackend, WidgetClass (S c)) =>
      Controller c where

  -- | Entity type, i.e., type of the elements shown in the panel (e.g Debtor).
  type E c

  -- | Type of the selector widget (e.g. TreeView or ComboBox).
  type S c

  -- | Type for raw data taken from entries, representing an entity of type E c.
  data D c

  -- All instances need to implement, at least, the following functions
  panelId              ::                                                     c -> PanelId
  -- ^ Instances need to implement this.
  builder              ::                                                     c -> Builder
  -- ^ Instances need to implement this.
  selector             ::                                                     c -> IO (S c)
  setSelectorModel     :: (TreeModelClass m)      => S c         -> m      -> c -> IO ()
  connectSelector      :: (TreeModelSortClass sm) => S c         -> sm
                       -> (PanelState c -> IO ())                          -> c -> IO (IO ())
  readData             :: [Entry]                                          -> c -> IO (D c)
  validData            :: D c                                              -> c -> IO Bool
  createFromData       :: D c                                              -> c -> IO (E c)
  updateFromData       :: D c -> E c                                       -> c -> IO (E c)
  selectElement        :: (TreeModelSortClass sm) => TreeIter -> S c -> sm -> c -> IO ()

  -- The following functions may be re-implemented, default instance implementation does
  -- nothing
  setSelectorRenderers ::                            S c -> LS c           -> c -> IO ()
  setSelectorSorting   :: (TreeSortableClass sm)  => S c -> LS c -> sm     -> c -> IO ()
  setSelectorSearching :: (TreeModelSortClass sm) => S c -> LS c -> sm     -> c -> IO ()
  editEntries          ::                                                     c -> IO [Entry]
  editWidgets          ::                                                     c -> IO [Widget]
  selectWidgets        ::                                                     c -> IO [Widget]
  subElemWidgets       ::                                                     c -> IO [Widget]
  renderers            ::                                                     c -> IO [PS c -> String]
  putSubElement        :: TreeIter -> LS c                                 -> c -> IO ()
  putElement'          :: TreeIter -> LS c                                 -> c -> IO ()
  mkSubElemController  :: (TreeModelSortClass sm) =>
                          LS c -> sm -> DB.ConnectionPool
                       -> IORef (PanelState c)                             -> c -> IO ()

  -- The following functions have a generally applicable default implementation. Some of
  -- them ar based on conventions for Glade names.
  panel                ::                                                     c -> IO VBox
  chooser              ::                                                     c -> IO ToggleButton
  newTb                ::                                                     c -> IO ToggleButton
  editTb               ::                                                     c -> IO ToggleButton
  deleteBt             ::                                                     c -> IO Button
  saveBt               ::                                                     c -> IO Button
  cancelBt             ::                                                     c -> IO Button
  deleteDg             ::                                                     c -> IO Dialog
  elements             :: DB.ConnectionPool                                -> c -> IO [PS c]
  putElement           :: TreeIter -> LS c -> [PS c -> String]  -> [Entry] -> c -> IO ()
  deleteElement        :: TreeIter -> LS c -> DB.ConnectionPool            -> c -> IO ()
  insertElement        ::             LS c -> DB.ConnectionPool -> [Entry] -> c -> IO TreeIter
  updateElement        :: TreeIter -> LS c -> DB.ConnectionPool -> [Entry] -> c -> IO TreeIter

  -- | A Template Method pattern (it is implemented once for all instances) that
  -- initializes all the panel widgets, including connection with persistent model and
  -- callback events. All the other functions in this class are here only to be called by
  -- @mkController@, except @panelId@, @panel@ and @chooser@.
  mkController         :: DB.ConnectionPool -> (MainWindowState -> IO ()) -> c -> IO (LS c)

  -- Default implementations for some functions

  setSelectorRenderers _ _ _    = return ()

  setSelectorSorting   _ _ _ _  = return ()

  setSelectorSearching _ _ _ _  = return ()

  editEntries _                 = return []

  editWidgets _                 = return []

  selectWidgets _               = return []

  subElemWidgets _              = return []

  renderers _                   = return []

  putSubElement _ _ _           = return ()

  putElement'   _ _ _           = return ()

  mkSubElemController _ _ _ _ _ = return ()

  panel   c      = builderGetObject (builder c) castToVBox         (panelId c ++ "_Vb")

  chooser c      = builderGetObject (builder c) castToToggleButton (panelId c ++ "_Tb")

  newTb          = getGladeObject castToToggleButton "_newTb"

  editTb         = getGladeObject castToToggleButton "_editTb"

  deleteBt       = getGladeObject castToButton       "_deleteBt"

  saveBt         = getGladeObject castToButton       "_saveBt"

  cancelBt       = getGladeObject castToButton       "_cancelBt"

  deleteDg c     = builderGetObject (builder c) castToDialog "deleteDg"

  elements db _c = flip DB.runMongoDBPoolDef db $ DB.selectList ([] :: [DB.Filter (E c)]) []

  putElement iter ls renderers_ entries c = do
    entity <- treeModelGetRow ls iter
    forM_ (zip entries renderers_) $ \(e, r) -> set e [entryText := r entity]
    putSubElement iter ls c
    putElement'   iter ls c

  deleteElement iter ls db _ = do
    entity <- treeModelGetRow ls iter
    flip DB.runMongoDBPoolDef db $ DB.delete (DB.entityKey entity)
    let index = listStoreIterToIndex iter
    listStoreRemove ls index

  insertElement ls db entries c = do
    data_ <- readData entries c  -- Assume data is valid
    val   <- createFromData data_ c
    key   <- flip DB.runMongoDBPoolDef db $ DB.insert val -- FIXME: check unique constraints
    index <- listStoreAppend ls (DB.Entity key val)
    let treePath = stringToTreePath (show index)
    Just iter <- treeModelGetIter ls treePath
    return iter

  updateElement iter ls db entries c = do
    dataNew <- readData entries c  -- Assume data is valid
    old     <- treeModelGetRow ls iter
    new     <- updateFromData dataNew (DB.entityVal old) c
    flip DB.runMongoDBPoolDef db $ DB.replace (DB.entityKey old) new
    let index = listStoreIterToIndex iter
    listStoreSetValue ls index (DB.Entity (DB.entityKey old) new)
    return iter

  mkController = mkControllerImpl -- Implemented as a top-level function

mkControllerImpl :: forall c . (Controller c,
                     DB.PersistEntity (E c), DB.PersistEntityBackend (E c) ~ DB.MongoBackend) =>
                    DB.ConnectionPool -> (MainWindowState -> IO ()) -> c -> IO (LS c)
mkControllerImpl db setMainWdState c = do

  -- FIXME: NoSel -> newTb -> Cannot save

  -- We could put most of the variables in the following code (s, e, ls, etc.) under the
  -- monad M, but prefer to show them to make explicit the dependencies among Gtk
  -- operations.

  selector_  <- selector c
  e          <- elements db c
  ls         <- listStoreNew e
  sm         <- treeModelSortNewWithModel ls

  setSelectorModel     selector_    sm c
  setSelectorRenderers selector_ ls    c
  setSelectorSorting   selector_ ls sm c
  setSelectorSearching selector_ ls sm c

  let panelId_ = panelId c
  newTb_         <- newTb c
  editTb_        <- editTb c
  deleteBt_      <- deleteBt c
  saveBt_        <- saveBt c
  cancelBt_      <- cancelBt c
  rs             <- renderers c
  editEntries_   <- editEntries c
  editWidgets_   <- editWidgets c
  selectWidgets_ <- selectWidgets c
  deleteDg_      <- deleteDg c
  subElemWidgets_<- subElemWidgets c

  -- Place panel initial state in an IORef

  stRef <- newIORef NoSel :: IO (IORef (PanelState c))

  -- Panel state function
  let setState' :: PanelState c -> IO ()
      setState' NoSel = do
        forM_ editEntries_                    (`set` [widgetSensitive    := False, entryText := ""])
        forM_ editWidgets_                    (`set` [widgetSensitive    := False])
        forM_ selectWidgets_                  (`set` [widgetSensitive    := False])
        forM_ subElemWidgets_                 (`set` [widgetSensitive    := False])
        forM_ [deleteBt_, saveBt_, cancelBt_] (`set` [widgetSensitive    := False])
        forM_ [editTb_]                       (`set` [widgetSensitive    := False])
        forM_ [selector_]                     (`set` [widgetSensitive    := True ])
        forM_ [newTb_]                        (`set` [widgetSensitive    := True ])
        forM_ [editTb_, newTb_]               (`set` [toggleButtonActive := False])
        setMainWdState (View panelId_)
      setState' (Sel iter) = do
        putElement iter ls rs editEntries_ c
        forM_ editEntries_                    (`set` [widgetSensitive    := False])
        forM_ editWidgets_                    (`set` [widgetSensitive    := False])
        forM_ [saveBt_, cancelBt_]            (`set` [widgetSensitive    := False])
        forM_ selectWidgets_                  (`set` [widgetSensitive    := True ])
        forM_ subElemWidgets_                 (`set` [widgetSensitive    := True ])
        forM_ [selector_]                     (`set` [widgetSensitive    := True ])
        forM_ [editTb_, newTb_]               (`set` [widgetSensitive    := True ])
        forM_ [deleteBt_]                     (`set` [widgetSensitive    := True ])
        forM_ [editTb_, newTb_]               (`set` [toggleButtonActive := False])
        setMainWdState (View panelId_)
      setState' (EditNew _iter valid) = do
        forM_ selectWidgets_                  (`set` [widgetSensitive    := False])
        forM_ subElemWidgets_                 (`set` [widgetSensitive    := False])
        forM_ [selector_]                     (`set` [widgetSensitive    := False])
        forM_ [editTb_, newTb_]               (`set` [widgetSensitive    := False])
        forM_ [deleteBt_]                     (`set` [widgetSensitive    := False])
        forM_ editEntries_                    (`set` [widgetSensitive    := True, entryText := "" ])
        forM_ editWidgets_                    (`set` [widgetSensitive    := True ])
        forM_ [saveBt_]                       (`set` [widgetSensitive    := valid])
        forM_ [cancelBt_]                     (`set` [widgetSensitive    := True ])
        setMainWdState (Edit panelId_)
      setState' (EditOld _iter valid) = do
        forM_ selectWidgets_                  (`set` [widgetSensitive    := False])
        forM_ subElemWidgets_                 (`set` [widgetSensitive    := False])
        forM_ [selector_]                     (`set` [widgetSensitive    := False])
        forM_ [editTb_, newTb_]               (`set` [widgetSensitive    := False])
        forM_ [deleteBt_]                     (`set` [widgetSensitive    := False])
        forM_ editEntries_                    (`set` [widgetSensitive    := True ])
        forM_ editWidgets_                    (`set` [widgetSensitive    := True ])
        forM_ [saveBt_]                       (`set` [widgetSensitive    := valid])
        forM_ [cancelBt_]                     (`set` [widgetSensitive    := True ])
        setMainWdState (Edit panelId_)
  let setState :: PanelState c -> IO ()
      setState newSt = do
        setState' newSt
        writeIORef stRef newSt

  -- Set panel initial state

  st <- readIORef stRef
  setState st

  -- Connect panel widgets

  onSelectionChangedAction <- connectSelector selector_ sm setState c

  _ <- on cancelBt_ buttonActivated onSelectionChangedAction

  _ <- on editTb_ toggled $ do
    isActive <- toggleButtonGetActive editTb_
    when isActive $ do
      Sel iter <- readIORef stRef -- FIXME: unsafe pattern
      -- OLD: elem <- treeModelGetRow ls iter
      -- OLD: let valid = validStrings (itemToStrings item)
      -- Assume that before an Edit has been a Sel that has filled editEntries
      d <- readData editEntries_ c
      v <- validData d c
      setState (EditOld iter v)

  _ <- on newTb_ toggled $ do
    isActive <- toggleButtonGetActive newTb_
    when isActive $ do
      -- OLD: let s = guiNewItemToStrings gui
      d <- readData editEntries_ c
      v <- validData d c
      setState (EditNew d v)

  _ <- on deleteBt_ buttonActivated $ do
    (Sel iter) <- readIORef stRef -- FIXME: unsafe pattern
    resp <- dialogRun deleteDg_
    widgetHide deleteDg_
    when (resp == ResponseOk) $ do
      deleteElement iter ls db c
      setState NoSel

  _ <- on saveBt_ buttonActivated $ do
    st' <- readIORef stRef
    iter <- case st' of
      EditNew _    True -> insertElement      ls db editEntries_ c
      EditOld iter True -> updateElement iter ls db editEntries_ c
      _                 -> error "mkControllerImpl: Unexpected state when saving"
    selectElement iter selector_ sm c
    setState (Sel iter)

  -- Change state if validation state changes (check at every edit)
  -- forM_ editEntries_ $ \entry -> on entry editableChanged $ do
  --   st'    <- readIORef stRef
  --   d      <- readData c editEntries_
  --   v      <- validData c d
  --   case (st', v) of
  --     (EditNew s vOld, vNew) | vNew /= vOld -> setState (EditNew s vNew)
  --     (EditOld i vOld, vNew) | vNew /= vOld -> setState (EditOld i vNew)
  --     _                                     -> return ()

  mkSubElemController ls sm db stRef c
  return ls

getGladeObject :: (GObjectClass b, Controller c) => (GObject -> b) -> String -> c -> IO b
getGladeObject cast name c =
  builderGetObject (builder c) cast (panelId c ++ name)

selectTreeViewElement ::  (Controller c, TreeModelSortClass sm) =>
                          TreeIter -> TreeView -> sm -> c -> IO ()
selectTreeViewElement iter treeView sortedModel _c = do
  sortedIter <- treeModelSortConvertChildIterToIter sortedModel iter
  selection  <- treeViewGetSelection treeView
  treeSelectionSelectIter selection sortedIter

setTreeViewRenderers :: (TreeViewClass self, TreeModelClass (model row),
                         TypedTreeModelClass model) =>
                         self -> model row -> [row -> String] -> IO ()
setTreeViewRenderers treeView listStore renderFuncs = do
  -- forall columns: set renderer, set sorting func
  columns <- treeViewGetColumns treeView
  forM_ (zip columns renderFuncs) $ \(col, renderFunc) -> do
    -- FIXME: column manual resizing doesn't work
    let cellLayout = toCellLayout col
    (cell : _) <- cellLayoutGetCells cellLayout    -- FIXME: unsafe pattern, dep. on glade
    let textRenderer = castToCellRendererText cell -- FIXME: unsafe cast, depends on glade
    cellLayoutSetAttributes col textRenderer listStore $ \row -> [ cellText := renderFunc row ]

setTreeViewSorting :: (TreeViewClass treeview, TreeSortableClass sortable,
                       TypedTreeModelClass model) =>
                      treeview
                   -> model row
                   -> Maybe (TypedTreeModelFilter row)
                   -> sortable
                   -> [t -> t -> Ordering]
                   -> [row -> t]
                   -> IO ()
setTreeViewSorting treeView listStore mFilterModel sortedModel orderings renderFuncs = do
  columns <- treeViewGetColumns treeView
  forM_ (zip4 columns renderFuncs orderings [0..]) $ \(col, renderFunc, ordering, colId) -> do
    let sortFunc xIter yIter = do
          (xIter', yIter') <- case mFilterModel of
            Just filterModel -> do
              childXIter <- treeModelFilterConvertIterToChildIter filterModel xIter
              childYIter <- treeModelFilterConvertIterToChildIter filterModel yIter
              return (childXIter, childYIter)
            Nothing          -> return (xIter, yIter)
          xRow <- customStoreGetRow listStore xIter'
          yRow <- customStoreGetRow listStore yIter'
          return $ ordering (renderFunc xRow) (renderFunc yRow)
    treeSortableSetSortFunc sortedModel colId sortFunc
    treeViewColumnSetSortColumnId col colId

-- | Sets incremental search in tree view.
setTreeViewSearching :: (Controller c, TreeModelSortClass sm) =>
                        TreeView
                     -> LS c
                     -> sm
                     -> (String -> [String] -> Bool)
                     -> c
                     -> IO ()
setTreeViewSearching treeView listStore sortedModel isPartOf c = do
  -- TODO: accent-independent search or TreeModelFilter
  renderFuncs <- renderers c
  let rowEqualFunc :: (String -> TreeIter -> IO Bool)
      rowEqualFunc txt iter = do
        childIter <- treeModelSortConvertIterToChildIter sortedModel iter
        row <- treeModelGetRow listStore childIter
        return $ txt `isPartOf` map ($ row) renderFuncs
  treeViewSetSearchEqualFunc treeView (Just rowEqualFunc)

connectTreeView :: (Controller c, TreeModelSortClass sm) =>
                   TreeView
                -> sm
                -> (PanelState c -> IO ())
                -> c
                -> IO (IO ())
connectTreeView treeView sortedModel setState _c = do
  selection <- treeViewGetSelection treeView
  let toChildIter = treeModelSortConvertIterToChildIter sortedModel
  let onSelectionChangedAction = do
        count <- treeSelectionCountSelectedRows selection
        if count == 0
          then setState NoSel
          else treeSelectionSelectedForeach selection $ \iter -> do
            cIter <- toChildIter iter
            setState (Sel cIter)

  _ <- on selection treeSelectionSelectionChanged onSelectionChangedAction

  return onSelectionChangedAction
