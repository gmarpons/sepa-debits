{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Sepa.Controller.Class where

import           Control.Monad
import           Control.Monad.IO.Class
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

-- | Monad type for @Controller@ actions. It's a MonadIO. We don't need to thread-in the
-- controller, as it doesn't have any state in itself, all the state is kept inside IO.
-- We need the type @c@ to appear in the type of functions only for the compiler selecting
-- the correct controller instance. The main purpose of this monad is to shorten type
-- annotations in class @Controller@ functions.
newtype PanelM c a = PanelM { runPanel :: c -> IO a }

instance (Controller c) => Monad (PanelM c) where
  return a = PanelM (\_ -> return a)
  PanelM f >>= m = PanelM (\c -> f c >>= g c)
    where g c a = let (PanelM g') = m a in g' c

instance (Controller c) => MonadIO (PanelM c) where
  liftIO io = PanelM (const io)

controller :: (Controller c) => PanelM c c
controller = PanelM return

-- type PanelMSt c = StateT PanelState (PanelM c)

-- | Generic panel controller, used to factorize implementation of debtors, billing
-- concepts and direct debits panels (an VBox with all its contained widgets). It's main
-- entry point is template method @mkController@. In general, functions in this class are
-- non-pure, as they use state stored by Gtk in the IO monad. The class is parametrized
-- (through associated types) with an entity type and a @selector@ (e.g. TreeView) type.
class (DB.PersistEntity (E c), DB.PersistEntityBackend (E c) ~ DB.MongoBackend, WidgetClass (S c)) =>
      Controller c where

  -- | Entity type, i.e., type of the elements shown in the panel (e.g Debtor).
  type E c

  -- | Type of the selector widget (e.g. TreeView or ComboBox).
  type S c

  -- | Type for raw data taken from entries, representing an entity of type E c.
  data D c

  -- All instances need to implement, at least, the following functions
  panelId              :: c -> PanelId -- ^ Instances need to implement this.
  builder              :: c -> Builder -- ^ Instances need to implement this.
  selector             ::                                                       PanelM c (S c)
  setSelectorModel     :: (TreeModelClass m)      => S c         -> m        -> PanelM c ()
  connectSelector      :: (TreeModelSortClass sm) => S c         -> sm
                       -> (PanelState c -> IO ()) -> PanelM c (IO ())
  readData             :: c                                      -> [Entry] -> IO (D c)
  validData            :: c -> D c                                          -> IO Bool
  createFromData       :: c -> D c                                          -> IO (E c)
  updateFromData       :: c -> D c -> E c                                   -> IO (E c)
  selectElement        :: (TreeModelSortClass sm) => c -> TreeIter -> S c -> sm -> IO ()

  -- The following functions may be re-implemented, default instance implementation does
  -- nothing
  setSelectorRenderers ::                            S c -> LS c       -> PanelM c ()
  setSelectorSorting   :: (TreeSortableClass sm)  => S c -> LS c -> sm -> PanelM c ()
  setSelectorSearching :: (TreeModelSortClass sm) => S c -> LS c -> sm -> PanelM c ()
  editEntries          ::                                        PanelM c [Entry]
  editWidgets          ::                                        PanelM c [Widget]
  selectWidgets        ::                                        PanelM c [Widget]
  subElemWidgets       ::                                        PanelM c [Widget]
  renderers            ::                                        PanelM c [PS c -> String]
  putSubElement        :: c -> TreeIter -> LS c                                 -> IO ()
  mkSubElemController  :: (TreeModelSortClass sm) => c -> LS c -> sm -> DB.ConnectionPool -> IORef (PanelState c) -> IO ()

  -- FIXME: I've needed a c param in putElement and other functions to fix their type (as
  -- they are not in the monad PanelM).
  -- The following functions have a generally applicable default implementation. Some of
  -- them ar based on conventions for Glade names.
  panel                :: c -> IO VBox
  chooser              :: c -> IO ToggleButton
  newTb                ::                                        PanelM c ToggleButton
  editTb               ::                                        PanelM c ToggleButton
  deleteBt             ::                                        PanelM c Button
  saveBt               ::                                        PanelM c Button
  cancelBt             ::                                        PanelM c Button
  deleteDg             ::                                        PanelM c Dialog
  elements             :: DB.ConnectionPool                   -> PanelM c [PS c]
  panelIdM             ::                                        PanelM c PanelId
  putElement           :: c -> TreeIter -> LS c -> [PS c -> String]  -> [Entry] -> IO ()
  deleteElement        :: c -> TreeIter -> LS c -> DB.ConnectionPool            -> IO ()
  insertElement        :: c             -> LS c -> DB.ConnectionPool -> [Entry] -> IO TreeIter
  updateElement        :: c -> TreeIter -> LS c -> DB.ConnectionPool -> [Entry] -> IO TreeIter

  -- | A Template Method pattern (it is implemented once for all instances) that
  -- initializes all the panel widgets, including connection with persistent model and
  -- callback events. All the other functions in this class are here only to be called by
  -- @mkController@, except @panelId@, @panel@ and @chooser@.
  mkController         :: DB.ConnectionPool -> (MainWindowState -> IO ()) -> PanelM c ()

  -- Default implementations for some functions

  setSelectorRenderers _ _   = return ()

  setSelectorSorting   _ _ _ = return ()

  setSelectorSearching _ _ _ = return ()

  editEntries                = return []

  editWidgets                = return []

  selectWidgets              = return []

  subElemWidgets             = return []

  renderers                  = return []

  putSubElement _ _ _        = return ()

  mkSubElemController _ _ _ _ _ = return ()

  panel   c    = builderGetObject (builder c) castToVBox         (panelId c ++ "_Vb")

  chooser c    = builderGetObject (builder c) castToToggleButton (panelId c ++ "_Tb")

  newTb        = getGladeObject castToToggleButton "_newTb"

  editTb       = getGladeObject castToToggleButton "_editTb"

  deleteBt     = getGladeObject castToButton       "_deleteBt"

  saveBt       = getGladeObject castToButton       "_saveBt"

  cancelBt     = getGladeObject castToButton       "_cancelBt"

  deleteDg     = do
    c <- controller
    liftIO $ builderGetObject (builder c) castToDialog "deleteDg"

  elements db  = liftIO $ flip DB.runMongoDBPoolDef db $ DB.selectList ([] :: [DB.Filter (E c)]) []

  panelIdM     = do { c <- controller; return (panelId c) }

  putElement c iter ls renderers_ entries = do
    entity <- treeModelGetRow ls iter
    forM_ (zip entries renderers_) $ \(e, r) -> set e [entryText := r entity]
    putSubElement c iter ls

  deleteElement _ iter ls db = do
    entity <- treeModelGetRow ls iter
    flip DB.runMongoDBPoolDef db $ DB.delete (DB.entityKey entity)
    let index = listStoreIterToIndex iter
    listStoreRemove ls index

  insertElement c ls db entries = do
    data_ <- readData c entries   -- Assume data is valid
    val   <- createFromData c data_
    key   <- flip DB.runMongoDBPoolDef db $ DB.insert val -- FIXME: check unique constraints
    index <- listStoreAppend ls (DB.Entity key val)
    let treePath = stringToTreePath (show index)
    Just iter <- treeModelGetIter ls treePath
    return iter

  updateElement c iter ls db entries = do
    dataNew <- readData c entries   -- Assume data is valid
    old     <- treeModelGetRow ls iter
    new     <- updateFromData c dataNew (DB.entityVal old)
    flip DB.runMongoDBPoolDef db $ DB.replace (DB.entityKey old) new
    let index = listStoreIterToIndex iter
    listStoreSetValue ls index (DB.Entity (DB.entityKey old) new)
    return iter

  mkController = mkControllerImpl -- Implemented as a top-level function

mkControllerImpl :: forall c . (Controller c,
                     DB.PersistEntity (E c), DB.PersistEntityBackend (E c) ~ DB.MongoBackend) =>
                    DB.ConnectionPool -> (MainWindowState -> IO ()) -> PanelM c ()
mkControllerImpl db setMainWdState = do

  -- FIXME: NoSel -> newTb -> Cannot save

  -- We could put most of the variables in the following code (s, e, ls, etc.) under the
  -- monad M, but prefer to show them to make explicit the dependencies among Gtk
  -- operations.

  selector_  <- selector
  e          <- elements db
  ls         <- liftIO $ listStoreNew e
  sm         <- liftIO $ treeModelSortNewWithModel ls
  setSelectorModel     selector_ sm
  setSelectorRenderers selector_ ls
  setSelectorSorting   selector_ ls sm
  setSelectorSearching selector_ ls sm

  newTb_         <- newTb
  editTb_        <- editTb
  deleteBt_      <- deleteBt
  saveBt_        <- saveBt
  cancelBt_      <- cancelBt
  panelId_       <- panelIdM
  c              <- controller
  rs             <- renderers
  editEntries_   <- editEntries
  editWidgets_   <- editWidgets
  selectWidgets_ <- selectWidgets
  deleteDg_      <- deleteDg
  subElemWidgets_<- subElemWidgets

  -- Place panel initial state in an IORef

  stRef <- liftIO $ newIORef NoSel :: PanelM c (IORef (PanelState c))

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
        putElement c iter ls rs editEntries_
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

  st <- liftIO $ readIORef stRef
  liftIO $ setState st

  -- Connect panel widgets

  onSelectionChangedAction <- connectSelector selector_ sm setState

  _ <- liftIO $ on cancelBt_ buttonActivated onSelectionChangedAction

  _ <- liftIO $ on editTb_ toggled $ do
    isActive <- toggleButtonGetActive editTb_
    when isActive $ do
      Sel iter <- readIORef stRef -- FIXME: unsafe pattern
      -- OLD: elem <- treeModelGetRow ls iter
      -- OLD: let valid = validStrings (itemToStrings item)
      -- Assume that before an Edit has been a Sel that has filled editEntries
      d <- readData c editEntries_
      v <- validData c d
      setState (EditOld iter v)

  _ <- liftIO $ on newTb_ toggled $ do
    isActive <- toggleButtonGetActive newTb_
    when isActive $ do
      -- OLD: let s = guiNewItemToStrings gui
      d <- readData c editEntries_
      v <- validData c d
      setState (EditNew d v)

  _ <- liftIO $ on deleteBt_ buttonActivated $ do
    (Sel iter) <- readIORef stRef -- FIXME: unsafe pattern
    resp <- dialogRun deleteDg_
    widgetHide deleteDg_
    when (resp == ResponseOk) $ do
      deleteElement c iter ls db
      setState NoSel

  _ <- liftIO $ on saveBt_ buttonActivated $ do
    st' <- readIORef stRef
    iter <- case st' of
      EditNew _    True -> insertElement c      ls db editEntries_
      EditOld iter True -> updateElement c iter ls db editEntries_
      _                 -> error "mkControllerImpl: Unexpected state when saving"
    selectElement c iter selector_ sm
    setState (Sel iter)

  -- Change state if validation state changes (check at every edit)
  -- forM_ editEntries_ $ \entry -> liftIO $ on entry editableChanged $ do
  --   st'    <- readIORef stRef
  --   d      <- readData c editEntries_
  --   v      <- validData c d
  --   case (st', v) of
  --     (EditNew s vOld, vNew) | vNew /= vOld -> setState (EditNew s vNew)
  --     (EditOld i vOld, vNew) | vNew /= vOld -> setState (EditOld i vNew)
  --     _                                     -> return ()

  liftIO $ mkSubElemController c ls sm db stRef

  return ()

getGladeObject :: (GObjectClass b, Controller c) => (GObject -> b) -> String -> PanelM c b
getGladeObject cast name = do
  c <- controller
  liftIO $ builderGetObject (builder c) cast (panelId c ++ name)

selectTreeViewElement ::  (TreeModelSortClass sm) => c -> TreeIter -> TreeView -> sm -> IO ()
selectTreeViewElement _ iter treeView sortedModel = do
  sortedIter <- treeModelSortConvertChildIterToIter sortedModel iter
  selection  <- treeViewGetSelection treeView
  treeSelectionSelectIter selection sortedIter

setTreeViewRenderers :: Controller c => TreeView -> LS c -> PanelM c ()
setTreeViewRenderers treeView listStore = do
  renderFuncs <- renderers
  -- forall columns: set renderer, set sorting func
  columns <- liftIO $ treeViewGetColumns treeView
  forM_ (zip columns renderFuncs) $ \(col, renderFunc) -> do
    -- FIXME: column manual resizing doesn't work
    let cellLayout = toCellLayout col
    (cell : _) <- liftIO $ cellLayoutGetCells cellLayout    -- FIXME: unsafe pattern, dep. on glade
    let textRenderer = castToCellRendererText cell -- FIXME: unsafe cast, depends on glade
    liftIO $ cellLayoutSetAttributes col textRenderer listStore $ \row -> [ cellText := renderFunc row ]

setTreeViewSorting :: (Controller c, TreeSortableClass sm) =>
                      TreeView
                   -> LS c
                   -> sm
                   -> [String -> String -> Ordering]
                   -> PanelM c ()
setTreeViewSorting treeView listStore sortedModel orderings = do
  renderFuncs <- renderers
  columns <- liftIO $ treeViewGetColumns treeView
  forM_ (zip4 columns renderFuncs orderings [0..]) $ \(col, renderFunc, ordering, colId) -> do
    let sortFunc xIter yIter = do
          xRow <- liftIO $ customStoreGetRow listStore xIter
          yRow <- liftIO $ customStoreGetRow listStore yIter
          return $ ordering (renderFunc xRow) (renderFunc yRow)
    liftIO $ treeSortableSetSortFunc sortedModel colId sortFunc
    liftIO $ treeViewColumnSetSortColumnId col colId

-- | Sets incremental search in tree view.
setTreeViewSearching :: (Controller c, TreeModelSortClass sm) =>
                        TreeView
                     -> LS c
                     -> sm
                     -> (String -> [String] -> Bool)
                     -> PanelM c ()
setTreeViewSearching treeView listStore sortedModel isPartOf = do
  -- TODO: accent-independent search or TreeModelFilter
  renderFuncs <- renderers
  let rowEqualFunc :: (String -> TreeIter -> IO Bool)
      rowEqualFunc txt iter = do
        childIter <- treeModelSortConvertIterToChildIter sortedModel iter
        row <- treeModelGetRow listStore childIter
        return $ txt `isPartOf` map ($ row) renderFuncs
  liftIO $ treeViewSetSearchEqualFunc treeView (Just rowEqualFunc)

connectTreeView :: (Controller c, TreeModelSortClass sm) =>
                   TreeView
                   -> sm
                   -> (PanelState c -> IO ())
                   -> PanelM c (IO ())
connectTreeView treeView sortedModel setState = do
  selection <- liftIO $ treeViewGetSelection treeView
  let toChildIter = treeModelSortConvertIterToChildIter sortedModel
  let onSelectionChangedAction = do
        count <- treeSelectionCountSelectedRows selection
        if count == 0
          then setState NoSel
          else treeSelectionSelectedForeach selection $ \it -> do
            cIt <- toChildIter it
            setState (Sel cIt)

  _ <- liftIO $ on selection treeSelectionSelectionChanged $ liftIO onSelectionChangedAction

  return onSelectionChangedAction
