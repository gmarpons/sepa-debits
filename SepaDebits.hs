{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Main
       ( main
       ) where

import           Control.Lens             hiding (element, elements, set, view)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.List
import qualified Data.Text                as T (Text, pack, replace, strip, unpack)
import qualified Data.Text.Lazy           as TL (unpack)
import qualified Data.Time.Clock          as C (NominalDiffTime)
import qualified Data.Time.Calendar       as C
import qualified Database.Persist.MongoDB as DB
import           Formatting               hiding (builder)
import           Graphics.UI.Gtk
import qualified Network                  as N (PortID (PortNumber))
import           Sepa.BillingConcept
import           Sepa.Debtor

-- -- | I assume that the elements in this record have their own state (e.g. through IORef's
-- -- or MVar's), so they can be used in callbacks.
-- data AppData
--   = AppData
--     { _builder :: Builder
--     , _db      :: DB.ConnectionPool
--     }

-- makeLenses ''AppData

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

main :: IO ()
main = do
  _ <- initGUI
  db_ <- DB.createMongoDBPool dbName hostName port Nothing poolSize stripeSize time
  builder_ <- builderNew
  builderAddFromFile builder_ gladeFile
  mainWd <- mkMainWindowGui builder_ db_
  widgetShowAll mainWd
  mainGUI
    where
      gladeFile  = "glade/SepaDebits.glade"
      dbName     = "sepadebits" :: DB.Database
      hostName   = "localhost" :: DB.HostName
      port       = N.PortNumber 27017 :: N.PortID -- Standard MongoDB port
      poolSize   = 10                             -- Num. of stripes
      stripeSize = 10                             -- Num. of connections per stripe
      time       = 60 :: C.NominalDiffTime        -- Seconds

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
  readData          :: c                                         -> [Entry] -> IO (D c)
  validData            :: c -> D c                                          -> IO Bool

  -- The following functions may be re-implemented, default instance implementation does
  -- nothing
  setSelectorRenderers ::                            S c -> LS c       -> PanelM c ()
  setSelectorSorting   :: (TreeSortableClass sm)  => S c -> LS c -> sm -> PanelM c ()
  setSelectorSearching :: (TreeModelSortClass sm) => S c -> LS c -> sm -> PanelM c ()
  editEntries          ::                                        PanelM c [Entry]
  editWidgets          ::                                        PanelM c [Widget]
  selectWidgets        ::                                        PanelM c [Widget]
  renderers            ::                                        PanelM c [PS c -> String]

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
  elements             :: DB.ConnectionPool                   -> PanelM c [PS c]
  panelIdM             ::                                        PanelM c PanelId
  putElement           :: c -> TreeIter -> LS c -> [PS c -> String] -> [Entry] -> IO ()

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
  renderers                  = return []
  panel   c    = builderGetObject (builder c) castToVBox         (panelId c ++ "_Vb")
  chooser c    = builderGetObject (builder c) castToToggleButton (panelId c ++ "_Tb")
  newTb        = getGladeObject castToToggleButton "_newTb"
  editTb       = getGladeObject castToToggleButton "_editTb"
  deleteBt     = getGladeObject castToButton       "_deleteBt"
  saveBt       = getGladeObject castToButton       "_saveBt"
  cancelBt     = getGladeObject castToButton       "_cancelBt"
  elements db  = liftIO $ flip DB.runMongoDBPoolDef db $ DB.selectList ([] :: [DB.Filter (E c)]) []
  panelIdM     = do { c <- controller; return (panelId c) }
  putElement _ iter ls renderers_ entries = do
    entity <- treeModelGetRow ls iter
    forM_ (zip entries renderers_) $ \(e, r) -> set e [entryText := r entity]
  mkController = mkControllerImpl -- Implemented as a top-level function

getGladeObject :: (GObjectClass b, Controller c) => (GObject -> b) -> String -> PanelM c b
getGladeObject cast name = do
  c <- controller
  liftIO $ builderGetObject (builder c) cast (panelId c ++ name)

-- | Box for heterogeneous collections of @Controller@'s.
data BController where
  MkBController :: Controller c => c -> BController

-- TODO: if I could only declare the following instance ...
-- instance Controller BController where
--   panelId  (MkBController c) = panelId c
--   builder  (MkBController c) = builder c
--   selector (MkBController c) = selector c

bPanelId      :: BController -> PanelId
bBuilder      :: BController -> Builder
bChooser      :: BController -> IO ToggleButton
bPanel        :: BController -> IO VBox
bRunMkController :: BController -> DB.ConnectionPool -> (MainWindowState -> IO ()) -> IO ()
bPanelId      (MkBController boxed)      = panelId boxed
bBuilder      (MkBController boxed)      = builder boxed
bChooser      (MkBController boxed)      = chooser boxed
bPanel        (MkBController boxed)      = panel   boxed
bRunMkController (MkBController boxed) db f = runPanel (mkController db f) boxed

data BillingConceptsController = BC PanelId Builder

instance Controller BillingConceptsController where
  type E BillingConceptsController = BillingConcept
  type S BillingConceptsController = TreeView
  data D BillingConceptsController =
    DBC { longNameD    :: T.Text
        , shortNameD   :: T.Text
        , basePriceD   :: Maybe Int
        , vatRatioD    :: Maybe Int }
  builder  (BC _        builder_) = builder_
  panelId  (BC panelId_ _       ) = panelId_
  selector = getGladeObject castToTreeView "_Tv"
  setSelectorModel     s m     = liftIO $ treeViewSetModel s m
  setSelectorRenderers s ls    = setTreeViewRenderers s ls
  setSelectorSorting   s ls sm = setTreeViewSorting   s ls sm orderings
    where orderings = repeat compare -- TODO: catalan collation
  setSelectorSearching s ls sm = setTreeViewSearching s ls sm isPartOf
    where tx `isPartOf` txs = any (tx `isInfixOf`) txs -- TODO: better searching
  renderers = return [ T.unpack      . (^. longName)   . DB.entityVal
                     , T.unpack      . (^. shortName)  . DB.entityVal
                     , priceToString . (^. basePrice)  . DB.entityVal
                     , priceToString . (^. vatRatio)   . DB.entityVal
                     , priceToString . (^. finalPrice) . DB.entityVal
                     ]
  editEntries = do e1 <- getGladeObject castToEntry "_EE_longNameEn"
                   e2 <- getGladeObject castToEntry "_EE_shortNameEn"
                   e3 <- getGladeObject castToEntry "_EE_basePriceEn"
                   e4 <- getGladeObject castToEntry "_EE_vatRatioEn"
                   e5 <- getGladeObject castToEntry "_EE_finalPriceEn"
                   return [e1, e2, e3, e4, e5]
  readData _ [longNameEn, shortNameEn, basePriceEn, vatRatioEn, _] = do
    longName_    <- get longNameEn    entryText
    shortName_   <- get shortNameEn   entryText
    basePrice_   <- get basePriceEn   entryText
    vatRatio_    <- get vatRatioEn    entryText
    return DBC { longNameD   = T.strip (T.pack longName_)
               , shortNameD  = T.strip (T.pack shortName_)
               , basePriceD  = Just (stringToPrice basePrice_)
               , vatRatioD   = Just (stringToPrice vatRatio_)
               }
  readData _ _ = error "readData (BC): wrong number of entries"
  validData _ d = do
    return $ validBillingConcept (longNameD d) (shortNameD d) (basePriceD d) (vatRatioD d)
  connectSelector s sm f = connectTreeView s sm f

data DebtorsController = DE PanelId Builder

instance Controller DebtorsController where
  type E DebtorsController = Debtor
  type S DebtorsController = TreeView
  data D DebtorsController =
    DDE { lastNameD  :: T.Text
        , firstNameD :: T.Text }
  builder  (DE _        builder_) = builder_
  panelId  (DE panelId_ _       ) = panelId_
  selector = getGladeObject castToTreeView "_Tv"
  setSelectorModel     s m     = liftIO $ treeViewSetModel s m
  setSelectorRenderers s ls    = setTreeViewRenderers s ls
  setSelectorSorting   s ls sm = setTreeViewSorting   s ls sm orderings
    where orderings = repeat compare -- TODO: catalan collation
  setSelectorSearching s ls sm = setTreeViewSearching s ls sm isPartOf
    where tx `isPartOf` txs = any (tx `isInfixOf`) txs -- TODO: better searching
  renderers  = return [ T.unpack        . (^. lastName)          . DB.entityVal
                      , T.unpack        . (^. firstName)         . DB.entityVal
                      , C.showGregorian . (^. registrationDate)  . DB.entityVal
                      ]
  editEntries = do e1 <- getGladeObject castToEntry "_EE_lastNameEn"
                   e2 <- getGladeObject castToEntry "_EE_firstNameEn"
                   return [e1, e2]
  readData _ [lastNameEn, firstNameEn] = do
    lastName_    <- get lastNameEn    entryText
    firstName_   <- get firstNameEn   entryText
    return DDE { lastNameD   = T.pack lastName_
               , firstNameD  = T.pack firstName_
               }
  readData _ _ = error "readData (DE): wrong number of entries"
  validData _ d = do
    return $ validDebtorName (firstNameD d) (lastNameD d)
  connectSelector s sm f = connectTreeView s sm f

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


mkMainWindowGui :: Builder -> DB.ConnectionPool -> IO Window
mkMainWindowGui builder_ db = do

  -- Create panel controllers and helper lists

  let controllers   = [ MkBController (BC "BC" builder_)
                      , MkBController (DE "DE" builder_)
                      ] :: [BController]
  choosers          <- mapM bChooser controllers :: IO [ToggleButton]
  panels            <- mapM bPanel controllers   :: IO [VBox]
  let panelsAL      = zip choosers panels

  -- Get main window widgets from Glade builder

  mainWd            <- builderGetObject builder_ castToWindow "mainWd"
  mwExitBt          <- builderGetObject builder_ castToButton "mwExitBt"
  mainVb            <- builderGetObject builder_ castToVBox   "mainVb"
  cannotQuitDg      <- builderGetObject builder_ castToDialog "cannotQuitDg"
  quitDg            <- builderGetObject builder_ castToDialog "quitDg"

  -- Place main window initial state in an IORef

  -- FIXME: partial func 'head'
  stRef <- newIORef (View (bPanelId (head controllers))) :: IO (IORef MainWindowState)

  -- Auxiliary functions

  -- FIXME: partial func 'head'
  let chooserFromId panelId_ = bChooser $ head $ filter ((== panelId_) . bPanelId) controllers

  let setState :: MainWindowState -> IO ()
      setState (View i) = do -- st <- readIORef stRef
                             set mwExitBt [ widgetSensitive := True ]
                             chs <- chooserFromId i
                             let otherChoosers = filter (/= chs) choosers
                             mapM_ (`set` [ widgetSensitive := True ]) otherChoosers
                             writeIORef stRef (View i)
                             putStrLn $ "View " ++ i
      setState (Edit i) = do -- st <- readIORef stRef
                             set mwExitBt [ widgetSensitive := False ]
                             chs <- chooserFromId i
                             let otherChoosers = filter (/= chs) choosers
                             mapM_ (`set` [ widgetSensitive := False ]) otherChoosers
                             writeIORef stRef (Edit i)
                             putStrLn $ "Edit " ++ i

  -- Connect signals

  _ <- on mwExitBt buttonActivated $ do
    resp <- dialogRun quitDg
    widgetHide quitDg
    when (resp == ResponseOk) $ do
      widgetDestroy quitDg
      widgetDestroy mainWd
      mainQuit

  _ <- on mainWd deleteEvent $ liftIO $ do
    st <- readIORef stRef
    case st of
      View _ -> do resp <- dialogRun quitDg
                   widgetHide quitDg
                   return $ resp /= ResponseOk
      Edit _ -> do _ <- dialogRun cannotQuitDg
                   widgetHide cannotQuitDg
                   return True

  _ <- on mainWd objectDestroy mainQuit

  forM_ choosers $ \chs -> on chs toggled $ do
    -- If button can be toggled then it was active, and its panel selected
    isActive <- toggleButtonGetActive chs
    when isActive $ do
      set chs [ widgetSensitive := False ]
      let otherChoosers = filter (/= chs) choosers
      forM_ otherChoosers $ flip set [ toggleButtonActive := False
                                     , widgetSensitive := True ]
      (oldVBox : _) <- containerGetChildren mainVb      -- FIXME: unsafe pattern
      let (Just newVBox) = lookup chs panelsAL          -- FIXME: unsafe pattern
      containerRemove mainVb oldVBox
      boxPackStart mainVb newVBox PackGrow 0

  -- Set main window initial state

  st <- readIORef stRef
  setState st

  -- Create controllers for all panels, and return

  mapM_ (\boxed -> bRunMkController boxed db setState) controllers
  return mainWd

mkControllerImpl :: forall c . (Controller c,
                     DB.PersistEntity (E c), DB.PersistEntityBackend (E c) ~ DB.MongoBackend) =>
                    DB.ConnectionPool -> (MainWindowState -> IO ()) -> PanelM c ()
mkControllerImpl db setMainWdState = do

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

  -- Place panel initial state in an IORef

  stRef <- liftIO $ newIORef NoSel :: PanelM c (IORef (PanelState c))

  -- Panel state function
  let setState' :: PanelState c -> IO ()
      setState' NoSel = do
        forM_ editEntries_                    (`set` [widgetSensitive    := False, entryText := ""])
        forM_ editWidgets_                    (`set` [widgetSensitive    := False])
        forM_ selectWidgets_                  (`set` [widgetSensitive    := False])
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
        forM_ [selector_]                     (`set` [widgetSensitive    := True ])
        forM_ [editTb_, newTb_]               (`set` [widgetSensitive    := True ])
        forM_ [deleteBt_]                     (`set` [widgetSensitive    := True ])
        forM_ [editTb_, newTb_]               (`set` [toggleButtonActive := False])
        setMainWdState (View panelId_)
      setState' (EditNew _iter valid) = do
        forM_ selectWidgets_                  (`set` [widgetSensitive    := False])
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

  return ()

-- TODO: Merge this function with BC.priceToText
-- | We pad with spaces, to correct show prices in right-aligned columns.
priceToString :: Int -> String
priceToString num = TL.unpack $ format (left padding ' ') (toText num)
  where
    toText    = T.replace "." (T.pack separator) . priceToText
    separator = ","             -- FIXME: Take separator from locale
    padding   = 10

-- TODO: set signal on all numeric entries to guarantee only valid chars

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
