{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}

--{-# LANGUAGE UndecidableInstances      #-}
--{-# LANGUAGE MultiParamTypeClasses         #-}
--{-# LANGUAGE FlexibleInstances         #-}

module Main
       ( main
       ) where

import           Control.Lens             hiding (elements, set, view)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.List
import qualified Data.Text                as T (pack, replace, unpack)
import qualified Data.Text.Lazy           as TL (unpack)
import qualified Data.Time.Clock          as C (NominalDiffTime)
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

makeLenses ''MainWindowState

data PanelState
  = NoSel
  | Sel     { _item :: TreeIter }
  | EditNew { _data :: [String], _isValid :: Bool }
  | EditOld { _item :: TreeIter, _isValid :: Bool }
  deriving Show

makeLenses ''PanelState

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
      dbName     = "test" :: DB.Database
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
newtype M c a = M { runController :: c -> IO a }

instance (Controller c) => Monad (M c) where
  return a = M (\_ -> return a)
  M f >>= m = M (\c -> f c >>= g c)
    where g c a = let (M g') = m a in g' c

instance (Controller c) => MonadIO (M c) where
  liftIO io = M (const io)

controller :: (Controller c) => M c c
controller = M return

-- | Generic panel controller, used to factorize implementation of debtors, billing
-- concepts and direct debits panels (an VBox with all its contained widgets). It's main
-- entry point is template method @mkController@. In general, functions in this class are
-- non-pure, as they use state stored by Gtk in the IO monad. The class is parametrized
-- (through associated types) with an entity type and a @selector@ (e.g. TreeView) type.
class (DB.PersistEntity (E c), DB.PersistEntityBackend (E c) ~ DB.MongoBackend) =>
      Controller c where

  -- | Entity type, i.e., type of the elements shown in the panel (e.g Debtor).
  type E c

  -- | Type of the selector widget (e.g. TreeView or ComboBox).
  type S c

  -- All instances need to implement, at least, the following functions
  panelId              :: c -> PanelId -- ^ Instances need to implement this.
  builder              :: c -> Builder -- ^ Instances need to implement this.
  selector             ::                                                       M c (S c)
  setSelectorModel     :: (TreeModelClass m)      => S c         -> m        -> M c ()
  connectSelector      :: (TreeModelSortClass sm) => S c         -> sm -> () -> M c ()

  -- The following functions may be re-implemented, default instance implementation does
  -- nothing
  setSelectorRenderers ::                            S c -> LS c       -> M c ()
  setSelectorSorting   :: (TreeSortableClass sm)  => S c -> LS c -> sm -> M c ()
  setSelectorSearching :: (TreeModelSortClass sm) => S c -> LS c -> sm -> M c ()

  -- The following functions have a generally applicable default implementation. Some of
  -- them ar based on conventions for Glade names.
  panel                :: c -> IO VBox
  chooser              :: c -> IO ToggleButton
  newTb                ::                                        M c ToggleButton
  editTb               ::                                        M c ToggleButton
  deleteBt             ::                                        M c Button
  saveBt               ::                                        M c Button
  cancelBt             ::                                        M c Button
  elements             :: DB.ConnectionPool                   -> M c [PS c]

  -- The following are lists of functions.
  renderers            ::                                        M c [PS c -> String]

  -- | A Template Method pattern (it is implemented once for all instances) that
  -- initializes all the panel widgets, including connection with persistent model and
  -- callback events. All the other functions in this class are here only to be called by
  -- @mkController@, except @panelId@, @panel@ and @chooser@.
  mkController         :: DB.ConnectionPool -> (MainWindowState -> IO ()) -> M c ()

  -- Default implementations for some functions
  setSelectorRenderers _ _   = return ()
  setSelectorSorting   _ _ _ = return ()
  setSelectorSearching _ _ _ = return ()
  panel   c    = builderGetObject (builder c) castToVBox         (panelId c ++ "_Vb")
  chooser c    = builderGetObject (builder c) castToToggleButton (panelId c ++ "_Tb")
  newTb        = getGladeObject castToToggleButton "_newTb"
  editTb       = getGladeObject castToToggleButton "_editTb"
  deleteBt     = getGladeObject castToButton       "_deleteBt"
  saveBt       = getGladeObject castToButton       "_saveBt"
  cancelBt     = getGladeObject castToButton       "_cancelBt"
  elements db  = liftIO $ flip DB.runMongoDBPoolDef db $ DB.selectList ([] :: [DB.Filter (E c)]) []
  renderers    = return []
  mkController = mkPanelGui    -- Implemented as a top-level function

getGladeObject :: (GObjectClass b, Controller c) => (GObject -> b) -> String -> M c b
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
bRunMkController (MkBController boxed) db f = runController (mkController db f) boxed

data BillingConceptsController = BC PanelId Builder

instance Controller BillingConceptsController where
  type E BillingConceptsController = BillingConcept
  type S BillingConceptsController = TreeView
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
  connectSelector s sm f = connectTreeView s sm f

data DebtorsController = DE PanelId Builder

instance Controller DebtorsController where
  type E DebtorsController = Debtor
  type S DebtorsController = TreeView
  builder  (DE _        builder_) = builder_
  panelId  (DE panelId_ _       ) = panelId_
  selector = getGladeObject castToTreeView "_Tv"
  setSelectorModel     s m     = liftIO $ treeViewSetModel s m
  setSelectorRenderers s ls    = setTreeViewRenderers s ls
  setSelectorSorting   s ls sm = setTreeViewSorting   s ls sm orderings
    where orderings = repeat compare -- TODO: catalan collation
  setSelectorSearching s ls sm = setTreeViewSearching s ls sm isPartOf
    where tx `isPartOf` txs = any (tx `isInfixOf`) txs -- TODO: better searching
  renderers  = return [ T.unpack      . (^. lastName)   . DB.entityVal
                      , T.unpack      . (^. firstName)  . DB.entityVal
                      ]
  connectSelector s sm f = connectTreeView s sm f

setTreeViewRenderers :: Controller c => TreeView -> LS c -> M c ()
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
                   -> M c ()
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
                     -> M c ()
setTreeViewSearching treeView listStore sortedModel isPartOf = do
  -- TODO: accent-independent search or TreeModelFilter
  renderFuncs <- renderers
  let rowEqualFunc :: (String -> TreeIter -> IO Bool)
      rowEqualFunc txt iter = do
        childIter <- treeModelSortConvertIterToChildIter sortedModel iter
        -- row <- customStoreGetRow listStore childIter
        row <- treeModelGetRow listStore childIter
        -- return $ any (\f -> text_ `isInfixOf` f row) renderFuncs
        return $ txt `isPartOf` map ($ row) renderFuncs
  liftIO $ treeViewSetSearchEqualFunc treeView (Just rowEqualFunc)

connectTreeView :: (Controller c, TreeModelSortClass sm) =>
                   TreeView
                   -> sm
                   -> ()
                   -> M c ()
connectTreeView treeView sortedModel _ = do
  selection <- liftIO $ treeViewGetSelection treeView
  let toChildIter = treeModelSortConvertIterToChildIter sortedModel
  let onSelectionChangedAction = do
        count <- treeSelectionCountSelectedRows selection
        if count == 0
          then {- setState NoSel adRef stRef gui panelId -}
            print "NoSel"
          else treeSelectionSelectedForeach selection $ \it -> do
            cIt <- toChildIter it
            {- setState (Sel cIt) adRef stRef gui panelId -}
            print cIt

  _ <- liftIO $ on selection treeSelectionSelectionChanged $ liftIO onSelectionChangedAction

  cancelBt_ <- cancelBt
  _ <- liftIO $ on cancelBt_ buttonActivated onSelectionChangedAction
  return ()


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

  _ <- on mwExitBt buttonActivated $ widgetDestroy mainWd >> mainQuit

  _ <- on mainWd objectDestroy mainQuit -- FIXME: don't exit if dirty state

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

  -- Create Gui for all panels, and return

  mapM_ (\boxed -> bRunMkController boxed db setState) controllers
  return mainWd

mkPanelGui :: (Controller c,
               DB.PersistEntity (E c), DB.PersistEntityBackend (E c) ~ DB.MongoBackend) =>
              DB.ConnectionPool -> (MainWindowState -> IO ()) -> M c ()
mkPanelGui db _setMainWindowState = do

  -- Setting tree view models from glade doesn't work: set connection here.
  -- Tried: ls <- treeViewGetListStore tv
  --        mapM_ (listStoreAppend ls) e

  -- We could put most of the variables in the following code (s, e, ls, etc.) under the
  -- monad M, but prefer to show them to make explicit the dependencies among Gtk
  -- operations.

  s  <- selector
  e  <- elements db
  ls <- liftIO $ listStoreNew e
  sm <- liftIO $ treeModelSortNewWithModel ls
  setSelectorModel     s sm
  setSelectorRenderers s ls
  setSelectorSorting   s ls sm
  setSelectorSearching s ls sm
--  connectSelector      s    sm setState
  connectSelector      s    sm ()

  return ()

priceToString :: Int -> String
priceToString num = TL.unpack $ format (left padding ' ') (toText num)
  where
    toText    = T.replace "." (T.pack separator) . priceToText
    separator = ","
    padding   = 10
