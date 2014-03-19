{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

--{-# LANGUAGE UndecidableInstances      #-}
--{-# LANGUAGE MultiParamTypeClasses         #-}
--{-# LANGUAGE FlexibleInstances         #-}

module Main
       ( main
       ) where

import           Control.Lens             hiding (set, view)
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
import qualified System.Glib.GObject      as G

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

-- | Generic panel controller, used to factorize implementation of debtors, billing
-- concepts and direct debits panels (an VBox with all its contained widgets). It's main
-- entry point is template method @mkController@. In general, functions in this class are
-- non-pure, as they use state stored by Gtk in the IO monad. The class is parametrized
-- (through associated types) with an entity type and a @selector@ (e.g. TreeView) type.
class (DB.PersistEntity (E c), DB.PersistEntityBackend (E c) ~ DB.MongoBackend) =>
      Controller c where

  -- | Type of elements shown in the panel (e.g Debtor).
  type E c

  -- | Type of the selector widget (e.g. TreeView or ComboBox).
  type S c

  -- All instances need to implement, at least, the following functions
  panelId              :: c -> PanelId -- ^ Instances need to implement this.
  builder              :: c -> Builder -- ^ Instances need to implement this.
  selector             ::                                        c -> IO (S c)
  setSelectorModel     :: (TreeModelClass m) =>     S c -> m  -> c -> IO ()

  -- The following functions may be re-implemented, default instance implementation does
  -- nothing
  setSelectorRenderers ::                           S c -> LS c       -> c -> IO ()
  setSelectorSorting   :: (TreeSortableClass sm) => S c -> LS c -> sm -> c -> IO ()

  -- The following functions have a generally applicable default implementation. Some of
  -- them ar based on conventions for Glade names.
  panel                ::                                        c -> IO VBox
  chooser              ::                                        c -> IO ToggleButton
  newTb                ::                                        c -> IO ToggleButton
  editTb               ::                                        c -> IO ToggleButton
  deleteBt             ::                                        c -> IO Button
  saveBt               ::                                        c -> IO Button
  cancelBt             ::                                        c -> IO Button
  entities             :: DB.ConnectionPool                   -> c -> IO [PS c]

  -- The following are lists of functions.
  renderers            ::                                        c -> [PS c -> String]

  -- | A Template Method pattern (it is implemented once for all instances) that
  -- initializes all the panel widgets, including connection with persistent model and
  -- callback events. All the other functions in this class are here only to be called by
  -- @mkController@, except @panelId@, @panel@ and @chooser@.
  mkController         :: DB.ConnectionPool -> (MainWindowState -> IO ()) -> c -> IO ()

  -- Default implementations for some functions
  setSelectorRenderers _ _ _   = return ()
  setSelectorSorting   _ _ _ _ = return ()
  panel    c    = builderGetObject (builder c) castToVBox         (panelId c ++ "_Vb")
  chooser  c    = builderGetObject (builder c) castToToggleButton (panelId c ++ "_Tb")
  newTb    c    = builderGetObject (builder c) castToToggleButton (panelId c ++ "_newTb")
  editTb   c    = builderGetObject (builder c) castToToggleButton (panelId c ++ "_editTb")
  deleteBt c    = builderGetObject (builder c) castToButton       (panelId c ++ "_deleteBt")
  saveBt   c    = builderGetObject (builder c) castToButton       (panelId c ++ "_saveBt")
  cancelBt c    = builderGetObject (builder c) castToButton       (panelId c ++ "_cancelBt")
  entities db _ = flip DB.runMongoDBPoolDef db $ DB.selectList ([] :: [DB.Filter (E c)]) []
  renderers _   = []
  mkController  = mkPanelGui    -- Implemented as a top-level function

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
bMkController :: BController -> DB.ConnectionPool -> (MainWindowState -> IO ()) -> IO ()
bPanelId      (MkBController c)      = panelId           c
bBuilder      (MkBController c)      = builder           c
bChooser      (MkBController c)      = chooser           c
bPanel        (MkBController c)      = panel             c
bMkController (MkBController c) db f = mkController db f c

data BillingConceptsController = BC PanelId Builder

instance Controller BillingConceptsController where
  type E BillingConceptsController = BillingConcept
  type S BillingConceptsController = TreeView
  builder  (BC _        builder_) = builder_
  panelId  (BC panelId_ _       ) = panelId_
  selector (BC panelId_ builder_) =
    builderGetObject builder_ castToTreeView (panelId_ ++ "_Tv")
  setSelectorModel s m   _ = treeViewSetModel s m
  setSelectorRenderers s ls    c = setTreeViewRenderers s ls    (renderers c) c
  setSelectorSorting   s ls sm c = setTreeViewSorting   s ls sm (renderers c) orderings c
    where orderings = repeat compare -- TODO: catalan collation
  renderers _ = [ T.unpack      . (^. longName)   . DB.entityVal
                , T.unpack      . (^. shortName)  . DB.entityVal
                , priceToString . (^. basePrice)  . DB.entityVal
                , priceToString . (^. vatRatio)   . DB.entityVal
                , priceToString . (^. finalPrice) . DB.entityVal
                ]

setTreeViewRenderers :: Controller c => TreeView -> LS c -> [PS c -> String] -> c -> IO ()
setTreeViewRenderers treeView listStore renderFuncs _ = do
  -- forall columns: set renderer, set sorting func
  columns <- treeViewGetColumns treeView
  forM_ (zip columns renderFuncs) $ \(col, renderFunc) -> do
    -- FIXME: column manual resizing doesn't work
    let cellLayout = toCellLayout col
    (cell : _) <- cellLayoutGetCells cellLayout    -- FIXME: unsafe pattern, dep. on glade
    let textRenderer = castToCellRendererText cell -- FIXME: unsafe cast, depends on glade
    cellLayoutSetAttributes col textRenderer listStore $ \row -> [ cellText := renderFunc row ]

setTreeViewSorting :: (Controller c, TreeSortableClass sm) =>
                      TreeView
                   -> LS c
                   -> sm
                   -> [PS c-> String]
                   -> [String -> String -> Ordering]
                   -> c
                   -> IO ()
setTreeViewSorting treeView listStore sortedModel renderFuncs orderings _ = do
  -- forall columns: set renderer, set sorting func
  columns <- treeViewGetColumns treeView
  forM_ (zip4 columns renderFuncs orderings [0..]) $ \(col, renderFunc, ordering, colId) -> do
    let sortFunc xIter yIter = do
          xRow <- customStoreGetRow listStore xIter
          yRow <- customStoreGetRow listStore yIter
          return $ ordering (renderFunc xRow) (renderFunc yRow)
    treeSortableSetSortFunc sortedModel colId sortFunc
    treeViewColumnSetSortColumnId col colId

mkMainWindowGui :: Builder -> DB.ConnectionPool -> IO Window
mkMainWindowGui builder_ db = do

  -- Create panel controllers and helper lists

  let controllers   = [ MkBController (BC "BC" builder_)
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

  mapM_ (\bc -> bMkController bc db setState) controllers
  return mainWd

mkPanelGui :: (Controller c,
               DB.PersistEntity (E c), DB.PersistEntityBackend (E c) ~ DB.MongoBackend) =>
              DB.ConnectionPool -> (MainWindowState -> IO ()) -> c -> IO ()
mkPanelGui db setMainWindowState c = do

  -- Setting tree view models from glade doesn't work: stablish connection here and we can
  -- use treeViewGet* functions afterwards.
  -- Tried: billingConceptsLs <- treeViewGetListStore billingConceptsTv
  --        mapM_ (listStoreAppend billingConceptsLs) billingConceptsEL

  s  <- selector c
  e  <- entities db c
  ls <- listStoreNew e
  sm <- treeModelSortNewWithModel ls
  setSelectorModel     s sm    c
  setSelectorRenderers s ls    c
  setSelectorSorting   s ls sm c

  -- -- Incremental search in tree view

  -- -- TODO: accent-independent search or TreeModelFilter
  -- let equalFunc text_ iter = do
  --       childIter <- liftIO $ treeModelSortConvertIterToChildIter billingConceptsSm iter
  --       row <- liftIO $ customStoreGetRow billingConceptsLs childIter
  --       return $ any (\f -> text_ `isInfixOf` f row) renderFuncs
  -- liftIO $ treeViewSetSearchEqualFunc billingConceptsTv (Just equalFunc)

  -- -- Connect selector

  -- let treeView = billingConceptsTv
  -- modelSort <- liftIO $ treeViewGetSortedModel treeView
  -- selection <- liftIO $ treeViewGetSelection treeView
  -- let toChildIter = treeModelSortConvertIterToChildIter modelSort
  -- -- let onSelectionChangedAction = do
  -- --                     count <- treeSelectionCountSelectedRows selection
  -- --                     if count == 0
  -- --                       then setState NoSel adRef stRef gui panelId
  -- --                       else treeSelectionSelectedForeach selection $ \it ->
  -- --                                do cIt <- toChildIter it
  -- --                                   -- row <- treeModelGetRow model cIt
  -- --                                   setState (Sel cIt) adRef stRef gui panelId
  -- liftIO $ on selection treeSelectionSelectionChanged $ do
  --   count <- liftIO $ treeSelectionCountSelectedRows selection
  --   if count == 0
  --     then return ()
  --     else liftIO $ treeSelectionSelectedForeach selection $ \it -> do
  --       cIt <- toChildIter it
  --       return ()
  -- -- on (cancelBt gui) buttonActivated $ onSelectionChangedAction
  return ()

priceToString :: Int -> String
priceToString num = TL.unpack $ format (left padding ' ') (toText num)
  where
    toText    = T.replace "." (T.pack separator) . priceToText
    separator = ","
    padding   = 10
