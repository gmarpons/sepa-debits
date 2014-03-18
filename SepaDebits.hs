{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

--{-# LANGUAGE FlexibleInstances         #-}
--{-# LANGUAGE UndecidableInstances      #-}

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

-- | Generic panel controllor (useful for debtors panel, billing concepts panel, etc.).
-- Parametrized (through associated types) with an entity type and a selector (e.g.
-- TreeView) type.
class (DB.PersistEntity (E c), DB.PersistEntityBackend (E c) ~ DB.MongoBackend) =>
      Controller c where
  -- | Type of elements shown in the panel.
  type E c

  -- | type of the selector widget (e.g. TreeView or ComboBox).
  type S c

  -- All instances need to implement the following functions
  panelId  :: c -> PanelId -- ^ Instances need to implement this.
  builder  :: c -> Builder -- ^ Instances need to implement this.
  selector :: c -> IO (S c)
  setModel :: (TreeModelClass m) => S c -> m -> IO ()

  -- The following functions have a generally applicable default implementation. Some of
  -- them ar based on conventions for Glade names.
  panel    :: c -> IO VBox
  chooser  :: c -> IO ToggleButton
  newTb    :: c -> IO ToggleButton
  editTb   :: c -> IO ToggleButton
  deleteBt :: c -> IO Button
  saveBt   :: c -> IO Button
  cancelBt :: c -> IO Button
  entities :: c -> DB.ConnectionPool -> IO [DB.Entity (E c)]
  runGui   :: DB.ConnectionPool -> (MainWindowState -> IO ()) -> c -> IO ()
  panel    c    = builderGetObject (builder c) castToVBox         (panelId c ++ "_Vb")
  chooser  c    = builderGetObject (builder c) castToToggleButton (panelId c ++ "_Tb")
  newTb    c    = builderGetObject (builder c) castToToggleButton (panelId c ++ "_newTb")
  editTb   c    = builderGetObject (builder c) castToToggleButton (panelId c ++ "_editTb")
  deleteBt c    = builderGetObject (builder c) castToButton       (panelId c ++ "_deleteBt")
  saveBt   c    = builderGetObject (builder c) castToButton       (panelId c ++ "_saveBt")
  cancelBt c    = builderGetObject (builder c) castToButton       (panelId c ++ "_cancelBt")
  entities _ db = flip DB.runMongoDBPoolDef db $ DB.selectList ([] :: [DB.Filter (E c)]) []
  runGui        = mkPanelGui    -- Implemented as a top-level function

-- | Box for heterogeneous collections of @Controller@'s.
data BController where
  MkBController :: Controller c => c -> BController

-- TODO: if I could only declare the following instance ...
-- instance Controller BController where
--   panelId  (MkBController c) = panelId c
--   builder  (MkBController c) = builder c
--   selector (MkBController c) = selector c

bPanelId :: BController -> PanelId
bBuilder :: BController -> Builder
bChooser :: BController -> IO ToggleButton
bPanel   :: BController -> IO VBox
bRunGui  :: DB.ConnectionPool -> (MainWindowState -> IO ()) -> BController -> IO ()
bPanelId  (MkBController c) = panelId  c
bBuilder  (MkBController c) = builder  c
bChooser  (MkBController c) = chooser  c
bPanel    (MkBController c) = panel    c
bRunGui   db f (MkBController c) = runGui db f c

data BillingConceptsController = BC PanelId Builder

instance Controller BillingConceptsController where
  type E BillingConceptsController = BillingConcept
  type S BillingConceptsController = TreeView
  builder (BC _ builder_) = builder_
  panelId (BC panelId_ _) = panelId_
  selector c = builderGetObject (builder c) castToTreeView (panelId c ++ "_Tv")
  setModel = treeViewSetModel

mkMainWindowGui :: Builder -> DB.ConnectionPool -> IO Window
mkMainWindowGui builder_ db = do

  -- Create panel controllers and helper lists

  let controllers   = [ MkBController (BC "BC" builder_)
                      ] :: [BController]
  choosers          <- mapM bChooser controllers :: IO [ToggleButton]
  panels            <- mapM bPanel controllers :: IO [VBox]
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

  mapM_ (bRunGui db setState) controllers
  return mainWd

  -- panelController =
  --   -- We're not interested in mutating panel controllers
  --   lens
  --   ( \(BC panelId_ builder) -> (panelControllerDef panelId_ builder) {
  --        _setModel = \db -> do
  --           putStrLn "bc"
  --           view <- builderGetObject builder castToTreeView (panelId_ ++ "_Tv")
  --           entities <- flip DB.runMongoDBPoolDef db $
  --                       DB.selectList ([] :: [DB.Filter BillingConcept]) []
  --           print $ length entities
  --           -- Setting tree view models from glade doesn't work: stablish connection here and we can
  --           -- use treeViewGet* functions afterwards.
  --           -- billingConceptsLs <- treeViewGetListStore billingConceptsTv
  --           -- mapM_ (listStoreAppend billingConceptsLs) billingConceptsEL
  --           listStore   <- listStoreNew entities
  --           sortedModel <- treeModelSortNewWithModel listStore
  --           treeViewSetModel view sortedModel
  --        } )
  --   const

mkPanelGui :: (Controller c,
               DB.PersistEntity (E c), DB.PersistEntityBackend (E c) ~ DB.MongoBackend) =>
              DB.ConnectionPool -> (MainWindowState -> IO ()) -> c -> IO ()
mkPanelGui db setMainWindowState c = do

  selector_ <- selector c
  entities_ <- entities c db
  -- Setting tree view models from glade doesn't work: stablish connection here and we can
  -- use treeViewGet* functions afterwards.
  -- billingConceptsLs <- treeViewGetListStore billingConceptsTv
  -- mapM_ (listStoreAppend billingConceptsLs) billingConceptsEL
  listStore   <- listStoreNew entities_
  sortedModel <- treeModelSortNewWithModel listStore
  setModel selector_ sortedModel
  -- treeViewSetModel view_ sortedModel

  let renderFuncs = [ T.unpack      . (^. longName)   . DB.entityVal
                    , T.unpack      . (^. shortName)  . DB.entityVal
                    , priceToString . (^. basePrice)  . DB.entityVal
                    , priceToString . (^. vatRatio)   . DB.entityVal
                    , priceToString . (^. finalPrice) . DB.entityVal
                    ]

  -- forall columns: set renderer, set sorting func

  -- billingConceptsCols <- treeViewGetColumns billingConceptsTv
  -- forM_ (zip3 billingConceptsCols renderFuncs [0..]) $ \(col, renderFunc, colId) -> do
  --   -- FIXME: column manual resizing doesn't work
  --   let cellLayout = toCellLayout col
  --   (cell : _) <- liftIO $ cellLayoutGetCells cellLayout    -- FIXME: unsafe pattern, dep. on glade
  --   let textRenderer = castToCellRendererText cell          -- FIXME: unsafe cast, depends on glade
  --   liftIO $ cellLayoutSetAttributes col textRenderer billingConceptsLs $ \row ->
  --     [ cellText := renderFunc row ]
  --   let sortFunc xIter yIter = do
  --         xRow <- liftIO $ customStoreGetRow billingConceptsLs xIter
  --         yRow <- liftIO $ customStoreGetRow billingConceptsLs yIter
  --         return $ compare (renderFunc xRow) (renderFunc yRow)
  --   liftIO $ treeSortableSetSortFunc billingConceptsSm colId sortFunc
  --   liftIO $ treeViewColumnSetSortColumnId col colId

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

-- | Possibly unsafe operation. Error if @view@ doesn't have a TreeModelSort model.
treeViewGetSortedModel :: forall a . TreeView -> IO (TypedTreeModelSort a)
treeViewGetSortedModel view = do
  (Just uncastedTreeModelSort) <- treeViewGetModel view
  let treeModelSort = (G.unsafeCastGObject . toGObject) uncastedTreeModelSort
                      :: TypedTreeModelSort a
  return treeModelSort

-- | Possibly unsafe operation. Error if @view@ doesn't have a ListStore model.
treeViewGetListStore :: forall a . TreeView -> IO (ListStore a)
treeViewGetListStore view = do
  (Just uncastedTreeModelSort) <- treeViewGetModel view
  let treeModelSort = (G.unsafeCastGObject . toGObject) uncastedTreeModelSort
                      :: TypedTreeModelSort a
  uncastedListStore <- treeModelSortGetModel treeModelSort
  let listStore = (G.unsafeCastGObject . toGObject) uncastedListStore
  return listStore

priceToString :: Int -> String
priceToString num = TL.unpack $ format (left padding ' ') (toText num)
  where
    toText    = T.replace "." (T.pack separator) . priceToText
    separator = ","
    padding   = 10
