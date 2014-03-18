{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}

--{-# LANGUAGE MultiParamTypeClasses           #-}
--{-# LANGUAGE FunctionalDependencies           #-}
--{-# LANGUAGE FlexibleInstances           #-}
--{-# LANGUAGE UndecidableInstances           #-}

module Main
       ( main
       ) where

import           Control.Lens                hiding (set, view)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.List
import qualified Data.Text                   as T (pack, replace, unpack)
import qualified Data.Text.Lazy              as TL (unpack)
import qualified Data.Time.Clock             as C (NominalDiffTime)
import qualified Database.Persist.MongoDB    as DB
import           Formatting                  hiding (builder)
import           Graphics.UI.Gtk
import qualified Network                     as N (PortID (PortNumber))
import           Sepa.BillingConcept
import qualified System.Glib.GObject         as G

-- | I assume that the elements in this record have their own state (e.g. through IORef's
-- or MVar's), so they can be used in callbacks.
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

-- | Meant to be used as class @HasPanelController@, generated with TH.
data PanelController =
  PanelController
  { _panelId            :: PanelId
  , _panel              :: IO VBox
  , _chooser            :: IO ToggleButton
  , _newTb              :: IO ToggleButton
  , _editTb             :: IO ToggleButton
  , _deleteBt           :: IO Button
  , _saveBt             :: IO Button
  , _cancelBt           :: IO Button
  , _setModel           :: DB.ConnectionPool -> IO ()
  }

makeClassy ''PanelController

data BoxedPanelController = forall a . HasPanelController a => MkBoxedPanelController a

instance HasPanelController BoxedPanelController where
  panelController =
    lens
    (\(MkBoxedPanelController b) -> b ^. panelController)
    const                       -- We're not interested in mutating panel controllers

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

mkMainWindowGui :: Builder -> DB.ConnectionPool  -> IO Window
mkMainWindowGui builder db = do
  let controllers   = [ MkBoxedPanelController (BC "BC" builder)
                      ] :: [BoxedPanelController]
  choosers          <- sequence $ controllers ^.. traversed . chooser :: IO [ToggleButton]
  panels            <- sequence $ controllers ^.. traversed . panel   :: IO [VBox]
  let panelsAL      = zip choosers panels

  -- Get main window widgets from Glade builder

  mainWd            <- builderGetObject builder castToWindow "mainWd"
  mwExitBt          <- builderGetObject builder castToButton "mwExitBt"
  mainVb            <- builderGetObject builder castToVBox   "mainVb"

  -- Place main window initial state in an IORef

  -- FIXME: partial func 'head'
  stRef <- newIORef (View (head controllers ^. panelId)) :: IO (IORef MainWindowState)

  -- Auxiliary functions

  -- FIXME: partial func 'head'. Have a look at 'firstOf' funcion in Lens.Fold
  let chooserFromId panelId_ =
        head $ controllers ^.. folded . filtered (\y -> y ^. panelId == panelId_) . chooser

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

  --mkBillingConceptsGui appData setState
  mapM_ (mkPanelGui db setState) controllers
  return mainWd

panelControllerDef :: PanelId -> Builder -> PanelController
panelControllerDef panelId_ builder_ = PanelController {
  _panel    = builderGetObject builder_ castToVBox         (panelId_ ++ "_Vb"),
  _chooser  = builderGetObject builder_ castToToggleButton (panelId_ ++ "_Tb"),
  _newTb    = builderGetObject builder_ castToToggleButton (panelId_ ++ "_newTb"),
  _editTb   = builderGetObject builder_ castToToggleButton (panelId_ ++ "_editTb"),
  _deleteBt = builderGetObject builder_ castToButton       (panelId_ ++ "_deleteBt"),
  _saveBt   = builderGetObject builder_ castToButton       (panelId_ ++ "_saveBt"),
  _cancelBt = builderGetObject builder_ castToButton       (panelId_ ++ "_cancelBt"),
  _setModel = \_ -> putStrLn "def",
  _panelId  = panelId_
  }

data BillingConceptsController = BC PanelId Builder

instance HasPanelController BillingConceptsController where
  panelController =
    -- We're not interested in mutating panel controllers
    lens
    ( \(BC panelId_ builder) -> (panelControllerDef panelId_ builder) {
         _setModel = \db -> do
            putStrLn "bc"
            view <- builderGetObject builder castToTreeView (panelId_ ++ "_Tv")
            entities <- flip DB.runMongoDBPoolDef db $
                        DB.selectList ([] :: [DB.Filter BillingConcept]) []
            print $ length entities
            -- Setting tree view models from glade doesn't work: stablish connection here and we can
            -- use treeViewGet* functions afterwards.
            -- billingConceptsLs <- treeViewGetListStore billingConceptsTv
            -- mapM_ (listStoreAppend billingConceptsLs) billingConceptsEL
            listStore   <- listStoreNew entities
            sortedModel <- treeModelSortNewWithModel listStore
            treeViewSetModel view sortedModel
         } )
    const

mkPanelGui :: DB.ConnectionPool -> (MainWindowState -> IO ()) -> BoxedPanelController -> IO ()
mkPanelGui db setMainWindowState c = do

  c ^. setModel $ db            -- FIXME: an IO action with getter syntax? DIRTY

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
