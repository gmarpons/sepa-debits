{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main
       ( main
       ) where

import           Control.Lens             hiding (set, view)
import           Control.Monad
import           Data.IORef
import           Data.List
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.Text                as T (pack, replace, unpack)
import qualified Data.Text.Lazy           as TL (unpack)
import qualified Data.Time.Clock          as C (NominalDiffTime)
import qualified Database.Persist.MongoDB as DB
import           Formatting               hiding (builder)
import           Graphics.UI.Gtk
import qualified Network                  as N (PortID (PortNumber))
import           Sepa.BillingConcept
import qualified System.Glib.GObject      as G

type PanelId = String
type PanelDescr = (PanelId, (String, String))

panels :: [PanelDescr]
panels =
  [ ("debtors",         ("debtorsTb",         "debtorsVb"        ))
  , ("billingConcepts", ("billingConceptsTb", "billingConceptsVb"))
  , ("directDebits",    ("directDebitsTb",    "directDebitsVb"   ))
  ]

data AppState
  = AppState
    { _guiSt   :: GuiState
    , _builder :: Builder
    , _db      :: DB.ConnectionPool
    }

data GuiState
  = View { _panelId :: PanelId, _panelSt :: PanelState }
  | Edit { _panelId :: PanelId, _panelSt :: PanelState }
  deriving Show

data PanelState
  = NoSel
  | Sel     { _item :: TreeIter }
  | EditNew { _data :: [String], _isValid :: Bool }
  | EditOld { _item :: TreeIter, _isValid :: Bool }
  deriving Show

makeLenses ''AppState
makeLenses ''GuiState
makeLenses ''PanelState

type App = ReaderT (IORef AppState) IO

askAppSt :: App AppState
askAppSt = do
  appStRef <- ask
  liftIO $ readIORef appStRef

main :: IO ()
main = do
  _ <- initGUI
  db_ <- DB.createMongoDBPool dbName hostName port Nothing poolSize stripeSize time
  builder_ <- builderNew
  builderAddFromFile builder_ gladeFile
  let initialAppState = AppState (View "billingConcepts" NoSel) builder_ db_
  appStRef <- newIORef initialAppState
  mainWd <- runReaderT mkGui appStRef
  widgetShowAll mainWd
  mainGUI
    where
      gladeFile  = "glade/SepaDebits.glade"
      dbName     = "test" :: DB.Database
      hostName   = "localhost" :: DB.HostName
      port       = N.PortNumber 27017 :: N.PortID -- Standard MongoDB port
      poolSize   = 10                             -- Num. of stripes
      stripeSize = 10                             -- Num. of connections per stripe
      time       = 5 :: C.NominalDiffTime         -- Seconds

mkGui :: App Window
mkGui = do
  st <- askAppSt
  let bldr = st ^. builder

  -- Main window widgets

  let panelIds          = (fst . unzip)               panels
  let panelChooserNames = (fst . unzip . snd . unzip) panels
  let panelVBoxes       = (snd . unzip . snd . unzip) panels
  mainWd <- liftIO $ builderGetObject bldr castToWindow "mainWd"
  mwExitBt <- liftIO $ builderGetObject bldr castToButton "mwExitBt"
  _ <- liftIO $ on mwExitBt buttonActivated $ widgetDestroy mainWd >> mainQuit
  _ <- liftIO $ on mainWd objectDestroy mainQuit -- FIXME: don't exit if dirty state
  panelChoosers <- forM panelChooserNames $ \name -> do
    widget <- liftIO $ builderGetObject bldr castToToggleButton name
    liftIO $ widgetSetName widget name
    return widget
  panelBoxes <- mapM (liftIO . builderGetObject bldr castToVBox) panelVBoxes
  mainVb <- liftIO $ builderGetObject bldr castToVBox "mainVb"
  forM_ panelChoosers $ \chooser -> liftIO $ on chooser toggled $ do
    isActive <- liftIO $ toggleButtonGetActive chooser
    when isActive $ do
      chooserName <- liftIO $ widgetGetName chooser
      liftIO $ putStrLn "toggled"
      (oldBox : _) <- liftIO $ containerGetChildren mainVb -- FIXME: unsafe pattern
      let otherChoosers = filter (/= chooser) panelChoosers
      let mbNewBox = lookup chooserName $ zip panelChooserNames panelBoxes
      let (Just newBox) = mbNewBox
      liftIO $ set chooser [widgetSensitive := False]
      forM_ otherChoosers $ \o -> liftIO $ set o [ toggleButtonActive := False
                                                 , widgetSensitive := True ]
      liftIO $ containerRemove mainVb oldBox
      liftIO $ boxPackStart mainVb newBox PackGrow 0

  -- Sub-panel widgets

  mkBillingConceptsGui
  return mainWd

mkBillingConceptsGui :: App ()
mkBillingConceptsGui = do
  st <- askAppSt

  -- Populate tree view

  billingConceptsTv <- liftIO $ builderGetObject (st ^. builder) castToTreeView "billingConceptsTv"
  billingConceptsEL <- flip DB.runMongoDBPoolDef (st ^. db) $
                       DB.selectList ([] :: [DB.Filter BillingConcept]) []
  -- Setting tree view models from glade doesn't work: stablish connection here and we can
  -- use treeViewGet* functions afterwards.
  -- billingConceptsLs <- treeViewGetListStore billingConceptsTv
  -- mapM_ (listStoreAppend billingConceptsLs) billingConceptsEL
  billingConceptsLs <- liftIO $ listStoreNew billingConceptsEL
  billingConceptsSm <- liftIO $ treeModelSortNewWithModel billingConceptsLs
  liftIO $ treeViewSetModel billingConceptsTv billingConceptsSm

  let renderFuncs = [ T.unpack      . (^. longName)   . DB.entityVal
                    , T.unpack      . (^. shortName)  . DB.entityVal
                    , priceToString . (^. basePrice)  . DB.entityVal
                    , priceToString . (^. vatRatio)   . DB.entityVal
                    , priceToString . (^. finalPrice) . DB.entityVal
                    ]

  -- forall columns: set renderer, set sorting func

  billingConceptsCols <- liftIO $ treeViewGetColumns billingConceptsTv
  forM_ (zip3 billingConceptsCols renderFuncs [0..]) $ \(col, renderFunc, colId) -> do
    -- FIXME: column manual resizing doesn't work
    let cellLayout = toCellLayout col
    (cell : _) <- liftIO $ cellLayoutGetCells cellLayout    -- FIXME: unsafe pattern, dep. on glade
    let textRenderer = castToCellRendererText cell          -- FIXME: unsafe cast, depends on glade
    liftIO $ cellLayoutSetAttributes col textRenderer billingConceptsLs $ \row ->
      [ cellText := renderFunc row ]
    let sortFunc xIter yIter = do
          xRow <- liftIO $ customStoreGetRow billingConceptsLs xIter
          yRow <- liftIO $ customStoreGetRow billingConceptsLs yIter
          return $ compare (renderFunc xRow) (renderFunc yRow)
    liftIO $ treeSortableSetSortFunc billingConceptsSm colId sortFunc
    liftIO $ treeViewColumnSetSortColumnId col colId

  -- Incremental search in tree view

  -- TODO: accent-independent search or TreeModelFilter
  let equalFunc text_ iter = do
        childIter <- liftIO $ treeModelSortConvertIterToChildIter billingConceptsSm iter
        row <- liftIO $ customStoreGetRow billingConceptsLs childIter
        return $ any (\f -> text_ `isInfixOf` f row) renderFuncs
  liftIO $ treeViewSetSearchEqualFunc billingConceptsTv (Just equalFunc)
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
