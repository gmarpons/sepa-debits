{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
       ( main
       ) where

import           Control.Lens             hiding (set, view)
import           Control.Monad
import           Data.List
import qualified Data.Text                as T  (pack, replace, unpack)
import qualified Data.Text.Lazy           as TL (unpack)
import qualified Database.Persist.MongoDB as DB
import           Formatting               hiding (builder)
import           Graphics.UI.Gtk
import           Sepa.BillingConcept
import           Sepa.MongoUtils
--import qualified System.Glib.GObject         as G


main :: IO ()
main = do
  _ <- initGUI
  builder <- builderNew
  builderAddFromFile builder "glade/SepaDebits.glade"
  mainWd <- builderGetObject builder castToWindow "mainWd"
  mwExitBt <- builderGetObject builder castToButton "mwExitBt"
  _ <- on mwExitBt buttonActivated $ widgetDestroy mainWd >> mainQuit
  _ <- on mainWd objectDestroy mainQuit -- FIXME: don't exit if dirty state
  mkGui builder
  widgetShowAll mainWd
  mainGUI

type PanelId = String
type PanelDescr = (PanelId, (String, String))

panels :: [PanelDescr]
panels =
  [ ("debtors",         ("debtorsTb",         "debtorsVb"        ))
  , ("billingConcepts", ("billingConceptsTb", "billingConceptsVb"))
  , ("directDebits",    ("directDebitsTb",    "directDebitsVb"   ))
  ]

mkGui :: Builder -> IO ()
mkGui builder = do
  -- Main panel widgets
  let panelIds          = (fst . unzip)               panels
  let panelChooserNames = (fst . unzip . snd . unzip) panels
  let panelVBoxes       = (snd . unzip . snd . unzip) panels
  panelChoosers <- forM panelChooserNames $ \name -> do
    widget <- builderGetObject builder castToToggleButton name
    widgetSetName widget name
    return widget
  panelBoxes <- mapM (builderGetObject builder castToVBox) panelVBoxes
  mainVb <- builderGetObject builder castToVBox "mainVb"
  forM_ panelChoosers $ \chooser -> on chooser toggled $ do
    isActive <- toggleButtonGetActive chooser
    when isActive $ do
      chooserName <- widgetGetName chooser
      putStrLn "toggled"
      (oldBox : _) <- containerGetChildren mainVb -- FIXME: unsafe pattern
      let otherChoosers = filter (/= chooser) panelChoosers
      let mbNewBox = lookup chooserName $ zip panelChooserNames panelBoxes
      let (Just newBox) = mbNewBox
      set chooser [widgetSensitive := False]
      forM_ otherChoosers $ \o -> set o [ toggleButtonActive := False
                                        , widgetSensitive := True ]
      containerRemove mainVb oldBox
      boxPackStart mainVb newBox PackGrow 0
  -- Sub-panel widgets
  mkBillingConceptsGui builder
  return ()

mkBillingConceptsGui :: Builder -> IO ()
mkBillingConceptsGui builder = do
  billingConceptsTv <- builderGetObject builder castToTreeView "billingConceptsTv"
  editBillingConceptTb <- builderGetObject builder castToToggleButton "editBillingConceptTb"
  newBillingConceptTb  <- builderGetObject builder castToToggleButton "newBillingConceptTb"
  deleteBillingConceptBt <- builderGetObject builder castToButton "deleteBillingConceptBt"
  billingConceptDescriptionEn <- builderGetObject builder castToEntry "billingConceptDescriptionEn"
  billingConceptBasePriceEn <- builderGetObject builder castToEntry "billingConceptBasePriceEn"
  billingConceptVatRatioEn <- builderGetObject builder castToEntry "billingConceptVatRatioEn"
  billingConceptFinalPriceEn <- builderGetObject builder castToEntry "billingConceptFinalPriceEn"
  saveBillingConceptBt <- builderGetObject builder castToButton "saveBillingConceptBt"
  cancelBillingConceptBt <- builderGetObject builder castToButton "cancelBillingConceptBt"
  billingConceptsTm <- builderGetObject builder castToTreeModel "billingConceptsLs"
  -- Populate tree view
  billingConceptsEL <- runDb $ DB.selectList ([] :: [DB.Filter BillingConcept]) []
  billingConceptsLs <- listStoreNew billingConceptsEL
  billingConceptsSm <- treeModelSortNewWithModel billingConceptsLs
  -- lsSize <- listStoreGetSize billingConceptsLs
  -- putStrLn $ "Number of billing concepts: " ++ show lsSize
  treeViewSetModel billingConceptsTv billingConceptsSm
  billingConceptsCols <- treeViewGetColumns billingConceptsTv
  let renderFuncs = [ T.unpack      . (^. longName)   . DB.entityVal
                    , T.unpack      . (^. shortName)  . DB.entityVal
                    , priceToString . (^. basePrice)  . DB.entityVal
                    , priceToString . (^. vatRatio)   . DB.entityVal
                    , priceToString . (^. finalPrice) . DB.entityVal
                    ]
  -- forall columns: set renderer, set sorting func
  forM_ (zip3 billingConceptsCols renderFuncs [0..]) $ \(col, renderFunc, colId) -> do
    -- FIXME: column manual resizing doesn't work
    let cellLayout = toCellLayout col
    (cell : _) <- cellLayoutGetCells cellLayout    -- FIXME: unsafe pattern, dep. on glade
    let textRenderer = castToCellRendererText cell -- FIXME: unsafe cast, depends on glade
    cellLayoutSetAttributes col textRenderer billingConceptsLs $ \row ->
      [ cellText := renderFunc row ]
    let sortFunc xIter yIter = do
          xRow <- customStoreGetRow billingConceptsLs xIter
          yRow <- customStoreGetRow billingConceptsLs yIter
          return $ compare (renderFunc xRow) (renderFunc yRow)
    treeSortableSetSortFunc billingConceptsSm colId sortFunc
    treeViewColumnSetSortColumnId col colId
  -- Incremental search in tree view
  -- TODO: accent-independent search or TreeModelFilter
  let equalFunc text iter = do
        childIter <- treeModelSortConvertIterToChildIter billingConceptsSm iter
        row <- customStoreGetRow billingConceptsLs childIter
        return $ any (\f -> text `isInfixOf` f row) renderFuncs
  treeViewSetSearchEqualFunc billingConceptsTv (Just equalFunc)
  return ()

priceToString :: Int -> String
priceToString num = TL.unpack $ format (left padding ' ') (toText num)
  where
    toText    = T.replace "." (T.pack separator) . priceToText
    separator = ","
    padding   = 10
