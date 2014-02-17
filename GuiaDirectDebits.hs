{-# LANGUAGE
  FlexibleContexts,
  OverloadedStrings,
  ScopedTypeVariables
  #-}


module Main
       ( main
       ) where

import           Control.Lens                 hiding (set, view)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.List
import           Data.Maybe
import           Data.Text
  (pack, unpack)
import qualified Database.Persist.MongoDB                                       as DB
import           Graphics.UI.Gtk
import           Guia.BillingConcept
import           Guia.MongoUtils


main :: IO ()
main = do
  initGUI
  builder <- builderNew
  builderAddFromFile builder "glade/GuiaDirectDebits.glade"
  mainWd <- builderGetObject builder castToWindow "mainWd"
  mwExitBt <- builderGetObject builder castToButton "mwExitBt"
  on mwExitBt buttonActivated $ widgetDestroy mainWd >> mainQuit
  -- onDestroy window mainQuit
  mkGui builder
  widgetShowAll mainWd
  mainGUI

type PanelId = String
type PanelDescr = (PanelId, (String, String))

panels :: [PanelDescr]
panels =
    [ ("payers",          ("payersTb",          "payersVb"         ))
    , ("billingConcepts", ("billingConceptsTb", "billingConceptsVb"))
    , ("invoicings",      ("invoicingsTb",      "invoicingsVb"     ))
    ]

mkGui :: Builder -> IO ()
mkGui builder = do
  let panelIds = (fst . unzip) panels
  let panelChooserNames = (fst . unzip . snd . unzip) panels
  panelChoosers <- mapM (\n -> do w <- builderGetObject builder castToToggleButton n
                                  widgetSetName w n
                                  return w
                        ) panelChooserNames
  panelBoxes <- mapM (builderGetObject builder castToVBox)
                $ (snd . unzip . snd . unzip) panels
  mainVb <- builderGetObject builder castToVBox "mainVb"
  mapM_ (\chs -> on chs toggled $ do
                   isActive <- toggleButtonGetActive chs
                   when isActive $ do
                     chsName <- widgetGetName chs
                     putStrLn "toggled"
                     children <- containerGetChildren mainVb
                     let otherChs = filter (/= chs) panelChoosers
                         oldBox = head children
                         mbNewBox = lookup chsName
                                    $ zip panelChooserNames panelBoxes
                         (Just newBox) = mbNewBox
                     set chs [widgetSensitive := False]
                     mapM_ (\o -> set o [ toggleButtonActive := False
                                        , widgetSensitive := True ]
                           ) otherChs
                     containerRemove mainVb oldBox
                     boxPackStart mainVb newBox PackGrow 0
        ) panelChoosers

  -- Billing concepts widgets

  billingConceptsTv <- builderGetObject builder castToTreeView "billingConceptsTv"
  editBillingConceptTb
      <- builderGetObject builder castToToggleButton "editBillingConceptTb"
  newBillingConceptTb
      <- builderGetObject builder castToToggleButton "newBillingConceptTb"
  deleteBillingConceptBt
      <- builderGetObject builder castToButton "deleteBillingConceptBt"
  billingConceptDescriptionEn
      <- builderGetObject builder castToEntry "billingConceptDescriptionEn"
  billingConceptBasePriceEn
      <- builderGetObject builder castToEntry "billingConceptBasePriceEn"
  billingConceptVatRatioEn
      <- builderGetObject builder castToEntry "billingConceptVatRatioEn"
  billingConceptFinalPriceEn
      <- builderGetObject builder castToEntry "billingConceptFinalPriceEn"
  saveBillingConceptBt <- builderGetObject builder castToButton "saveBillingConceptBt"
  cancelBillingConceptBt
      <- builderGetObject builder castToButton "cancelBillingConceptBt"
  billingConceptsSl <- treeViewGetSelection billingConceptsTv
  treeSelectionSetMode billingConceptsSl SelectionSingle
  mkTreeViewColumns billingConceptsTv
                        ["Descripció", "Preu base", "% IVA", "Preu final"]

  -- Populate tree views

  bcEL <- runDb $ DB.selectList ([] :: [DB.Filter BillingConcept]) []
  let bcL = map DB.entityVal bcEL
  putStrLn $ "Number of billing concepts: " ++ show (length bcL)
  billingConceptsLs <- mkTreeViewModel billingConceptsTv Nothing
                       [ unpack . (^. shortName)]
  mapM_ (listStoreAppend billingConceptsLs) bcL

  return ()

-- populateBillingConcepts :: (MonadIO m, MonadBaseControl IO m) => t -> m ()
-- populateBillingConcepts builder =

mkTreeViewModel :: forall a .
                   TreeView
                -> Maybe (ListStore a) -- ^ If @Nothing@, a new model is created.
                -> [a -> String]       -- ^ One function per pair (column,
                                       -- renderer), as some columns can
                                       -- have more than one renderer
                -> IO (ListStore a)
mkTreeViewModel view mModel funcs = do
  model <- maybe (listStoreNew ([] :: [a])) return mModel
  sortedModel <- treeModelSortNewWithModel model
  treeViewSetModel view sortedModel
  columns <- treeViewGetColumns view
  mapM_ (\(col, func, sortColumnId) -> do
           -- renderers <- treeViewColumnGetCellRenderers (col)
           -- mapM_ (\renderer -> do
           --          let rendText = castToCellRendererText renderer
           --          cellLayoutSetAttributes col rendText model
           --                                      (\row -> [cellText := func row])
           --       ) renderers
           treeViewColumnSetSortIndicator col True
           let sortFunc xIter yIter = do
                 xRow <- customStoreGetRow model xIter
                 yRow <- customStoreGetRow model yIter
                 return $ compare (func xRow) (func yRow)
           treeSortableSetSortFunc sortedModel sortColumnId sortFunc
           treeViewColumnSetSortColumnId col sortColumnId
        ) $ zip3 columns funcs [0..]

  -- Enable incremental search in TreeView
  let equalFunc text iter = do
        childIter <- treeModelSortConvertIterToChildIter sortedModel iter
        row <- customStoreGetRow model childIter
        let rowTexts = map ($ row) funcs
        return $ text `isInfixOf` foldl (++) "" rowTexts
  treeViewSetSearchEqualFunc view (Just equalFunc)
  return model

mkTreeViewColumns :: TreeView -> [String] -> IO ()
mkTreeViewColumns view titles = do
  mapM_ (\title -> do
           col <- treeViewColumnNew
           treeViewColumnSetTitle col title
           num <- treeViewAppendColumn view col
           treeViewColumnSetSizing col (if num == length titles
                                          then TreeViewColumnFixed
                                          else TreeViewColumnAutosize)
           rd <- cellRendererTextNew
           cellLayoutPackStart col rd (num /= length titles)
           treeViewColumnSetResizable col True
        ) titles
  treeViewColumnsAutosize view
