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
import           Data.Text                                                      as T
  (concat, pack, split, unpack)
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

  -- Populate tree views

  bcEL <- runDb $ DB.selectList ([] :: [DB.Filter BillingConcept]) []
  let bcL = map DB.entityVal bcEL
  putStrLn $ "Number of billing concepts: " ++ show (length bcL)
  billingConceptsLs <- mkTreeViewColumnsAndModel billingConceptsTv Nothing
                       [ ("Descripció (no es pot repetir)", T.unpack . (^. longName))
                       , ("Etiqueta al rebut", T.unpack . (^. shortName))
                       , ("Preu base", priceToStringSep "," . (^. basePrice))
                       , ("% IVA", priceToStringSep "," . (^. vatRatio))
                       , ("Preu final", priceToStringSep "," . (^. finalPrice))]
  mapM_ (listStoreAppend billingConceptsLs) bcL

  return ()


-- Helper functions

mkTreeViewColumnsAndModel :: TreeView
                -> Maybe (ListStore a)     -- ^ If @Nothing@, a new model is created.
                -> [(String, a -> String)] -- ^ One function per pair (column, renderer),
                                           -- as some columns can have more than one
                                           -- renderer
                -> IO (ListStore a)
mkTreeViewColumnsAndModel view mModel titlesAndFuncs = do
  -- Set tree model
  model <- maybe (listStoreNew ([] :: [a])) return mModel
  sortedModel <- treeModelSortNewWithModel model
  treeViewSetModel view sortedModel

  -- Set tree columns and cell renderers
  mapM_ (\((title, func), sortColumnId) -> do
            col <- treeViewColumnNew
            treeViewColumnSetTitle col title
            num <- treeViewAppendColumn view col
            treeViewColumnSetSizing col (if num == length titlesAndFuncs
                                           then TreeViewColumnFixed
                                           else TreeViewColumnAutosize)
            rd <- cellRendererTextNew
            cellLayoutPackStart col rd (num /= length titlesAndFuncs)
            treeViewColumnSetResizable col True

            -- Render text in cells
            let rendText = castToCellRendererText rd
            cellLayoutSetAttributes col rendText model (\row -> [cellText := func row])
            -- Sortable columns
            treeViewColumnSetSortIndicator col True
            let sortFunc xIter yIter = do
                  xRow <- customStoreGetRow model xIter
                  yRow <- customStoreGetRow model yIter
                  return $ compare (func xRow) (func yRow)
            treeSortableSetSortFunc sortedModel sortColumnId sortFunc
            treeViewColumnSetSortColumnId col sortColumnId
        ) $ zip titlesAndFuncs [0..]

  -- Enable incremental search in TreeView
  let equalFunc text iter = do
        childIter <- treeModelSortConvertIterToChildIter sortedModel iter
        row <- customStoreGetRow model childIter
        let rowTexts = map ($ row) ((snd . unzip) titlesAndFuncs)
        return $ text `isInfixOf` foldl (++) "" rowTexts
  treeViewSetSearchEqualFunc view (Just equalFunc)
  return model

priceToStringSep :: String -> Int -> String
priceToStringSep separator =
  T.unpack . T.concat . intersperse (pack separator) . T.split (=='.') . priceToText
