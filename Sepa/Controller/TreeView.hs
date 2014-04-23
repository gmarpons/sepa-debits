{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- | Implementation for of some functions in Sepa.Controller.Class for Controllers that
-- use a TreeView as a selector.
module Sepa.Controller.TreeView where

import           Control.Monad
import           Data.List
import           Graphics.UI.Gtk
import           Sepa.Controller.Class

selectTreeViewElement ::  (TreeModelSortClass sm) => TreeIter -> TreeView -> sm -> IO ()
selectTreeViewElement iter treeView sortedModel = do
  sortedIter <- treeModelSortConvertChildIterToIter sortedModel iter
  selection  <- treeViewGetSelection treeView
  treeSelectionSelectIter selection sortedIter

setTreeViewRenderers :: (TreeViewClass treeview, TreeModelClass (model row),
                         TypedTreeModelClass model) =>
                         treeview -> model row -> [row -> String] -> IO ()
setTreeViewRenderers treeView listStore renderFuncs = do
  -- forall columns: set renderer, set sorting func
  columns <- treeViewGetColumns treeView
  forM_ (zip columns renderFuncs) $ \(col, renderFunc) -> do
    -- FIXME: column manual resizing doesn't work
    let cellLayout = toCellLayout col
    (cell : _) <- cellLayoutGetCells cellLayout    -- FIXME: unsafe pattern, dep. on glade
    let textRenderer = castToCellRendererText cell -- FIXME: unsafe cast, depends on glade
    cellLayoutSetAttributes col textRenderer listStore $ \row -> [ cellText := renderFunc row ]

setTreeViewSorting :: (TreeViewClass treeview, TreeSortableClass sortable,
                       TypedTreeModelClass model) =>
                      treeview
                   -> model row
                   -> Maybe (TypedTreeModelFilter row)
                   -> sortable
                   -> [t -> t -> Ordering]
                   -> [row -> t]
                   -> IO ()
setTreeViewSorting treeView listStore mFilterModel sortedModel orderings renderFuncs = do
  columns <- treeViewGetColumns treeView
  forM_ (zip4 columns renderFuncs orderings [0..]) $ \(col, renderFunc, ordering, colId) -> do
    let sortFunc xIter yIter = do
          (xIter', yIter') <- case mFilterModel of
            Just filterModel -> do
              childXIter <- treeModelFilterConvertIterToChildIter filterModel xIter
              childYIter <- treeModelFilterConvertIterToChildIter filterModel yIter
              return (childXIter, childYIter)
            Nothing          -> return (xIter, yIter)
          xRow <- customStoreGetRow listStore xIter'
          yRow <- customStoreGetRow listStore yIter'
          return $ ordering (renderFunc xRow) (renderFunc yRow)
    treeSortableSetSortFunc sortedModel colId sortFunc
    treeViewColumnSetSortColumnId col colId

-- | Sets incremental search in tree view.
setTreeViewSearching :: (TreeViewClass treeview, TreeModelSortClass sm,
                         TypedTreeModelClass model) =>
                        treeview
                     -> model row
                     -> sm
                     -> (String -> [String] -> Bool)
                     -> [row -> String]
                     -> IO ()
setTreeViewSearching treeView listStore sortedModel isPartOf renderFuncs = do
  -- TODO: accent- and caps-independent search or TreeModelFilter
  let rowEqualFunc :: (String -> TreeIter -> IO Bool)
      rowEqualFunc txt iter = do
        childIter <- treeModelSortConvertIterToChildIter sortedModel iter
        row <- treeModelGetRow listStore childIter
        return $ txt `isPartOf` map ($ row) renderFuncs
  treeViewSetSearchEqualFunc treeView (Just rowEqualFunc)

connectTreeView :: (TreeViewClass treeview, TreeModelSortClass sm) =>
                   treeview
                -> sm
                -> (PanelState c -> IO ())
                -> IO (IO ())
connectTreeView treeView sortedModel setState = do
  selection <- treeViewGetSelection treeView
  let toChildIter = treeModelSortConvertIterToChildIter sortedModel
  let onSelectionChangedAction = do
        count <- treeSelectionCountSelectedRows selection
        if count == 0
          then setState NoSel
          else treeSelectionSelectedForeach selection $ \iter -> do
            childIter <- toChildIter iter
            setState (Sel childIter)

  _ <- on selection treeSelectionSelectionChanged onSelectionChangedAction

  return onSelectionChangedAction
