{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Main
       ( main
       ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import qualified Data.Text                as T (unpack)
import qualified Data.Time.Clock          as C (NominalDiffTime)
import qualified Database.Persist.MongoDB as DB
import           Graphics.UI.Gtk
import qualified Network                  as N (PortID (PortNumber))
import           Sepa.Controller.BillingConcept
import           Sepa.Controller.Class
import           Sepa.Controller.Debtor
import           Sepa.Controller.DirectDebit
import           Sepa.Controller.TreeView

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

mkMainWindowGui :: Builder -> DB.ConnectionPool -> IO Window
mkMainWindowGui builder_ db = do

  -- items TreeView
  -- TODO: move itTv and itLs creation to another place

  itTv <- builderGetObject builder_ castToTreeView "DD_itemsTv"
  itLs <- listStoreNew ([] :: [Item])
  itSm <- treeModelSortNewWithModel itLs
  let itRf = [ T.unpack      . itemLastName
             , T.unpack      . itemFirstName
             , T.unpack      . itemShortName
             , priceToString . itemActualPrice
             ]
  treeViewSetModel     itTv              itSm
  setTreeViewRenderers itTv itLs                        itRf
  setTreeViewSorting   itTv itLs Nothing itSm [compare] itRf

  -- Create panel controllers and helper lists

  let bcController = BC "BC" builder_
      deController = DE "DE" builder_
      ddController = DD "DD" builder_ itTv itLs
      controllers  = [ MkBController bcController
                     , MkBController deController
                     , MkBController ddController
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

  bcLs <- mkController  db setState bcController
  deLs <- mkController  db setState deController
  _    <- mkController' db setState ddController bcLs deLs
  return mainWd

-- | Box for heterogeneous collections of @Controller@'s.
data BController where
  MkBController :: Controller c => c -> BController

-- TODO: if I could only declare the following instance ...
-- instance Controller BController where
--   panelId  (MkBController c) = panelId c
--   builder  (MkBController c) = builder c
--   selector (MkBController c) = selector c

bPanelId      :: BController -> PanelId
bChooser      :: BController -> IO ToggleButton
bPanel        :: BController -> IO VBox
bPanelId      (MkBController c)      = panelId c
bChooser      (MkBController c)      = chooser c
bPanel        (MkBController c)      = panel   c
-- bRunMkController :: BController -> DB.ConnectionPool -> (MainWindowState -> IO ()) -> IO ()
-- bRunMkController (MkBController c) db f = mkController db f c
