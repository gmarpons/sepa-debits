module Main
       ( main
       ) where

import           Control.Monad
  (when)
import           Graphics.UI.Gtk


main :: IO ()
main = do
  initGUI
  builder <- builderNew
  builderAddFromFile builder "glade/GuiaDirectDebits.glade"
  mainWd <- builderGetObject builder castToWindow "mainWd"
  mwExitBt <- builderGetObject builder castToButton "mwExitBt"
  on mwExitBt buttonActivated $ widgetDestroy mainWd >> mainQuit
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
  return ()
