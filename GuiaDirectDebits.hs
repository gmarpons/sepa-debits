module Main where

import           Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI
  builder <- builderNew
  builderAddFromFile builder "glade/GuiaDirectDebits.glade"
  mainWd <- builderGetObject builder castToWindow "mainWd"
  mainVb <- builderGetObject builder castToVBox "mainVb"
  mwExitBt <- builderGetObject builder castToButton "mwExitBt"
  on mwExitBt buttonActivated $ widgetDestroy mainWd >> mainQuit
  widgetShowAll mainWd
  mainGUI

