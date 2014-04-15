{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Sepa.Controller.Debtor where

import           Control.Lens             hiding (element, elements, index, set, view)
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.List
import qualified Data.Text                as T (Text, pack, unpack)
import qualified Data.Time.Calendar       as C
import qualified Data.Time.LocalTime      as C
import qualified Database.Persist.MongoDB as DB
import           Graphics.UI.Gtk
import           Sepa.Controller.Class
import           Sepa.Debtor

data DebtorsController = DE PanelId Builder

instance Controller DebtorsController where

  type E DebtorsController = Debtor

  type S DebtorsController = TreeView

  data D DebtorsController =
    DDE { lastNameD  :: T.Text
        , firstNameD :: T.Text }

  builder  (DE _        builder_) = builder_

  panelId  (DE panelId_ _       ) = panelId_

  selector = getGladeObject castToTreeView "_Tv"

  setSelectorModel s m _ = liftIO $ treeViewSetModel s m

  setSelectorRenderers = setTreeViewRenderers

  setSelectorSorting   s ls sm = setTreeViewSorting   s ls sm orderings
    where orderings = repeat compare -- TODO: catalan collation

  setSelectorSearching s ls sm = setTreeViewSearching s ls sm isPartOf
    where tx `isPartOf` txs = any (tx `isInfixOf`) txs -- TODO: better searching

  renderers _ = return [ T.unpack        . (^. lastName)          . DB.entityVal
                       , T.unpack        . (^. firstName)         . DB.entityVal
                       , C.showGregorian . (^. registrationDate)  . DB.entityVal
                       ]

  editEntries c = do e1 <- getGladeObject castToEntry "_EE_lastNameEn"  c
                     e2 <- getGladeObject castToEntry "_EE_firstNameEn" c
                     return [e1, e2]

  subElemWidgets c = do
    s1  <- getGladeObject castToWidget "_iban1En" c
    s2  <- getGladeObject castToWidget "_iban2En" c
    s3  <- getGladeObject castToWidget "_iban3En" c
    s4  <- getGladeObject castToWidget "_iban4En" c
    s5  <- getGladeObject castToWidget "_iban5En" c
    s6  <- getGladeObject castToWidget "_iban6En" c
    s7  <- getGladeObject castToWidget "_mandateNewTb" c
    s8  <- getGladeObject castToWidget "_mandateRefEn" c
    s9  <- getGladeObject castToWidget "_mandateSignatureEn" c
    s10 <- getGladeObject castToWidget "_mandateLastTimeActiveEn" c
    s11 <- getGladeObject castToWidget "_saveMandateBt" c
    s12 <- getGladeObject castToWidget "_cancelMandateBt" c
    s13 <- getGladeObject castToWidget "_mandateFormBt" c
    s14 <- getGladeObject castToWidget "_mandatesTv" c
    return [s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14]

  readData [lastNameEn, firstNameEn] _ = do
    lastName_    <- get lastNameEn    entryText
    firstName_   <- get firstNameEn   entryText
    return DDE { lastNameD   = T.pack lastName_
               , firstNameD  = T.pack firstName_
               }

  readData _ _ = error "readData (DE): wrong number of entries"

  validData d _c = return $ validDebtorName (firstNameD d) (lastNameD d)

  createFromData d _ = do
    zonedTime <- C.getZonedTime
    let today  = C.localDay (C.zonedTimeToLocalTime zonedTime)
    return $ mkDebtor (firstNameD d) (lastNameD d) [] today

  updateFromData d old _ =
    return $ old & firstName .~ firstNameD d & lastName .~ lastNameD d

  selectElement = selectTreeViewElement

  putSubElement = putMandate

  mkSubElemController = mkMandateController

  connectSelector = connectTreeView

putMandate :: TreeIter -> LS DebtorsController -> DebtorsController -> IO ()
putMandate iter ls c = do
  iban1En   <- builderGetObject (builder c) castToEntry "DE_iban1En"
  iban2En   <- builderGetObject (builder c) castToEntry "DE_iban2En"
  iban3En   <- builderGetObject (builder c) castToEntry "DE_iban3En"
  iban4En   <- builderGetObject (builder c) castToEntry "DE_iban4En"
  iban5En   <- builderGetObject (builder c) castToEntry "DE_iban5En"
  iban6En   <- builderGetObject (builder c) castToEntry "DE_iban6En"
  newTb_    <- builderGetObject (builder c) castToToggleButton "DE_mandateNewTb"
  saveBt_   <- builderGetObject (builder c) castToButton "DE_saveMandateBt"
  cancelBt_ <- builderGetObject (builder c) castToButton "DE_cancelMandateBt"
  refEn            <- builderGetObject (builder c) castToEntry "DE_mandateRefEn"
  signatureEn      <- builderGetObject (builder c) castToEntry "DE_mandateSignatureEn"
  lastTimeActiveEn <- builderGetObject (builder c) castToEntry "DE_mandateLastTimeActiveEn"
  mapM_ (`set` [entryText := ""])
    [iban1En, iban2En, iban3En, iban4En, iban5En, iban6En, refEn, signatureEn, lastTimeActiveEn]
  set newTb_ [widgetSensitive := True]
  set saveBt_ [widgetSensitive := False]
  set cancelBt_ [widgetSensitive := False]
  (DB.Entity _key debtor)  <- treeModelGetRow ls iter
  print $ length (debtor ^. mandates)
  zonedTime <- C.getZonedTime
  let today  = C.localDay (C.zonedTimeToLocalTime zonedTime)
  case getActiveMandate today debtor of
    Nothing -> putStrLn "Rien"
    Just mandate -> do
      set refEn          [entryText := T.unpack (mandate ^. mandateRef)]
      set signatureEn    [entryText := C.showGregorian (mandate ^. signatureDate)]
      set lastTimeActiveEn [entryText := maybe "" C.showGregorian (mandate ^. lastTimeActive)]
      let (iban1, iban'    ) = splitAt 4 (T.unpack (mandate ^. iban))
          (iban2, iban''   ) = splitAt 4 iban'
          (iban3, iban'''  ) = splitAt 4 iban''
          (iban4, iban'''' ) = splitAt 4 iban'''
          (iban5, iban6    ) = splitAt 4 iban''''
      set iban1En [entryText := iban1]
      set iban2En [entryText := iban2]
      set iban3En [entryText := iban3]
      set iban4En [entryText := iban4]
      set iban5En [entryText := iban5]
      set iban6En [entryText := iban6]

mkMandateController  :: (TreeModelSortClass sm) =>
                        LS DebtorsController
                     -> sm
                     -> DB.ConnectionPool
                     -> IORef (PanelState c)
                     -> DebtorsController
                     -> IO ()
mkMandateController ls _sm db stRef c = do
  iban1En   <- builderGetObject (builder c) castToEntry "DE_iban1En"
  iban2En   <- builderGetObject (builder c) castToEntry "DE_iban2En"
  iban3En   <- builderGetObject (builder c) castToEntry "DE_iban3En"
  iban4En   <- builderGetObject (builder c) castToEntry "DE_iban4En"
  iban5En   <- builderGetObject (builder c) castToEntry "DE_iban5En"
  iban6En   <- builderGetObject (builder c) castToEntry "DE_iban6En"
  newTb_   <- builderGetObject (builder c) castToToggleButton "DE_mandateNewTb"
  saveBt_   <- builderGetObject (builder c) castToButton "DE_saveMandateBt"
  cancelBt_   <- builderGetObject (builder c) castToButton "DE_cancelMandateBt"
  refEn            <- builderGetObject (builder c) castToEntry "DE_mandateRefEn"
  signatureEn      <- builderGetObject (builder c) castToEntry "DE_mandateSignatureEn"
  lastTimeActiveEn <- builderGetObject (builder c) castToEntry "DE_mandateLastTimeActiveEn"
  zonedTime <- C.getZonedTime
  let today  = C.localDay (C.zonedTimeToLocalTime zonedTime)

  _ <- on newTb_ toggled $ do
    mapM_ (`set` [entryText := ""])
      [iban1En, iban2En, iban3En, iban4En, iban5En, iban6En, refEn, signatureEn, lastTimeActiveEn]
    set newTb_ [widgetSensitive := False]
    set saveBt_ [widgetSensitive := True]
    set cancelBt_ [widgetSensitive := True]
--    (Sel iter) <- readIORef stRef -- WARNING: unsafe pattern

  _ <- on saveBt_ buttonActivated $ do
    (Sel iter) <- readIORef stRef -- WARNING: unsafe pattern
    (DB.Entity key oldDebtor)  <- treeModelGetRow ls iter
    iban1 <- get iban1En entryText
    iban2 <- get iban2En entryText
    iban3 <- get iban3En entryText
    iban4 <- get iban4En entryText
    iban5 <- get iban5En entryText
    iban6 <- get iban6En entryText
    ref   <- get refEn   entryText
    let mandate = mkMandate (T.pack ref) (T.pack (iban1++iban2++iban3++iban4++iban5++iban6)) today Nothing
        mandates_ = oldDebtor ^. mandates
        newDebtor = oldDebtor & mandates .~ (mandate : mandates_)
    flip DB.runMongoDBPoolDef db $ DB.replace key newDebtor
    let index = listStoreIterToIndex iter
    listStoreSetValue ls index (DB.Entity key newDebtor)

  return ()
