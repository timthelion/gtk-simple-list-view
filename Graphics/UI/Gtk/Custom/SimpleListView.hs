{-GPLV3.0 or later copyright Timothy Hobbs <timothyhobbs@seznam.cz>

Copyright 2012.

This program is free software:
you can redistribute it and/or modify it
under the terms of the GNU General Public License
as published by the Free Software Foundation,
either version 3 of the License,
or
(at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY;
without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy
of the GNU General Public License along with this program.
If not,
see <http://www.gnu.org/licenses/>.
-}

-- Some code from http://www.haskell.org/haskellwiki/Gtk2Hs/Tutorials/TreeView

--module Main where

module Graphics.UI.Gtk.Custom.SimpleListView where

import Graphics.UI.Gtk as GTK
import Control.Concurrent.MVar as MV

{-main :: IO ()
main = do
   initGUI       -- is start
   window <- windowNew
   (treeview,UpdateTreeView updateList) <- simpleListView "Subjects" ["Fred","Bob","Mary"] putStr
   containerAdd window treeview
   onDestroy window mainQuit
   widgetShowAll window
   updateList' <- updateList (map show [1..10]) putStrLn
   mainGUI
   return ()-}

newtype UpdateTreeView =
 UpdateTreeView
  ([String] -> (String -> IO()) ->
   IO UpdateTreeView)

simpleListView ::
 String ->
 [String] ->
 (String -> IO()) ->
 IO (GTK.Widget,UpdateTreeView)
simpleListView title intitialListItems onSelect = do
 vb <- GTK.vBoxNew False 0
 let
  createListView listItems onSelect' =
   do
    list <- listStoreNew listItems
    listMVar <- MV.newMVar list
    treeview <- GTK.treeViewNewWithModel list
    GTK.boxPackEnd vb treeview GTK.PackGrow 0
    GTK.treeViewSetHeadersVisible treeview True
    -- there should be a simpler way to render a list as the following!
    col <- GTK.treeViewColumnNew
    GTK.treeViewColumnSetTitle col title
    renderer <- GTK.cellRendererTextNew
    GTK.cellLayoutPackStart col renderer False
    GTK.cellLayoutSetAttributes col renderer list
            $ \ind -> [GTK.cellText := ind]
    GTK.treeViewAppendColumn treeview col

    tree <- GTK.treeViewGetSelection treeview
    GTK.treeSelectionSetMode tree  SelectionSingle
    GTK.onSelectionChanged tree (oneSelection listMVar tree onSelect')
    let
     {-updateList1 newListItems =
      do
       putStrLn "1"
       newList <- listStoreNew newListItems
       putStrLn "2"
       GTK.cellLayoutSetAttributes col renderer newList
         $ \ind -> [GTK.cellText := ind]
       putStrLn "3"
       GTK.treeViewSetModel treeview newList
       --SimpleListView: Prelude.head: empty list
       putStrLn "4"
       return $ UpdateTreeView updateList1
     updateList2 newListItems =
      do
       size <- GTK.listStoreGetSize list
       mapM_ (GTK.listStoreRemove list) [0..size-1]
       -- SimpleListView: Prelude.head: empty list
       mapM_ (GTK.listStoreAppend list) newListItems
       return $ UpdateTreeView updateList2
     updateList3 newListItems =
      do
       GTK.listStoreClear list
       mapM_ (GTK.listStoreAppend list) newListItems
       -- SimpleListView: Prelude.head: empty list
       return $ UpdateTreeView updateList3
     updateList4 newListItems =
      do
       GTK.listStoreRemove list 0
       -- SimpleListView: Prelude.head: empty list
       return $ UpdateTreeView updateList4 -}
     updateList5 newListItems onSelect'' = --Works!
      do
       GTK.widgetDestroy treeview
       (_,update) <- createListView newListItems onSelect''
       widgetShowAll vb
       return update
    return $ (treeview,UpdateTreeView updateList5)
 (_,update) <- createListView intitialListItems onSelect
 return (GTK.castToWidget vb,update)


oneSelection :: MV.MVar (GTK.ListStore String) -> GTK.TreeSelection -> (String -> IO ()) ->  IO ()
oneSelection listMVar tree onSelect = do
   list <- MV.takeMVar listMVar
   sel <- GTK.treeSelectionGetSelectedRows tree
   let s = head  (head sel)
   v <- GTK.listStoreGetValue list s
   onSelect v
   MV.putMVar listMVar list
