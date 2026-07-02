module Test.Model.PortableImportSpec where

import Prelude

import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (hush)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Model.PortableImport (portableToSnapshot)
import Model.Types (Kind(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

portable :: String
portable =
  """{"schema":"tabs-outliner-tree","version":1,"roots":[
       {"kind":"window","title":"G","children":[
         {"kind":"tab","title":"T","url":"http://t","children":[]}]}]}"""

spec :: Spec Unit
spec = describe "Model.PortableImport" do
  it "converts the original's nested portable tree into a flat snapshot" do
    case hush (jsonParser portable) >>= portableToSnapshot of
      Nothing -> fail "expected a snapshot"
      Just snap -> do
        snap.roots `shouldEqual` [ "p0" ]
        Array.length snap.nodes `shouldEqual` 2
        let byId = Map.fromFoldable (map (\n -> Tuple n.id n) snap.nodes)
        -- a "window"/group becomes a folder; its tab child becomes a tab node
        (_.kind <$> Map.lookup "p0" byId) `shouldEqual` Just KGroup
        (_.children <$> Map.lookup "p0" byId) `shouldEqual` Just [ "p1" ]
        (_.kind <$> Map.lookup "p1" byId) `shouldEqual` Just KTab
        (_.url <$> Map.lookup "p1" byId) `shouldEqual` Just (Just "http://t")
        (_.parent <$> Map.lookup "p1" byId) `shouldEqual` Just (Just "p0")

  it "rejects a file that isn't a portable tree" do
    (hush (jsonParser """{"foo":1}""") >>= portableToSnapshot) `shouldEqual` Nothing

  it "converts Chrome Tabs Outliner record-array exports" do
    case hush (jsonParser chromeExport) >>= portableToSnapshot of
      Nothing -> fail "expected a snapshot"
      Just snap -> do
        snap.roots `shouldEqual` [ "p0" ]
        Array.length snap.nodes `shouldEqual` 5
        let byId = Map.fromFoldable (map (\n -> Tuple n.id n) snap.nodes)
        (_.customTitle <$> Map.lookup "p0" byId) `shouldEqual` Just (Just "Chrome Tab Outliner import")
        (_.children <$> Map.lookup "p0" byId) `shouldEqual` Just [ "p1" ]
        (_.customTitle <$> Map.lookup "p1" byId) `shouldEqual` Just (Just "Research")
        (_.children <$> Map.lookup "p1" byId) `shouldEqual` Just [ "p2", "p4" ]
        (_.title <$> Map.lookup "p2" byId) `shouldEqual` Just "Parent"
        (_.url <$> Map.lookup "p2" byId) `shouldEqual` Just (Just "https://chrome-import.example/parent")
        (_.favIconUrl <$> Map.lookup "p2" byId) `shouldEqual` Just (Just "https://chrome-import.example/favicon.ico")
        (_.children <$> Map.lookup "p2" byId) `shouldEqual` Just [ "p3" ]
        (_.title <$> Map.lookup "p3" byId) `shouldEqual` Just "Child"
        (_.customTitle <$> Map.lookup "p4" byId) `shouldEqual` Just (Just "Reading")

  it "skips Chrome Tabs Outliner extension pages and promotes useful descendants" do
    case hush (jsonParser chromeSelfPageExport) >>= portableToSnapshot of
      Nothing -> fail "expected a snapshot"
      Just snap -> do
        let byId = Map.fromFoldable (map (\n -> Tuple n.id n) snap.nodes)
        Array.length snap.nodes `shouldEqual` 3
        (_.children <$> Map.lookup "p0" byId) `shouldEqual` Just [ "p1" ]
        (_.children <$> Map.lookup "p1" byId) `shouldEqual` Just [ "p2" ]
        (_.title <$> Map.lookup "p2" byId) `shouldEqual` Just "Promoted Child"
        (_.url <$> Map.lookup "p2" byId) `shouldEqual` Just (Just "https://chrome-import.example/promoted")

chromeExport :: String
chromeExport =
  """[
    {"type":2000,"node":{"type":"session","data":{"treeId":"1483340179831.8303"}}},
    [2001,{"type":"savedwin","marks":{"customTitle":"Research"},"data":{"type":"normal"}},[0]],
    [2001,{"data":{"title":"Parent","url":"https://chrome-import.example/parent","favIconUrl":"https://chrome-import.example/favicon.ico"}},[0,0]],
    [2001,{"type":"tab","data":{"title":"Child","url":"https://chrome-import.example/child"}},[0,0,0]],
    [2001,{"type":"group","marks":{"customTitle":"Reading"},"data":{}},[0,1]]
  ]"""

chromeSelfPageExport :: String
chromeSelfPageExport =
  """[
    [2001,{"type":"win","data":{"type":"popup"}},[0]],
    [2001,{"type":"tab","data":{"title":"Tabs Outliner","url":"chrome-extension://eggkanocgddhmamlbiijnphhppkpkmkl/activesessionview.html"}},[0,0]],
    [2001,{"data":{"title":"Promoted Child","url":"https://chrome-import.example/promoted"}},[0,0,0]],
    [2001,{"type":"tab","data":{"title":"Tabs Outliner Options","url":"chrome-extension://eggkanocgddhmamlbiijnphhppkpkmkl/options.html"}},[1]]
  ]"""
