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
