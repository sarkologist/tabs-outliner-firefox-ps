module Test.Model.SearchSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Model.Search (matchesSearch, normalizeSearchQuery, segmentSearchText)
import Model.Types (Kind(..), Node, defaultNode)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

tab :: String -> Maybe String -> Maybe String -> Node
tab title customTitle url =
  (defaultNode "t" KTab 0.0) { title = title, customTitle = customTitle, url = url }

spec :: Spec Unit
spec = describe "Model.Search" do
  it "normalizes by trimming and lowercasing" do
    normalizeSearchQuery "  DoCs  " `shouldEqual` "docs"

  it "matches display titles and URLs case-insensitively" do
    matchesSearch "  docs  " (tab "Project Docs" Nothing Nothing) `shouldEqual` true
    matchesSearch "spec" (tab "Docs" Nothing (Just "https://example.test/SPEC")) `shouldEqual` true
    matchesSearch "custom" (tab "Title" (Just "Custom Name") Nothing) `shouldEqual` true
    matchesSearch "missing" (tab "Title" Nothing (Just "https://example.test")) `shouldEqual` false

  it "does not treat an empty normalized query as a match" do
    matchesSearch "   " (tab "Title" Nothing (Just "https://example.test")) `shouldEqual` false

  it "segments repeated title matches without losing original text" do
    segmentSearchText "Docs docs DOCS" "docs" `shouldEqual`
      [ { text: "Docs", isMatch: true }
      , { text: " ", isMatch: false }
      , { text: "docs", isMatch: true }
      , { text: " ", isMatch: false }
      , { text: "DOCS", isMatch: true }
      ]

  it "keeps html-like title text as plain text segments" do
    segmentSearchText "One <b>Needle</b>" "needle" `shouldEqual`
      [ { text: "One <b>", isMatch: false }
      , { text: "Needle", isMatch: true }
      , { text: "</b>", isMatch: false }
      ]

  it "returns plain text when the title misses or the query is empty" do
    segmentSearchText "Project Docs" "spec" `shouldEqual` [ { text: "Project Docs", isMatch: false } ]
    segmentSearchText "Project Docs" "   " `shouldEqual` [ { text: "Project Docs", isMatch: false } ]
