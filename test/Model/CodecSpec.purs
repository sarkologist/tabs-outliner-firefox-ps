module Test.Model.CodecSpec where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as String
import Model.Codec (decodeNode, encodeNode)
import Model.Types (Kind(..), Node, defaultNode, isLive)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- A record shaped like a *legacy* persisted node: it still carries the removed
-- `status` field, and uses kind "window" for browser windows. Encoding it yields
-- the JSON an older build would have written to IndexedDB.
legacyJson
  :: { kind :: String, status :: String, tabId :: Maybe Int, windowId :: Maybe Int, url :: Maybe String }
  -> Json
legacyJson o = encodeJson
  { id: "n1"
  , kind: o.kind
  , status: o.status
  , parent: (Nothing :: Maybe String)
  , children: ([] :: Array String)
  , title: "T"
  , customTitle: (Nothing :: Maybe String)
  , url: o.url
  , favIconUrl: (Nothing :: Maybe String)
  , active: false
  , collapsed: false
  , createdAt: 0.0
  , closedAt: (Nothing :: Maybe Number)
  , tabId: o.tabId
  , windowId: o.windowId
  , sessionId: (Nothing :: Maybe String)
  }

-- the decoded facts we care about for migration: kind, both bindings, liveness
project :: Node -> { kind :: Kind, tabId :: Maybe Int, windowId :: Maybe Int, live :: Boolean }
project n = { kind: n.kind, tabId: n.tabId, windowId: n.windowId, live: isLive n }

spec :: Spec Unit
spec = describe "Model.Codec" do
  describe "decoding tolerates legacy records (old \"window\" kind, dropped status field)" do
    it "a legacy live window decodes to a live container (kind window -> group)" do
      (project <$> decodeNode (legacyJson { kind: "window", status: "live", tabId: Nothing, windowId: Just 7, url: Nothing }))
        `shouldEqual` Right { kind: KGroup, tabId: Nothing, windowId: Just 7, live: true }

    it "a legacy saved window decodes to a plain (non-live) container" do
      (project <$> decodeNode (legacyJson { kind: "window", status: "closed", tabId: Nothing, windowId: Nothing, url: Nothing }))
        `shouldEqual` Right { kind: KGroup, tabId: Nothing, windowId: Nothing, live: false }

    it "a legacy live tab decodes to a live tab" do
      (project <$> decodeNode (legacyJson { kind: "tab", status: "live", tabId: Just 5, windowId: Nothing, url: Just "http://a" }))
        `shouldEqual` Right { kind: KTab, tabId: Just 5, windowId: Nothing, live: true }

    it "a legacy closed tab decodes to restorable (non-live) history" do
      (project <$> decodeNode (legacyJson { kind: "tab", status: "closed", tabId: Nothing, windowId: Nothing, url: Just "http://a" }))
        `shouldEqual` Right { kind: KTab, tabId: Nothing, windowId: Nothing, live: false }

  describe "current format" do
    it "encoding no longer emits a status field" do
      let json = stringify (encodeNode ((defaultNode "n1" KGroup 0.0) { windowId = Just 3 }))
      String.contains (String.Pattern "\"status\"") json `shouldEqual` false
      String.contains (String.Pattern "\"kind\":\"group\"") json `shouldEqual` true

    it "round-trips a node through encode/decode" do
      let n = (defaultNode "n5" KTab 0.0) { title = "X", url = Just "http://x", tabId = Just 9 }
      decodeNode (encodeNode n) `shouldEqual` Right n

    it "round-trips the restoredFromClosed flag (persisted so a suspend can't lose it)" do
      let n = (defaultNode "n6" KTab 0.0) { url = Just "http://y", tabId = Just 7, restoredFromClosed = true }
      (_.restoredFromClosed <$> decodeNode (encodeNode n)) `shouldEqual` Right true
