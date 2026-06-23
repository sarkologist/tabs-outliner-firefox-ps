-- | The user-command half of the reducer: pure `Model -> {model, patch,
-- | actions}`. `actions` are the browser-side effects a command implies (focus,
-- | create, remove, restore) — produced purely here and interpreted by the
-- | background, so the reducer stays testable. Also defines the tiny request
-- | protocol (GetSnapshot | RunCommand) the channel carries.
-- |
-- | M3 implements collapse/expand + rename; M4 grows the ADT with focus,
-- | restore, close, delete, move, flatten, new-group.
module Model.Command where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Model.Tree (applyPatch)
import Model.Types (Model, Node, NodeId, Patch, emptyPatch)

data Command
  = Collapse NodeId Boolean
  | Rename NodeId String

-- | Browser-side effects a command implies (interpreted by the background).
data BrowserAction
  = FocusTab Int Int
  | CreateTab (Maybe Int) (Maybe String)
  | RemoveTab Int
  | RestoreSession String

type CmdResult = { model :: Model, patch :: Patch, actions :: Array BrowserAction }

applyCommand :: Number -> Command -> Model -> CmdResult
applyCommand _ cmd model = case cmd of
  Collapse nid value -> withNode nid \n -> pureResult (n { collapsed = value })
  Rename nid title -> withNode nid \n -> pureResult (n { customTitle = Just title })
  where
  withNode nid f = case Map.lookup nid model.nodes of
    Just n -> f n
    Nothing -> { model, patch: emptyPatch, actions: [] }
  pureResult :: Node -> CmdResult
  pureResult n =
    let patch = { upserts: [ n ], removes: [], roots: Nothing }
    in { model: applyPatch patch model, patch, actions: [] }

-- Request protocol -----------------------------------------------------------

data Request = GetSnapshot | RunCommand Command

encodeRequest :: Request -> Json
encodeRequest GetSnapshot = encodeJson { tag: "getSnapshot" }
encodeRequest (RunCommand c) = encodeJson { tag: "command", body: encodeCommand c }

decodeRequest :: Json -> Either String Request
decodeRequest json = do
  { tag } <- dec json :: Either String { tag :: String }
  case tag of
    "getSnapshot" -> Right GetSnapshot
    "command" -> do
      { body } <- dec json :: Either String { body :: Json }
      RunCommand <$> decodeCommand body
    other -> Left ("unknown request: " <> other)

encodeCommand :: Command -> Json
encodeCommand (Collapse nid value) = encodeJson { tag: "collapse", id: nid, value }
encodeCommand (Rename nid title) = encodeJson { tag: "rename", id: nid, title }

decodeCommand :: Json -> Either String Command
decodeCommand json = do
  { tag } <- dec json :: Either String { tag :: String }
  case tag of
    "collapse" -> (\r -> Collapse r.id r.value) <$> (dec json :: Either String { id :: NodeId, value :: Boolean })
    "rename" -> (\r -> Rename r.id r.title) <$> (dec json :: Either String { id :: NodeId, title :: String })
    other -> Left ("unknown command: " <> other)

dec :: forall a. DecodeJson a => Json -> Either String a
dec = lmap printJsonDecodeError <<< decodeJson
