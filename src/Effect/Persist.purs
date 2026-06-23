-- | Per-node IndexedDB persistence. Each node is one record (store "nodes",
-- | keyed by NodeId); the root list lives in store "meta". A patch writes only
-- | its touched records in a single transaction — O(change), never O(total).
-- | This is what makes the original's journal/backup machinery unnecessary.
module Effect.Persist
  ( Db
  , open
  , load
  , writePatch
  , modelFromLoaded
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (mapMaybe)
import Data.Bifunctor (lmap)
import Data.Either (hush)
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Model.Codec (decodeNode, encodeNode)
import Model.Types (Model, Node, NodeId, Patch, Status(..), emptyModel)

foreign import data Db :: Type

foreign import openImpl :: String -> Effect (Promise Db)

foreign import writePatchImpl
  :: Db
  -> { puts :: Array { id :: String, json :: String }, deletes :: Array String, roots :: Nullable String }
  -> Effect (Promise Unit)

foreign import loadImpl :: Db -> Effect (Promise { nodes :: Array String, roots :: Nullable String })

open :: Aff Db
open = toAffE (openImpl "tabs-outliner")

writePatch :: Db -> Patch -> Aff Unit
writePatch db p = toAffE (writePatchImpl db payload)
  where
  payload =
    { puts: map (\n -> { id: n.id, json: stringify (encodeNode n) }) p.upserts
    , deletes: p.removes
    , roots: toNullable (map (stringify <<< encodeJson) p.roots)
    }

load :: Db -> Aff { nodes :: Array Node, roots :: Array NodeId }
load db = do
  raw <- toAffE (loadImpl db)
  pure
    { nodes: mapMaybe decodeNodeString raw.nodes
    , roots: maybe [] decodeRoots (toMaybe raw.roots)
    }

decodeNodeString :: String -> Maybe Node
decodeNodeString s = hush (jsonParser s >>= decodeNode)

decodeRoots :: String -> Array NodeId
decodeRoots s = fromMaybe [] (hush (jsonParser s >>= (lmap printJsonDecodeError <<< decodeJson)))

-- | Rebuild a Model (indexes + id counter) from persisted nodes and roots.
modelFromLoaded :: Array Node -> Array NodeId -> Model
modelFromLoaded nodes roots =
  let
    base = emptyModel
      { roots = roots
      , nodes = Map.fromFoldable (map (\n -> Tuple n.id n) nodes)
      }
    withIdx = foldl addIndex base nodes
    maxId = foldl (\mx n -> max mx (idNum n.id)) 0 nodes
  in
    withIdx { nextId = maxId + 1 }
  where
  addIndex m n = m
    { byTab = maybe m.byTab (\t -> Map.insert t n.id m.byTab) (liveBound n.tabId n)
    , byWindow = maybe m.byWindow (\w -> Map.insert w n.id m.byWindow) (liveBound n.windowId n)
    }
  liveBound field n = case n.status of
    Live -> field
    Closed -> Nothing

idNum :: NodeId -> Int
idNum id = fromMaybe 0 (Int.fromString =<< String.stripPrefix (String.Pattern "n") id)
