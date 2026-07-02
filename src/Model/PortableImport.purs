-- | Convert the original "Tab Session Outliner" portable-tree export, plus the
-- | legacy Chrome "Tabs Outliner" tree backup array, into our flat snapshot.
-- | The portable export is a NESTED tree
-- |   { schema:"tabs-outliner-tree", version, roots:[ {kind,title,url,children} ] }
-- | The Chrome export is an array of [2001, nodePayload, numericPath] records.
-- | whereas our own export is a FLAT { nodes:[…], roots:[ids] }. Windows/groups
-- | become folders and tabs become tab nodes; the Import command then makes the
-- | whole thing inert, restorable history.
-- |
-- | O(nodes): the node accumulator is consed (real exports run to tens of
-- | thousands of nodes, so no quadratic array concatenation).
module Model.PortableImport (portableToSnapshot) where

import Prelude

import Control.Alternative (guard)
import Data.Argonaut.Core (Json, toArray, toNumber, toObject, toString)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int as Int
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.String.Common (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Model.Codec (Snapshot)
import Model.Types (Kind(..), Node, NodeId, defaultNode)

type St = { nodes :: List Node, next :: Int }
type ChromeRecord = { payload :: Object.Object Json, path :: Array Int }
type ChromeBuckets = { roots :: Array ChromeRecord, children :: Map.Map String (Array ChromeRecord) }

newtype ImportNode = ImportNode
  { kind :: Kind
  , title :: String
  , customTitle :: Maybe String
  , url :: Maybe String
  , favIconUrl :: Maybe String
  , children :: Array ImportNode
  }

portableToSnapshot :: Json -> Maybe Snapshot
portableToSnapshot json = case toArray json of
  Just entries -> chromeToSnapshot entries
  Nothing -> portableTreeToSnapshot json

portableTreeToSnapshot :: Json -> Maybe Snapshot
portableTreeToSnapshot json = do
  obj <- toObject json
  schema <- Object.lookup "schema" obj >>= toString
  guard (schema == "tabs-outliner-tree")
  rootsJ <- Object.lookup "roots" obj >>= toArray
  roots <- traverse parsePortableNode rootsJ
  pure (snapshotFromRoots roots)

chromeToSnapshot :: Array Json -> Maybe Snapshot
chromeToSnapshot entries = do
  parsed <- traverse parseChromeEntry entries
  let
    records = Array.sortBy comparePaths (Array.mapMaybe identity parsed)
    byKey = Map.fromFoldable (map (\r -> Tuple (chromePathKey r.path) unit) records)
    buckets = foldl (bucketChromeRecord byKey) { roots: [], children: Map.empty } records
    roots = Array.concatMap (chromeNodesFromRecord buckets) buckets.roots
  pure
    if Array.null roots then { nodes: [], roots: [] }
    else snapshotFromRoots
      [ ImportNode
          { kind: KGroup
          , title: "Chrome Tab Outliner import"
          , customTitle: Just "Chrome Tab Outliner import"
          , url: Nothing
          , favIconUrl: Nothing
          , children: roots
          }
      ]

snapshotFromRoots :: Array ImportNode -> Snapshot
snapshotFromRoots roots =
  let res = walkForest Nothing roots { nodes: Nil, next: 0 }
  in { nodes: Array.fromFoldable res.st.nodes, roots: res.ids }

parsePortableNode :: Json -> Maybe ImportNode
parsePortableNode j = do
  obj <- toObject j
  let
    field k = Object.lookup k obj >>= toString
    kind = if fromMaybe "tab" (field "kind") == "tab" then KTab else KGroup
  children <- traverse parsePortableNode (fromMaybe [] (Object.lookup "children" obj >>= toArray))
  pure (ImportNode
    { kind
    , title: fromMaybe "" (field "title")
    , customTitle: field "customTitle"
    , url: field "url"
    , favIconUrl: field "favIconUrl"
    , children
    })

-- Walk a forest of portable nodes left-to-right, threading the id counter and
-- the (consed) node accumulator. Returns the child ids and the updated state.
walkForest :: Maybe NodeId -> Array ImportNode -> St -> { ids :: Array NodeId, st :: St }
walkForest parent js st0 = foldl step { ids: [], st: st0 } js
  where
  step acc j = case walkOne parent j acc.st of
    Just (Tuple id st') -> { ids: Array.snoc acc.ids id, st: st' }
    Nothing -> acc

walkOne :: Maybe NodeId -> ImportNode -> St -> Maybe (Tuple NodeId St)
walkOne parent (ImportNode src) st =
  let
    id = "p" <> show st.next
    childRes = walkForest (Just id) src.children (st { next = st.next + 1 })
    node = (defaultNode id src.kind 0.0)
      { title = src.title
      , customTitle = src.customTitle
      , url = src.url
      , favIconUrl = src.favIconUrl
      , parent = parent
      , children = childRes.ids
      }
  in pure (Tuple id (childRes.st { nodes = Cons node childRes.st.nodes }))

parseChromeEntry :: Json -> Maybe (Maybe ChromeRecord)
parseChromeEntry j = case toArray j of
  Nothing -> Just Nothing -- marker records in the Chrome export are plain objects
  Just entry -> do
    tag <- Array.index entry 0 >>= intJson
    guard (tag == 2001)
    payload <- Array.index entry 1 >>= toObject
    pathJ <- Array.index entry 2 >>= toArray
    guard (not (Array.null pathJ))
    path <- traverse intJson pathJ
    guard (Array.all (_ >= 0) path)
    pure (Just { payload, path })

intJson :: Json -> Maybe Int
intJson j = do
  n <- toNumber j
  Int.fromNumber n

bucketChromeRecord :: Map.Map String Unit -> ChromeBuckets -> ChromeRecord -> ChromeBuckets
bucketChromeRecord byKey buckets record =
  case nearestChromeParentKey byKey record.path of
    Just parent ->
      buckets { children = Map.alter (Just <<< maybe [ record ] (\xs -> Array.snoc xs record)) parent buckets.children }
    Nothing -> buckets { roots = Array.snoc buckets.roots record }

nearestChromeParentKey :: Map.Map String Unit -> Array Int -> Maybe String
nearestChromeParentKey byKey path = go (Array.length path - 1)
  where
  go n
    | n <= 0 = Nothing
    | otherwise =
        let key = chromePathKey (Array.take n path)
        in if Map.member key byKey then Just key else go (n - 1)

chromeNodesFromRecord :: ChromeBuckets -> ChromeRecord -> Array ImportNode
chromeNodesFromRecord buckets record =
  chromeNodesFromPayload record.payload children
  where
  children = Array.concatMap (chromeNodesFromRecord buckets)
    (fromMaybe [] (Map.lookup (chromePathKey record.path) buckets.children))

chromeNodesFromPayload :: Object.Object Json -> Array ImportNode -> Array ImportNode
chromeNodesFromPayload payload children =
  case url of
    Just u
      | isChromeTabOutlinerPage u title -> children
      | otherwise ->
          [ ImportNode
              { kind: KTab
              , title
              , customTitle: Nothing
              , url: Just u
              , favIconUrl
              , children
              }
          ]
    Nothing
      | isChromeContainer payload || not (Array.null children) ->
          [ ImportNode
              { kind: KGroup
              , title: "Group"
              , customTitle: nonEmpty title
              , url: Nothing
              , favIconUrl: Nothing
              , children
              }
          ]
      | otherwise -> children
  where
  dataObj = chromeObjectField "data" payload
  marksObj = chromeObjectField "marks" payload
  url = stringField "url" dataObj
  favIconUrl = stringField "favIconUrl" dataObj
  title = fromMaybe "Group"
    ( firstJust
        [ stringField "customTitle" marksObj >>= nonEmpty
        , stringField "title" dataObj >>= nonEmpty
        , stringField "title" payload >>= nonEmpty
        , url
        ]
    )

chromeObjectField :: String -> Object.Object Json -> Object.Object Json
chromeObjectField key obj = fromMaybe Object.empty (Object.lookup key obj >>= toObject)

stringField :: String -> Object.Object Json -> Maybe String
stringField key obj = Object.lookup key obj >>= toString

firstJust :: forall a. Array (Maybe a) -> Maybe a
firstJust = Array.head <<< Array.mapMaybe identity

nonEmpty :: String -> Maybe String
nonEmpty s = if s == "" then Nothing else Just s

isChromeContainer :: Object.Object Json -> Boolean
isChromeContainer payload = case stringField "type" payload of
  Just "savedwin" -> true
  Just "win" -> true
  Just "group" -> true
  _ -> false

isChromeTabOutlinerPage :: String -> String -> Boolean
isChromeTabOutlinerPage url title =
  String.contains (String.Pattern "chrome-extension://") url
    && ( String.contains (String.Pattern "tabs outliner") (String.toLower title)
        || String.contains (String.Pattern "/activesessionview.html") url
        || String.contains (String.Pattern "/options.html") url
       )

comparePaths :: ChromeRecord -> ChromeRecord -> Ordering
comparePaths left right = comparePathParts left.path right.path

comparePathParts :: Array Int -> Array Int -> Ordering
comparePathParts left right = case Array.uncons left, Array.uncons right of
  Nothing, Nothing -> EQ
  Nothing, Just _ -> LT
  Just _, Nothing -> GT
  Just l, Just r -> case compare l.head r.head of
    EQ -> comparePathParts l.tail r.tail
    other -> other

chromePathKey :: Array Int -> String
chromePathKey path = joinWith "/" (map show path)
