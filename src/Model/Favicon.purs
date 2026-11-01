-- | Where a row's icon comes from, and what the tree is allowed to keep of it.
-- |
-- | A saved tree must never make the sidebar talk to the sites in it: a closed
-- | row is a page the user is NOT visiting, so loading its icon from that page's
-- | host would leak the outline (with the user's IP and timing) to every site in
-- | it — and an imported tree is importer-controlled, so a crafted export could
-- | give each node a unique url at one host and turn scrolling into a beacon. So
-- | a closed row renders only icon BYTES the tree already holds (`data:image/`),
-- | and those bytes are captured while the tab is live.
-- |
-- | Capture needs no fetch of our own. The background page could not make one
-- | anyway: it is a `moz-extension://` page with no host permissions, so a
-- | `fetch` of a remote icon fails CORS and drawing a remote `<img>` taints the
-- | canvas (both verified on Firefox 159). It does not need to: Firefox reports a
-- | live tab's `favIconUrl` as a `data:` url of the icon it already loaded for the
-- | tab strip — bytes, at the icon's native size. Those are stored as they are
-- | when small; one too big to keep on every node (a 256px icon arrives as
-- | ~350 KB) is re-encoded to a 32px/16px PNG on a canvas in the background
-- | (`Effect.Browser.shrinkFavicon`), which a `data:` source never taints. Either
-- | way nothing goes over the network that the tab itself had not already done.
module Model.Favicon
  ( storedIconByteLimit
  , isIconBytes
  , isStorableIcon
  , needsShrink
  , mergeReported
  , storedBytes
  , sameSite
  , origin
  , importedIcon
  , reportedIcon
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (codePointFromChar, drop, indexOf, length, stripPrefix, takeWhile, toLower) as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Model.Event (BrowserEvent(..))

-- | The most a node may keep, in characters of its `data:` url. A 32px PNG — what
-- | a 14px slot needs on a HiDPI display — is ~1–3 KB as base64; the cap is the
-- | per-node worst case for IndexedDB and Export (the original Tabs Outliner kept
-- | icons uncapped: 4.5 MB of one 13.4 MB export). Anything bigger is shrunk
-- | before it is stored, or not stored.
storedIconByteLimit :: Int
storedIconByteLimit = 4096

-- | Icon bytes, as opposed to a pointer to them: rendering one makes no request.
isIconBytes :: String -> Boolean
isIconBytes u = isJust (String.stripPrefix (Pattern "data:image/") u)

-- | Bytes small enough to keep on a node.
isStorableIcon :: String -> Boolean
isStorableIcon u = isIconBytes u && String.length u <= storedIconByteLimit

-- | Bytes too big to keep: the background re-encodes these before storing them.
needsShrink :: String -> Boolean
needsShrink u = isIconBytes u && String.length u > storedIconByteLimit

-- | What a node keeps when its live tab reports `reported` (Nothing = the event
-- | said nothing about the icon). Stored bytes are the thing worth keeping, so:
-- |
-- | * storable bytes replace whatever was there;
-- | * over-cap bytes leave the old icon in place until their shrunk copy arrives
-- |   (as an ordinary `TabChanged`; see `Background.Main`);
-- | * a pointer (an `https://` url — Firefox reports `data:`, but the API does not
-- |   promise it) is kept for the live row to draw, but never displaces stored
-- |   bytes: it would be dropped from view the moment the tab closed.
-- |
-- | An empty url is the tab saying it has no icon any more.
mergeReported :: Maybe String -> Maybe String -> Maybe String
mergeReported old = case _ of
  Nothing -> old
  Just "" -> Nothing
  Just u
    | isStorableIcon u -> Just u
    | isIconBytes u -> old
    | otherwise -> case old of
        Just o | isIconBytes o -> old
        _ -> Just u

-- | What a node carries across going live again (a restore, a startup re-find):
-- | its stored bytes, never a url. A url on a closed node is never drawn, but one
-- | saved before icons were captured — or claimed by a file imported then — must
-- | not come back to life the moment its tab reopens, before the tab reports its
-- | own icon.
storedBytes :: Maybe String -> Maybe String
storedBytes icon = icon >>= \u -> if isIconBytes u then Just u else Nothing

-- | Do two page urls share a host? A tab that navigates to another site must not
-- | keep the old site's icon until (if ever) the new page reports one.
sameSite :: Maybe String -> Maybe String -> Boolean
sameSite a b = map host a == map host b
  where
  host u = String.takeWhile (\c -> c /= String.codePointFromChar '/' && c /= String.codePointFromChar '?' && c /= String.codePointFromChar '#')
    (fromMaybe u (afterScheme u))
  afterScheme u = map (\i -> String.drop (i + 3) u) (String.indexOf (Pattern "://") u)

-- | A web page url's origin (scheme, host, port), lowercased: the scope a site's
-- | fallback `/favicon.ico` lives in, so the unit `Model.View` shares a captured
-- | icon across. Nothing for anything but http(s) — no site, no shared icon.
origin :: String -> Maybe String
origin u = do
  let lower = String.toLower u
  Tuple scheme rest <- after "https://" lower <|> after "http://" lower
  let host = String.takeWhile (\c -> c /= String.codePointFromChar '/' && c /= String.codePointFromChar '?' && c /= String.codePointFromChar '#') rest
  guard (host /= "")
  pure (scheme <> host)
  where
  after scheme s = Tuple scheme <$> String.stripPrefix (Pattern scheme) s

-- | What an imported node keeps of the icon its file claimed: storable bytes
-- | only. A claimed url is dropped outright — an imported node is closed, so it
-- | would never be drawn, and once the user restores it the tab reports its own.
importedIcon :: Maybe String -> Maybe String
importedIcon icon = icon >>= \u -> if isStorableIcon u then Just u else Nothing

-- | The icon a browser event reports for a tab, if any.
reportedIcon :: BrowserEvent -> Maybe { tabId :: Int, icon :: String }
reportedIcon = case _ of
  TabOpened t -> t.favIconUrl <#> \icon -> { tabId: t.tabId, icon }
  TabChanged c -> c.favIconUrl <#> \icon -> { tabId: c.tabId, icon }
  _ -> Nothing
