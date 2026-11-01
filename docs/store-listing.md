# AMO store listing — copy & checklist

Paste-ready content for the [addons.mozilla.org](https://addons.mozilla.org)
Developer Hub, plus the assets you need to gather. Keep this file in sync with
`public/manifest.json` (name/description) so the browser UI and the store agree.

---

## Name

**Grove — Persistent Tab Tree Manager**

(Matches `manifest.name`. AMO allows ≤ 50 chars; the descriptor after the em dash
is what makes Grove findable in AMO search — don't drop it. "Persistent" is the
honest differentiator vs. live-only tree-tab tools; avoid "Sessions", which reads
as a session-manager the add-on isn't.)

## Summary  (≤ 250 characters)

> Keep as many tabs as you like. Grove is a Firefox sidebar that turns your live
> and saved tabs into a durable, searchable outline — close tabs to reclaim
> memory and lose nothing, then bring anything back with a click.

## Full description

**Stop rationing your tabs.**

Most tab tools treat your open tabs as a problem to contain — prune them, suspend
them, feel guilty about the count. Grove starts from the opposite belief: your
tabs are worth keeping, and you should never have to choose between an open tab
and a calm mind.

Grove gives you a living outline of every window and tab, right in the Firefox
sidebar — and makes closing a tab completely safe. Close freely to reclaim
memory; nothing is ever lost. Everything you save, close, or restore stays in a
durable, searchable tree you can reorganize, rename, and return to anytime.

**Nothing is lost**
- Unlimited local storage — keep thousands of tabs and windows without hitting a wall.
- An automatic daily backup you can also export or re-import as JSON.
- On restart, reopened tabs re-bind to their place in your tree — positions, custom titles, and collapsed groups survive.

**Everything is findable**
- A live, nested outline of your windows and tabs that updates as you browse.
- Group, rename, drag-to-reorder, collapse/expand, and flatten nodes.
- Search the whole tree — including matches hidden inside collapsed groups.
- The sidebar scrolls to your active tab so you never lose your place.

**Close without fear**
- Close a kept tab and it greys out as restorable history — one click brings it back to the same spot, no duplicate.
- Reclaim RAM on demand without the anxiety of losing what you were reading.
- Full undo/redo of every outline edit — rename, move, group, delete, import.

**Yours, and private**
- Everything lives locally in your browser. Grove has no account, no server and no analytics, and sends your outline nowhere. Icons for saved tabs are stored with your outline, so browsing your saved tabs never contacts their sites.
- Configurable keyboard shortcuts, font zoom, and a browser shortcut to toggle the sidebar (default Ctrl+Shift+Y / ⌘⇧Y).

Grove is a clean-slate, open-source rewrite of the classic tree-style
tab-outliner idea, rebuilt small and fast in PureScript.

## Why Grove asks for each permission (for the listing / privacy text)

- **Tabs** — to show your tabs and windows in the outline and focus them when you click.
- **Sessions** — to remember which outline node a tab belongs to, so a restore lands it back in place instead of creating a duplicate.
- **Storage / Unlimited storage** — to save your tree locally with no size cap.
- **Alarms + Downloads** — to schedule and write the automatic daily backup file, and to write the file when you click Export. (Import needs no permission — it's a file picker.)

Grove does **not** collect, transmit, or sell any data. Everything is stored
locally in the browser (`data_collection_permissions.required = ["none"]` in the
manifest), so no privacy policy should be required — but confirm this in the
"Data collection" step of the submission form.

---

## Listing metadata

- **Category:** Tabs (primary); Privacy & Security or "Other" as secondary if allowed.
- **Tags / keywords:** tab manager, tab tree, tree tabs, persistent tabs, save tabs, restore tabs, outline, backup, productivity, sidebar.
- **Homepage:** the GitHub repo.
- **Support site / email:** repo Issues, or a support email you monitor.
- **License:** MPL-2.0 — the `LICENSE` file is in the repo root. Select "Mozilla Public License 2.0" in the AMO submission form to match.

## Screenshots

`pnpm screenshots` renders the sidebar shots into `docs/screenshots/` from a
curated demo outline (`screenshots/demo-tree.ts`: well-known sites, no personal
data), in Playwright's Firefox (one-time setup: `pnpm exec playwright install
firefox`). Where that Firefox can't launch, `pnpm screenshots:chromium` renders
the same shots in Chromium; its form controls, accent colour and font
rendering differ, so prefer Firefox for what gets uploaded. Output follows the
host's system font and accent colour, so re-rendering on another machine changes
the PNGs slightly. Re-run it whenever the sidebar's look changes.

AMO frames screenshots at 1280×800 (16:10); these are sidebar-shaped and get
letterboxed, as the 1.0.0 listing's were.

Captions, as on the listing:

1. `1-outline.png` — Keep as many tabs as you like. Close them to reclaim memory — nothing is lost, and your tree survives every restart. Hover any row for quick actions — group, rename, cut, delete.
2. `2-drag.png` — Reorganize by dragging, with a live drop preview.
3. `3-search.png` — Find anything in seconds — search across thousands of tabs, including matches tucked inside collapsed groups.
4. Options page (captured by hand) — Make it yours — fully remappable keyboard shortcuts and automatic daily backups.

## Before you upload

- Version is `1.1.1` (`public/manifest.json` and `package.json`). **AMO never lets a version number be reused**, so bump it for every future upload.
- Build the package: `pnpm run package` → `web-ext-artifacts/*.zip`.
- Because the bundle is compiled + minified, you must also submit source: see [`reviewer-notes.md`](reviewer-notes.md) and run `pnpm run package:source`.

## Version notes

Paste into the upload's "Release notes" field.

### 1.1.1

- Saved rows with no icon of their own now show their site's icon, borrowed from other tabs of the same site (still from local bytes, no network request).
- Fixed: in a long list of one site's tabs, rows past the first few dozen lost their icon.
- Fixed: after Firefox had been idle, the first click in the sidebar could be ignored (e.g. restoring a saved window took two clicks).

### 1.1.0

- Rows show each site's favicon, and hovering a row shows its url.
- Saved (closed) rows draw their icon from bytes stored locally, so browsing your outline never contacts the sites in it. Rows saved before this version get their icon the next time their tab is open.
- Fixed: Export could write a file containing only "undefined" on a large tree.
- Fixed: one url Firefox refuses to open no longer stops a whole window from restoring, and a restore blocked by such urls now says why.
- Polished the toolbar icon.
