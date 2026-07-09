# AMO store listing — copy & checklist

Paste-ready content for the [addons.mozilla.org](https://addons.mozilla.org)
Developer Hub, plus the assets you need to gather. Keep this file in sync with
`public/manifest.json` (name/description) so the browser UI and the store agree.

---

## Name

**Grove — Tab Tree & Sessions**

(Matches `manifest.name`. AMO allows ≤ 50 chars; the descriptor after the em dash
is what makes Grove findable in AMO search — don't drop it.)

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
- Everything lives locally in your browser. Grove collects nothing and sends nothing anywhere.
- Configurable keyboard shortcuts, font zoom, and a browser shortcut to toggle the sidebar (default Ctrl+Shift+Y / ⌘⇧Y).

Grove is a clean-slate, open-source rewrite of the classic tree-style
tab-outliner idea, rebuilt small and fast in PureScript.

## Why Grove asks for each permission (for the listing / privacy text)

- **Tabs** — to show your tabs and windows in the outline and focus them when you click.
- **Sessions** — to remember which outline node a tab belongs to, so a restore lands it back in place instead of creating a duplicate.
- **Storage / Unlimited storage** — to save your tree locally with no size cap.
- **Alarms + Downloads** — to schedule and write the automatic daily backup file. (Manual export/import needs no download permission — export is a plain file download, import is a file picker.)

Grove does **not** collect, transmit, or sell any data. Everything is stored
locally in the browser (`data_collection_permissions.required = ["none"]` in the
manifest), so no privacy policy should be required — but confirm this in the
"Data collection" step of the submission form.

---

## Listing metadata

- **Category:** Tabs (primary); Privacy & Security or "Other" as secondary if allowed.
- **Tags / keywords:** tab manager, tab tree, tree tabs, sessions, session manager, outline, tab outliner, backup, productivity, sidebar.
- **Homepage:** the GitHub repo.
- **Support site / email:** repo Issues, or a support email you monitor.
- **License:** MPL-2.0 — the `LICENSE` file is in the repo root. Select "Mozilla Public License 2.0" in the AMO submission form to match.

## Screenshots to capture (at least 1, ideally 4–5; PNG, ~1280×800)

1. The sidebar showing a rich nested tree — live tabs plus greyed-out restorable history.
2. Search with matches highlighted inside a collapsed group.
3. A drag-reorder in progress showing the drop-preview guide line.
4. The options page (configurable keyboard shortcuts).
5. Undo/redo or JSON export in action.

## Before you upload

- Version is `1.0.0` for the debut. **AMO never lets a version number be reused**, so bump it for every future upload.
- Build the package: `pnpm run package` → `web-ext-artifacts/*.zip`.
- Because the bundle is compiled + minified, you must also submit source: see [`reviewer-notes.md`](reviewer-notes.md) and run `pnpm run package:source`.
