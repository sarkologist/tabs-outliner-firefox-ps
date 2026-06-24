# Tabs Outliner (PureScript rewrite)

A Firefox sidebar extension that keeps a durable, editable outline of your live and
recently-closed tabs and windows — a clean-slate rewrite aimed at the smallest design
that still delivers the essential product.

## What it does

- Shows windows and tabs as a nested, live-updating sidebar outline.
- Keeps recently-closed tabs/windows in the outline as greyed-out, **restorable** history.
- Click a live tab to focus it; click a closed one to restore it (re-binding the same
  node — no duplicate).
- Close, delete, rename, collapse/expand, flatten, drag-reorder, and group nodes.
- Hovering a row draws faint **guide lines** tracing that node's subtree — its attachment to its
  parent and the connectors down to every visible descendant — so structure is legible at a glance.
- Dragging a node shows a **live drop preview** — a guide-styled insertion line marking exactly
  where it will land (before a sibling, or as a group's last child) — and the dragged row dims.
- **Undo/redo** of outline edits — rename, move, group, flatten, delete, import — with
  `Ctrl+Z` / `Ctrl+Shift+Z` (`⌘Z` / `⇧⌘Z` on macOS) or the toolbar's ↶ ↷. Undoing a delete
  brings the subtree back as restorable history (its live tabs were already closed).
- Search the outline, including matches inside collapsed groups.
- Font zoom and JSON export/import of the outline.
- **Configurable keyboard shortcuts** for the toolbar actions (new group, focus search, zoom,
  export, import), editable on a dedicated options page — plus a browser-level shortcut to
  toggle the sidebar open/closed (default `Ctrl+Shift+Y`, `Cmd+Shift+Y` on macOS; unset on
  Linux, where it collides with Firefox's Downloads). The toggle is editable right on the
  options page (via the `commands` API) or in Firefox's *Manage Extension Shortcuts*.
- Persists locally; on restart, **re-binds reopened tabs to their existing nodes by URL**,
  so your organization (tree position, custom titles, collapse) survives a restart.

## Reference: the original extension

The extension this is modeled on lives locally at `~/code/tabs-outliner`. It's kept purely as
a **reference for user-facing behavior** — what the product does, how it looks, how it feels —
**not** as a source of implementation. The goal here is to be functionally equivalent to it
(mostly; see [Scope](#scope)), reproducing its features and styling afresh while leaving behind
the accidental complexity its internals accreted. When in doubt about how a feature should
behave or look, that tree is the spec.

## Design (why it's small)

The original carried ~67 background modules and 900 KB+ of trace-bug/perf notes, almost all
of it machinery to make live-sync bulletproof and to dodge O(N) full-snapshot saves. This
rewrite removes that machinery:

| Concern | Here |
| --- | --- |
| State | One forest of nodes (`{kind, status, parent, children, …}`). Liveness is an attribute. |
| Logic | One **pure reducer** — `Model.Reconcile` (browser events) + `Model.Command` (user commands) — each step returns `{model, patch}`. Undo/redo is just that patch, inverted (`Model.Undo`) — no snapshots, no second apply path. |
| Persistence | **One IndexedDB record per node**; a patch writes only touched records (`Effect.Persist`). The journal/backup layers simply don't exist. |
| Sidebar sync | Background owns the model; the sidebar pulls one snapshot then applies broadcast **patches** (`Effect.Channel`, ~3 messages). `applyPatch` is shared, so the two stay consistent by construction. |
| Restart | One bounded pure function, `Model.Rematch.rematchOnStartup` (O(live tabs)). |

**Asymptotics.** Every per-event / per-command / persist / broadcast path is O(change)
(more precisely O(siblings of the touched node)). Only initial load, the sidebar-open
snapshot, and on-demand search are O(total) — enforced by a 52k-node guard test.

```
src/Model/      Types, Tree, Reconcile, Command, Undo, Rematch, Codec, Shortcuts   — pure, the bulk of the logic
src/Effect/     Browser (the only globalThis.browser seam), Persist, Channel, Settings
src/Background/  Main — owns the model; observes events; persists + broadcasts
src/Sidebar/     Main — the Halogen tree view + toolbar
src/Options/     Main — the Halogen keyboard-shortcuts options page
```

## Develop

Requires Node (a recent LTS), pnpm, and — for the build — Node 22+ (handled automatically by
`.npmrc`'s `use-node-version`; the PureScript toolchain is pinned in `package.json`).

```sh
pnpm install
pnpm run build     # spago build -> esbuild (iife) -> dist, + copy static files
pnpm test          # PureScript unit + property + asymptotics-guard tests (spago)
pnpm run test:e2e  # Playwright: real background + real sidebar in-page over a fake browser
pnpm run check     # the one gate: unit + build + e2e
```

The whole suite is deterministic and runs headless — there is **no manual test step**. Unit
tests cover the pure core; the e2e tests inject a fake `globalThis.browser` and drive the
real compiled background + sidebar, asserting both the rendered DOM and the persisted
IndexedDB. The same code runs against the real Firefox API and the fake.

## Load in Firefox (temporary add-on)

```sh
pnpm run build
```

1. Open `about:debugging#/runtime/this-firefox`.
2. **Load Temporary Add-on…** and pick `dist/manifest.json`.
3. Open the **Tabs Outliner** sidebar.

Temporary add-ons are removed when Firefox restarts; rebuild and reload after changes.

## Scope

This is an experiment in minimalism, so some original behavior is intentionally dropped
(foreign Chrome-Tab-Outliner import, free-text notes/separators, move-to-new-window) and the
restart re-match is a deliberately bounded heuristic, not a bulletproof reconciler.
