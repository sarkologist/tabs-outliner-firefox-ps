# Notes for AMO reviewers — source & build

Grove's shipped JavaScript is **compiled from PureScript** (`spago build`) and
then **bundled and minified with esbuild**. Under Mozilla's
[source-code submission policy](https://extensionworkshop.com/documentation/publish/source-code-submission/),
that means the review requires the original source plus instructions to
reproduce the bundle. This file is the paste-ready "Notes to reviewer" text and
the build recipe; the source itself ships as `grove-source.zip`
(`pnpm run package:source`).

---

## What Grove is

A Firefox sidebar extension that keeps a durable, editable outline of your live
and saved tabs/windows, stored locally in IndexedDB. No analytics, no remote
code, no server of its own, and the add-on never `fetch`es anything; the one
request its pages can make is an open tab's favicon image (below). Nothing is sent to the developer or to any third party, hence
`data_collection_permissions.required = ["none"]`.

Row favicons: a saved (closed) row draws its icon only from bytes stored in the
outline — the `data:` URL the `tabs` API reported while that tab was open,
downscaled on a canvas when large — or a built-in glyph, so browsing saved tabs
never contacts their sites. An imported file's icon URLs are discarded on
import, and a stored icon URL (from a tree saved by an older version) is never
drawn on a saved row nor carried over when that tab is reopened. An open tab's
row shows the `favIconUrl` that tab reports. For web pages Firefox typically
reports a `data:` URL (no request); if it is an `http(s)` URL, the browser loads
that image, with `referrerpolicy="no-referrer"`, from the host of the page that
tab already has open.

## Build environment (pinned)

| Tool | Version | Pinned in |
| --- | --- | --- |
| Node.js | **24.11.0** | `.npmrc` (`use-node-version`) |
| pnpm | **10.28.1** | `package.json` (`packageManager`) |
| PureScript (`purs`) | **0.15.15** | `package.json` devDependencies |
| Spago | **^1.0.4** | `package.json` devDependencies |
| esbuild | **^0.28.1** | `package.json` devDependencies |
| PureScript package set | registry **77.8.0** | `spago.yaml` + `spago.lock` |

A recent Linux or macOS with [Corepack](https://nodejs.org/api/corepack.html)
(bundled with Node) is enough — Corepack fetches the exact pnpm version, and
pnpm's `use-node-version` fetches the exact Node version.

## Reproduce the shipped bundle

From a clean checkout of `grove-source.zip`:

```sh
corepack enable                 # activates the pinned pnpm
pnpm install --frozen-lockfile  # installs the locked JS toolchain + PureScript package set
pnpm run build                  # clean dist/ -> spago build -> esbuild bundle -> copy static files
```

The result in `dist/` is exactly what the submitted XPI contains. CI checks this
on every change (`pnpm run check:source`): it rebuilds from the source zip with
these steps on Linux and on macOS and requires a byte-identical `dist/`. Build the
package — and the source zip (`pnpm run package:source`) — from the **same clean
commit**; `package:source` refuses a dirty tree so the XPI and source can't drift:

```sh
pnpm run package           # runs the build, then `web-ext build` over dist/
```

`pnpm run check` additionally runs the full test suite (PureScript unit +
property + asymptotics-guard tests, then Playwright end-to-end tests that drive
the real compiled background + sidebar) — not required for review, but it's the
project's single green-gate.

## Source → shipped file map

Each entry module is compiled to `output/<Module>/index.js`, then esbuild bundles
it into one IIFE (see `scripts/bundle.mjs`):

| Source entry | Shipped file |
| --- | --- |
| `src/Background/Main.purs` | `dist/background/background.js` |
| `src/Sidebar/Main.purs` | `dist/sidebar/sidebar.js` |
| `src/Options/Main.purs` | `dist/options/options.js` |

Static assets (`manifest.json`, the HTML/CSS, icons) are copied verbatim from
`public/` to `dist/` by `scripts/copy-static.mjs`.

## Permissions justification

| Permission | Used for |
| --- | --- |
| `tabs` | Read the tab/window list to render the outline; focus a tab on click. |
| `sessions` | Stamp each live tab with its outline-node id so a restore re-binds to the same node. |
| `storage`, `unlimitedStorage` | Persist the outline locally (IndexedDB) with no quota cap. |
| `alarms` | Schedule the once-a-day automatic backup. |
| `downloads` | Write the automatic daily backup file, and the manual JSON export. Both are written from the background page, because the whole-tree payload is too large to hand to the sidebar over `runtime.sendMessage`. |

No host permissions, no content scripts, no remote code execution. Favicons are
rendered as plain `<img>` elements; the add-on never `fetch`es one. Downscaling
draws the `data:` URL the `tabs` API reported onto a canvas in the background
page, which needs no host permission (a `data:` image does not taint it).

---

## "Notes to reviewer" — paste this into the submission form

> Grove is compiled from PureScript and minified with esbuild, so I'm attaching
> the full source (grove-source.zip). Build on Linux/macOS with Node 24.11.0 and
> pnpm 10.28.1 (both auto-fetched via Corepack + pnpm's use-node-version):
>
>     corepack enable
>     pnpm install --frozen-lockfile
>     pnpm run build
>
> The resulting `dist/` is the extension. Entry points: src/Background/Main.purs
> → dist/background/background.js, src/Sidebar/Main.purs → dist/sidebar/sidebar.js,
> src/Options/Main.purs → dist/options/options.js. Everything runs locally in the
> browser (IndexedDB); the add-on never fetches anything and collects no data.
> Saved rows draw their favicons from bytes stored locally (captured from the
> `data:` favIconUrl the tabs API reports while the tab is open). An open tab's
> row shows the favIconUrl that tab reports — typically a `data:` URL; should it
> be an http(s) URL, the browser loads that image from the host of the page that
> tab has open. Nothing is sent anywhere else.
