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
and saved tabs/windows, stored locally in IndexedDB. No network access, no
analytics, no remote code. `data_collection_permissions.required = ["none"]`.

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
corepack enable            # activates the pinned pnpm
pnpm install               # installs JS toolchain + PureScript package set
pnpm run build             # clean dist/ -> spago build -> esbuild bundle -> copy static files
```

The result in `dist/` is exactly what the submitted XPI contains. To verify it
matches, build the package the same way the XPI was produced:

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
| `downloads` | Write the backup file and user-initiated JSON exports. |

No host permissions, no content scripts, no remote code execution.

---

## "Notes to reviewer" — paste this into the submission form

> Grove is compiled from PureScript and minified with esbuild, so I'm attaching
> the full source (grove-source.zip). Build on Linux/macOS with Node 24.11.0 and
> pnpm 10.28.1 (both auto-fetched via Corepack + pnpm's use-node-version):
>
>     corepack enable
>     pnpm install
>     pnpm run build
>
> The resulting `dist/` is the extension. Entry points: src/Background/Main.purs
> → dist/background/background.js, src/Sidebar/Main.purs → dist/sidebar/sidebar.js,
> src/Options/Main.purs → dist/options/options.js. Everything runs locally in the
> browser (IndexedDB); the add-on makes no network requests and collects no data.
