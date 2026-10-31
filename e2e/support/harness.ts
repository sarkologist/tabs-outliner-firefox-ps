import type { Page } from "@playwright/test";
import { installFakeBrowser, type Seed } from "./fake-browser";

// Boot the real background module in the page against an injected fake browser.
export async function bootBackground(page: Page, seed: Seed): Promise<void> {
  await page.addInitScript(installFakeBrowser, seed);
  await page.goto("/blank.html");
  await page.addScriptTag({ path: "dist/background/background.js" });
}

// Boot both the real background and the real sidebar in one page (mirroring the
// original's harness: separate contexts collapsed into one, bridged by the fake
// runtime message bus). The sidebar mounts Halogen onto <body>.
export async function bootBackgroundAndSidebar(
  page: Page,
  seed: Seed,
  options: { sidebarUrl?: string; currentWindowId?: number; persisted?: Persisted } = {}
): Promise<void> {
  await page.addInitScript(installFakeBrowser, seed);
  await page.goto("/blank.html");
  if (options.persisted) await writePersisted(page, options.persisted);
  await page.addScriptTag({ path: "dist/background/background.js" });
  if (typeof options.currentWindowId === "number") {
    await page.evaluate((id) => (globalThis as any).__fake.focusWindow(id), options.currentWindowId);
  }
  if (options.sidebarUrl) {
    await page.evaluate((url) => history.replaceState(null, "", url), options.sidebarUrl);
  }
  // the real stylesheet, so the flex/scroll layout virtualization relies on applies
  await page.addStyleTag({ path: "dist/sidebar/sidebar.css" });
  await page.addScriptTag({ path: "dist/sidebar/sidebar.js" });
}

// A tree as a previous session left it in IndexedDB: node records (the
// `Model.Codec` shape) and the root list.
export type Persisted = { nodes: Array<Record<string, unknown>>; roots: string[] };

// Write `persisted` into the store the background loads at boot — the same
// layout `Effect.Persist` writes (one JSON string per node, roots under "roots").
async function writePersisted(page: Page, persisted: Persisted): Promise<void> {
  await page.evaluate(
    ({ nodes, roots }) =>
      new Promise<void>((resolve, reject) => {
        const req = indexedDB.open("tabs-outliner", 1);
        req.onupgradeneeded = () => {
          req.result.createObjectStore("nodes");
          req.result.createObjectStore("meta");
        };
        req.onsuccess = () => {
          const db = req.result;
          const tx = db.transaction(["nodes", "meta"], "readwrite");
          for (const n of nodes) tx.objectStore("nodes").put(JSON.stringify(n), n.id as string);
          tx.objectStore("meta").put(JSON.stringify(roots), "roots");
          tx.oncomplete = () => {
            db.close();
            resolve();
          };
          tx.onerror = () => reject(tx.error);
        };
        req.onerror = () => reject(req.error);
      }),
    persisted
  );
}

// All persisted node records (parsed from the per-node JSON store).
export async function readNodes(page: Page): Promise<any[]> {
  return page.evaluate(
    () =>
      new Promise<any[]>((resolve, reject) => {
        const req = indexedDB.open("tabs-outliner", 1);
        req.onsuccess = () => {
          const db = req.result;
          const all = db.transaction("nodes", "readonly").objectStore("nodes").getAll();
          all.onsuccess = () => resolve((all.result as string[]).map((s) => JSON.parse(s)));
          all.onerror = () => reject(all.error);
        };
        req.onerror = () => reject(req.error);
      })
  );
}

// Liveness mirrors the model: a node is live iff it carries a browser binding
// (a tab's tabId, or a container's windowId). Liveness is no longer persisted as
// a field — it is exactly the presence of that binding.
export const isLive = (n?: { tabId?: number | null; windowId?: number | null } | null): boolean =>
  n != null && (n.tabId != null || n.windowId != null);

// Drive a fake browser event from the test.
export function fake(page: Page, method: string, ...args: unknown[]): Promise<void> {
  return page.evaluate(
    ([m, a]) => (globalThis as any).__fake[m as string](...(a as unknown[])),
    [method, args] as const
  );
}
