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
export async function bootBackgroundAndSidebar(page: Page, seed: Seed): Promise<void> {
  await page.addInitScript(installFakeBrowser, seed);
  await page.goto("/blank.html");
  await page.addScriptTag({ path: "dist/background/background.js" });
  // the real stylesheet, so the flex/scroll layout virtualization relies on applies
  await page.addStyleTag({ path: "dist/sidebar/sidebar.css" });
  await page.addScriptTag({ path: "dist/sidebar/sidebar.js" });
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
