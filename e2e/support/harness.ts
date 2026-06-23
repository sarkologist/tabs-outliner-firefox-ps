import type { Page } from "@playwright/test";
import { installFakeBrowser, type Seed } from "./fake-browser";

// Boot the real background module in the page against an injected fake browser.
// (M3 will add the sidebar bundle on top of the same fake.)
export async function bootBackground(page: Page, seed: Seed): Promise<void> {
  await page.addInitScript(installFakeBrowser, seed);
  await page.goto("/blank.html");
  await page.addScriptTag({ path: "dist/background/background.js" });
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

// Drive a fake browser event from the test.
export function fake(page: Page, method: string, ...args: unknown[]): Promise<void> {
  return page.evaluate(
    ([m, a]) => (globalThis as any).__fake[m as string](...(a as unknown[])),
    [method, args] as const
  );
}
