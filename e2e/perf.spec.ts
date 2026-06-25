import { test, expect, type Page, type BrowserContext } from "@playwright/test";
import { readFile } from "node:fs/promises";
import { existsSync } from "node:fs";
import { installFakeBrowser } from "./support/fake-browser";

const REAL_EXPORT = "/Users/sark/code/tabs-outliner/tabs-outliner-tree-2026-06-12.json";

const countNodes = (page: Page) =>
  page.evaluate(
    () =>
      new Promise<number>((resolve, reject) => {
        const req = indexedDB.open("tabs-outliner", 1);
        req.onsuccess = () => {
          const c = req.result.transaction("nodes", "readonly").objectStore("nodes").count();
          c.onsuccess = () => resolve(c.result);
          c.onerror = () => reject(c.error);
        };
        req.onerror = () => reject(req.error);
      })
  );

// Populate IndexedDB by importing the real export in a throwaway page.
async function populateIdb(context: BrowserContext) {
  const a = await context.newPage();
  await a.addInitScript(installFakeBrowser, { windows: [{ id: 1, tabs: [] }] });
  await a.goto("/blank.html");
  await a.addScriptTag({ path: "dist/background/background.js" });
  await a.addStyleTag({ path: "dist/sidebar/sidebar.css" });
  await a.addScriptTag({ path: "dist/sidebar/sidebar.js" });
  const buffer = await readFile(REAL_EXPORT);
  a.on("filechooser", (fc) => fc.setFiles({ name: "tree.json", mimeType: "application/json", buffer }));
  await a.locator("#import").click();
  await expect.poll(() => countNodes(a), { timeout: 60_000 }).toBeGreaterThan(26_000);
  await a.close();
}

test("baseline: empty-tree sidebar boot (fixed overhead floor)", async ({ context }) => {
  const b = await context.newPage();
  await b.addInitScript(installFakeBrowser, { windows: [] });
  await b.goto("/blank.html");
  await b.addStyleTag({ path: "dist/sidebar/sidebar.css" });
  const t0 = Date.now();
  await b.addScriptTag({ path: "dist/sidebar/sidebar.js" });
  await b.locator("#search").waitFor({ timeout: 30_000 });
  console.log(`\n>>> empty-tree boot (fixed overhead): ${Date.now() - t0} ms\n`);
});

test("opening the sidebar on a ~26k-node tree (window projection, warm background)", async ({ context }) => {
  test.skip(!existsSync(REAL_EXPORT), "real export not present on this machine");
  test.setTimeout(120_000);

  await populateIdb(context);

  const b = await context.newPage();
  await b.addInitScript(installFakeBrowser, { windows: [] });
  await b.goto("/blank.html");
  await b.addStyleTag({ path: "dist/sidebar/sidebar.css" });
  await b.addScriptTag({ path: "dist/background/background.js" });

  // Warm the background: poll a GetView until it answers with the whole tree
  // loaded (the once-per-wake O(N) load — the common case is a sidebar opening
  // against an already-running background).
  await expect
    .poll(
      () =>
        b.evaluate(async () => {
          try {
            // channel envelope: { kind: "req", body: <request> }
            const v: any = await (globalThis as any).browser.runtime.sendMessage({
              kind: "req",
              body: { tag: "getView", start: 0, count: 1, query: "", myWindow: null, wantFocus: false },
            });
            return v && v.total ? v.total : 0;
          } catch {
            return 0;
          }
        }),
      { timeout: 60_000 }
    )
    .toBeGreaterThan(26_000);

  // Now open the sidebar against the warm background and time it to first paint.
  const t0 = Date.now();
  await b.addScriptTag({ path: "dist/sidebar/sidebar.js" });
  await b.locator("[role=treeitem]").first().waitFor({ timeout: 30_000 });
  const ms = Date.now() - t0;
  console.log(`\n>>> sidebar open on warm background (26k nodes): ${ms} ms`);

  // It rendered only a viewport window — not 26k rows.
  const mounted = await b.locator("[role=treeitem]").count();
  console.log(`>>> rows mounted: ${mounted} of 26061\n`);
  expect(mounted).toBeLessThan(300);
  expect(ms).toBeLessThan(250);
});
