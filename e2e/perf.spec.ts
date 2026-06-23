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

test("opening the sidebar on a ~26k-node tree (loads from IndexedDB)", async ({ context }) => {
  test.skip(!existsSync(REAL_EXPORT), "real export not present on this machine");
  test.setTimeout(120_000);

  await populateIdb(context);

  // a fresh sidebar over the same (populated) IndexedDB — no background needed
  // for the initial load anymore
  const b = await context.newPage();
  await b.addInitScript(installFakeBrowser, { windows: [] });
  await b.goto("/blank.html");
  await b.addStyleTag({ path: "dist/sidebar/sidebar.css" });
  const t0 = Date.now();
  await b.addScriptTag({ path: "dist/sidebar/sidebar.js" });
  await b.locator("[role=treeitem]").first().waitFor({ timeout: 60_000 });
  const ms = Date.now() - t0;
  console.log(`\n>>> sidebar boot from IndexedDB (26k nodes): ${ms} ms`);

  // breakdown: how much is raw IndexedDB read vs JSON.parse of the records?
  const bd = await b.evaluate(
    () =>
      new Promise<{ idbRead: number; jsonParse: number; n: number }>((resolve, reject) => {
        const t = performance.now();
        const req = indexedDB.open("tabs-outliner", 1);
        req.onsuccess = () => {
          const all = req.result.transaction("nodes", "readonly").objectStore("nodes").getAll();
          all.onsuccess = () => {
            const t1 = performance.now();
            const parsed = (all.result as string[]).map((s) => JSON.parse(s));
            const t2 = performance.now();
            resolve({ idbRead: Math.round(t1 - t), jsonParse: Math.round(t2 - t1), n: parsed.length });
          };
          all.onerror = () => reject(all.error);
        };
        req.onerror = () => reject(req.error);
      })
  );
  console.log(`>>> breakdown: idbRead=${bd.idbRead}ms jsonParse=${bd.jsonParse}ms (n=${bd.n})\n`);
  expect(ms).toBeLessThan(5000);
});
