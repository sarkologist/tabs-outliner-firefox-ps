import { test, expect, type Page } from "@playwright/test";
import { readFile } from "node:fs/promises";
import { existsSync } from "node:fs";
import { bootBackgroundAndSidebar } from "./support/harness";

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

const seed = {
  windows: [
    {
      id: 1,
      tabs: [
        { id: 11, url: "http://a", title: "Alpha", active: true },
        { id: 12, url: "http://b", title: "Beta" },
      ],
    },
  ],
};

const node = (over: Record<string, unknown>) => ({
  id: "",
  kind: "tab",
  parent: null,
  children: [],
  title: "",
  customTitle: null,
  url: null,
  favIconUrl: null,
  active: false,
  collapsed: false,
  createdAt: 0,
  closedAt: null,
  tabId: null,
  windowId: null,
  sessionId: null,
  ...over,
});

test.describe("toolbar", () => {
  test("search filters to matches and their ancestors", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Beta")).toBeVisible();
    await page.locator("#search").fill("Alph");
    await expect(page.getByText("Alpha")).toBeVisible();
    await expect(page.getByText("Beta")).toHaveCount(0);
    // window (ancestor) + Alpha
    await expect(page.locator("[role=treeitem]")).toHaveCount(2);
  });

  test("search reaches inside collapsed groups", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await page.locator(".toggle").first().click(); // collapse the window
    await expect(page.getByText("Beta")).toHaveCount(0);
    await page.locator("#search").fill("Beta");
    await expect(page.getByText("Beta")).toBeVisible(); // search ignores collapse
  });

  test("zoom changes the font scale", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    const scale = () =>
      page.locator("#app").evaluate((el) => (el as HTMLElement).style.getPropertyValue("--font-scale"));
    await page.locator("#zoom-in").click();
    await expect.poll(() => scale().then(Number)).toBeGreaterThan(1);
    await page.locator("#zoom-out").click();
    await page.locator("#zoom-out").click();
    await expect.poll(() => scale().then(Number)).toBeLessThan(1);
  });

  test("export downloads the outline as JSON", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    const downloadPromise = page.waitForEvent("download");
    await page.locator("#export").click();
    const download = await downloadPromise;
    expect(download.suggestedFilename()).toBe("tabs-outliner.json");
    const parsed = JSON.parse(await readFile(await download.path(), "utf8"));
    expect(parsed.roots.length).toBeGreaterThan(0);
    expect(parsed.nodes.map((n: { title: string }) => n.title)).toContain("Alpha");
  });

  test("import adds an exported outline as closed history", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    const snapshot = JSON.stringify({
      nodes: [
        node({ id: "g1", kind: "group", children: ["t1"], title: "ImportedGroup" }),
        node({ id: "t1", kind: "tab", parent: "g1", title: "ImportedTab", url: "http://imp", tabId: 999 }),
      ],
      roots: ["g1"],
    });
    page.on("filechooser", (fc) =>
      fc.setFiles({ name: "outline.json", mimeType: "application/json", buffer: Buffer.from(snapshot) })
    );
    await page.locator("#import").click();
    await expect(page.getByText("ImportedGroup")).toBeVisible();
    // the imported tab is inert (closed), not a live tab
    await expect(page.locator('[data-status="closed"]').filter({ hasText: "ImportedTab" })).toBeVisible();
  });

  test("import accepts the original's nested portable-tree format", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    const portable = JSON.stringify({
      schema: "tabs-outliner-tree",
      version: 1,
      roots: [
        { kind: "window", title: "OrigGroup", children: [{ kind: "tab", title: "OrigTab", url: "http://orig", children: [] }] },
      ],
    });
    page.on("filechooser", (fc) =>
      fc.setFiles({ name: "tree.json", mimeType: "application/json", buffer: Buffer.from(portable) })
    );
    await page.locator("#import").click();
    await expect(page.getByText("OrigGroup")).toBeVisible();
    await expect(page.locator('[data-status="closed"]').filter({ hasText: "OrigTab" })).toBeVisible();
  });

  // Regression: an imported window was never a live browser window here, so it can
  // only be restored by recreating it via windows.create — and Firefox can fire the
  // new window's tabs.onCreated before its windows.onCreated. Restoring must still
  // match the runtime window/tabs onto the existing imported nodes (they go live in
  // place), not mint a duplicate window + tabs and strand the imports as closed.
  test("restoring an imported window rebinds its nodes instead of duplicating them", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();

    const snapshot = JSON.stringify({
      nodes: [
        node({ id: "w1", kind: "group", children: ["t1", "t2"], title: "ImportedWindow" }),
        node({ id: "t1", kind: "tab", parent: "w1", title: "ImpA", url: "http://impa" }),
        node({ id: "t2", kind: "tab", parent: "w1", title: "ImpB", url: "http://impb" }),
      ],
      roots: ["w1"],
    });
    page.on("filechooser", (fc) =>
      fc.setFiles({ name: "outline.json", mimeType: "application/json", buffer: Buffer.from(snapshot) })
    );
    await page.locator("#import").click();
    await expect(page.getByText("ImportedWindow")).toBeVisible();
    // the three imported nodes (window + two tabs) land as closed history
    await expect(page.locator('[data-status="closed"]')).toHaveCount(3);

    // restore it by clicking the imported window row's title
    await page
      .locator('.row[data-status="closed"]')
      .filter({ hasText: "ImportedWindow" })
      .locator(".title")
      .click();

    // the imported nodes go live IN PLACE: no closed rows remain, and the tree still
    // holds exactly six nodes (3 seeded + 3 imported) — no phantom window/tab nodes
    await expect(page.locator('[data-status="closed"]')).toHaveCount(0);
    await expect(page.locator("[role=treeitem]")).toHaveCount(6);
    await expect.poll(() => countNodes(page)).toBe(6);

    // the restored tabs really reopened in one brand-new browser window
    const windows = await page.evaluate(() => (globalThis as any).__fake.listWindows());
    expect(windows.length).toBe(2);
    const restored = windows.find((w: any) => w.id !== 1);
    expect(restored.tabs.map((t: any) => t.url).sort()).toEqual(["http://impa", "http://impb"]);
  });

  // The real original export (tabs-outliner-tree) nests tabs UNDER other tabs. Such a
  // nested tab's owning window is its nearest container ancestor, not its parent tab —
  // so restoring the window must reopen the whole tab forest into ONE window and
  // rebind every tab in place, not treat each parent tab as its own window (which
  // can't bind, minting duplicate window/tab nodes). This is the reported bug.
  test("restoring an imported window with tabs nested under tabs rebinds them all", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();

    // window NestWin -> tab NestA -> (child) tab NestB -> (grandchild) tab NestC
    const portable = JSON.stringify({
      schema: "tabs-outliner-tree",
      version: 1,
      roots: [
        {
          kind: "window",
          title: "NestWin",
          children: [
            {
              kind: "tab",
              title: "NestA",
              url: "http://na",
              children: [
                { kind: "tab", title: "NestB", url: "http://nb", children: [{ kind: "tab", title: "NestC", url: "http://nc", children: [] }] },
              ],
            },
          ],
        },
      ],
    });
    page.on("filechooser", (fc) =>
      fc.setFiles({ name: "tree.json", mimeType: "application/json", buffer: Buffer.from(portable) })
    );
    await page.locator("#import").click();
    await expect(page.getByText("NestWin")).toBeVisible();
    // window + three nested tabs land as closed history
    await expect(page.locator('[data-status="closed"]')).toHaveCount(4);

    await page
      .locator('.row[data-status="closed"]')
      .filter({ hasText: "NestWin" })
      .locator(".title")
      .click();

    // every node goes live in place — no closed rows, no duplicate window/tab nodes
    await expect(page.locator('[data-status="closed"]')).toHaveCount(0);
    await expect(page.locator("[role=treeitem]")).toHaveCount(7); // 3 seeded + 4 imported
    await expect.poll(() => countNodes(page)).toBe(7);

    // all three tabs reopened in ONE new browser window (not three)
    const windows = await page.evaluate(() => (globalThis as any).__fake.listWindows());
    expect(windows.length).toBe(2);
    const restored = windows.find((w: any) => w.id !== 1);
    expect(restored.tabs.map((t: any) => t.url).sort()).toEqual(["http://na", "http://nb", "http://nc"]);
  });

  test("imports a real ~26k-node portable export without choking", async ({ page }) => {
    test.skip(!existsSync(REAL_EXPORT), "real export file not present on this machine");
    test.setTimeout(90_000);
    const errors: string[] = [];
    page.on("pageerror", (e) => errors.push(String(e)));
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    const buffer = await readFile(REAL_EXPORT);
    page.on("filechooser", (fc) => fc.setFiles({ name: "tree.json", mimeType: "application/json", buffer }));
    await page.locator("#import").click();
    // the whole tree persists (3 seeded + 26061 imported)
    await expect.poll(() => countNodes(page), { timeout: 60_000 }).toBeGreaterThan(26_000);
    // ...but it imports EXPANDED, and virtualization keeps only a viewport's worth
    // of rows in the DOM (not 26k)
    await expect.poll(() => page.locator("[role=treeitem]").count()).toBeGreaterThan(10);
    expect(await page.locator("[role=treeitem]").count()).toBeLessThan(300);
    // scrolling swaps which rows are mounted
    const firstId = await page.locator("[role=treeitem]").first().getAttribute("data-node-id");
    await page.locator("#tree").evaluate((el) => (el.scrollTop = 8000));
    await expect
      .poll(() => page.locator("[role=treeitem]").first().getAttribute("data-node-id"))
      .not.toBe(firstId);
    await expect(page.locator("#notice")).toHaveCount(0);
    expect(errors).toEqual([]);
  });

  test("import shows a notice on an unrecognized file (no silent failure)", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    page.on("filechooser", (fc) =>
      fc.setFiles({ name: "junk.json", mimeType: "application/json", buffer: Buffer.from('{"foo":1}') })
    );
    await page.locator("#import").click();
    await expect(page.locator("#notice")).toBeVisible();
    await expect(page.locator("#notice")).toContainText("unrecognized format");
    // dismissable
    await page.locator("#notice").click();
    await expect(page.locator("#notice")).toHaveCount(0);
  });
});
