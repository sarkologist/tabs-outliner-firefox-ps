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
  status: "live",
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
