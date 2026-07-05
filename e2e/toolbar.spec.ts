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

const searchSeed = {
  windows: [
    {
      id: 1,
      tabs: [
        { id: 11, url: "http://a", title: "AlphaAlpha", active: true },
        { id: 12, url: "https://needle-url.example/path", title: "Plain" },
      ],
    },
  ],
};

const tallSeed = {
  windows: [
    {
      id: 1,
      tabs: Array.from({ length: 90 }, (_, i) => ({
        id: 200 + i,
        url: `http://t${i}`,
        title: `Tab ${i}`,
        active: i === 0,
      })),
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

const rowOf = (page: Page, text: string) => page.locator(".row").filter({ hasText: text });
const blur = (page: Page) => page.evaluate(() => (document.activeElement as HTMLElement | null)?.blur());
const treeScrollTop = (page: Page) => page.locator("#tree").evaluate((el) => (el as HTMLElement).scrollTop);

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

  test("clear search button clears, restores the outline, and focuses search", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await page.locator("#search").fill("Alph");
    await expect(page.locator("#clear-search")).toBeVisible();
    await expect(page.getByText("Beta")).toHaveCount(0);
    await page.locator("#clear-search").click();
    await expect(page.locator("#search")).toHaveValue("");
    await expect(page.locator("#search")).toBeFocused();
    await expect(page.locator("#clear-search")).toBeHidden();
    await expect(page.getByText("Beta")).toBeVisible();
  });

  test("Escape clears search from the input and from body focus", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await page.locator("#search").fill("Alph");
    await page.locator("#search").press("Escape");
    await expect(page.locator("#search")).toHaveValue("");
    await expect(page.locator("#search")).toBeFocused();
    await expect(page.getByText("Beta")).toBeVisible();

    await page.locator("#search").fill("Bet");
    await expect(page.getByText("Alpha")).toHaveCount(0);
    await blur(page);
    await page.keyboard.press("Escape");
    await expect(page.locator("#search")).toHaveValue("");
    await expect(page.getByText("Alpha")).toBeVisible();
  });

  test("search highlights title matches but not URL-only matches", async ({ page }) => {
    await bootBackgroundAndSidebar(page, searchSeed);
    await page.locator("#search").fill("alpha");
    await expect(rowOf(page, "AlphaAlpha").locator(".title-search-match")).toHaveText(["Alpha", "Alpha"]);

    await page.locator("#search").fill("needle-url");
    await expect(rowOf(page, "Plain")).toBeVisible();
    await expect(rowOf(page, "Plain").locator(".title-search-match")).toHaveCount(0);
  });

  test("show in tree clears search, expands ancestors, and jumps to the result", async ({ page }) => {
    await bootBackgroundAndSidebar(page, tallSeed);
    await page.locator(".toggle").first().click(); // collapse the window
    await expect(page.getByText("Tab 70", { exact: true })).toHaveCount(0);
    await page.locator("#search").fill("Tab 70");
    await expect(page.getByText("Tab 70", { exact: true })).toBeVisible();
    await expect(rowOf(page, "Window").locator(".btn-show-in-tree")).toHaveCount(1);

    const result = rowOf(page, "Tab 70");
    await result.hover();
    await result.locator(".btn-show-in-tree").click();

    await expect(page.locator("#search")).toHaveValue("");
    await expect(page.locator("#clear-search")).toBeHidden();
    await expect(result).toBeVisible();
    await expect(result).toHaveClass(/show-in-tree-flash/);
    await expect(page.locator(".row.show-in-tree-flash")).toHaveCount(1);
    await expect.poll(() => treeScrollTop(page)).toBeGreaterThan(0);
    await expect(page.getByText("Tab 69", { exact: true })).toBeVisible();
    await expect(page.locator(".row.show-in-tree-flash")).toHaveCount(0, { timeout: 2500 });
  });

  test("show in tree works from search ancestor rows", async ({ page }) => {
    await bootBackgroundAndSidebar(page, tallSeed);
    await expect(page.locator(".btn-show-in-tree")).toHaveCount(0);
    await page.locator(".toggle").first().click(); // collapse the window
    await page.locator("#search").fill("Tab 70");

    const windowRow = rowOf(page, "Window");
    await expect(windowRow.locator(".btn-show-in-tree")).toHaveCount(1);
    await windowRow.hover();
    await windowRow.locator(".btn-show-in-tree").click();

    await expect(page.locator("#search")).toHaveValue("");
    await expect(windowRow).toBeVisible();
    await expect(windowRow).toHaveClass(/show-in-tree-flash/);
    await expect(page.locator(".btn-show-in-tree")).toHaveCount(0);
    await expect(page.locator(".row.show-in-tree-flash")).toHaveCount(0, { timeout: 2500 });
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

  test("shows compact counts, with full tooltip text", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.locator("#toolbar-status")).toHaveText("3 / 2");
    await expect(page.locator("#toolbar-status")).toHaveAttribute("title", "3 nodes / 2 open");
    await expect(page.locator("#toolbar-status")).toHaveAttribute("aria-label", "3 nodes / 2 open");

    await page.locator("#search").fill("Alpha");
    await expect(page.locator("#toolbar-status")).toHaveText("1 / 3 / 2");
    await expect(page.locator("#toolbar-status")).toHaveAttribute("title", "1 direct search match / 3 nodes / 2 open");
    await expect(page.locator("[role=treeitem]")).toHaveCount(2); // window ancestor + direct match

    await page.locator("#search").fill("a");
    await expect(page.locator("#toolbar-status")).toHaveText("2 / 3 / 2");
    await expect(page.locator("#toolbar-status")).toHaveAttribute("title", "2 direct search matches / 3 nodes / 2 open");
  });

  test("shows all toolbar actions inline at wide widths", async ({ page }) => {
    await page.setViewportSize({ width: 900, height: 600 });
    await bootBackgroundAndSidebar(page, seed);

    for (const id of [
      "undo",
      "redo",
      "zoom-out",
      "zoom-in",
      "export",
      "import",
      "open-full-size",
      "options",
    ]) {
      await expect(page.locator(`#${id}`)).toBeVisible();
    }
    await expect(page.locator(".toolbar-more")).toBeHidden();
  });

  test("keeps full-size inline after undo and redo at narrow widths", async ({ page }) => {
    await page.setViewportSize({ width: 380, height: 600 });
    await bootBackgroundAndSidebar(page, seed);

    await expect(page.locator(".toolbar-more")).toBeVisible();
    await expect(page.locator("#export")).toBeHidden();
    await expect(page.locator("#options")).toBeHidden();
    await expect(page.locator("#open-full-size")).toBeVisible();

    const xs = await page.locator("#undo, #redo, #open-full-size").evaluateAll((els) =>
      els.map((el) => ({ id: el.id, left: el.getBoundingClientRect().left })),
    );
    expect(xs.map((x) => x.id)).toEqual(["undo", "redo", "open-full-size"]);
    expect(xs[0].left).toBeLessThan(xs[1].left);
    expect(xs[1].left).toBeLessThan(xs[2].left);
  });

  test("folds toolbar actions into More instead of wrapping when narrow", async ({ page }) => {
    await page.setViewportSize({ width: 300, height: 600 });
    await bootBackgroundAndSidebar(page, seed);

    await expect(page.locator(".toolbar-more")).toBeVisible();
    await expect(page.locator("#export")).toBeHidden();
    await expect(page.locator("#open-full-size")).toBeHidden();
    await expect(page.locator("#new-group")).toHaveCount(0);
    await expect(page.locator("#new-group-menu")).toHaveCount(0);
    await expect.poll(() => page.locator("#toolbar").evaluate((el) => el.getBoundingClientRect().height)).toBeLessThanOrEqual(45);

    await page.locator(".toolbar-more-summary").click();
    await expect(page.locator("#open-full-size-menu")).toBeVisible();
    await page.locator("#tree").click({ position: { x: 5, y: 5 } });
    await expect(page.locator("#open-full-size-menu")).toBeHidden();
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

  test("import accepts legacy Chrome Tabs Outliner record-array exports", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    const legacy = JSON.stringify([
      { type: 2000, node: { type: "session", data: { treeId: "1483340179831.8303" } } },
      [2001, { type: "savedwin", marks: { customTitle: "ChromeResearch" }, data: { type: "normal" } }, [0]],
      [
        2001,
        { data: { title: "ChromeParent", url: "https://chrome-import.example/parent", favIconUrl: "https://chrome-import.example/favicon.ico" } },
        [0, 0],
      ],
      [2001, { type: "tab", data: { title: "ChromeChild", url: "https://chrome-import.example/child" } }, [0, 0, 0]],
    ]);
    page.on("filechooser", (fc) =>
      fc.setFiles({ name: "chrome-tabs-outliner.json", mimeType: "application/json", buffer: Buffer.from(legacy) })
    );
    await page.locator("#import").click();
    await expect(page.getByText("Chrome Tab Outliner import")).toBeVisible();
    await expect(page.getByText("ChromeResearch")).toBeVisible();
    await expect(page.locator('[data-status="closed"]').filter({ hasText: "ChromeParent" })).toBeVisible();
    await expect(page.locator('[data-status="closed"]').filter({ hasText: "ChromeChild" })).toBeVisible();
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
