import { test, expect } from "@playwright/test";
import { readFile } from "node:fs/promises";
import { bootBackgroundAndSidebar } from "./support/harness";

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
});
