import { test, expect, type Page } from "@playwright/test";
import { bootBackgroundAndSidebar, fake, readNodes } from "./support/harness";

const OUTLINER_URL = "moz-extension://extension-id/sidebar/sidebar.html?view=window";

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

const windows = (page: Page) => page.evaluate(() => (globalThis as any).__fake.listWindows());
const popupWindows = async (page: Page) => (await windows(page)).filter((w: any) => w.type === "popup");
const scrollTop = (page: Page) => page.locator("#tree").evaluate((el) => (el as HTMLElement).scrollTop);

const expectTreeNodeCount = async (page: Page, count: number) => {
  await expect(page.locator("[role=treeitem]")).toHaveCount(count);
  expect(await readNodes(page)).toHaveLength(count);
};

const expectScrollTopToStayAt = async (page: Page, expected: number, durationMs = 1000) => {
  const deadline = Date.now() + durationMs;
  do {
    expect(await scrollTop(page)).toBe(expected);
    await page.waitForTimeout(50);
  } while (Date.now() < deadline);
};

test.describe("full-size outliner view", () => {
  test("opens a maximized popup without adding it to the outline", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();

    await page.locator("#open-full-size").click();

    await expect.poll(async () => (await windows(page)).length).toBe(2);
    const popup = (await windows(page)).find((w: any) => w.type === "popup");
    expect(popup).toMatchObject({
      focused: true,
      tabs: [{ url: OUTLINER_URL }],
    });

    await expectTreeNodeCount(page, 3);
  });

  test("reopening from a docked sidebar focuses the existing full-size view", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();

    await page.locator("#open-full-size").click();
    await expect.poll(async () => (await popupWindows(page)).length).toBe(1);
    const popupId = (await popupWindows(page))[0].id;
    await page.evaluate(() => ((globalThis as any).__fake.winFocusLog.length = 0));

    await page.locator("#open-full-size").click();

    await expect.poll(async () => (await popupWindows(page)).length).toBe(1);
    expect(await page.evaluate(() => (globalThis as any).__fake.winFocusLog)).toContain(popupId);
  });

  test("reopening from a full-size view creates another full-size view", async ({ page }) => {
    await bootBackgroundAndSidebar(
      page,
      {
        currentWindowId: 999,
        windows: [
          ...seed.windows,
          {
            id: 999,
            type: "popup" as const,
            focused: true,
            tabs: [{ id: 901, url: OUTLINER_URL, title: "Tabs Outliner", active: true }],
          },
        ],
      },
      { sidebarUrl: "/sidebar/sidebar.html?view=window", currentWindowId: 999 }
    );
    await expect(page.getByText("Alpha")).toBeVisible();
    await expectTreeNodeCount(page, 3);

    await page.locator("#open-full-size").click();

    await expect.poll(async () => (await popupWindows(page)).length).toBe(2);
  });

  test("forgetting a closed full-size view lets the docked sidebar create a fresh popup", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();

    await page.locator("#open-full-size").click();
    await expect.poll(async () => (await popupWindows(page)).length).toBe(1);
    const firstPopupId = (await popupWindows(page))[0].id;

    await fake(page, "closeWindowWithTabEvents", firstPopupId);
    await expect.poll(async () => (await popupWindows(page)).length).toBe(0);
    await expectTreeNodeCount(page, 3);

    await page.locator("#open-full-size").click();

    await expect.poll(async () => (await popupWindows(page)).length).toBe(1);
    const secondPopupId = (await popupWindows(page))[0].id;
    expect(secondPopupId).not.toBe(firstPopupId);
    await expectTreeNodeCount(page, 3);
  });

  test("opens at the top and does not chase active-tab changes", async ({ page }) => {
    const tallSeed = {
      currentWindowId: 999,
      windows: [
        {
          id: 1,
          tabs: Array.from({ length: 100 }, (_, i) => ({
            id: 200 + i,
            url: `http://t${i}`,
            title: `Tab ${i}`,
            active: i === 90,
          })),
        },
      ],
    };

    await bootBackgroundAndSidebar(page, tallSeed, {
      sidebarUrl: "/sidebar/sidebar.html?view=window",
      currentWindowId: 999,
    });

    await expect(page.getByText("Tab 0", { exact: true })).toBeVisible();
    await expect.poll(() => scrollTop(page)).toBe(0);

    await fake(page, "activateTab", 290);

    await expect(page.getByText("Tab 0", { exact: true })).toBeVisible();
    await expectScrollTopToStayAt(page, 0);
  });
});
