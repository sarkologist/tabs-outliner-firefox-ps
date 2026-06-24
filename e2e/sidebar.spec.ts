import { test, expect } from "@playwright/test";
import { bootBackgroundAndSidebar, fake } from "./support/harness";

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

test.describe("sidebar view", () => {
  test("renders the snapshot tree from the background", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    await expect(page.getByText("Beta")).toBeVisible();
    // one window row + two tab rows
    await expect(page.locator("[role=treeitem]")).toHaveCount(3);
  });

  test("reflects a live tab open via a broadcast patch", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    await fake(page, "openTab", { id: 13, windowId: 1, url: "http://c", title: "Gamma" });
    await expect(page.getByText("Gamma")).toBeVisible();
  });

  test("greys out a tab closed live (kept as history)", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    await fake(page, "closeTab", 11);
    await expect(page.locator('[data-status="closed"]').filter({ hasText: "Alpha" })).toBeVisible();
  });

  test("hovering a row draws subtree guide lines, cleared on leave", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    // no guide lines are drawn until the pointer is over a row
    await expect(page.locator(".guide-line")).toHaveCount(0);

    // hovering a tab traces its connection up to the parent window: a vertical
    // segment on the tab and on the window, and one horizontal stub into the tab
    await page.locator(".row").filter({ hasText: "Alpha" }).hover();
    await expect(page.locator(".guide-vertical").first()).toBeVisible();
    await expect(page.locator(".guide-horizontal")).toHaveCount(1);

    // hovering the window connects it down to BOTH child tabs (two stubs)
    await page.locator(".row").filter({ hasText: "Window" }).hover();
    await expect(page.locator(".guide-horizontal")).toHaveCount(2);

    // leaving the tree clears the guides
    await page.locator("#search").hover();
    await expect(page.locator(".guide-line")).toHaveCount(0);
  });

  test("collapse hides descendants (command round-trip, persisted)", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    // the window row is the only one with a toggle initially
    await page.locator(".toggle").first().click();
    await expect(page.getByText("Alpha")).toHaveCount(0);
    await expect(page.getByText("Beta")).toHaveCount(0);
    // expand again
    await page.locator(".toggle").first().click();
    await expect(page.getByText("Alpha")).toBeVisible();
  });
});
