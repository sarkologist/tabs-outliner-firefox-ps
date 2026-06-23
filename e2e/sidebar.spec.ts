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
