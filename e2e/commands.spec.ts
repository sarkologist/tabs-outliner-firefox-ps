import { test, expect, type Page } from "@playwright/test";
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

const focusLog = (page: Page) => page.evaluate(() => (globalThis as any).__fake.focusLog as number[]);
const titles = (page: Page) => page.locator("[role=treeitem] .title").allInnerTexts();
const rowOf = (page: Page, text: string) => page.locator(".row").filter({ hasText: text });

// Row actions are revealed on hover (the original's affordance), so hover the row
// before clicking one — mirrors a real interaction and lets Playwright's pointer
// hit-test see the button (it is pointer-events:none until :hover).
const clickAction = async (page: Page, text: string, btn: string) => {
  const row = rowOf(page, text);
  await row.hover();
  await row.locator(btn).click();
};

test.describe("commands", () => {
  test("clicking a live tab focuses it", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await page.getByText("Beta").click();
    await expect.poll(() => focusLog(page)).toContain(12);
  });

  test("close keeps the node as greyed-out history", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await clickAction(page, "Beta", ".btn-close");
    await expect(page.locator('.row[data-status="closed"]').filter({ hasText: "Beta" })).toBeVisible();
  });

  test("delete removes the node entirely", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await clickAction(page, "Beta", ".btn-delete");
    await expect(page.getByText("Beta")).toHaveCount(0);
    await expect(page.locator("[role=treeitem]")).toHaveCount(2);
  });

  test("rename updates the title", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await clickAction(page, "Alpha", ".btn-rename");
    const input = page.locator(".rename-input");
    await input.fill("Renamed");
    await input.press("Enter");
    await expect(page.getByText("Renamed")).toBeVisible();
    await expect(page.getByText("Alpha")).toHaveCount(0);
  });

  test("new group adds a folder at the top", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await page.locator("#new-group").click();
    // scope to the tree: the toolbar's own "New group" button shares this text,
    // so an unscoped getByText is a strict-mode race (button vs. created node)
    await expect(page.locator("[role=treeitem]").filter({ hasText: "New group" })).toHaveCount(1);
  });

  test("clicking a closed tab restores it (re-binds the node, no duplicate)", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await fake(page, "closeTab", 11);
    await expect(page.locator('[data-status="closed"]')).toHaveCount(1);
    // click the (now closed) Alpha row -> Activate -> Restore
    await rowOf(page, "Alpha").locator(".title").click();
    await expect(page.locator('[data-status="closed"]')).toHaveCount(0);
    // still exactly window + 2 tabs (re-bound, not duplicated)
    await expect(page.locator("[role=treeitem]")).toHaveCount(3);
  });

  test("restoring a closed window re-opens it as a new browser window", async ({ page }) => {
    // two windows: window 1 (the one that stays open) and window 2 (to be closed)
    await bootBackgroundAndSidebar(page, {
      windows: [
        { id: 1, tabs: [{ id: 11, url: "http://keep", title: "Keep", active: true }] },
        {
          id: 2,
          tabs: [
            { id: 21, url: "http://a", title: "Alpha" },
            { id: 22, url: "http://b", title: "Beta" },
          ],
        },
      ],
    });
    await expect(page.getByText("Alpha")).toBeVisible();

    // close window 2 -> its window node + both tabs become closed history
    await fake(page, "closeWindow", 2);
    await expect(page.locator('[data-status="closed"]')).toHaveCount(3);
    expect(await page.evaluate(() => (globalThis as any).__fake.listWindows().length)).toBe(1);

    // restore it: click the closed Window row's title
    await page.locator('.row[data-status="closed"]').filter({ hasText: "Window" }).locator(".title").click();

    // everything goes live again, in place — no leftover closed rows, no duplicates
    await expect(page.locator('[data-status="closed"]')).toHaveCount(0);
    await expect(page.locator("[role=treeitem]")).toHaveCount(5);

    // a brand-new browser window holds the restored tabs; window 1 is untouched
    const windows = await page.evaluate(() => (globalThis as any).__fake.listWindows());
    expect(windows.length).toBe(2);
    const kept = windows.find((w: any) => w.id === 1);
    expect(kept.tabs.map((t: any) => t.url)).toEqual(["http://keep"]);
    const restored = windows.find((w: any) => w.id !== 1);
    expect(restored.tabs.map((t: any) => t.url).sort()).toEqual(["http://a", "http://b"]);
  });

  test("drag reorders siblings", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Beta")).toBeVisible();
    expect(await titles(page)).toEqual(["Window", "Alpha", "Beta"]);
    await page.getByText("Beta").dragTo(page.getByText("Alpha"));
    await expect.poll(() => titles(page)).toEqual(["Window", "Beta", "Alpha"]);
  });

  test("shows a drop preview that tracks the landing spot and clears on drop", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    // nothing until a drag is in progress
    await expect(page.locator(".drop-indicator")).toHaveCount(0);

    await rowOf(page, "Beta").dispatchEvent("dragstart");
    // over a tab: lands before it
    await rowOf(page, "Alpha").dispatchEvent("dragover");
    await expect(page.locator(".drop-indicator")).toHaveCount(1);
    const overTab = await page.locator(".drop-indicator").getAttribute("style");

    // over a different row: the preview moves to the new landing spot
    await rowOf(page, "Window").dispatchEvent("dragover");
    await expect(page.locator(".drop-indicator")).toHaveCount(1);
    expect(await page.locator(".drop-indicator").getAttribute("style")).not.toBe(overTab);

    // the dragged row is dimmed while dragging
    await expect(rowOf(page, "Beta")).toHaveClass(/dragging/);

    await rowOf(page, "Alpha").dispatchEvent("drop");
    await expect(page.locator(".drop-indicator")).toHaveCount(0);
  });
});
