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

  // A window whose active tab sits far below the fold, to exercise auto-scroll.
  const tallSeed = (activeIndex: number) => ({
    windows: [
      {
        id: 1,
        tabs: Array.from({ length: 80 }, (_, i) => ({
          id: 200 + i,
          url: `http://t${i}`,
          title: `Tab ${i}`,
          active: i === activeIndex,
        })),
      },
    ],
  });

  const treeScrollTop = (page: import("@playwright/test").Page) =>
    page.locator("#tree").evaluate((el) => (el as HTMLElement).scrollTop);

  test("on open, scrolls to this window's active tab when it's below the fold", async ({ page }) => {
    await bootBackgroundAndSidebar(page, tallSeed(70));
    // the active tab gets scrolled into view (and is the only .active row)
    await expect(page.locator(".row.active")).toHaveText("Tab 70");
    await expect.poll(() => treeScrollTop(page)).toBeGreaterThan(0);
  });

  test("on open, leaves the scroll alone when the active tab is already visible", async ({ page }) => {
    await bootBackgroundAndSidebar(page, tallSeed(0));
    await expect(page.getByText("Tab 0", { exact: true })).toBeVisible();
    // active tab is near the top, already in view — no scroll
    await expect.poll(() => treeScrollTop(page)).toBe(0);
  });

  test("follows focus: scrolls when a far-down tab becomes active", async ({ page }) => {
    await bootBackgroundAndSidebar(page, tallSeed(0));
    await expect.poll(() => treeScrollTop(page)).toBe(0);
    // activate a tab well below the fold; the sidebar should reveal it
    await fake(page, "activateTab", 270);
    await expect(page.locator(".row.active")).toHaveText("Tab 70");
    await expect.poll(() => treeScrollTop(page)).toBeGreaterThan(0);
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

  // Firefox spends the first click on an unfocused sidebar document focusing it
  // rather than activating what was clicked, so actions need a second click. We
  // can't reproduce that focus-eating headlessly (Playwright drives a focused
  // page), but we can assert the guard that defeats it: any pointer over the
  // sidebar reacquires window focus *only* while the document lacks it.
  test("reacquires window focus on pointer activity while unfocused", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();

    const calls = await page.evaluate(() => {
      const real = window.focus.bind(window);
      let focused: boolean;
      let n = 0;
      Object.defineProperty(document, "hasFocus", { configurable: true, value: () => focused });
      window.focus = () => {
        n++;
      };
      const row = document.querySelector(".row") as HTMLElement;
      const fire = (type: string) => row.dispatchEvent(new PointerEvent(type, { bubbles: true }));

      focused = true; // already focused: the guard must make these no-ops
      fire("pointerover");
      fire("pointerdown");
      const whileFocused = n;

      focused = false; // unfocused: both hooks must grab focus back
      fire("pointerover");
      const afterOver = n;
      fire("pointerdown");
      const afterDown = n;

      window.focus = real;
      delete (document as { hasFocus?: unknown }).hasFocus;
      return { whileFocused, afterOver, afterDown };
    });

    expect(calls.whileFocused).toBe(0); // never steals focus when we already hold it
    expect(calls.afterOver).toBe(1); // pointerover reacquires before the click
    expect(calls.afterDown).toBe(2); // pointerdown is the backstop
  });
});
