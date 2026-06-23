import { test, expect, type Page } from "@playwright/test";
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

const fontScale = (page: Page) =>
  page.locator("#app").evaluate((el) => Number((el as HTMLElement).style.getPropertyValue("--font-scale")));

// Group rows in the tree only — scoped to [role=treeitem] so the toolbar's own
// "New group" button (same label) never counts.
const groupRows = (page: Page) => page.locator("[role=treeitem]").filter({ hasText: "New group" });

// Make sure the keydown lands on <body>, not a lingering focused input.
const blur = (page: Page) => page.evaluate(() => (document.activeElement as HTMLElement | null)?.blur());

// Load the standalone options page (no background/fake browser needed). Optional
// overrides are seeded into localStorage before the page boots so it reads them.
async function bootOptions(page: Page, overrides?: Record<string, string>): Promise<void> {
  if (overrides) {
    await page.addInitScript((o) => localStorage.setItem("shortcuts", JSON.stringify(o)), overrides);
  }
  await page.goto("/blank.html");
  await page.addStyleTag({ path: "dist/options/options.css" });
  await page.addScriptTag({ path: "dist/options/options.js" });
}

test.describe("sidebar keyboard shortcuts", () => {
  test("the New group shortcut (default n) adds a folder", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    await expect(groupRows(page)).toHaveCount(0);
    await blur(page);
    await page.keyboard.press("n");
    await expect(groupRows(page)).toHaveCount(1);
  });

  test("the zoom shortcuts (default = / -) change the font scale", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    await blur(page);
    await page.keyboard.press("=");
    await expect.poll(() => fontScale(page)).toBeGreaterThan(1);
    await page.keyboard.press("-");
    await page.keyboard.press("-");
    await expect.poll(() => fontScale(page)).toBeLessThan(1);
  });

  test("the focus-search shortcut (default /) focuses the search box", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    await blur(page);
    await page.keyboard.press("/");
    await expect(page.locator("#search")).toBeFocused();
  });

  test("shortcuts don't fire while typing in the search box", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    await page.locator("#search").click();
    await page.keyboard.type("n"); // would create a group if the shortcut fired
    await expect(page.locator("#search")).toHaveValue("n");
    await page.waitForTimeout(300); // let any erroneous command round-trip land
    await expect(groupRows(page)).toHaveCount(0);
  });

  test("auto-repeat (held key) does not re-fire a shortcut", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    await blur(page);
    // a repeat keydown (as the browser sends while a key is held) is ignored
    await page.evaluate(() =>
      document.dispatchEvent(new KeyboardEvent("keydown", { key: "n", repeat: true, bubbles: true }))
    );
    await page.waitForTimeout(300);
    await expect(groupRows(page)).toHaveCount(0);
    // a normal (non-repeat) press still works
    await page.keyboard.press("n");
    await expect(groupRows(page)).toHaveCount(1);
  });

  test("a user override re-binds a command, picked up live (no reload)", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    // remap New group from "n" to "g" after the sidebar is already running
    await page.evaluate(() => localStorage.setItem("shortcuts", JSON.stringify({ newGroup: "g" })));
    await blur(page);
    // the newly-bound key creates a group (proves the override is read live)...
    await page.keyboard.press("g");
    await expect(groupRows(page)).toHaveCount(1);
    // ...and the old default "n" is now inert
    await page.keyboard.press("n");
    await page.waitForTimeout(300);
    await expect(groupRows(page)).toHaveCount(1);
  });
});

test.describe("options page", () => {
  test("records a new binding and persists it", async ({ page }) => {
    await bootOptions(page);
    const row = page.locator("#shortcuts tr", { hasText: "New group" });
    await expect(row.locator(".kbd")).toHaveText("N"); // formatted default
    await row.getByRole("button", { name: "Change", exact: true }).click();
    await expect(row.locator(".recording")).toBeVisible();
    await page.keyboard.press("g");
    await expect(row.locator(".kbd")).toHaveText("G");
    const stored = await page.evaluate(() => JSON.parse(localStorage.getItem("shortcuts") || "{}"));
    expect(stored).toEqual({ newGroup: "g" });
  });

  test("Reset restores a command's default", async ({ page }) => {
    await bootOptions(page, { newGroup: "g" });
    const row = page.locator("#shortcuts tr", { hasText: "New group" });
    await expect(row.locator(".kbd")).toHaveText("G");
    await row.getByRole("button", { name: "Reset", exact: true }).click();
    await expect(row.locator(".kbd")).toHaveText("N");
    const stored = await page.evaluate(() => JSON.parse(localStorage.getItem("shortcuts") || "{}"));
    expect(stored).toEqual({});
  });

  test("warns when two actions share a binding", async ({ page }) => {
    // bind New group to "/", which collides with Focus search's default
    await bootOptions(page, { newGroup: "/" });
    await expect(page.locator(".warn")).toBeVisible();
    await expect(page.locator(".warn")).toContainText("more than one action");
  });
});
