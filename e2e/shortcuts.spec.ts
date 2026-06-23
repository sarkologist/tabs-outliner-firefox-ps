import { test, expect, type Page } from "@playwright/test";
import { bootBackgroundAndSidebar } from "./support/harness";
import { installFakeBrowser } from "./support/fake-browser";

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

// Options page with a fake browser injected, so browser.commands exists and the
// sidebar-toggle row becomes editable (rather than showing the fallback note).
async function bootOptionsWithCommands(page: Page): Promise<void> {
  await page.addInitScript(installFakeBrowser, { windows: [] });
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

  test("explains the browser-level sidebar-toggle shortcut", async ({ page }) => {
    await bootOptions(page);
    await expect(page.getByRole("heading", { name: "Toggle the sidebar" })).toBeVisible();
    await expect(page.getByText("Manage Extension Shortcuts")).toBeVisible();
  });
});

test.describe("manifest", () => {
  test("declares the sidebar-toggle command with per-platform keys", async ({ page }) => {
    const res = await page.request.get("/manifest.json");
    const manifest = await res.json();
    const key = manifest.commands?.["_execute_sidebar_action"]?.suggested_key;
    // Windows + macOS get a working default; Linux is intentionally left unset
    // (Ctrl+Shift+Y is Firefox's Downloads there), assignable via the browser UI.
    expect(key?.windows).toBe("Ctrl+Shift+Y");
    expect(key?.mac).toBe("Command+Shift+Y");
    expect(key?.default).toBeUndefined();
  });
});

test.describe("options page — sidebar toggle (commands API)", () => {
  const TOGGLE = "_execute_sidebar_action";
  const stored = (page: Page) =>
    page.evaluate((name) => (globalThis as any).__fake.commandShortcut(name), TOGGLE);

  test("shows the current browser shortcut", async ({ page }) => {
    await bootOptionsWithCommands(page);
    await expect(page.locator("#toggle .kbd")).toHaveText("Ctrl+Shift+Y");
  });

  test("records a new shortcut and writes it through the commands API", async ({ page }) => {
    await bootOptionsWithCommands(page);
    await page.locator("#toggle-change").click();
    await expect(page.locator("#toggle .recording")).toBeVisible();
    // Meta maps to "Command" on every platform (no Ctrl->MacCtrl variance), so
    // this is deterministic regardless of where the test runs.
    await page.keyboard.press("Meta+Shift+K");
    await expect(page.locator("#toggle .kbd")).toHaveText("Command+Shift+K");
    expect(await stored(page)).toBe("Command+Shift+K");
  });

  test("rejects a shortcut with no modifier and leaves the binding unchanged", async ({ page }) => {
    await bootOptionsWithCommands(page);
    await page.locator("#toggle-change").click();
    await page.keyboard.press("k"); // no primary modifier
    await expect(page.locator("#toggle-error")).toBeVisible();
    await expect(page.locator("#toggle-error")).toContainText("modifier");
    expect(await stored(page)).toBe("Ctrl+Shift+Y"); // unchanged
  });

  test("surfaces the browser's rejection of a reserved shortcut", async ({ page }) => {
    await bootOptionsWithCommands(page);
    await page.locator("#toggle-change").click();
    // valid format, but the fake marks Command+Shift+Q reserved (as Firefox might)
    await page.keyboard.press("Meta+Shift+Q");
    await expect(page.locator("#toggle-error")).toBeVisible();
    await expect(page.locator("#toggle-error")).toContainText("reserved");
    expect(await stored(page)).toBe("Ctrl+Shift+Y"); // unchanged
  });

  test("Reset restores the manifest default", async ({ page }) => {
    await bootOptionsWithCommands(page);
    await page.locator("#toggle-change").click();
    await page.keyboard.press("Meta+Shift+K");
    await expect(page.locator("#toggle .kbd")).toHaveText("Command+Shift+K");
    await page.locator("#toggle-reset").click();
    await expect(page.locator("#toggle .kbd")).toHaveText("Ctrl+Shift+Y");
    expect(await stored(page)).toBe("Ctrl+Shift+Y");
  });
});
