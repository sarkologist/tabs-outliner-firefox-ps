import { test, expect } from "@playwright/test";
import { bootBackgroundAndSidebar } from "./support/harness";

const seed = {
  windows: [{ id: 1, tabs: [{ id: 11, url: "http://a", title: "Alpha", active: true }] }],
};

test("profiling: sidebar records the open profile and the options page shows it", async ({ context }) => {
  // Enable profiling before the sidebar boots (the flag lives in shared localStorage).
  const a = await context.newPage();
  await a.addInitScript(() => localStorage.setItem("tabsOutlinerProfileEnabled", "1"));
  await bootBackgroundAndSidebar(a, seed);
  await expect(a.getByText("Alpha")).toBeVisible();

  // The sidebar persisted a phase-broken-down open profile.
  await expect.poll(() => a.evaluate(() => localStorage.getItem("tabsOutlinerLastProfile"))).toBeTruthy();
  const parsed = JSON.parse((await a.evaluate(() => localStorage.getItem("tabsOutlinerLastProfile")))!);
  const names = (parsed.entries as Array<{ name: string }>).map((e) => e.name);
  for (const phase of ["boot.bootstrap", "boot.setup", "boot.fetch", "boot.server", "boot.decode", "boot.paint"]) {
    expect(names).toContain(phase);
  }

  // The options page (same context → shared localStorage) reflects the flag and the profile.
  const b = await context.newPage();
  await b.goto("/blank.html");
  await b.addStyleTag({ path: "dist/options/options.css" });
  await b.addScriptTag({ path: "dist/options/options.js" });
  await expect(b.locator("#profiling-enabled")).toBeChecked();
  await expect(b.locator("#profile")).toContainText("boot.fetch");

  // Toggling it off writes the flag through; Clear removes the stored profile.
  // (Halogen handles the click asynchronously, so poll for the effect.)
  await b.locator("#profiling-enabled").uncheck();
  await expect.poll(() => b.evaluate(() => localStorage.getItem("tabsOutlinerProfileEnabled"))).toBe("0");
  await b.locator("#profile-clear").click();
  await expect.poll(() => b.evaluate(() => localStorage.getItem("tabsOutlinerLastProfile"))).toBeNull();
});

test("profiling stays off by default (no profile written)", async ({ page }) => {
  await bootBackgroundAndSidebar(page, seed);
  await expect(page.getByText("Alpha")).toBeVisible();
  await page.waitForTimeout(150);
  expect(await page.evaluate(() => localStorage.getItem("tabsOutlinerLastProfile"))).toBeNull();
});
