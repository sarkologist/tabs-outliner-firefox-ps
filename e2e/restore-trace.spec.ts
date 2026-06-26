// Drives the exact reported scenario (single-tab restore of a non-imported closed
// group) with tracing enabled, and asserts the lifecycle lands in the shared
// localStorage trace buffer the options page reads. The fake is window-first, so
// this also dumps the KNOWN-GOOD reference sequence to diff the real Firefox
// console/options-trace against. Delete once the bug is pinned.
import { test, expect } from "@playwright/test";
import { bootBackgroundAndSidebar, fake } from "./support/harness";

test("single-tab restore of a closed (non-imported) group writes its lifecycle to the trace buffer", async ({ page }) => {
  await page.addInitScript(() => localStorage.setItem("tabsOutlinerTraceEnabled", "1"));

  await bootBackgroundAndSidebar(page, {
    windows: [
      { id: 1, tabs: [{ id: 10, url: "http://keep", title: "Keep", active: true }] },
      {
        id: 2,
        tabs: [
          { id: 11, url: "http://a", title: "Alpha", active: true },
          { id: 12, url: "http://b", title: "Beta" },
        ],
      },
    ],
  });
  await expect(page.getByText("Alpha")).toBeVisible();

  // close window 2 -> a non-imported saved group with two closed tabs
  await fake(page, "closeWindow", 2);
  await expect(page.locator('[data-status="closed"]')).toHaveCount(3);

  await page.evaluate(() => localStorage.removeItem("tabsOutlinerTrace")); // keep only the restore
  // restore a SINGLE tab (Alpha) by clicking its closed row title
  await page.locator('.row[data-status="closed"]').filter({ hasText: "Alpha" }).locator(".title").click();
  await page.waitForTimeout(400);

  const buf = await page.evaluate(() => localStorage.getItem("tabsOutlinerTrace") || "");
  console.log("\n========== RESTORE TRACE (fake browser, window-first) ==========\n" + buf + "\n================================================================\n");

  // the lifecycle was captured: command -> browser action -> the window event binds
  // the EXISTING group node (this is the line that differs in real Firefox).
  expect(buf).toContain("CMD Activate");
  expect(buf).toContain("RUN CreateWindow");
  expect(buf).toContain("WindowOpened");
});
