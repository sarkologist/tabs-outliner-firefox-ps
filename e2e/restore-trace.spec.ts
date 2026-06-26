// Throwaway: drives the exact reported scenario (single-tab restore of a
// non-imported closed group) against the fake browser and dumps the background
// trace. The fake is window-first, so this is the KNOWN-GOOD reference sequence
// to diff the real-Firefox console against. Delete once the bug is pinned.
import { test, expect } from "@playwright/test";
import { bootBackgroundAndSidebar, fake } from "./support/harness";

test("TRACE single-tab restore of a closed (non-imported) group", async ({ page }) => {
  const logs: string[] = [];
  page.on("console", (m) => {
    const t = m.text();
    if (t.includes("[trace")) logs.push(t);
  });

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

  logs.length = 0; // keep only the restore
  // restore a SINGLE tab (Alpha) by clicking its closed row title
  await page.locator('.row[data-status="closed"]').filter({ hasText: "Alpha" }).locator(".title").click();
  await page.waitForTimeout(600);

  console.log("\n========== RESTORE TRACE (fake browser, window-first) ==========");
  for (const l of logs) console.log(l);
  console.log("========== END ==========");
  const wins = await page.evaluate(() => (globalThis as any).__fake.listWindows());
  console.log("FINAL windows:", JSON.stringify(wins));
  console.log("================================================================\n");
});
