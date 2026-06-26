// The options-page "Restore tracing" controls: toggle the (localStorage-backed)
// flag the background reads, and view / clear the captured trace buffer without
// opening devtools.
import { test, expect, type Page } from "@playwright/test";

async function bootOptions(page: Page, ls?: Record<string, string>): Promise<void> {
  if (ls) {
    await page.addInitScript((o) => {
      for (const k of Object.keys(o)) localStorage.setItem(k, o[k]);
    }, ls);
  }
  await page.goto("/blank.html");
  await page.addStyleTag({ path: "dist/options/options.css" });
  await page.addScriptTag({ path: "dist/options/options.js" });
}

test.describe("options: restore tracing", () => {
  test("shows the captured trace and reflects the persisted enabled flag", async ({ page }) => {
    await bootOptions(page, {
      tabsOutlinerTraceEnabled: "1",
      tabsOutlinerTrace: "[1] CMD Activate n4\n[2] EV WindowOpened win=5",
    });
    // toggle reflects the persisted flag, and the buffer is shown on load
    await expect(page.locator("#tracing-enabled")).toBeChecked();
    await expect(page.locator("#trace-text")).toHaveValue(/CMD Activate n4[\s\S]*WindowOpened win=5/);

    // Clear empties the buffer and the view
    await page.locator("#trace-clear").click();
    await expect(page.locator("#trace-text")).toHaveCount(0);
    await expect(page.getByText("No trace captured yet.")).toBeVisible();
    expect(await page.evaluate(() => localStorage.getItem("tabsOutlinerTrace"))).toBeNull();
  });

  test("the checkbox flips the persisted flag the background reads", async ({ page }) => {
    await bootOptions(page);
    await expect(page.locator("#tracing-enabled")).not.toBeChecked();
    await page.locator("#tracing-enabled").check();
    expect(await page.evaluate(() => localStorage.getItem("tabsOutlinerTraceEnabled"))).toBe("1");
    await page.locator("#tracing-enabled").uncheck();
    expect(await page.evaluate(() => localStorage.getItem("tabsOutlinerTraceEnabled"))).toBe("0");
  });
});
