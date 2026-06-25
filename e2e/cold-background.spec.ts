import { test, expect } from "@playwright/test";
import { installFakeBrowser } from "./support/fake-browser";

// Reproduces the suspended-event-page race: the sidebar opens and fires its first
// GetView before the background is serving requests. The sidebar must recover once
// the background comes up — both via the background's unconditional post-boot ping
// and via the sidebar's own bounded retry — without the user having to reopen it.
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

test("sidebar recovers when the background starts after it", async ({ page }) => {
  await page.addInitScript(installFakeBrowser, seed);
  await page.goto("/blank.html");
  await page.addStyleTag({ path: "dist/sidebar/sidebar.css" });

  // Sidebar opens first — nothing is listening for its GetView yet.
  await page.addScriptTag({ path: "dist/sidebar/sidebar.js" });
  await page.waitForTimeout(300);
  expect(await page.locator("[role=treeitem]").count()).toBe(0);

  // The (woken) background finally comes up.
  await page.addScriptTag({ path: "dist/background/background.js" });

  // ...and the sidebar fills in without a reopen.
  await expect(page.getByText("Alpha")).toBeVisible();
  await expect(page.getByText("Beta")).toBeVisible();
});
