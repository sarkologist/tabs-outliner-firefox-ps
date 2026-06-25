import { test, expect } from "@playwright/test";
import { installFakeBrowser } from "./support/fake-browser";
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

test("a cold open paints instantly from the cached top window (no background yet)", async ({ context }) => {
  // Session 1: a normal boot renders the tree and caches the top window.
  const a = await context.newPage();
  await bootBackgroundAndSidebar(a, seed);
  await expect(a.getByText("Alpha")).toBeVisible();
  await expect.poll(() => a.evaluate(() => localStorage.getItem("tabsOutlinerBootWindow"))).toBeTruthy();
  await a.close();

  // Session 2: a FRESH sidebar with NO background running (the suspended-event-page
  // case). It must still show content immediately, from the cache.
  const b = await context.newPage();
  await b.addInitScript(installFakeBrowser, { windows: [] });
  await b.goto("/blank.html");
  await b.addStyleTag({ path: "dist/sidebar/sidebar.css" });
  await b.addScriptTag({ path: "dist/sidebar/sidebar.js" }); // no background.js

  await expect(b.getByText("Alpha")).toBeVisible();
  await expect(b.getByText("Beta")).toBeVisible();
});
