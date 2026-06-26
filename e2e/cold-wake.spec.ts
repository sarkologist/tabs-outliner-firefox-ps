import { test, expect } from "@playwright/test";
import { installFakeBrowser } from "./support/fake-browser";
import { readNodes, fake } from "./support/harness";

// A non-persistent background ("event") page is suspended when idle and woken by
// the very event that fires. For a link opened from another application that is
// the new tab's `tabs.onCreated`. The listeners must be registered synchronously
// at the top of boot — before the async open-DB / load / window-snapshot work —
// or Firefox never re-delivers that waking event to the freshly woken page and the
// link goes untracked.
//
// This drives that exact race deterministically: boot is held open AFTER it has
// snapshotted the windows (so the snapshot can't be what tracks the tab), the link
// opens, then boot finishes. The only way the tab can end up tracked is the live
// listener having buffered its event during boot.
test.describe("cold wake (event page)", () => {
  test("tracks a link opened while the page is still booting", async ({ page }) => {
    await page.addInitScript(installFakeBrowser, {
      windows: [{ id: 1, tabs: [{ id: 11, url: "http://a", title: "Alpha", active: true }] }],
    });
    await page.goto("/blank.html");

    // hold boot open on the windows snapshot
    await fake(page, "blockGetAll");
    await page.addScriptTag({ path: "dist/background/background.js" });
    // wait until boot has taken its snapshot (Alpha only) and parked on the gate,
    // so the link below arrives strictly after the snapshot
    await expect
      .poll(() => page.evaluate(() => (globalThis as any).__fake.getAllCalls))
      .toBeGreaterThan(0);

    // an external app opens a link in a new tab while the page is mid-boot
    await fake(page, "openTab", { id: 12, windowId: 1, url: "http://ext", title: "External" });

    // boot finishes; the buffered event drains through the normal dispatch path
    await fake(page, "releaseGetAll");

    // the externally-opened tab is tracked (its onCreated was buffered, not lost) —
    // exactly once (no duplicate against the boot snapshot) and live
    await expect
      .poll(() => readNodes(page).then((n) => n.filter((x) => x.title === "External").length))
      .toBe(1);
    const ext = (await readNodes(page)).find((n) => n.title === "External");
    expect(ext.tabId).toBe(12);
    expect(ext.parent).toBe((await readNodes(page)).find((n) => n.windowId === 1)?.id);
  });

  // The same waking event, two ways: the link's tab already exists when boot
  // snapshots the windows (so the snapshot seeds a node for it) AND its queued
  // onCreated is then replayed to the woken page and buffered. The buffered event
  // must not mint a second node.
  test("does not duplicate a tab present in both the boot snapshot and the backlog", async ({ page }) => {
    await page.addInitScript(installFakeBrowser, {
      windows: [
        {
          id: 1,
          tabs: [
            { id: 11, url: "http://a", title: "Alpha", active: true },
            { id: 12, url: "http://ext", title: "External" },
          ],
        },
      ],
    });
    await page.goto("/blank.html");

    await fake(page, "blockGetAll");
    await page.addScriptTag({ path: "dist/background/background.js" });
    await expect
      .poll(() => page.evaluate(() => (globalThis as any).__fake.getAllCalls))
      .toBeGreaterThan(0);

    // Firefox replays the queued onCreated to the woken page after it snapshotted
    await fake(page, "emitTabCreated", 12);
    await fake(page, "releaseGetAll");

    // exactly one node for the tab — seeded by the snapshot; its buffered onCreated
    // is a no-op (the already-tracking guard), not a duplicate
    await expect
      .poll(() => readNodes(page).then((n) => n.filter((x) => x.tabId === 12).length))
      .toBe(1);
    expect((await readNodes(page)).filter((n) => n.title === "External").length).toBe(1);
  });
});
