import { test, expect } from "@playwright/test";
import { installFakeBrowser } from "./support/fake-browser";
import { bootBackgroundAndSidebar, readNodes, fake } from "./support/harness";

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

  // A sidebar click while the page is suspended is the `runtime.sendMessage` that
  // wakes it. The request listener is registered before boot's first await, so
  // Firefox has a listener to deliver that message to; the request is held until
  // boot finishes and then served — exactly once, since a lost click retried by
  // hand (or by the sidebar) would restore the same window twice.
  test("a click sent while the page is still booting runs exactly once", async ({ context }) => {
    const seed = { windows: [{ id: 1, tabs: [{ id: 11, url: "http://a", title: "Alpha", active: true }] }] };
    const closedTab = (id: string, url: string) => ({
      id, kind: "tab", parent: "g", children: [], title: id, customTitle: null, url, favIconUrl: null,
      active: false, collapsed: false, createdAt: 0, closedAt: 0, tabId: null, windowId: null, sessionId: null,
    });

    // Session 1 saves a window and leaves the sidebar's boot cache behind, so the
    // next cold open paints it before the background can answer.
    const a = await context.newPage();
    await bootBackgroundAndSidebar(a, seed, {
      persisted: {
        nodes: [
          { ...closedTab("g", ""), kind: "group", parent: null, title: "Saved", url: null, children: ["Old1", "Old2"] },
          closedTab("Old1", "http://old1"),
          closedTab("Old2", "http://old2"),
        ],
        roots: ["g"],
      },
    });
    await expect(a.getByText("Saved")).toBeVisible();
    await expect.poll(() => a.evaluate(() => localStorage.getItem("tabsOutlinerBootWindow"))).toBeTruthy();
    await a.close();

    // Session 2: the background wakes and parks mid-boot; the sidebar paints from
    // its cache and the user clicks the saved window.
    const b = await context.newPage();
    await b.addInitScript(installFakeBrowser, seed);
    await b.goto("/blank.html");
    await fake(b, "blockGetAll");
    await b.addScriptTag({ path: "dist/background/background.js" });
    await expect.poll(() => b.evaluate(() => (globalThis as any).__fake.getAllCalls)).toBeGreaterThan(0);
    await b.addStyleTag({ path: "dist/sidebar/sidebar.css" });
    await b.addScriptTag({ path: "dist/sidebar/sidebar.js" });
    await b.locator(".row").filter({ hasText: "Saved" }).locator(".title").click();

    const created = () => b.evaluate(() => (globalThis as any).__fake.windowCreateLog.length as number);
    // held, not run against a model that isn't loaded yet
    await b.waitForTimeout(300);
    expect(await created()).toBe(0);

    await fake(b, "releaseGetAll");
    await expect.poll(created).toBe(1);
    await expect
      .poll(() => readNodes(b).then((ns) => ns.filter((n) => n.id.startsWith("Old") && n.tabId != null).length))
      .toBe(2);
    // and not a second time
    await b.waitForTimeout(500);
    expect(await created()).toBe(1);
  });

  // A boot that throws must not leave requests waiting forever: held and later
  // requests reject, as they did when nothing was listening.
  test("a failed boot rejects held and later requests", async ({ page }) => {
    await page.addInitScript(installFakeBrowser, { windows: [] });
    await page.goto("/blank.html");
    await page.evaluate(() => ((globalThis as any).__fake.failGetAll = true));
    await fake(page, "blockGetAll");
    await page.addScriptTag({ path: "dist/background/background.js" });
    await expect.poll(() => page.evaluate(() => (globalThis as any).__fake.getAllCalls)).toBeGreaterThan(0);

    const send = () =>
      page.evaluate(() =>
        Promise.race([
          (globalThis as any).browser.runtime
            .sendMessage({ kind: "req", body: { tag: "undo" } })
            .then(() => "resolved", () => "rejected"),
          new Promise((r) => setTimeout(() => r("pending"), 2000)),
        ])
      );
    // sent while boot is still running, so it is held on the gate...
    const held = send();
    await fake(page, "releaseGetAll");
    // ...and fails once boot does, like one sent afterwards
    expect(await held).toBe("rejected");
    expect(await send()).toBe("rejected");
  });
});
