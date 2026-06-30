import { test, expect } from "@playwright/test";
import { bootBackground, readNodes, fake, isLive } from "./support/harness";

const titles = async (page: import("@playwright/test").Page) =>
  (await readNodes(page)).map((n) => n.title).sort();

test.describe("background owner", () => {
  test("seeds live windows and persists one record per node", async ({ page }) => {
    await bootBackground(page, {
      windows: [
        {
          id: 1,
          tabs: [
            { id: 11, url: "http://a", title: "Alpha", active: true },
            { id: 12, url: "http://b", title: "Beta" },
          ],
        },
      ],
    });
    // 1 window node + 2 tab nodes
    await expect.poll(() => readNodes(page).then((n) => n.length)).toBe(3);
    expect(await titles(page)).toEqual(["Alpha", "Beta", "Window"]);
  });

  test("a live tab open is persisted incrementally", async ({ page }) => {
    await bootBackground(page, { windows: [{ id: 1, tabs: [{ id: 11, url: "http://a", title: "Alpha" }] }] });
    await expect.poll(() => readNodes(page).then((n) => n.length)).toBe(2);

    await fake(page, "openTab", { id: 12, windowId: 1, url: "http://c", title: "Gamma" });
    await expect.poll(() => readNodes(page).then((n) => n.length)).toBe(3);
    expect(await titles(page)).toContain("Gamma");
  });

  test("dragging a tab out to a brand-new window re-homes it under a new window node", async ({ page }) => {
    await bootBackground(page, {
      windows: [{ id: 1, tabs: [{ id: 11, url: "http://a", title: "Alpha", active: true }, { id: 12, url: "http://b", title: "Beta" }] }],
    });
    await expect.poll(() => readNodes(page).then((n) => n.length)).toBe(3);

    // tear Beta off into a brand-new browser window (Firefox fires only onDetached)
    await fake(page, "tearOffTabToNewWindow", 12);

    // Beta now hangs under a NEW live window node — a distinct node from window 1
    await expect
      .poll(async () => {
        const nodes = await readNodes(page);
        const beta = nodes.find((n) => n.title === "Beta");
        const parent = nodes.find((n) => n.id === beta?.parent);
        return {
          betaLive: isLive(beta),
          parentIsNewWindow: parent != null && parent.windowId != null && parent.windowId !== 1,
        };
      })
      .toEqual({ betaLive: true, parentIsNewWindow: true });

    const nodes = await readNodes(page);
    const beta = nodes.find((n) => n.title === "Beta");
    const oldWin = nodes.find((n) => n.windowId === 1)!;
    // window 1 stays live, keeping only Alpha; Beta moved out from under it
    expect(beta.parent).not.toBe(oldWin.id);
    expect(oldWin.children).toEqual([nodes.find((n) => n.title === "Alpha")!.id]);
  });

  test("a browser move of a window's only tab elsewhere re-homes it, despite the source window closing", async ({ page }) => {
    await bootBackground(page, {
      windows: [
        { id: 1, tabs: [{ id: 11, url: "http://solo", title: "Solo", active: true }] },
        { id: 2, tabs: [{ id: 21, url: "http://other", title: "Other", active: true }] },
      ],
    });
    await expect.poll(() => readNodes(page).then((n) => n.length)).toBe(4);

    // the browser moves Solo into window 2: fires onDetached + onAttached, then —
    // since window 1 emptied — winRemoved(1), which races the async detach lookup
    await fake(page, "attachTab", 11, 2, 1);

    // Solo survives the racing close: it re-homes under window 2 and is NOT lost to
    // history; the emptied window 1 node is pruned
    await expect
      .poll(async () => {
        const nodes = await readNodes(page);
        const solo = nodes.find((n) => n.title === "Solo");
        const parent = nodes.find((n) => n.id === solo?.parent);
        return { soloLive: isLive(solo), parentWindowId: parent?.windowId ?? null, hasWindow1: nodes.some((n) => n.windowId === 1) };
      })
      .toEqual({ soloLive: true, parentWindowId: 2, hasWindow1: false });
  });

  test("a live tab close drops the node (never restored)", async ({ page }) => {
    await bootBackground(page, {
      windows: [{ id: 1, tabs: [{ id: 11, url: "http://a", title: "Alpha" }, { id: 12, url: "http://b", title: "Beta" }] }],
    });
    await expect.poll(() => readNodes(page).then((n) => n.length)).toBe(3);

    await fake(page, "closeTab", 11);
    // a freshly-opened tab closed in the browser is dropped, not kept as history:
    // Alpha's node is gone and the record count falls (window + Beta remain)
    await expect.poll(() => readNodes(page).then((ns) => ns.some((n) => n.title === "Alpha"))).toBe(false);
    expect(await readNodes(page).then((n) => n.length)).toBe(2);
  });

  test("stamps each tab with its node id via browser.sessions", async ({ page }) => {
    const tabValue = (p: import("@playwright/test").Page, tabId: number) =>
      p.evaluate((id) => (globalThis as any).__fake.tabValue(id, "outlinerNode"), tabId);
    await bootBackground(page, { windows: [{ id: 1, tabs: [{ id: 11, url: "http://a", title: "Alpha" }] }] });
    await expect.poll(() => readNodes(page).then((n) => n.length)).toBe(2);

    // boot stamps the seeded tab with the node id re-match bound it to
    const alpha = (await readNodes(page)).find((n) => n.title === "Alpha")!;
    await expect.poll(() => tabValue(page, 11)).toBe(alpha.id);

    // a newly-opened tab is stamped too (so it survives the next restart by identity)
    await fake(page, "openTab", { id: 12, windowId: 1, url: "http://b", title: "Beta" });
    await expect.poll(() => readNodes(page).then((ns) => ns.some((n) => n.title === "Beta"))).toBe(true);
    const beta = (await readNodes(page)).find((n) => n.title === "Beta")!;
    await expect.poll(() => tabValue(page, 12)).toBe(beta.id);
  });
});
