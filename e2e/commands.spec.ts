import { test, expect, type Page } from "@playwright/test";
import { bootBackgroundAndSidebar, fake, readNodes } from "./support/harness";

// Click both closed Window rows back-to-back, without waiting for the first
// restore to settle (that concurrency is the point of the tests using this).
// Pin each row by node id FIRST: restoring one re-renders the list, so a
// positional locator resolved after that click (`nth(1)`) can find nothing and
// time out — a real flake these tests hit roughly a quarter of the time.
async function clickRestoreBoth(page: Page) {
  const closedWindowRows = page.locator('.row[data-status="closed"]').filter({ hasText: "Window" });
  await expect(closedWindowRows).toHaveCount(2);
  const ids = await closedWindowRows.evaluateAll((els) =>
    els.map((e) => e.getAttribute("data-node-id"))
  );
  for (const id of ids) await page.locator(`.row[data-node-id="${id}"] .title`).click();
}

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

const focusLog = (page: Page) => page.evaluate(() => (globalThis as any).__fake.focusLog as number[]);
const windowCreateLog = (page: Page) =>
  page.evaluate(() => (globalThis as any).__fake.windowCreateLog as Array<Record<string, unknown>>);
const titles = (page: Page) => page.locator("[role=treeitem] .title").allInnerTexts();
const rowOf = (page: Page, text: string) => page.locator(".row").filter({ hasText: text });
const windowUrls = (page: Page) =>
  page.evaluate(() =>
    ((globalThis as any).__fake.listWindows() as Array<{ id: number; tabs: Array<{ url: string }> }>)
      .map((w) => ({ id: w.id, urls: w.tabs.map((t) => t.url) }))
      .sort((a, b) => a.id - b.id)
  );
const node = (over: Record<string, unknown>) => ({
  id: "",
  kind: "tab",
  parent: null,
  children: [],
  title: "",
  customTitle: null,
  url: null,
  favIconUrl: null,
  active: false,
  collapsed: false,
  createdAt: 0,
  closedAt: null,
  tabId: null,
  windowId: null,
  sessionId: null,
  ...over,
});

// Row actions are revealed on hover (the original's affordance), so hover the row
// before clicking one — mirrors a real interaction and lets Playwright's pointer
// hit-test see the button (it is pointer-events:none until :hover).
const clickAction = async (page: Page, text: string, btn: string) => {
  const row = rowOf(page, text);
  await row.hover();
  await row.locator(btn).click();
};

test.describe("commands", () => {
  test("clicking a live tab focuses it", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await page.getByText("Beta").click();
    await expect.poll(() => focusLog(page)).toContain(12);
  });

  test("close keeps the node as greyed-out history", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await clickAction(page, "Beta", ".btn-close");
    await expect(page.locator('.row[data-status="closed"]').filter({ hasText: "Beta" })).toBeVisible();
  });

  test("delete removes the node entirely", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await clickAction(page, "Beta", ".btn-delete");
    await expect(page.getByText("Beta")).toHaveCount(0);
    await expect(page.locator("[role=treeitem]")).toHaveCount(2);
  });

  test("cut then paste moves a subtree after the target row", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await clickAction(page, "Alpha", ".btn-cut");
    await expect(rowOf(page, "Alpha")).toHaveClass(/cut/);

    await clickAction(page, "Beta", ".btn-paste");

    await expect.poll(() => titles(page)).toEqual(["Window", "Beta", "Alpha"]);
    await expect(rowOf(page, "Alpha")).not.toHaveClass(/cut/);
    await expect
      .poll(async () => {
        const nodes = await readNodes(page);
        const win = nodes.find((n) => n.title === "Window")!;
        return win.children.map((id: string) => nodes.find((n) => n.id === id)?.title);
      })
      .toEqual(["Beta", "Alpha"]);
  });

  test("paste is disabled for visible targets inside the cut subtree", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await clickAction(page, "Window", ".btn-cut");
    await rowOf(page, "Alpha").hover();
    await expect(rowOf(page, "Alpha").locator(".btn-paste")).toBeDisabled();
  });

  test("deleting the cut source clears the stale cut on the next paste attempt", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await clickAction(page, "Alpha", ".btn-cut");
    await clickAction(page, "Alpha", ".btn-delete");
    await expect(page.getByText("Alpha")).toHaveCount(0);

    await rowOf(page, "Beta").hover();
    await expect(rowOf(page, "Beta").locator(".btn-paste")).toBeEnabled();
    await rowOf(page, "Beta").locator(".btn-paste").click();
    await expect(rowOf(page, "Beta").locator(".btn-paste")).toHaveCount(0);
  });

  test("rename updates the title", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await clickAction(page, "Alpha", ".btn-rename");
    const input = page.locator(".rename-input");
    await input.fill("Renamed");
    await input.press("Enter");
    await expect(page.getByText("Renamed")).toBeVisible();
    await expect(page.getByText("Alpha")).toHaveCount(0);
  });

  test("group wraps a closed tab in a saved group", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await clickAction(page, "Beta", ".btn-close");
    await clickAction(page, "Beta", ".btn-group");

    const nodes = await readNodes(page);
    const beta = nodes.find((n) => n.title === "Beta")!;
    const group = nodes.find((n) => n.title === "Group")!;
    expect(beta.parent).toBe(group.id);
    expect(group.windowId ?? null).toBeNull();
    await expect(page.locator("[role=treeitem]").filter({ hasText: "Group" })).toHaveCount(1);
  });

  test("group wraps a live tab in a new browser window", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await clickAction(page, "Beta", ".btn-group");

    await expect
      .poll(() =>
        page.evaluate(() =>
          ((globalThis as any).__fake.listWindows() as Array<{ tabs: Array<{ url: string }> }>)
            .map((w) => w.tabs.map((t) => t.url).sort())
            .sort()
        )
      )
      .toEqual([["http://a"], ["http://b"]]);
    const nodes = await readNodes(page);
    const beta = nodes.find((n) => n.title === "Beta")!;
    const group = nodes.find((n) => n.title === "Group")!;
    expect(beta.parent).toBe(group.id);
    expect(group.windowId).not.toBeNull();
    expect(await windowCreateLog(page)).toContainEqual(expect.objectContaining({ type: "normal", tabId: 12 }));
  });

  test("clicking a closed tab restores it (re-binds the node, no duplicate)", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    // save Alpha as closed history via the outliner (a browser close would drop it)
    await clickAction(page, "Alpha", ".btn-close");
    await expect(page.locator('[data-status="closed"]')).toHaveCount(1);
    // click the (now closed) Alpha row -> Activate -> Restore
    await rowOf(page, "Alpha").locator(".title").click();
    await expect(page.locator('[data-status="closed"]')).toHaveCount(0);
    // still exactly window + 2 tabs (re-bound, not duplicated)
    await expect(page.locator("[role=treeitem]")).toHaveCount(3);
  });

  test("restore rebinds the node even when the recreated tab's url differs (redirect)", async ({ page }) => {
    await bootBackgroundAndSidebar(page, { ...seed, redirectCreatedTabs: true });
    await clickAction(page, "Alpha", ".btn-close"); // save Alpha as closed history
    await expect(page.locator('[data-status="closed"]')).toHaveCount(1);
    // restore it; the recreated tab's onCreated reports a different url than stored
    await rowOf(page, "Alpha").locator(".title").click();
    // the SAME node is rebound (matched by window, not url): no closed row, no dup
    await expect(page.locator('[data-status="closed"]')).toHaveCount(0);
    await expect(page.locator("[role=treeitem]")).toHaveCount(3);
  });

  test("a browser close of a fresh tab drops it (never saved)", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    // Beta was never restored or saved; closing it in the browser discards it
    await fake(page, "closeTab", 12);
    await expect.poll(() => page.locator("[role=treeitem]").count()).toBe(2); // window + Alpha
    await expect(page.getByText("Beta")).toHaveCount(0);
    await expect(page.locator('[data-status="closed"]')).toHaveCount(0);
  });

  test("a browser close of a restored tab keeps it as history", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    // node id is stable across the close/restore round-trip (the SAME node rebinds)
    const alphaId = (await readNodes(page)).find((n) => n.title === "Alpha")!.id;
    await clickAction(page, "Alpha", ".btn-close"); // save Alpha as closed history
    await rowOf(page, "Alpha").locator(".title").click(); // restore -> live, flagged
    await expect(page.locator('[data-status="closed"]')).toHaveCount(0);
    // the BROWSER now closes the restored tab -> kept as history (it belongs in the tree)
    const alphaTabId = (await readNodes(page)).find((n) => n.id === alphaId)!.tabId;
    await fake(page, "closeTab", alphaTabId);
    await expect(page.locator('.row[data-status="closed"]')).toHaveCount(1);
    await expect.poll(() => readNodes(page).then((ns) => ns.some((n) => n.id === alphaId))).toBe(true);
  });

  test("the outliner's own close keeps a tab as history (save & close)", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    const alphaId = (await readNodes(page)).find((n) => n.title === "Alpha")!.id;
    await clickAction(page, "Alpha", ".btn-close"); // save Alpha
    await rowOf(page, "Alpha").locator(".title").click(); // restore (row title becomes its url)
    await expect(page.locator('[data-status="closed"]')).toHaveCount(0);
    // close it from the outliner again ("save & close"): kept as closed history
    await clickAction(page, "http://a", ".btn-close");
    await expect(page.locator('.row[data-status="closed"]')).toHaveCount(1);
    await expect.poll(() => readNodes(page).then((ns) => ns.some((n) => n.id === alphaId))).toBe(true);
    await expect(page.locator("[role=treeitem]")).toHaveCount(3);
  });

  test("restoring a closed window re-opens it as a new browser window", async ({ page }) => {
    // two windows: window 1 (the one that stays open) and window 2 (to be closed)
    await bootBackgroundAndSidebar(page, {
      windows: [
        { id: 1, tabs: [{ id: 11, url: "http://keep", title: "Keep", active: true }] },
        {
          id: 2,
          tabs: [
            { id: 21, url: "http://a", title: "Alpha" },
            { id: 22, url: "http://b", title: "Beta" },
          ],
        },
      ],
    });
    await expect(page.getByText("Alpha")).toBeVisible();

    // close window 2 -> its window node + both tabs become closed history
    await fake(page, "closeWindow", 2);
    await expect(page.locator('[data-status="closed"]')).toHaveCount(3);
    expect(await page.evaluate(() => (globalThis as any).__fake.listWindows().length)).toBe(1);

    // restore it: click the closed Window row's title
    await page.locator('.row[data-status="closed"]').filter({ hasText: "Window" }).locator(".title").click();

    // everything goes live again, in place — no leftover closed rows, no duplicates
    await expect(page.locator('[data-status="closed"]')).toHaveCount(0);
    await expect(page.locator("[role=treeitem]")).toHaveCount(5);

    // a brand-new browser window holds the restored tabs; window 1 is untouched
    const windows = await page.evaluate(() => (globalThis as any).__fake.listWindows());
    expect(windows.length).toBe(2);
    const kept = windows.find((w: any) => w.id === 1);
    expect(kept.tabs.map((t: any) => t.url)).toEqual(["http://keep"]);
    const restored = windows.find((w: any) => w.id !== 1);
    expect(restored.tabs.map((t: any) => t.url)).toEqual(["http://a", "http://b"]);
    expect(await windowCreateLog(page)).toContainEqual(
      expect.objectContaining({ type: "normal", url: ["http://a", "http://b"] })
    );
  });

  test("restoring two closed windows with same-url tabs binds each to its own window (no cross-wire)", async ({ page }) => {
    // Two windows, each holding a tab with the SAME url (as when the reported bug
    // moved same-url tabs into different groups). Each restore must reopen into
    // its OWN new window and rebind its OWN node — not hijack the other's.
    await bootBackgroundAndSidebar(page, {
      windows: [
        { id: 1, tabs: [{ id: 11, url: "http://x", title: "First", active: true }] },
        { id: 2, tabs: [{ id: 21, url: "http://x", title: "Second" }] },
      ],
    });
    await expect(page.getByText("First")).toBeVisible();

    // close both windows -> two closed Window rows, each with its closed tab
    await fake(page, "closeWindow", 1);
    await fake(page, "closeWindow", 2);
    await expect(page.locator('[data-status="closed"]')).toHaveCount(4);
    expect(await page.evaluate(() => (globalThis as any).__fake.listWindows().length)).toBe(0);

    // restore both closed windows
    await clickRestoreBoth(page);

    await expect(page.locator('[data-status="closed"]')).toHaveCount(0);

    // two distinct live browser windows, each holding exactly its own single tab
    const windows = await page.evaluate(() => (globalThis as any).__fake.listWindows());
    expect(windows.length).toBe(2);
    for (const w of windows) expect(w.tabs.map((t: any) => t.url)).toEqual(["http://x"]);
    // no tabs got dumped together into one window (the cross-wire failure mode)
    expect(windows.every((w: any) => w.tabs.length === 1)).toBe(true);
  });

  test("restoring two same-url windows binds correctly even when tabs.onCreated beats windows.onCreated", async ({ page }) => {
    // Same two-window same-url case, but the browser reports each new window's
    // tab BEFORE its windows.onCreated. The background must hold those tab events
    // until the window binds, or the FIFO fallback would cross-wire the restores.
    await bootBackgroundAndSidebar(page, {
      windowCreateReportsTabBeforeWindow: true,
      windows: [
        { id: 1, tabs: [{ id: 11, url: "http://x", title: "First", active: true }] },
        { id: 2, tabs: [{ id: 21, url: "http://x", title: "Second" }] },
      ],
    });
    await expect(page.getByText("First")).toBeVisible();

    await fake(page, "closeWindow", 1);
    await fake(page, "closeWindow", 2);
    await expect(page.locator('[data-status="closed"]')).toHaveCount(4);

    await clickRestoreBoth(page);

    await expect(page.locator('[data-status="closed"]')).toHaveCount(0);
    const windows = await page.evaluate(() => (globalThis as any).__fake.listWindows());
    expect(windows.length).toBe(2);
    for (const w of windows) expect(w.tabs.map((t: any) => t.url)).toEqual(["http://x"]);
    expect(windows.every((w: any) => w.tabs.length === 1)).toBe(true);
  });

  test("restoring a window with a file:// tab reopens the openable tabs, not nothing", async ({ page }) => {
    // A window whose tabs include a file:// url the extension can't open. Batching
    // every url into one windows.create would be rejected whole (nothing opens);
    // restore must skip the un-openable tab and reopen the rest.
    await bootBackgroundAndSidebar(page, {
      windows: [
        {
          id: 1,
          tabs: [
            { id: 11, url: "https://a", title: "Alpha", active: true },
            { id: 12, url: "file:///Users/me/pic.webp", title: "Pic" },
            { id: 13, url: "https://c", title: "Gamma" },
          ],
        },
      ],
    });
    await expect(page.getByText("Alpha")).toBeVisible();

    await fake(page, "closeWindow", 1);
    await expect(page.locator('[data-status="closed"]')).toHaveCount(4); // window + 3 tabs

    await page.locator('.row[data-status="closed"]').filter({ hasText: "Window" }).locator(".title").click();

    // the window reopens with the two openable tabs (not nothing); the file:// tab
    // stays as closed history (so one closed row — "Pic" — remains)
    await expect(page.locator('.row[data-status="closed"]').filter({ hasText: "Pic" })).toHaveCount(1);
    await expect.poll(() => page.evaluate(() => (globalThis as any).__fake.listWindows().length)).toBe(1);
    const windows = await page.evaluate(() => (globalThis as any).__fake.listWindows());
    expect(windows[0].tabs.map((t: any) => t.url)).toEqual(["https://a", "https://c"]);
  });

  test("a rejected windows.create leaves the window restorable instead of stuck", async ({ page }) => {
    // The runtime half of the WindowCreateFailed contract: a rejected create fires
    // no onCreated, so nothing consumes the container's pending-window entry. Left
    // queued, Command.restore filters the container — and every tab under it — out
    // as already-in-flight, forever, with no error surfaced anywhere. This drives
    // the real background: the compensation has to come from runActions.
    await bootBackgroundAndSidebar(page, {
      rejectWindowCreateUrlsContaining: ["poison"],
      windows: [
        {
          id: 1,
          tabs: [
            { id: 11, url: "http://a", title: "Alpha", active: true },
            { id: 12, url: "http://poison", title: "Poison" },
          ],
        },
      ],
    });
    await expect.poll(() => titles(page)).toEqual(["Window", "Alpha", "Poison"]);

    await fake(page, "closeWindow", 1);
    await expect(page.locator('[data-status="closed"]')).toHaveCount(3);

    const creates = () =>
      page.evaluate(() => (globalThis as any).__fake.windowCreateLog.length as number);
    const windowRow = page.locator('.row[data-status="closed"]').filter({ hasText: "Window" });

    // Absolute counts, and wait for the first create to land before clicking again:
    // sampling a baseline mid-flight would let the FIRST create satisfy a
    // "one more than before" assertion, passing even with no compensation at all.
    await windowRow.locator(".title").click();
    await expect.poll(creates).toBe(1);
    // the create was rejected, so nothing came back
    await expect.poll(() => page.evaluate(() => (globalThis as any).__fake.listWindows().length)).toBe(0);
    await expect(page.locator('[data-status="closed"]')).toHaveCount(3);

    // ...and the retraction ran, so a retry actually reaches the browser again
    // (before the fix, every later click produced no windows.create at all)
    await windowRow.locator(".title").click();
    await expect.poll(creates).toBe(2);
  });

  test("restoring a closed window restores its tabs in order", async ({ page }) => {
    await bootBackgroundAndSidebar(page, {
      windows: [
        {
          id: 1,
          tabs: [
            { id: 11, url: "http://a", title: "Alpha", active: true },
            { id: 12, openerTabId: 11, url: "http://b", title: "Beta" },
            { id: 13, url: "http://c", title: "Gamma" },
          ],
        },
      ],
    });
    await expect.poll(() => titles(page)).toEqual(["Window", "Alpha", "Beta", "Gamma"]);

    await fake(page, "closeWindow", 1);
    await expect(page.locator('[data-status="closed"]')).toHaveCount(4);
    expect(await page.evaluate(() => (globalThis as any).__fake.listWindows().length)).toBe(0);

    await page.locator('.row[data-status="closed"]').filter({ hasText: "Window" }).locator(".title").click();

    await expect(page.locator('[data-status="closed"]')).toHaveCount(0);
    const windows = await page.evaluate(() => (globalThis as any).__fake.listWindows());
    expect(windows.length).toBe(1);
    expect(windows[0].tabs.map((t: any) => t.url)).toEqual(["http://a", "http://b", "http://c"]);
  });

  test("restoring imported history descends through tabs but stops at groups", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    const snapshot = JSON.stringify({
      nodes: [
        node({ id: "g1", kind: "group", title: "ImportedGroup", children: ["t1", "g2", "t3"] }),
        node({ id: "t1", kind: "tab", parent: "g1", title: "ParentTab", url: "http://parent", children: ["t2"] }),
        node({ id: "t2", kind: "tab", parent: "t1", title: "ChildTab", url: "http://child" }),
        node({ id: "g2", kind: "group", parent: "g1", title: "NestedGroup", children: ["tHidden"] }),
        node({ id: "tHidden", kind: "tab", parent: "g2", title: "HiddenTab", url: "http://hidden" }),
        node({ id: "t3", kind: "tab", parent: "g1", title: "SiblingTab", url: "http://sibling" }),
      ],
      roots: ["g1"],
    });
    page.on("filechooser", (fc) =>
      fc.setFiles({ name: "nested-history.json", mimeType: "application/json", buffer: Buffer.from(snapshot) })
    );
    await page.locator("#import").click();
    await expect(page.getByText("ImportedGroup")).toBeVisible();

    await rowOf(page, "ImportedGroup").locator(".title").click();

    await expect.poll(() => windowUrls(page).then((ws) => ws.map((w) => w.urls))).toEqual([
      ["http://a", "http://b"],
      ["http://parent", "http://child", "http://sibling"],
    ]);
    const nodes = await readNodes(page);
    expect(nodes.find((n) => n.title === "ParentTab")?.tabId).not.toBeNull();
    expect(nodes.find((n) => n.title === "ChildTab")?.tabId).not.toBeNull();
    expect(nodes.find((n) => n.title === "SiblingTab")?.tabId).not.toBeNull();
    expect(nodes.find((n) => n.title === "HiddenTab")?.tabId ?? null).toBeNull();
  });

  test("drag reorders siblings", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Beta")).toBeVisible();
    expect(await titles(page)).toEqual(["Window", "Alpha", "Beta"]);
    await page.getByText("Beta").dragTo(page.getByText("Alpha"));
    await expect.poll(() => titles(page)).toEqual(["Window", "Beta", "Alpha"]);
    const windows = await page.evaluate(() => (globalThis as any).__fake.listWindows());
    expect(windows[0].tabs.map((t: any) => t.url)).toEqual(["http://b", "http://a"]);
  });

  test("shows a drop preview that tracks the landing spot and clears on drop", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    // nothing until a drag is in progress
    await expect(page.locator(".drop-indicator")).toHaveCount(0);

    await rowOf(page, "Beta").dispatchEvent("dragstart");
    // over a tab: lands before it
    await rowOf(page, "Alpha").dispatchEvent("dragover");
    await expect(page.locator(".drop-indicator")).toHaveCount(1);
    const overTab = await page.locator(".drop-indicator").getAttribute("style");

    // over a different row: the preview moves to the new landing spot
    await rowOf(page, "Window").dispatchEvent("dragover");
    await expect(page.locator(".drop-indicator")).toHaveCount(1);
    expect(await page.locator(".drop-indicator").getAttribute("style")).not.toBe(overTab);

    // the dragged row is dimmed while dragging
    await expect(rowOf(page, "Beta")).toHaveClass(/dragging/);

    await rowOf(page, "Alpha").dispatchEvent("drop");
    await expect(page.locator(".drop-indicator")).toHaveCount(0);
  });

  test("dragging a node downward past a sibling lands it before the drop target", async ({ page }) => {
    await bootBackgroundAndSidebar(page, {
      windows: [
        {
          id: 1,
          tabs: [
            { id: 11, url: "http://a", title: "A", active: true },
            { id: 12, url: "http://b", title: "B" },
            { id: 13, url: "http://c", title: "C" },
          ],
        },
      ],
    });
    await expect(page.getByText("C", { exact: true })).toBeVisible();
    expect(await titles(page)).toEqual(["Window", "A", "B", "C"]);
    // drag A down onto C: it must land immediately BEFORE C, i.e. [B, A, C]
    await page.getByText("A", { exact: true }).dragTo(page.getByText("C", { exact: true }));
    await expect.poll(() => titles(page)).toEqual(["Window", "B", "A", "C"]);
  });
});

// "Move to top level" / "Move to bottom" pull a node out to the root. They are
// offered on every kind: a non-live node moves purely in the tree, while a live tab
// (which can't sit bare at the root) is promoted into its own new window.
test.describe("move to top level / bottom", () => {
  // window 1 (Alpha, Beta) is closed to leave its tabs as nested history; window 2
  // (Keep) stays live as the last top-level node, so "after the window" (top level)
  // and "the very bottom" are distinguishable positions.
  const twoWindows = {
    windows: [
      {
        id: 1,
        tabs: [
          { id: 11, url: "http://a", title: "Alpha", active: true },
          { id: 12, url: "http://b", title: "Beta" },
        ],
      },
      { id: 2, tabs: [{ id: 21, url: "http://keep", title: "Keep", active: true }] },
    ],
  };

  // The persisted parent of the (unique) node with this title — null once top-level.
  const parentOf = async (page: Page, title: string): Promise<string | null> => {
    const nodes = await readNodes(page);
    return nodes.find((n) => n.title === title)?.parent ?? null;
  };

  test("the move buttons are offered on a live tab (promotes it into a new window)", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed); // one live window: Alpha (active), Beta
    await rowOf(page, "Alpha").hover();
    await expect(rowOf(page, "Alpha").locator(".btn-to-top-level")).toHaveCount(1);
    await expect(rowOf(page, "Alpha").locator(".btn-to-bottom")).toHaveCount(1);

    await clickAction(page, "Alpha", ".btn-to-top-level");

    // the real tab is promoted into its own brand-new browser window; window 1 keeps Beta
    await expect
      .poll(() =>
        page.evaluate(() =>
          ((globalThis as any).__fake.listWindows() as Array<{ tabs: Array<{ url: string }> }>)
            .map((w) => w.tabs.map((t) => t.url).sort())
            .sort()
        )
      )
      .toEqual([["http://a"], ["http://b"]]);
  });

  // a closed tab can't sit bare at the root, so promoting one wraps it in a fresh
  // top-level group (closing the parentless-root-tab restore gap).
  const wrappedAtTopLevel = async (page: Page, title: string): Promise<boolean> => {
    const nodes = await readNodes(page);
    const node = nodes.find((n) => n.title === title);
    const parent = node?.parent ? nodes.find((n) => n.id === node.parent) : null;
    return parent != null && (parent.parent ?? null) === null; // parent is a top-level group
  };

  test("move to top level pulls a nested node out, just after its window (tab wrapped)", async ({ page }) => {
    await bootBackgroundAndSidebar(page, twoWindows);
    await fake(page, "closeWindow", 1); // Alpha + Beta become closed history under the closed window
    await expect(page.locator('[data-status="closed"]')).toHaveCount(3);
    expect(await parentOf(page, "Beta")).not.toBeNull(); // nested to start

    await clickAction(page, "Beta", ".btn-to-top-level");

    // Beta is wrapped in a new top-level group landing just after its old window — so
    // the live "Keep" window stays last; Beta did not go to the very bottom.
    await expect.poll(() => wrappedAtTopLevel(page, "Beta")).toBe(true);
    expect((await titles(page)).at(-1)).toBe("Keep");
  });

  test("move to bottom pulls a nested node to the very end (tab wrapped)", async ({ page }) => {
    await bootBackgroundAndSidebar(page, twoWindows);
    await fake(page, "closeWindow", 1);
    await expect(page.locator('[data-status="closed"]')).toHaveCount(3);

    await clickAction(page, "Beta", ".btn-to-bottom");

    // wrapped in a top-level group at the very end, so Beta is the last visible row
    await expect.poll(() => wrappedAtTopLevel(page, "Beta")).toBe(true);
    expect((await titles(page)).at(-1)).toBe("Beta");
  });

  test("move to bottom is offered on a non-last top-level node (top level is not)", async ({ page }) => {
    await bootBackgroundAndSidebar(page, twoWindows);
    await fake(page, "closeWindow", 1); // closed window is now a non-last top-level node
    const closedWindow = page.locator('.row[data-status="closed"]').filter({ hasText: "Window" });
    await closedWindow.hover();
    await expect(closedWindow.locator(".btn-to-bottom")).toHaveCount(1);
    await expect(closedWindow.locator(".btn-to-top-level")).toHaveCount(0); // already top-level

    await closedWindow.locator(".btn-to-bottom").click();
    // it moved below the live "Keep" window, taking its Alpha/Beta subtree with it
    await expect.poll(() => titles(page)).toEqual(["Window", "Keep", "Window", "Alpha", "Beta"]);
  });
});
