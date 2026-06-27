import { test, expect, type Page } from "@playwright/test";
import { bootBackgroundAndSidebar, fake, readNodes } from "./support/harness";

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
const titles = (page: Page) => page.locator("[role=treeitem] .title").allInnerTexts();
const rowOf = (page: Page, text: string) => page.locator(".row").filter({ hasText: text });

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

  test("rename updates the title", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await clickAction(page, "Alpha", ".btn-rename");
    const input = page.locator(".rename-input");
    await input.fill("Renamed");
    await input.press("Enter");
    await expect(page.getByText("Renamed")).toBeVisible();
    await expect(page.getByText("Alpha")).toHaveCount(0);
  });

  test("new group adds a folder at the top", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await page.locator("#new-group").click();
    // scope to the tree: the toolbar's own "New group" button shares this text,
    // so an unscoped getByText is a strict-mode race (button vs. created node)
    await expect(page.locator("[role=treeitem]").filter({ hasText: "New group" })).toHaveCount(1);
  });

  test("clicking a closed tab restores it (re-binds the node, no duplicate)", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await fake(page, "closeTab", 11);
    await expect(page.locator('[data-status="closed"]')).toHaveCount(1);
    // click the (now closed) Alpha row -> Activate -> Restore
    await rowOf(page, "Alpha").locator(".title").click();
    await expect(page.locator('[data-status="closed"]')).toHaveCount(0);
    // still exactly window + 2 tabs (re-bound, not duplicated)
    await expect(page.locator("[role=treeitem]")).toHaveCount(3);
  });

  test("restore rebinds the node even when the recreated tab's url differs (redirect)", async ({ page }) => {
    await bootBackgroundAndSidebar(page, { ...seed, redirectCreatedTabs: true });
    await fake(page, "closeTab", 11); // close Alpha -> closed history
    await expect(page.locator('[data-status="closed"]')).toHaveCount(1);
    // restore it; the recreated tab's onCreated reports a different url than stored
    await rowOf(page, "Alpha").locator(".title").click();
    // the SAME node is rebound (matched by window, not url): no closed row, no dup
    await expect(page.locator('[data-status="closed"]')).toHaveCount(0);
    await expect(page.locator("[role=treeitem]")).toHaveCount(3);
  });

  test("a browser close of a restored tab drops it (does not re-save it)", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    // node id is stable across the close/restore round-trip (the SAME node rebinds)
    const alphaId = (await readNodes(page)).find((n) => n.title === "Alpha")!.id;
    await fake(page, "closeTab", 11); // close Alpha -> closed history
    await expect(page.locator('[data-status="closed"]')).toHaveCount(1);
    // restore it (re-binds the node, now flagged as restored-from-closed)
    await rowOf(page, "Alpha").locator(".title").click();
    await expect(page.locator('[data-status="closed"]')).toHaveCount(0);
    await expect(page.locator("[role=treeitem]")).toHaveCount(3);
    // the BROWSER now closes the restored tab -> the node is dropped, not re-saved
    const alphaTabId = (await readNodes(page)).find((n) => n.id === alphaId)!.tabId;
    await fake(page, "closeTab", alphaTabId);
    await expect.poll(() => readNodes(page).then((ns) => ns.some((n) => n.id === alphaId))).toBe(false);
    // Beta keeps window 1 alive, so only Alpha is gone: window + Beta remain
    await expect(page.locator("[role=treeitem]")).toHaveCount(2);
  });

  test("the outliner's own close still keeps a restored tab as history", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    const alphaId = (await readNodes(page)).find((n) => n.title === "Alpha")!.id;
    await fake(page, "closeTab", 11);
    await rowOf(page, "Alpha").locator(".title").click(); // restore
    await expect(page.locator('[data-status="closed"]')).toHaveCount(0);
    // close it from the outliner ("save & close"): kept as closed history, NOT dropped
    // (the restored row's title mirrors its url under the fake browser)
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
    expect(restored.tabs.map((t: any) => t.url).sort()).toEqual(["http://a", "http://b"]);
  });

  test("drag reorders siblings", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Beta")).toBeVisible();
    expect(await titles(page)).toEqual(["Window", "Alpha", "Beta"]);
    await page.getByText("Beta").dragTo(page.getByText("Alpha"));
    await expect.poll(() => titles(page)).toEqual(["Window", "Beta", "Alpha"]);
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

  test("move to top level pulls a nested node out, just after its window", async ({ page }) => {
    await bootBackgroundAndSidebar(page, twoWindows);
    await fake(page, "closeWindow", 1); // Alpha + Beta become closed history under the closed window
    await expect(page.locator('[data-status="closed"]')).toHaveCount(3);
    expect(await parentOf(page, "Beta")).not.toBeNull(); // nested to start

    await clickAction(page, "Beta", ".btn-to-top-level");

    // Beta is now a top-level node, landing just after its old window — so the live
    // "Keep" window stays last; Beta did not go to the very bottom.
    await expect.poll(() => parentOf(page, "Beta")).toBeNull();
    expect((await titles(page)).at(-1)).toBe("Keep");
  });

  test("move to bottom pulls a nested node to the very end", async ({ page }) => {
    await bootBackgroundAndSidebar(page, twoWindows);
    await fake(page, "closeWindow", 1);
    await expect(page.locator('[data-status="closed"]')).toHaveCount(3);

    await clickAction(page, "Beta", ".btn-to-bottom");

    // top-level now, and the last visible row
    await expect.poll(() => parentOf(page, "Beta")).toBeNull();
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
