import { test, expect, type Page } from "@playwright/test";
import { bootBackgroundAndSidebar, readNodes } from "./support/harness";

// The live windows + each tab's url, normalized for stable comparison.
const windowUrls = (page: Page) =>
  page.evaluate(() =>
    ((globalThis as any).__fake.listWindows() as Array<{ id: number; tabs: Array<{ url: string }> }>)
      .map((w) => w.tabs.map((t) => t.url).sort())
      .sort()
  );

test.describe("live-tab moves drive the browser", () => {
  test("dragging a live tab into another window moves the real tab there", async ({ page }) => {
    await bootBackgroundAndSidebar(page, {
      windows: [
        { id: 1, tabs: [{ id: 11, url: "http://a", title: "Alpha", active: true }, { id: 12, url: "http://b", title: "Beta" }] },
        { id: 2, tabs: [{ id: 21, url: "http://g", title: "Gamma", active: true }] },
      ],
    });
    await expect(page.getByText("Gamma")).toBeVisible();

    // drag Beta (window 1) onto Gamma (window 2): lands BEFORE Gamma
    await page.getByText("Beta").dragTo(page.getByText("Gamma"));

    // the REAL browser tab moved AND landed at the dropped position (before Gamma);
    // window 1 keeps Alpha
    await expect
      .poll(async () => {
        const ws: Array<{ id: number; tabs: Array<{ url: string }> }> = await page.evaluate(() => (globalThis as any).__fake.listWindows());
        return {
          w1: ws.find((w) => w.id === 1)!.tabs.map((t) => t.url),
          w2: ws.find((w) => w.id === 2)!.tabs.map((t) => t.url),
        };
      })
      .toEqual({ w1: ["http://a"], w2: ["http://b", "http://g"] });

    // and the tree re-settles from the events: Beta now hangs under window 2's node
    const nodes = await readNodes(page);
    const beta = nodes.find((n) => n.title === "Beta");
    const win2 = nodes.find((n) => n.windowId === 2);
    expect(beta.parent).toBe(win2.id);
  });

  test("dragging a live tab into a group makes the group go live as a new window", async ({ page }) => {
    await bootBackgroundAndSidebar(page, {
      windows: [{ id: 1, tabs: [{ id: 11, url: "http://a", title: "Alpha", active: true }, { id: 12, url: "http://b", title: "Beta" }] }],
    });
    await expect(page.getByText("Beta")).toBeVisible();

    // a fresh group at the top, then drag Beta into it
    await page.locator("#new-group").click();
    const group = page.locator("[role=treeitem]").filter({ hasText: "New group" });
    await expect(group).toHaveCount(1);
    await page.getByText("Beta").dragTo(group.locator(".title"));

    // a brand-new browser window now holds Beta; window 1 keeps Alpha
    await expect.poll(() => windowUrls(page)).toEqual([["http://a"], ["http://b"]]);

    // the group node itself went live (bound to the new window) and owns Beta
    const nodes = await readNodes(page);
    const grp = nodes.find((n) => n.title === "New group");
    expect(grp.windowId).not.toBeNull();
    const beta = nodes.find((n) => n.title === "Beta");
    expect(beta.parent).toBe(grp.id);
  });

  test("flattening a live window re-homes its tabs into a fresh window", async ({ page }) => {
    await bootBackgroundAndSidebar(page, {
      windows: [{ id: 1, tabs: [{ id: 11, url: "http://a", title: "Alpha", active: true }, { id: 12, url: "http://b", title: "Beta" }] }],
    });
    await expect(page.getByText("Alpha")).toBeVisible();

    // flatten the window row (its action is revealed on hover)
    const win = page.locator(".row").filter({ hasText: "Window" });
    await win.hover();
    await win.locator(".btn-flatten").click();

    // both tabs land in ONE fresh browser window; the old (now empty) window closed
    await expect.poll(async () => {
      const ws: Array<{ id: number; tabs: Array<{ url: string }> }> = await page.evaluate(() => (globalThis as any).__fake.listWindows());
      return { count: ws.length, hasOld: ws.some((w) => w.id === 1), urls: ws.flatMap((w) => w.tabs.map((t) => t.url)).sort() };
    }).toEqual({ count: 1, hasOld: false, urls: ["http://a", "http://b"] });
  });

  test("moving a window's only tab away prunes the emptied window", async ({ page }) => {
    await bootBackgroundAndSidebar(page, {
      windows: [
        { id: 1, tabs: [{ id: 11, url: "http://solo", title: "Solo", active: true }] },
        { id: 2, tabs: [{ id: 21, url: "http://other", title: "Other", active: true }] },
      ],
    });
    await expect(page.getByText("Other")).toBeVisible();

    // drag Solo (window 1's only tab) into window 2
    await page.getByText("Solo").dragTo(page.getByText("Other"));

    // window 1 emptied, so it's gone: one browser window, and no window-1 node left
    await expect.poll(() => page.evaluate(() => (globalThis as any).__fake.listWindows().length)).toBe(1);
    const nodes = await readNodes(page);
    expect(nodes.some((n) => n.windowId === 1)).toBe(false);
    expect(nodes.find((n) => n.title === "Solo").parent).toBe(nodes.find((n) => n.windowId === 2).id);
  });
});
