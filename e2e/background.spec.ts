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

  test("a live tab close keeps the node as closed history", async ({ page }) => {
    await bootBackground(page, {
      windows: [{ id: 1, tabs: [{ id: 11, url: "http://a", title: "Alpha" }, { id: 12, url: "http://b", title: "Beta" }] }],
    });
    await expect.poll(() => readNodes(page).then((n) => n.length)).toBe(3);

    await fake(page, "closeTab", 11);
    // node count unchanged; the closed node is still present, now closed history
    // (its tab binding dropped, so no longer live)
    await expect
      .poll(async () => isLive((await readNodes(page)).find((n) => n.title === "Alpha")))
      .toBe(false);
    expect(await readNodes(page).then((n) => n.length)).toBe(3);
  });
});
