import { test, expect, type Page, type Locator } from "@playwright/test";
import { bootBackgroundAndSidebar, fake } from "./support/harness";

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

const rowOf = (page: Page, text: string) => page.locator(".row").filter({ hasText: text });
const groupRows = (page: Page) => page.locator("[role=treeitem]").filter({ hasText: "New group" });
// Row actions are hover-revealed (pointer-events:none until :hover), so hover the
// row before clicking one — as a real user does, and as the original's tests do.
const clickAction = async (row: Locator, btn: string) => {
  await row.hover();
  await row.locator(btn).click();
};
// land subsequent keystrokes on <body>, not a lingering focused button/input
const blur = (page: Page) => page.evaluate(() => (document.activeElement as HTMLElement | null)?.blur());

test.describe("undo / redo", () => {
  test("undo then redo a delete via the toolbar buttons", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();

    // a group has no live-tab side effects, so this isolates the tree edit
    await page.locator("#new-group").click();
    await expect(groupRows(page)).toHaveCount(1);

    await clickAction(groupRows(page), ".btn-delete");
    await expect(groupRows(page)).toHaveCount(0);

    await page.locator("#undo").click();
    await expect(groupRows(page)).toHaveCount(1); // back

    await page.locator("#redo").click();
    await expect(groupRows(page)).toHaveCount(0); // gone again
  });

  test("Ctrl+Z restores a deleted live tab as closed history; Ctrl+Shift+Z re-deletes", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Beta")).toBeVisible();

    await clickAction(rowOf(page, "Beta"), ".btn-delete");
    await expect(page.getByText("Beta")).toHaveCount(0);

    await blur(page);
    await page.keyboard.press("Control+z");
    // the tab returns, but greyed-out: its browser tab was closed by the delete,
    // so undo brings it back as restorable history, not a live tab
    await expect(page.locator('.row[data-status="closed"]').filter({ hasText: "Beta" })).toBeVisible();

    await page.keyboard.press("Control+Shift+z");
    await expect(page.getByText("Beta")).toHaveCount(0);
  });

  test("Ctrl+Z reverts a rename", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await clickAction(rowOf(page, "Alpha"), ".btn-rename");
    const input = page.locator(".rename-input");
    await input.fill("Renamed");
    await input.press("Enter");
    await expect(page.getByText("Renamed")).toBeVisible();

    await blur(page);
    await page.keyboard.press("Control+z");
    await expect(page.getByText("Alpha")).toBeVisible();
    await expect(page.getByText("Renamed")).toHaveCount(0);
  });

  test("Ctrl+Z while renaming edits the text field, not the outline", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();

    // put an undoable edit on the stack
    await page.locator("#new-group").click();
    await expect(groupRows(page)).toHaveCount(1);

    // type into the rename box; the shortcut handler must ignore keystrokes here.
    // locator.press targets the input directly (an editable element).
    await clickAction(rowOf(page, "Alpha"), ".btn-rename");
    await page.locator(".rename-input").press("Control+z");
    await page.waitForTimeout(200); // let any erroneous undo round-trip land

    // the outline edit was NOT undone — the keystroke belonged to the text field
    await expect(groupRows(page)).toHaveCount(1);
  });

  test("undo of a rename does not resurrect a tab closed in the meantime", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await clickAction(rowOf(page, "Alpha"), ".btn-rename");
    const input = page.locator(".rename-input");
    await input.fill("Renamed");
    await input.press("Enter");
    await expect(page.getByText("Renamed")).toBeVisible();

    // its browser tab closes (a live event) — the row greys out as history
    await fake(page, "closeTab", 11);
    await expect(page.locator('.row[data-status="closed"]').filter({ hasText: "Renamed" })).toBeVisible();

    // undo the rename: the title reverts, but the tab stays closed history —
    // it must NOT come back as a live tab bound to the (now gone) browser tab
    await blur(page);
    await page.keyboard.press("Control+z");
    await expect(page.locator('.row[data-status="closed"]').filter({ hasText: "Alpha" })).toBeVisible();
    await expect(page.locator('.row[data-status="live"]').filter({ hasText: "Alpha" })).toHaveCount(0);
  });

  test("undo with an empty history is a harmless no-op", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.locator("[role=treeitem]")).toHaveCount(3);
    await page.locator("#undo").click();
    await page.waitForTimeout(200);
    await expect(page.locator("[role=treeitem]")).toHaveCount(3); // unchanged
  });
});
