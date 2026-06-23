import { test, expect, type Page } from "@playwright/test";
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

const rowOf = (page: Page, text: string) => page.locator(".row").filter({ hasText: text });
const groupRows = (page: Page) => page.locator("[role=treeitem]").filter({ hasText: "New group" });
// land subsequent keystrokes on <body>, not a lingering focused button/input
const blur = (page: Page) => page.evaluate(() => (document.activeElement as HTMLElement | null)?.blur());

test.describe("undo / redo", () => {
  test("undo then redo a delete via the toolbar buttons", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();

    // a group has no live-tab side effects, so this isolates the tree edit
    await page.locator("#new-group").click();
    await expect(groupRows(page)).toHaveCount(1);

    await groupRows(page).locator(".btn-delete").click();
    await expect(groupRows(page)).toHaveCount(0);

    await page.locator("#undo").click();
    await expect(groupRows(page)).toHaveCount(1); // back

    await page.locator("#redo").click();
    await expect(groupRows(page)).toHaveCount(0); // gone again
  });

  test("Ctrl+Z restores a deleted live tab as closed history; Ctrl+Shift+Z re-deletes", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Beta")).toBeVisible();

    await rowOf(page, "Beta").locator(".btn-delete").click();
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
    await rowOf(page, "Alpha").locator(".btn-rename").click();
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
    await rowOf(page, "Alpha").locator(".btn-rename").click();
    await page.locator(".rename-input").press("Control+z");
    await page.waitForTimeout(200); // let any erroneous undo round-trip land

    // the outline edit was NOT undone — the keystroke belonged to the text field
    await expect(groupRows(page)).toHaveCount(1);
  });

  test("undo with an empty history is a harmless no-op", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.locator("[role=treeitem]")).toHaveCount(3);
    await page.locator("#undo").click();
    await page.waitForTimeout(200);
    await expect(page.locator("[role=treeitem]")).toHaveCount(3); // unchanged
  });
});
