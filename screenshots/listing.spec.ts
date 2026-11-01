// Renders the AMO listing screenshots into docs/screenshots/. Run `pnpm screenshots`.
import { test, expect, type Page } from "@playwright/test";
import { readFile } from "node:fs/promises";
import { installFakeBrowser } from "../e2e/support/fake-browser";
import { liveSeed, savedSnapshot } from "./demo-tree";

const OUT = "docs/screenshots";

// The real sidebar page — its icon sprite and stylesheet — with the real background
// loaded first, so the two talk over the fake runtime bus as in the e2e harness.
async function bootSidebar(page: Page) {
  await page.addInitScript(installFakeBrowser, liveSeed);
  await page.route("**/sidebar/sidebar.html", async (route) => {
    const html = await readFile("dist/sidebar/sidebar.html", "utf8");
    await route.fulfill({
      contentType: "text/html",
      body: html.replace(
        '<script src="sidebar.js"></script>',
        '<script src="../background/background.js"></script><script src="sidebar.js"></script>'
      ),
    });
  });
  await page.goto("/sidebar/sidebar.html");
  await expect(page.getByText("The Rust Programming Language")).toBeVisible();

  const snapshot = savedSnapshot();
  page.once("filechooser", (fc) =>
    fc.setFiles({ name: "outline.json", mimeType: "application/json", buffer: Buffer.from(snapshot) })
  );
  // at sidebar width Import lives in the overflow menu
  await page.locator(".toolbar-more-summary").click();
  await page.locator("#import-menu").click();
  await expect(page.getByText("Trip to Kyoto")).toBeVisible();
  // every drawn favicon decoded
  await expect
    .poll(() =>
      page.locator(".favicon-img").evaluateAll((imgs) => imgs.every((i) => (i as HTMLImageElement).complete && (i as HTMLImageElement).naturalWidth > 0))
    )
    .toBe(true);
  await page.addStyleTag({ content: hideChromiumSearchCancel });
  await calm(page);
}

// Settled frames only: the row actions fade in and out.
const shoot = (page: Page, name: string, clip?: { x: number; y: number; width: number; height: number }) =>
  page.screenshot({ path: `${OUT}/${name}.png`, clip, animations: "disabled" });

// Chromium draws its own clear button in a search field; Firefox, which the
// listing shows, does not (the app's own clear button is beside it).
const hideChromiumSearchCancel = "#search::-webkit-search-cancel-button { display: none; }";

// No stray hover or focus: either reveals a row's action buttons.
async function calm(page: Page) {
  await page.mouse.move(5, 5);
  await page.evaluate(() => (document.activeElement as HTMLElement | null)?.blur());
}

// A drag's pointer is drawn by the OS, so a screenshot has neither it nor the
// dragged row's ghost. Draw both into the page: the ghost (a faded copy of the
// dragged row) and an arrow cursor with its tip at (x, y).
async function drawPointer(page: Page, dragged: string, x: number, y: number) {
  await page.evaluate(
    ([text, x, y]) => {
      const row = [...document.querySelectorAll<HTMLElement>(".row")].find((r) => r.textContent?.includes(text as string))!;
      const box = row.getBoundingClientRect();
      const ghost = row.cloneNode(true) as HTMLElement;
      // just the row's content: not its status border, subtree guides or dimming
      ghost.querySelectorAll(".guide-layer, .node-actions").forEach((e) => e.remove());
      ghost.classList.remove("dragging");
      Object.assign(ghost.style, {
        position: "fixed",
        left: `${(x as number) - 110}px`,
        top: `${(y as number) - box.height / 2}px`,
        width: `${box.width}px`,
        height: `${box.height}px`,
        opacity: "0.35",
        borderLeftColor: "transparent",
        background: "transparent",
        pointerEvents: "none",
        zIndex: "1000",
      });
      document.body.append(ghost);
      const cursor = document.createElement("div");
      cursor.innerHTML =
        '<svg xmlns="http://www.w3.org/2000/svg" width="22" height="33" viewBox="0 0 12 18">' +
        '<path d="M.5.5v15.2l3.6-3.5 2.5 5.6 2.4-1-2.5-5.5h5z" fill="#111" stroke="#fff" stroke-width="1" stroke-linejoin="round"/></svg>';
      Object.assign(cursor.style, {
        position: "fixed",
        left: `${x as number}px`,
        top: `${y as number}px`,
        pointerEvents: "none",
        zIndex: "1001",
        filter: "drop-shadow(0 1px 1.5px rgb(0 0 0 / 0.35))",
      });
      document.body.append(cursor);
    },
    [dragged, x, y] as const
  );
}

const rowOf = (page: Page, text: string) => page.locator(".row").filter({ hasText: text });

test("outline", async ({ page }) => {
  await bootSidebar(page);
  await shoot(page, "1-outline");
});

test("drag", async ({ page }) => {
  await bootSidebar(page);
  // mid-drag: the dragged row stays put, the guide line marks where it would land
  await rowOf(page, "Arashiyama").dispatchEvent("dragstart");
  const target = rowOf(page, "Kinkaku-ji");
  await target.dispatchEvent("dragover");
  await expect(page.locator(".drop-indicator")).toHaveCount(1);
  // and the row actions, as hovering any row reveals them
  await rowOf(page, "Learn Japanese").hover();
  const line = (await target.boundingBox())!;
  await drawPointer(page, "Arashiyama", 230, line.y);
  // close in on the Kyoto group, like the original listing's zoomed shot
  const top = (await rowOf(page, "Trip to Kyoto").boundingBox())!.y;
  const last = (await rowOf(page, "Astronomy Picture").boundingBox())!;
  await shoot(page, "2-drag", { x: 0, y: top, width: 430, height: last.y + last.height - top });
});

test("search", async ({ page }) => {
  await bootSidebar(page);
  // matches inside the collapsed Python group are found too
  await page.locator("#search").fill("iter");
  await expect(rowOf(page, "itertools")).toBeVisible();
  const last = (await page.locator(".row").last().boundingBox())!;
  await shoot(page, "3-search", { x: 0, y: 0, width: 430, height: last.y + last.height + 12 });
});
