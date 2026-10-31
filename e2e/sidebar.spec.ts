import { test, expect, type Page, type Request } from "@playwright/test";
import { crc32, deflateSync } from "node:zlib";
import { bootBackgroundAndSidebar, fake, readNodes } from "./support/harness";

// A real, decodable `size`x`size` PNG of random pixels, as a `data:` url: noise
// does not compress, so even a modest size is far past what a node may keep.
function noisePng(size: number): string {
  const chunk = (type: string, data: Buffer) => {
    const head = Buffer.alloc(4);
    head.writeUInt32BE(data.length);
    const body = Buffer.concat([Buffer.from(type, "ascii"), data]);
    const crc = Buffer.alloc(4);
    crc.writeUInt32BE(crc32(body));
    return Buffer.concat([head, body, crc]);
  };
  const ihdr = Buffer.alloc(13);
  ihdr.writeUInt32BE(size, 0);
  ihdr.writeUInt32BE(size, 4);
  ihdr.set([8, 6, 0, 0, 0], 8); // 8-bit RGBA
  const raw = Buffer.alloc(size * (size * 4 + 1));
  for (let y = 0; y < size; y++) {
    const row = y * (size * 4 + 1);
    for (let i = 1; i <= size * 4; i++) raw[row + i] = Math.floor(Math.random() * 256);
    raw[row + 4 * size] = 255; // keep the last pixel opaque, so it is not blank
  }
  const png = Buffer.concat([
    Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]),
    chunk("IHDR", ihdr),
    chunk("IDAT", deflateSync(raw)),
    chunk("IEND", Buffer.alloc(0)),
  ]);
  return `data:image/png;base64,${png.toString("base64")}`;
}

// Every network request the page makes from now on (a `data:` image makes none).
function recordRequests(page: Page): Request[] {
  const seen: Request[] = [];
  page.on("request", (r) => {
    if (!r.url().startsWith("data:")) seen.push(r);
  });
  return seen;
}

// a node record in the shape `Model.Codec` persists
const record = (over: Record<string, unknown>) => ({
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

test.describe("sidebar view", () => {
  test("renders the snapshot tree from the background", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    await expect(page.getByText("Beta")).toBeVisible();
    // one window row + two tab rows
    await expect(page.locator("[role=treeitem]")).toHaveCount(3);
  });

  test("reflects a live tab open via a broadcast patch", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    await fake(page, "openTab", { id: 13, windowId: 1, url: "http://c", title: "Gamma" });
    await expect(page.getByText("Gamma")).toBeVisible();
  });

  test("places a tab opened from another tab directly under its window", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    await fake(page, "openTab", { id: 13, windowId: 1, openerTabId: 11, index: 1, url: "http://c", title: "Gamma" });
    await expect.poll(() => page.locator("[role=treeitem] .title").allInnerTexts()).toEqual(["Window", "Alpha", "Gamma", "Beta"]);
    const nodes = await readNodes(page);
    const win = nodes.find((n) => n.title === "Window");
    const gamma = nodes.find((n) => n.title === "Gamma");
    expect(gamma.parent).toBe(win.id);
  });

  test("a browser-closed fresh tab is removed from the view", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    await fake(page, "closeTab", 11);
    // never restored, so a browser close discards it rather than greying it out
    // (the greyed "save & close" rendering is covered in commands.spec)
    await expect(page.getByText("Alpha")).toHaveCount(0);
  });

  // a 1x1 transparent PNG: a favicon that really loads, with no network
  const PNG =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAAC0lEQVR42mNkAAIAAAoAAv/lxKUAAAAASUVORK5CYII=";

  const iconSeed = {
    windows: [
      {
        id: 1,
        tabs: [
          { id: 11, url: "http://a/page", title: "Alpha", active: true, favIconUrl: PNG },
          { id: 12, url: "http://b/page", title: "Beta" },
        ],
      },
    ],
  };

  test("rows show the page favicon and its url as a tooltip", async ({ page }) => {
    await bootBackgroundAndSidebar(page, iconSeed);
    await expect(page.getByText("Alpha")).toBeVisible();

    const alpha = page.locator(".row").filter({ hasText: "Alpha" });
    const img = alpha.locator(".favicon-img");
    await expect(img).toHaveAttribute("src", PNG);
    // it is a real image, not just an attribute
    await expect.poll(() => img.evaluate((e: HTMLImageElement) => e.naturalWidth)).toBeGreaterThan(0);
    // and it is not itself draggable — grabbing a row by its icon must drag the
    // row, not start an image drag
    await expect(img).toHaveAttribute("draggable", "false");
    // the url is the tooltip — the one thing a row has no room to show
    await expect(alpha.locator(".title")).toHaveAttribute("title", "http://a/page");
    await expect(alpha.locator(".favicon")).toHaveAttribute("title", "http://a/page");

    // no favicon reported: the slot keeps its place with a kind glyph
    const beta = page.locator(".row").filter({ hasText: "Beta" });
    await expect(beta.locator(".favicon-img")).toHaveCount(0);
    await expect(beta.locator(".favicon use")).toHaveAttribute("href", "#icon-globe");
    await expect(beta.locator(".title")).toHaveAttribute("title", "http://b/page");

    // a live window is a container: window glyph, and no url to show
    const win = page.locator(".row").filter({ hasText: "Window" });
    await expect(win.locator(".favicon use")).toHaveAttribute("href", "#icon-window");
    await expect(win.locator(".title")).not.toHaveAttribute("title", /./);
  });

  test("a favicon that fails to load falls back to the kind glyph", async ({ page }) => {
    // a saved page's icon url rots, and Firefox's upgrade-insecure-requests kills
    // an http-only one; either way the row must not lose its icon column
    await page.route("**/favicon.ico", (route) => route.abort());
    await bootBackgroundAndSidebar(page, {
      windows: [
        {
          id: 1,
          tabs: [{ id: 11, url: "http://a/page", title: "Alpha", active: true, favIconUrl: "/favicon.ico" }],
        },
      ],
    });
    const alpha = page.locator(".row").filter({ hasText: "Alpha" });
    await expect(alpha.locator(".favicon use")).toHaveAttribute("href", "#icon-globe");
    await expect(alpha.locator(".favicon-img")).toHaveCount(0);
  });

  test("a saved tab keeps its favicon; a group gets the folder glyph", async ({ page }) => {
    await bootBackgroundAndSidebar(page, iconSeed);
    const row = (text: string) => page.locator(".row").filter({ hasText: text });
    const act = async (text: string, btn: string) => {
      await row(text).hover();
      await row(text).locator(btn).click();
    };
    // the outliner's own close saves the row as closed history, icon and all —
    // the privacy-relevant case: its favicon is drawn for a page we are not on
    await act("Alpha", ".btn-close");
    await expect(page.locator('.row[data-status="closed"]')).toHaveCount(1);
    await expect(row("Alpha").locator(".favicon-img")).toHaveAttribute("src", PNG);

    // a container that is not a live window reads as a folder, not a window
    await act("Alpha", ".btn-group");
    const group = page.locator(".row.kind-group");
    await expect(group).toHaveCount(1);
    await expect(group.locator(".favicon use")).toHaveAttribute("href", "#icon-folder");
  });

  // The silent failure this guards: an over-large `runtime.sendMessage` *response*
  // resolves as `undefined` instead of rejecting (the Export bug — see
  // export.spec and `messageResponseLimitBytes`). Favicons ride in every view
  // window, and a tall full-size view at minimum zoom asks for ~200 rows, so
  // without a per-window budget the reply would vanish and the sidebar would just
  // stop painting. Icons captured now are capped at 4 KB, but a tree saved before
  // that keeps its icons as they were — up to 16 KB each, on closed rows.
  test("a favicon-heavy window still fits in one message", async ({ page }) => {
    const BIG_ICON = `data:image/png;base64,${"A".repeat(16_000)}`;
    const tabs = Array.from({ length: 120 }, (_, i) =>
      record({ id: `t${i}`, parent: "g", title: `Tab ${i}`, url: `http://t${i}/page`, favIconUrl: BIG_ICON, closedAt: 0 })
    );
    await bootBackgroundAndSidebar(
      page,
      { messageResponseLimitBytes: 256 * 1024, windows: [] },
      {
        persisted: {
          nodes: [record({ id: "g", kind: "group", title: "Saved", children: tabs.map((t) => t.id) }), ...tabs],
          roots: ["g"],
        },
      }
    );
    // the window arrived: a viewport's worth of 16 KB icons did not sink the reply
    await expect(page.locator("[role=treeitem]").first()).toBeVisible();
    expect(await page.locator("[role=treeitem]").count()).toBeGreaterThan(20);
    // rows past the budget keep everything but the icon (how many ride is pinned
    // in ViewSpec; these stand-in icons are not decodable images, so the DOM here
    // shows the load-failure glyph either way)
    expect(await page.locator(".favicon use").count()).toBeGreaterThan(0);
  });

  // Stored icons ---------------------------------------------------------------
  // A closed row is a page the user is not on: drawing it must not talk to that
  // site. It draws the bytes captured while the tab was live, or a glyph.

  test("a closed row draws its stored icon without a network request", async ({ page }) => {
    await bootBackgroundAndSidebar(page, iconSeed);
    const alpha = page.locator(".row").filter({ hasText: "Alpha" });
    await expect(alpha.locator(".favicon-img")).toHaveAttribute("src", PNG);
    const requests = recordRequests(page);
    await alpha.hover();
    await alpha.locator(".btn-close").click();
    await expect(page.locator('.row[data-status="closed"]')).toHaveCount(1);
    const img = page.locator('.row[data-status="closed"] .favicon-img');
    await expect(img).toHaveAttribute("src", PNG);
    await expect.poll(() => img.evaluate((e: HTMLImageElement) => e.naturalWidth)).toBeGreaterThan(0);
    // and it stays that way across a re-render
    await fake(page, "openTab", { id: 13, windowId: 1, url: "http://c", title: "Gamma" });
    await expect(page.getByText("Gamma")).toBeVisible();
    await expect(img).toHaveAttribute("src", PNG);
    expect(requests.map((r) => r.url())).toEqual([]);
  });

  test("a closed row never fetches the icon url its live tab reported", async ({ page }) => {
    let hits = 0;
    await page.route("https://icons.example/**", (route) => {
      hits++;
      return route.fulfill({ contentType: "image/png", body: Buffer.from(PNG.split(",")[1], "base64") });
    });
    await bootBackgroundAndSidebar(page, {
      windows: [
        {
          id: 1,
          tabs: [
            { id: 11, url: "http://a/page", title: "Alpha", active: true, favIconUrl: "https://icons.example/alpha.png" },
            { id: 12, url: "http://b/page", title: "Beta" },
          ],
        },
      ],
    });
    const alpha = page.locator(".row").filter({ hasText: "Alpha" });
    // live: the tab is already talking to that host, so its row may point there
    // (and this is how the test sees a request at all)
    await expect.poll(() => alpha.locator(".favicon-img").evaluate((e: HTMLImageElement) => e.naturalWidth)).toBeGreaterThan(0);
    expect(hits).toBeGreaterThan(0);
    const before = hits;
    await alpha.hover();
    await alpha.locator(".btn-close").click();
    await expect(page.locator('.row[data-status="closed"]')).toHaveCount(1);
    // closed, with no stored bytes: the kind glyph, not the url
    const closed = page.locator('.row[data-status="closed"]');
    await expect(closed.locator(".favicon-img")).toHaveCount(0);
    await expect(closed.locator(".favicon use")).toHaveAttribute("href", "#icon-globe");
    await fake(page, "openTab", { id: 13, windowId: 1, url: "http://c", title: "Gamma" });
    await expect(page.getByText("Gamma")).toBeVisible();
    expect(hits).toBe(before);
  });

  test("an imported tree's claimed icon urls are never fetched", async ({ page }) => {
    // a crafted export: a unique url per node at one host, so scrolling past a
    // row would tell that host the row was seen
    const beacon: string[] = [];
    await page.route("https://beacon.example/**", (route) => {
      beacon.push(route.request().url());
      return route.abort();
    });
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    const snapshot = JSON.stringify({
      nodes: [
        record({ id: "g1", kind: "group", children: ["t1", "t2"], title: "Crafted", favIconUrl: "https://beacon.example/g1.png" }),
        record({ id: "t1", parent: "g1", title: "Tracked", url: "http://t1", favIconUrl: "https://beacon.example/t1.png" }),
        record({ id: "t2", parent: "g1", title: "WithBytes", url: "http://t2", favIconUrl: PNG }),
      ],
      roots: ["g1"],
    });
    page.on("filechooser", (fc) =>
      fc.setFiles({ name: "outline.json", mimeType: "application/json", buffer: Buffer.from(snapshot) })
    );
    await page.locator("#import").click();
    await expect(page.getByText("Tracked")).toBeVisible();
    // imported bytes are drawn; an imported url is not even kept
    await expect(page.locator(".row").filter({ hasText: "WithBytes" }).locator(".favicon-img")).toHaveAttribute("src", PNG);
    const tracked = page.locator(".row").filter({ hasText: "Tracked" });
    await expect(tracked.locator(".favicon-img")).toHaveCount(0);
    await expect(tracked.locator(".favicon use")).toHaveAttribute("href", "#icon-globe");
    const nodes = await readNodes(page);
    expect(nodes.find((n) => n.title === "Tracked").favIconUrl).toBeNull();
    expect(nodes.find((n) => n.title === "Crafted").favIconUrl).toBeNull();
    expect(beacon).toEqual([]);
  });

  test("an oversized live icon is stored shrunk, and its closed row draws it", async ({ page }) => {
    // a 64px icon of noise is ~22 KB as a `data:` url — too big to keep on a node
    const BIG = noisePng(64);
    expect(BIG.length).toBeGreaterThan(16_384);
    const storedIcon = async (title: string) =>
      (await readNodes(page)).find((n) => n.title === title)?.favIconUrl ?? null;
    // one tab open at boot (the backfill path), one reported later (a live event)
    await bootBackgroundAndSidebar(page, {
      windows: [
        {
          id: 1,
          tabs: [
            { id: 11, url: "http://a/page", title: "Alpha", active: true, favIconUrl: BIG },
            { id: 12, url: "http://b/page", title: "Beta" },
          ],
        },
      ],
    });
    await expect(page.getByText("Beta")).toBeVisible();
    await fake(page, "updateTab", 12, { favIconUrl: BIG });
    for (const title of ["Alpha", "Beta"]) {
      await expect.poll(() => storedIcon(title)).toMatch(/^data:image\/png;base64,/);
      const stored = (await storedIcon(title)) as string;
      expect(stored.length).toBeLessThanOrEqual(4096);
    }
    const requests = recordRequests(page);
    const alpha = page.locator(".row").filter({ hasText: "Alpha" });
    await alpha.hover();
    await alpha.locator(".btn-close").click();
    const img = page.locator('.row[data-status="closed"] .favicon-img');
    await expect(img).toHaveAttribute("src", (await storedIcon("Alpha")) as string);
    // a real image, downscaled (never past the 32px a HiDPI slot needs)
    const width = await img.evaluate(async (e: HTMLImageElement) => (await e.decode(), e.naturalWidth));
    expect(width).toBeGreaterThan(0);
    expect(width).toBeLessThanOrEqual(32);
    expect(requests.map((r) => r.url())).toEqual([]);
  });

  test("hovering a row draws subtree guide lines, cleared on leave", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    // no guide lines are drawn until the pointer is over a row
    await expect(page.locator(".guide-line")).toHaveCount(0);

    // hovering a tab traces its connection up to the parent window: a vertical
    // segment on the tab and on the window, and one horizontal stub into the tab
    await page.locator(".row").filter({ hasText: "Alpha" }).hover();
    await expect(page.locator(".guide-vertical").first()).toBeVisible();
    await expect(page.locator(".guide-horizontal")).toHaveCount(1);

    // hovering the window connects it down to BOTH child tabs (two stubs)
    await page.locator(".row").filter({ hasText: "Window" }).hover();
    await expect(page.locator(".guide-horizontal")).toHaveCount(2);

    // leaving the tree clears the guides
    await page.locator("#search").hover();
    await expect(page.locator(".guide-line")).toHaveCount(0);
  });

  // A window whose active tab sits far below the fold, to exercise auto-scroll.
  const tallSeed = (activeIndex: number) => ({
    windows: [
      {
        id: 1,
        tabs: Array.from({ length: 80 }, (_, i) => ({
          id: 200 + i,
          url: `http://t${i}`,
          title: `Tab ${i}`,
          active: i === activeIndex,
        })),
      },
    ],
  });

  const treeScrollTop = (page: import("@playwright/test").Page) =>
    page.locator("#tree").evaluate((el) => (el as HTMLElement).scrollTop);

  test("on open, scrolls to this window's active tab when it's below the fold", async ({ page }) => {
    await bootBackgroundAndSidebar(page, tallSeed(70));
    // the active tab gets scrolled into view (and is the only .active row)
    await expect(page.locator(".row.active")).toHaveText("Tab 70");
    await expect.poll(() => treeScrollTop(page)).toBeGreaterThan(0);
  });

  test("on open, leaves the scroll alone when the active tab is already visible", async ({ page }) => {
    await bootBackgroundAndSidebar(page, tallSeed(0));
    await expect(page.getByText("Tab 0", { exact: true })).toBeVisible();
    // active tab is near the top, already in view — no scroll
    await expect.poll(() => treeScrollTop(page)).toBe(0);
  });

  // No active tab anywhere, so nothing to reveal — the open default alone decides
  // where we land. It should be the BOTTOM (new windows / live nodes collect there).
  const noActiveTall = {
    windows: [
      { id: 1, tabs: Array.from({ length: 100 }, (_, i) => ({ id: 200 + i, url: `http://t${i}`, title: `Tab ${i}` })) },
    ],
  };

  test("on open, defaults to the bottom of the tree", async ({ page }) => {
    await bootBackgroundAndSidebar(page, noActiveTall);
    await expect(page.getByText("Tab 99", { exact: true })).toBeVisible();
    await expect.poll(() => treeScrollTop(page)).toBeGreaterThan(0);
    // the top of the tree is scrolled past — not even mounted (virtualized)
    await expect(page.getByText("Tab 0", { exact: true })).toHaveCount(0);
  });

  test("follows focus: scrolls when a far-down tab becomes active", async ({ page }) => {
    await bootBackgroundAndSidebar(page, tallSeed(0));
    await expect.poll(() => treeScrollTop(page)).toBe(0);
    // activate a tab well below the fold; the sidebar should reveal it
    await fake(page, "activateTab", 270);
    await expect(page.locator(".row.active")).toHaveText("Tab 70");
    await expect.poll(() => treeScrollTop(page)).toBeGreaterThan(0);
  });

  test("collapse hides descendants (command round-trip, persisted)", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();
    // the window row is the only one with a toggle initially
    await page.locator(".toggle").first().click();
    await expect(page.getByText("Alpha")).toHaveCount(0);
    await expect(page.getByText("Beta")).toHaveCount(0);
    // expand again
    await page.locator(".toggle").first().click();
    await expect(page.getByText("Alpha")).toBeVisible();
  });

  // Firefox spends the first click on an unfocused sidebar document focusing it
  // rather than activating what was clicked, so actions need a second click. We
  // can't reproduce that focus-eating headlessly (Playwright drives a focused
  // page), but we can assert the guard that defeats it: a press reacquires window
  // focus only while the document lacks it — and a mere hover never does, so it
  // can't steal a page's keyboard focus when the pointer drifts over the sidebar.
  test("a press reacquires window focus while unfocused; hover never does", async ({ page }) => {
    await bootBackgroundAndSidebar(page, seed);
    await expect(page.getByText("Alpha")).toBeVisible();

    const calls = await page.evaluate(() => {
      const real = window.focus.bind(window);
      let focused: boolean;
      let n = 0;
      Object.defineProperty(document, "hasFocus", { configurable: true, value: () => focused });
      window.focus = () => {
        n++;
      };
      const row = document.querySelector(".row") as HTMLElement;
      const fire = (type: string) => row.dispatchEvent(new PointerEvent(type, { bubbles: true }));

      focused = true; // already focused: the guard makes the press a no-op
      fire("pointerdown");
      const pressWhileFocused = n;

      focused = false; // unfocused: hover must stay hands-off, the press grabs
      fire("pointerover");
      const hoverWhileUnfocused = n;
      fire("pointerdown");
      const pressWhileUnfocused = n;

      window.focus = real;
      delete (document as { hasFocus?: unknown }).hasFocus;
      return { pressWhileFocused, hoverWhileUnfocused, pressWhileUnfocused };
    });

    expect(calls.pressWhileFocused).toBe(0); // never steals focus when we hold it
    expect(calls.hoverWhileUnfocused).toBe(0); // hover must not steal page focus
    expect(calls.pressWhileUnfocused).toBe(1); // the press reacquires it
  });
});
