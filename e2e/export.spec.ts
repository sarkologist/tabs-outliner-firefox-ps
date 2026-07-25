// Export at scale — the case a small-tree test cannot see.
//
// Export used to be answered by returning the whole snapshot from the background
// over `runtime.sendMessage`, and the sidebar wrote whatever came back. That works
// on a two-tab tree and corrupts a real one: past some size Firefox does not
// deliver the reply, and the way it declines is the trap — the sender's promise
// RESOLVES WITH `undefined` instead of rejecting. So `attempt` saw success,
// `JSON.stringify(undefined)` gave `undefined`, `new Blob([undefined])`
// stringified it, and the user got a plausible `grove.json` whose entire contents
// were the seven characters "undefined". No error, anywhere. (Found on a real
// 19,451-node / 13.4 MB tree, 4.5 MB of it base64 `data:` favicons.)
//
// The fix is structural, not defensive: the payload never crosses a context. The
// background writes the file itself via `browser.downloads`, and the reply is a
// one-field ack. `messageResponseLimitBytes` holds it to that here — with the
// ceiling well below the snapshot, the pre-fix code cannot pass.
//
// The asymmetry that makes this the right shape: a GetView reply is bounded by the
// viewport (~50 rows, and ViewRow carries no url or favicon), so the view protocol
// is unaffected by tree size. Only the export payload ever scaled with it.

import { test, expect, type Page } from "@playwright/test";
import { installFakeBrowser } from "./support/fake-browser";

const GROUPS = 40;
const TABS_PER_GROUP = 50;
const TOTAL_NODES = GROUPS * (TABS_PER_GROUP + 1);

// ~1.5 KB of base64 on every 4th tab, mirroring the `data:` favicons that
// dominate a real tree's bytes.
const FAVICON = `data:image/png;base64,${"A".repeat(1500)}`;
const FAVICON_EVERY = 4;
const FAVICON_NODES = GROUPS * Math.ceil(TABS_PER_GROUP / FAVICON_EVERY);

// Far above any legitimate reply (a view window is ~15 KB), far below the
// snapshot this tree serializes to (~1.3 MB, asserted below).
const RESPONSE_LIMIT = 256 * 1024;

// Write the tree straight into IndexedDB, in the same two stores the background
// owns ("nodes": one JSON string per node; "meta".roots: the root list, also a
// JSON string), so the background boots with it already loaded — no import
// round-trip to pay for.
function seedLargeTree(page: Page) {
  return page.evaluate(
    ([groups, tabsPerGroup, favicon, faviconEvery]) =>
      new Promise<number>((resolve, reject) => {
        const base = {
          id: "",
          kind: "tab",
          parent: null as string | null,
          children: [] as string[],
          title: "",
          customTitle: null,
          url: null as string | null,
          favIconUrl: null as string | null,
          active: false,
          collapsed: false,
          createdAt: 0,
          closedAt: null as number | null,
          tabId: null,
          windowId: null,
          sessionId: null,
        };
        const records: Array<[string, string]> = [];
        const roots: string[] = [];
        for (let g = 0; g < groups; g++) {
          const gid = `g${g}`;
          const children: string[] = [];
          for (let t = 0; t < tabsPerGroup; t++) {
            const tid = `g${g}t${t}`;
            children.push(tid);
            records.push([
              tid,
              JSON.stringify({
                ...base,
                id: tid,
                kind: "tab",
                parent: gid,
                title: `Saved tab ${g}/${t} — about as long as a real page title runs`,
                url: `https://example.test/${g}/${t}?q=padding-to-a-realistic-length`,
                favIconUrl: t % faviconEvery === 0 ? favicon : null,
                closedAt: 1,
              }),
            ]);
          }
          roots.push(gid);
          records.push([
            gid,
            JSON.stringify({ ...base, id: gid, kind: "group", children, title: `Saved window ${g}` }),
          ]);
        }
        const req = indexedDB.open("tabs-outliner", 1);
        req.onupgradeneeded = () => {
          const db = req.result;
          if (!db.objectStoreNames.contains("nodes")) db.createObjectStore("nodes");
          if (!db.objectStoreNames.contains("meta")) db.createObjectStore("meta");
        };
        req.onsuccess = () => {
          const tx = req.result.transaction(["nodes", "meta"], "readwrite");
          const nodes = tx.objectStore("nodes");
          for (const [key, value] of records) nodes.put(value, key);
          tx.objectStore("meta").put(JSON.stringify(roots), "roots");
          tx.oncomplete = () => resolve(records.length);
          tx.onerror = () => reject(tx.error);
          tx.onabort = () => reject(tx.error);
        };
        req.onerror = () => reject(req.error);
      }),
    [GROUPS, TABS_PER_GROUP, FAVICON, FAVICON_EVERY] as const
  );
}

test("export writes the whole tree when it is far too large to travel over messaging", async ({ page }) => {
  test.setTimeout(60_000);

  await page.addInitScript(installFakeBrowser, {
    windows: [],
    messageResponseLimitBytes: RESPONSE_LIMIT,
  });
  await page.goto("/blank.html");
  expect(await seedLargeTree(page)).toBe(TOTAL_NODES);

  await page.addScriptTag({ path: "dist/background/background.js" });
  await page.addStyleTag({ path: "dist/sidebar/sidebar.css" });
  await page.addScriptTag({ path: "dist/sidebar/sidebar.js" });

  // The view protocol is unaffected by the ceiling: rows render as usual.
  await expect(page.locator("[role=treeitem]").first()).toBeVisible({ timeout: 30_000 });

  await page.locator("#export").click();
  await expect
    .poll(() => page.evaluate(() => (globalThis as any).__fake.downloads.length), { timeout: 30_000 })
    .toBe(1);

  const written = await page.evaluate(() => (globalThis as any).__fake.downloads[0]);
  expect(written.filename).toBe("grove.json");

  // The bug's signature: a file of exactly this text, with no error raised.
  expect(written.body).not.toBe("undefined");
  // The payload really is over the ceiling — otherwise this test would quietly
  // stop exercising anything if node sizes ever shrank.
  expect(written.body.length).toBeGreaterThan(RESPONSE_LIMIT);

  const parsed = JSON.parse(written.body);
  expect(parsed.nodes).toHaveLength(TOTAL_NODES);
  expect(parsed.roots).toHaveLength(GROUPS);
  expect(
    parsed.nodes.filter((n: { favIconUrl: string | null }) => n.favIconUrl === FAVICON)
  ).toHaveLength(FAVICON_NODES);

  // No banner: it worked.
  await expect(page.locator("#notice")).toBeHidden();
});

test("the export reply is an ack, not the tree", async ({ page }) => {
  await page.addInitScript(installFakeBrowser, {
    windows: [],
    messageResponseLimitBytes: RESPONSE_LIMIT,
  });
  await page.goto("/blank.html");
  await seedLargeTree(page);
  await page.addScriptTag({ path: "dist/background/background.js" });

  // Straight at the protocol: under the ceiling, a handler that answered with the
  // snapshot would hand back `undefined` here instead of an ack. Polled because
  // the background only registers its request listener once boot finishes.
  await expect
    .poll(
      () =>
        page.evaluate(() =>
          (globalThis as any).browser.runtime
            .sendMessage({ kind: "req", body: { tag: "export" } })
            .catch(() => "not serving yet")
        ),
      { timeout: 30_000 }
    )
    .toEqual({ ok: true });
});
