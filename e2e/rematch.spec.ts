import { test, expect, type BrowserContext } from "@playwright/test";
import { installFakeBrowser, type Seed } from "./support/fake-browser";
import { readNodes, isLive } from "./support/harness";

// Boot a fresh background in a new page of the SAME context, so it shares the
// IndexedDB written by the previous page — i.e. a browser restart.
async function boot(context: BrowserContext, seed: Seed) {
  const page = await context.newPage();
  await page.addInitScript(installFakeBrowser, seed);
  await page.goto("/blank.html");
  await page.addScriptTag({ path: "dist/background/background.js" });
  return page;
}

test("startup re-match reuses nodes across a restart (no duplication)", async ({ context }) => {
  // session 1: a window with tabs Alpha + Beta
  const p1 = await boot(context, {
    windows: [{ id: 1, tabs: [{ id: 11, url: "http://a", title: "Alpha" }, { id: 12, url: "http://b", title: "Beta" }] }],
  });
  await expect.poll(() => readNodes(p1).then((n) => n.length)).toBe(3);
  await p1.close();

  // restart: fresh browser ids; Beta did not reopen, a new tab Gamma did
  const p2 = await boot(context, {
    windows: [{ id: 99, tabs: [{ id: 91, url: "http://a", title: "Alpha" }, { id: 93, url: "http://c", title: "Gamma" }] }],
  });

  // Alpha re-bound, Beta dropped (a fresh tab orphaned in the reopened window),
  // Gamma created => window + 2 tabs = 3
  await expect.poll(() => readNodes(p2).then((n) => n.length)).toBe(3);
  const nodes = await readNodes(p2);
  const byTitle = (t: string) => nodes.find((n) => n.title === t);

  expect(isLive(byTitle("Alpha"))).toBe(true);
  expect(byTitle("Alpha").tabId).toBe(91); // same node, fresh browser id
  expect(byTitle("Beta")).toBeUndefined(); // never restored + didn't reopen -> dropped, not kept
  expect(isLive(byTitle("Gamma"))).toBe(true); // genuinely new
});

test("startup consolidates a window whose tabs came from different prior windows", async ({ context }) => {
  // session 1: Alpha in window 1, Gamma in window 2 (two separate windows)
  const p1 = await boot(context, {
    windows: [
      { id: 1, tabs: [{ id: 11, url: "http://alpha", title: "Alpha" }] },
      { id: 2, tabs: [{ id: 21, url: "http://gamma", title: "Gamma" }] },
    ],
  });
  await expect.poll(() => readNodes(p1).then((n) => n.length)).toBe(4); // 2 windows + 2 tabs
  await p1.close();

  // restart: a single window now holds BOTH Alpha and Gamma
  const p2 = await boot(context, {
    windows: [{ id: 9, tabs: [{ id: 91, url: "http://alpha", title: "Alpha" }, { id: 92, url: "http://gamma", title: "Gamma" }] }],
  });
  await expect.poll(() => readNodes(p2).then((n) => n.length)).toBe(4); // re-bound, not duplicated
  const nodes = await readNodes(p2);
  const live = nodes.filter((n) => n.windowId != null);
  expect(live.length).toBe(1); // exactly one live window, not two
  const liveId = live[0].id;
  expect(nodes.find((n) => n.title === "Alpha").parent).toBe(liveId); // both tabs under it
  expect(nodes.find((n) => n.title === "Gamma").parent).toBe(liveId);
});
