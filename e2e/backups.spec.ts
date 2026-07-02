import { test, expect } from "@playwright/test";
import { bootBackground } from "./support/harness";
import { installFakeBrowser } from "./support/fake-browser";

const BACKUP_ALARM = "tabs-outliner-automatic-backup";
const BACKUP_ENABLED_KEY = "tabsOutlinerAutomaticBackupsEnabled";
const BACKUP_LAST_SUCCESS_KEY = "tabsOutlinerAutomaticBackupLastSuccessfulAt";

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

test("automatic backups schedule daily exports from the options page", async ({ page }) => {
  await bootBackground(page, seed);
  await page.addStyleTag({ path: "dist/options/options.css" });
  await page.addScriptTag({ path: "dist/options/options.js" });

  const checkbox = page.locator("#automatic-backups-enabled");
  await expect(checkbox).not.toBeChecked();

  await checkbox.check();
  await expect.poll(() => page.evaluate(() => (globalThis as any).__fake.downloads.length)).toBe(1);

  const alarm = await page.evaluate((name) => (globalThis as any).__fake.alarm(name), BACKUP_ALARM);
  expect(alarm).toMatchObject({ name: BACKUP_ALARM, periodInMinutes: 1440 });

  const first = await page.evaluate(() => (globalThis as any).__fake.downloads[0]);
  expect(first.filename).toMatch(/^tabs-outliner-backups\/tabs-outliner-\d{4}-\d{2}-\d{2}\.json$/);
  const payload = JSON.parse(first.body);
  expect(payload.roots.length).toBeGreaterThan(0);
  expect(payload.nodes.map((n: { title: string }) => n.title)).toContain("Alpha");

  await page.evaluate((name) => {
    (globalThis as any).__fake.downloads.length = 0;
    (globalThis as any).__fake.emitAlarm(name);
  }, BACKUP_ALARM);
  await expect.poll(() => page.evaluate(() => (globalThis as any).__fake.downloads.length)).toBe(1);

  await checkbox.uncheck();
  await expect.poll(() => page.evaluate((name) => (globalThis as any).__fake.alarm(name), BACKUP_ALARM)).toBeNull();
});

test("automatic backups catch up once on startup when stale", async ({ page }) => {
  await page.addInitScript(installFakeBrowser, seed);
  await page.goto("/blank.html");
  await page.evaluate(
    ([enabledKey, lastKey]) =>
      (globalThis as any).browser.storage.local.set({
        [enabledKey as string]: true,
        [lastKey as string]: "1970-01-01T00:00:00.000Z",
      }),
    [BACKUP_ENABLED_KEY, BACKUP_LAST_SUCCESS_KEY] as const
  );

  await page.addScriptTag({ path: "dist/background/background.js" });

  await expect.poll(() => page.evaluate(() => (globalThis as any).__fake.downloads.length)).toBe(1);
  const alarm = await page.evaluate((name) => (globalThis as any).__fake.alarm(name), BACKUP_ALARM);
  expect(alarm).toMatchObject({ name: BACKUP_ALARM, periodInMinutes: 1440 });
});
