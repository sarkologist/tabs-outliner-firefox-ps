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
  expect(first.filename).toMatch(/^grove-backups\/grove-\d{4}-\d{2}-\d{2}\.json$/);
  // Unattended: it must never raise a save dialog, whatever the user's
  // "always ask where to save files" setting says (manual Export honours it).
  expect(first.saveAs).toBe(false);
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

test("automatic backup success is recorded only after the download completes", async ({ page }) => {
  await bootBackground(page, seed);

  await page.evaluate(() => {
    (globalThis as any).__fake.autoCompleteDownloads = false;
    (globalThis as any).__fake.pendingBackupRequest = (globalThis as any).browser.runtime.sendMessage({
      kind: "req",
      body: { tag: "setAutomaticBackups", enabled: true },
    });
  });

  await expect.poll(() => page.evaluate(() => (globalThis as any).__fake.downloads.length)).toBe(1);
  expect(await page.evaluate((key) => (globalThis as any).__fake.storageLocal(key), BACKUP_LAST_SUCCESS_KEY)).toBeNull();

  await page.evaluate(() => (globalThis as any).__fake.completeDownload(1));
  await page.evaluate(() => (globalThis as any).__fake.pendingBackupRequest);
  await expect
    .poll(() => page.evaluate((key) => (globalThis as any).__fake.storageLocal(key), BACKUP_LAST_SUCCESS_KEY))
    .not.toBeNull();
});

test("automatic backup startup preserves an existing alarm when not due", async ({ page }) => {
  await page.addInitScript(installFakeBrowser, seed);
  await page.goto("/blank.html");

  const scheduledTime = Date.now() + 6 * 60 * 60 * 1000;
  await page.evaluate(
    ([alarmName, enabledKey, lastKey, when]) =>
      (globalThis as any).browser.storage.local
        .set({
          [enabledKey as string]: true,
          [lastKey as string]: new Date().toISOString(),
        })
        .then(() =>
          (globalThis as any).browser.alarms.create(alarmName, {
            when,
            periodInMinutes: 1440,
          })
        ),
    [BACKUP_ALARM, BACKUP_ENABLED_KEY, BACKUP_LAST_SUCCESS_KEY, scheduledTime] as const
  );

  await page.addScriptTag({ path: "dist/background/background.js" });
  await page.evaluate(() =>
    (globalThis as any).browser.runtime.sendMessage({
      kind: "req",
      body: { tag: "getAutomaticBackups" },
    })
  );

  const alarm = await page.evaluate((name) => (globalThis as any).__fake.alarm(name), BACKUP_ALARM);
  expect(alarm.scheduledTime).toBe(scheduledTime);
  expect(await page.evaluate(() => (globalThis as any).__fake.downloads.length)).toBe(0);
});
