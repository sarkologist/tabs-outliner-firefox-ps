import { mkdirSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { defineConfig, devices } from "@playwright/test";

// AMO listing screenshots (`pnpm screenshots`), rendered in Firefox — the browser
// the listing is for. Not part of `pnpm check`: it produces images, it tests nothing.
const PORT = 5174;

// Playwright's Firefox (before r1564 / Firefox 158) still reads the real
// Firefox's app-data dir, ~/Library/Application Support/Firefox, which macOS 27
// TCC-protects: launch hangs, or exits with "Could not find profile folder".
// An empty home dir for Cocoa's lookups sidesteps it (microsoft/playwright#42768).
// Firefox's own sandboxes are untouched.
const firefoxHome = join(tmpdir(), "grove-screenshots-firefox-home");
mkdirSync(firefoxHome, { recursive: true });

export default defineConfig({
  testDir: "screenshots",
  workers: 1,
  reporter: [["list"]],
  use: {
    baseURL: `http://localhost:${PORT}`,
    viewport: { width: 430, height: 540 },
    deviceScaleFactor: 2,
    colorScheme: "light",
  },
  // firefox first: `--project firefox` is what the listing should use. chromium
  // is the fallback where Playwright's Firefox can't launch.
  projects: [
    {
      name: "firefox",
      use: {
        browserName: devices["Desktop Firefox"].defaultBrowserType,
        launchOptions: { env: { ...process.env, CFFIXED_USER_HOME: firefoxHome } },
      },
    },
    { name: "chromium", use: { browserName: devices["Desktop Chrome"].defaultBrowserType } },
  ],
  webServer: {
    command: "node scripts/serve.mjs",
    url: `http://localhost:${PORT}/sidebar/sidebar.html`,
    reuseExistingServer: false,
    env: { PORT: String(PORT) },
    timeout: 30_000,
  },
});
