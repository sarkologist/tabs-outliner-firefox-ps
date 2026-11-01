import { defineConfig, devices } from "@playwright/test";

// AMO listing screenshots (`pnpm screenshots`), rendered in Firefox — the browser
// the listing is for. Not part of `pnpm check`: it produces images, it tests nothing.
const PORT = 5174;

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
    { name: "firefox", use: { browserName: devices["Desktop Firefox"].defaultBrowserType } },
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
