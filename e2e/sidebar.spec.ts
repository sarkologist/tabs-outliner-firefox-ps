import { test, expect } from "@playwright/test";

// M0 smoke: the built sidebar bundle loads and Halogen mounts. This proves the
// whole loop — spago build -> esbuild bundle -> copy-static -> serve -> render —
// before any real logic exists. The fake browser API is introduced in M2/M3,
// when the sidebar first talks to browser.*.
test("sidebar bundle mounts and renders", async ({ page }) => {
  await page.goto("/sidebar/sidebar.html");
  await expect(page.getByRole("heading", { name: "Tabs Outliner" })).toBeVisible();
  await expect(page.getByText("hello")).toBeVisible();
});
