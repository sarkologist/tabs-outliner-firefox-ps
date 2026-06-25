// Bundle each PureScript entry module (compiled to output/<Module>/index.js by
// `spago build`) into a single browser IIFE under dist/. We drive esbuild
// directly so we fully control the output format (iife, loadable via a plain
// <script> tag). Entry modules whose output is absent are skipped, so the same
// script works before the background module exists.
import { build } from "esbuild";
import { existsSync } from "node:fs";

const targets = [
  { module: "Sidebar.Main", outfile: "dist/sidebar/sidebar.js" },
  { module: "Background.Main", outfile: "dist/background/background.js" },
  { module: "Options.Main", outfile: "dist/options/options.js" },
];

for (const t of targets) {
  if (!existsSync(`output/${t.module}/index.js`)) {
    console.log(`bundle: skip ${t.module} (no compiled output yet)`);
    continue;
  }
  await build({
    stdin: {
      contents: `import { main } from "./output/${t.module}/index.js"; main();`,
      resolveDir: process.cwd(),
      loader: "js",
    },
    bundle: true,
    // minify: the bundle is eagerly parsed + evaluated at page load, so its size
    // is a direct cost of opening the sidebar (the profiler's boot.bootstrap).
    minify: true,
    format: "iife",
    platform: "browser",
    target: "es2020",
    outfile: t.outfile,
    logLevel: "warning",
  });
  console.log(`bundle: ${t.module} -> ${t.outfile}`);
}
