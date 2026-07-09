// Zip the committed source for AMO's source-code submission. Because the
// shipped bundle is PureScript-compiled and then esbuild-minified, Mozilla
// requires the original source plus build instructions (see
// docs/reviewer-notes.md). `git archive` gives a clean-tree snapshot — no
// node_modules / output / dist noise, only what's committed.
import { mkdirSync } from "node:fs";
import { execFileSync } from "node:child_process";

mkdirSync("web-ext-artifacts", { recursive: true });
execFileSync(
  "git",
  [
    "archive",
    "--format=zip",
    "--prefix=grove-source/",
    "-o",
    "web-ext-artifacts/grove-source.zip",
    "HEAD",
  ],
  { stdio: "inherit" },
);
console.log("package:source: wrote web-ext-artifacts/grove-source.zip");
