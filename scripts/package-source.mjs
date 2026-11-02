// Zip the committed source for AMO's source-code submission. Because the
// shipped bundle is PureScript-compiled and then esbuild-minified, Mozilla
// requires the original source plus build instructions (see
// docs/reviewer-notes.md). `git archive` gives a clean-tree snapshot — no
// node_modules / output / dist noise, only what's committed.
import { mkdirSync } from "node:fs";
import { execFileSync } from "node:child_process";

// The source zip must correspond to the uploaded XPI. `git archive` snapshots
// HEAD, while the build reads the working tree — so a dirty tree would ship a
// source zip a reviewer can't reproduce the XPI from. Refuse in that case.
const dirty = execFileSync("git", ["status", "--porcelain"], { encoding: "utf8" }).trim();
if (dirty) {
  console.error(
    "packaging refused: working tree is dirty — commit or stash first, so the XPI\n" +
      "and the source zip are both built from that commit.",
  );
  process.exit(1);
}

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
console.log("source zip: wrote web-ext-artifacts/grove-source.zip");
