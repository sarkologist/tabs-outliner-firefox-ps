// Fail if any committed file contains an absolute developer path. The AMO
// source zip is `git archive HEAD` (scripts/package-source.mjs), so such a path
// ships the developer's home dir / username to Mozilla's reviewers. Generic
// fixture paths under `/Users/me/` are intentional and allowed.
import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { dirname, resolve } from "node:path";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const DEV_PATH = /\/Users\/(?!me\/)|\/opt\/homebrew|\/home\/[a-z]/;

let out = "";
try {
  // Tracked files only (what `git archive` ships); exclude this script, whose
  // pattern would match itself.
  out = execFileSync(
    "git",
    ["grep", "-nIE", "/Users/|/opt/homebrew|/home/", "--", ":!scripts/check-dev-paths.mjs"],
    { cwd: repoRoot, encoding: "utf8" },
  );
} catch (e) {
  // git grep exits 1 when nothing matches.
  if (e.status !== 1) throw e;
}

const hits = out.split("\n").filter((line) => DEV_PATH.test(line));
if (hits.length > 0) {
  console.error("check-dev-paths: absolute developer paths in tracked files:\n");
  for (const hit of hits) console.error(`  ${hit}`);
  console.error("\nGenericize them (e.g. an env var or /Users/me/...) before committing.");
  process.exit(1);
}
console.log("check-dev-paths: ok");
