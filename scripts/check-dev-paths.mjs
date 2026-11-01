// Fail if any committed file contains an absolute developer path. The AMO
// source zip is `git archive HEAD` (scripts/package-source.mjs), so such a path
// ships the developer's home dir / username to Mozilla's reviewers. Placeholder
// home dirs in fixtures (`/Users/me/`, `/home/x/`, ...) are intentional and allowed.
import { execFileSync } from "node:child_process";
import { fileURLToPath } from "node:url";
import { dirname, resolve } from "node:path";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..");

// A path counts only at the start of a token (or right after `file://`), so web
// paths like `https://example.com/home/` don't trip it.
const PLACEHOLDER = "(?:me|x|user|you|example|name)/";
const DEV_PATH = new RegExp(
  "(?:^|[\\s\"'`(=:,\\[]|file://)" +
    `(?:/Users/(?!${PLACEHOLDER})|/home/(?!${PLACEHOLDER})[a-z]|/opt/homebrew)`,
);

// Outside a git checkout (e.g. building from the unpacked source zip, which
// has no .git) there is nothing tracked to scan.
try {
  execFileSync("git", ["rev-parse", "--is-inside-work-tree"], { cwd: repoRoot, stdio: "ignore" });
} catch {
  console.log("check-dev-paths: skipped (not a git checkout)");
  process.exit(0);
}

let out = "";
try {
  // Tracked files only (what `git archive` ships); exclude this script, whose
  // patterns would match themselves. `-z` separates path:line:content with NULs
  // so only the content is tested, not the file name.
  out = execFileSync(
    "git",
    ["grep", "-nIzE", "/Users/|/opt/homebrew|/home/", "--", ":!scripts/check-dev-paths.mjs"],
    { cwd: repoRoot, encoding: "utf8" },
  );
} catch (e) {
  // git grep exits 1 when nothing matches.
  if (e.status !== 1) throw e;
}

const hits = out
  .split("\n")
  .filter(Boolean)
  .map((line) => line.split("\0"))
  .filter(([, , content]) => DEV_PATH.test(content));
if (hits.length > 0) {
  console.error("check-dev-paths: absolute developer paths in tracked files:\n");
  for (const [path, lineNo, content] of hits) console.error(`  ${path}:${lineNo}: ${content.trim()}`);
  console.error("\nGenericize them (e.g. an env var or a placeholder like /Users/me/) before committing.");
  process.exit(1);
}
console.log("check-dev-paths: ok");
