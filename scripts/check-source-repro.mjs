// Check that the AMO source zip reproduces the shipped extension. Mozilla's
// reviewers unpack grove-source.zip and follow docs/reviewer-notes.md
// (`pnpm install` + `pnpm run build`); their dist/ must match the one the XPI is
// zipped from (`pnpm run package` = this checkout's `pnpm run build` + web-ext).
// Catches files the build needs but the zip omits (uncommitted, export-ignore),
// reliance on local caches, and a nondeterministic build.
import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { mkdtempSync, readdirSync, readFileSync, rmSync, statSync } from "node:fs";
import { tmpdir } from "node:os";
import { delimiter, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const run = (cmd, args, opts) => execFileSync(cmd, args, { stdio: "inherit", ...opts });

// Refuses a dirty tree, so the zip is exactly what the local build sees.
run("node", ["scripts/package-source.mjs"], { cwd: repoRoot });
run("pnpm", ["run", "build"], { cwd: repoRoot });

const work = mkdtempSync(join(tmpdir(), "grove-source-repro-"));
run("unzip", ["-q", join(repoRoot, "web-ext-artifacts/grove-source.zip"), "-d", work]);
const source = join(work, "grove-source");

// Build like a reviewer would: nothing inherited from this checkout's pnpm run
// (its npm_* config, its node_modules/.bin on PATH).
const env = Object.fromEntries(Object.entries(process.env).filter(([k]) => !/^npm_|^PNPM_SCRIPT_SRC_DIR$/i.test(k)));
env.PATH = (env.PATH ?? "")
  .split(delimiter)
  .filter((p) => !resolve(p).startsWith(repoRoot))
  .join(delimiter);
run("pnpm", ["install"], { cwd: source, env });
run("pnpm", ["run", "build"], { cwd: source, env });

const digests = (dir) =>
  new Map(
    readdirSync(dir, { recursive: true })
      .filter((rel) => statSync(join(dir, rel)).isFile())
      .map((rel) => [rel, createHash("sha256").update(readFileSync(join(dir, rel))).digest("hex")]),
  );
const shipped = digests(join(repoRoot, "dist"));
const rebuilt = digests(join(source, "dist"));

const problems = [
  ...[...shipped.keys()].filter((f) => !rebuilt.has(f)).map((f) => `missing from rebuild: ${f}`),
  ...[...rebuilt.keys()].filter((f) => !shipped.has(f)).map((f) => `only in rebuild:      ${f}`),
  ...[...shipped].filter(([f, h]) => rebuilt.has(f) && rebuilt.get(f) !== h).map(([f]) => `differs:              ${f}`),
];
if (problems.length > 0) {
  console.error("\ncheck-source-repro: the source zip does not reproduce dist/:\n");
  for (const p of problems.sort()) console.error(`  ${p}`);
  console.error(`\nRebuilt tree kept for inspection: ${source}`);
  process.exit(1);
}
rmSync(work, { recursive: true, force: true });
console.log(`check-source-repro: ok (${shipped.size} files identical)`);
