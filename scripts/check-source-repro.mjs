// Check that the AMO source zip reproduces the shipped extension. Mozilla's
// reviewers unpack grove-source.zip and follow docs/reviewer-notes.md
// (`pnpm install --frozen-lockfile` + `pnpm run build`); their dist/ must match
// the one the XPI is zipped from (`pnpm run package` = this checkout's
// `pnpm run build` + web-ext). Catches files the build needs but the zip omits
// (uncommitted, export-ignore), absolute paths baked into the bundle, stale
// local output/, and a nondeterministic build. The rebuild shares this
// machine's pnpm store and spago cache, so it does not prove a cold fetch works.
//
// `pnpm run package` runs this and then zips the verified dist/, so the XPI is
// always the reviewers' build. Writes the shipped dist/'s digests to
// web-ext-artifacts/dist-digests.json so CI can compare builds across OSes.
import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { lstatSync, mkdirSync, mkdtempSync, readdirSync, readFileSync, realpathSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { delimiter, dirname, join, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";

const repoRoot = realpathSync(resolve(dirname(fileURLToPath(import.meta.url)), ".."));
let work;

const git = (...args) => execFileSync("git", args, { cwd: repoRoot, encoding: "utf8" }).trim();
const head = git("rev-parse", "HEAD");

const stage = (name, cmd, args, opts) => {
  try {
    execFileSync(cmd, args, { stdio: "inherit", ...opts });
  } catch {
    console.error(`\ncheck-source-repro: ${name} failed` + (work ? `; tree kept at ${work}` : ""));
    process.exit(1);
  }
};

// Refuses a dirty tree, so the zip is exactly what the local build sees.
stage("source zip", "node", ["scripts/package-source.mjs"], { cwd: repoRoot });
stage("checkout build", "pnpm", ["run", "build"], { cwd: repoRoot });

work = mkdtempSync(join(tmpdir(), "grove-source-repro-"));
stage("unzip", "unzip", ["-q", join(repoRoot, "web-ext-artifacts/grove-source.zip"), "-d", work]);
const source = join(work, "grove-source");

// Build like a reviewer would: nothing inherited from this checkout's pnpm run
// (its npm_* config, its node_modules/.bin on PATH).
const insideRepo = (p) => {
  let real;
  try {
    real = realpathSync(p);
  } catch {
    return false;
  }
  return real === repoRoot || real.startsWith(repoRoot + sep);
};
const env = Object.fromEntries(Object.entries(process.env).filter(([k]) => !/^npm_|^PNPM_SCRIPT_SRC_DIR$/i.test(k)));
env.PATH = (env.PATH ?? "").split(delimiter).filter((p) => p && !insideRepo(p)).join(delimiter);
stage("zip install", "pnpm", ["install", "--frozen-lockfile"], { cwd: source, env });
stage("zip build", "pnpm", ["run", "build"], { cwd: source, env });

// rel -> "sha256 size". Symlinks are refused: web-ext would package their
// targets, so neither tree should contain any.
const digests = (dir) =>
  new Map(
    readdirSync(dir, { recursive: true }).flatMap((rel) => {
      const st = lstatSync(join(dir, rel));
      if (st.isSymbolicLink()) {
        console.error(`check-source-repro: unexpected symlink ${join(dir, rel)}`);
        process.exit(1);
      }
      if (!st.isFile()) return [];
      const hash = createHash("sha256").update(readFileSync(join(dir, rel))).digest("hex");
      return [[rel.split(sep).join("/"), `${hash} ${st.size}`]];
    }),
  );
const shipped = digests(join(repoRoot, "dist"));
const rebuilt = digests(join(source, "dist"));

mkdirSync(join(repoRoot, "web-ext-artifacts"), { recursive: true });
writeFileSync(
  join(repoRoot, "web-ext-artifacts/dist-digests.json"),
  JSON.stringify(Object.fromEntries([...shipped].sort()), null, 2) + "\n",
);

const problems = [
  ...[...shipped.keys()].filter((f) => !rebuilt.has(f)).map((f) => `missing from rebuild: ${f}`),
  ...[...rebuilt.keys()].filter((f) => !shipped.has(f)).map((f) => `only in rebuild:      ${f}`),
  ...[...shipped]
    .filter(([f, d]) => rebuilt.has(f) && rebuilt.get(f) !== d)
    .map(([f, d]) => `differs:              ${f}\n    checkout ${d}\n    rebuild  ${rebuilt.get(f)}`),
];
// An edit or checkout during the build would make dist/ reflect neither HEAD
// nor the zip.
if (git("rev-parse", "HEAD") !== head || git("status", "--porcelain")) {
  problems.push("tree changed during the build (HEAD moved or files edited)");
}
if (problems.length > 0) {
  console.error("\ncheck-source-repro: the source zip does not reproduce dist/:\n");
  for (const p of problems.sort()) console.error(`  ${p}`);
  console.error(`\nRebuilt tree kept for inspection: ${source}`);
  process.exit(1);
}
rmSync(work, { recursive: true, force: true });
console.log(`check-source-repro: ok (${shipped.size} files identical)`);
