// Remove dist/ so every build starts from a pristine tree. Guards against
// stray files (stale bundles, an accidental JSON export dropped in dist/)
// getting packaged into the shipped XPI — web-ext packages *everything* under
// the source dir.
import { rm } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { dirname, resolve } from "node:path";

// Resolve dist/ relative to the repo root (this script's parent dir), not the
// caller's cwd — so a stray `node scripts/clean.mjs` run from elsewhere can't
// wipe some other directory's dist/.
const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..");
await rm(resolve(repoRoot, "dist"), { recursive: true, force: true });
console.log("clean: removed dist/");
