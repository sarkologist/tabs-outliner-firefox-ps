// Remove dist/ so every build starts from a pristine tree. Guards against
// stray files (stale bundles, an accidental JSON export dropped in dist/)
// getting packaged into the shipped XPI — web-ext packages *everything* under
// the source dir.
import { rm } from "node:fs/promises";

await rm("dist", { recursive: true, force: true });
console.log("clean: removed dist/");
