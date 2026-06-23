// Copy the static extension files (manifest, html, css, icons) from public/
// into dist/, preserving layout. The bundled JS is written directly into dist/
// by bundle.mjs, so we never clobber it here.
import { cp } from "node:fs/promises";

await cp("public", "dist", { recursive: true });
console.log("copy-static: public -> dist");
