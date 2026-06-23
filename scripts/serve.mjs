// Minimal static file server for the Playwright harness. Serves dist/ so the
// built sidebar page can be loaded over http (module/script semantics behave
// like the real extension; file:// would not).
import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { extname, join, normalize } from "node:path";

const root = "dist";
const port = Number(process.env.PORT ?? 5173);
const types = {
  ".html": "text/html",
  ".js": "text/javascript",
  ".css": "text/css",
  ".json": "application/json",
  ".svg": "image/svg+xml",
};

createServer(async (req, res) => {
  try {
    let p = decodeURIComponent(new URL(req.url, "http://x").pathname);
    // A blank same-origin page for the test harness (so IndexedDB + injected
    // fake browser work, without shipping a harness page in the extension).
    if (p === "/blank.html") {
      res.writeHead(200, { "content-type": "text/html" });
      res.end("<!doctype html><meta charset=utf-8><title>harness</title>");
      return;
    }
    if (p.endsWith("/")) p += "index.html";
    const file = join(root, normalize(p));
    const data = await readFile(file);
    res.writeHead(200, { "content-type": types[extname(file)] ?? "application/octet-stream" });
    res.end(data);
  } catch {
    res.writeHead(404);
    res.end("not found");
  }
}).listen(port, () => console.log(`serve: http://localhost:${port} (root=${root})`));
