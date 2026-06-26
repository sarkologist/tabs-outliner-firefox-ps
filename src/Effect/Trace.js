// Debug tracing for the restore flow, gated by a localStorage flag the options
// page toggles (shared across the extension's same-origin pages, like
// Effect.Settings / Effect.Profile — and it survives the background event page
// being suspended). Lines accumulate in a bounded localStorage buffer the options
// page reads, refreshes, clears, and downloads. Disabled = a single flag read.
const KEY = "tabsOutlinerTrace";
const KEY_ON = "tabsOutlinerTraceEnabled";
const MAX = 200000; // chars; trims oldest to ~150k when exceeded

const on = () => {
  try {
    return localStorage.getItem(KEY_ON) === "1";
  } catch {
    return false;
  }
};

// The one trace sink, installed on globalThis so the standalone Browser.js FFI can
// route its raw-event logs through the SAME buffer without importing this module.
const sink = (msg) => {
  if (!on()) return;
  const t =
    globalThis.performance && typeof performance.now === "function"
      ? Math.round(performance.now())
      : 0;
  const line = "[" + t + "] " + msg;
  try {
    const cur = localStorage.getItem(KEY) || "";
    let next = cur ? cur + "\n" + line : line;
    if (next.length > MAX) {
      const cut = next.indexOf("\n", next.length - 150000);
      next = cut >= 0 ? next.slice(cut + 1) : next.slice(next.length - 150000);
    }
    localStorage.setItem(KEY, next);
  } catch {}
  try {
    console.log("[trace] " + line);
  } catch {}
};
globalThis.__toTraceSink = sink;

export const traceImpl = (msg) => () => sink(msg);

export const getEnabled = () => on();
export const setEnabled = (b) => () => {
  try {
    localStorage.setItem(KEY_ON, b ? "1" : "0");
  } catch {}
};
export const readTrace = () => {
  try {
    return localStorage.getItem(KEY) || "";
  } catch {
    return "";
  }
};
export const clearTrace = () => {
  try {
    localStorage.removeItem(KEY);
  } catch {}
};
export const downloadTrace = () => {
  let data = "";
  try {
    data = localStorage.getItem(KEY) || "";
  } catch {}
  if (!data) return;
  const url = URL.createObjectURL(new Blob([data], { type: "text/plain" }));
  const a = document.createElement("a");
  a.href = url;
  a.download = "tabs-outliner-trace-" + new Date().toISOString().slice(0, 19).replace(/[:T]/g, "-") + ".txt";
  document.body.appendChild(a);
  a.click();
  a.remove();
  setTimeout(() => URL.revokeObjectURL(url), 0);
};
