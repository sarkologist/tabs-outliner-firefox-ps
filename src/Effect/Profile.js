// A tiny opt-in profiler, gated by a localStorage flag set from the options page
// (shared across the extension's pages, like Effect.Settings). When enabled, named
// timings accumulate in a per-page buffer (also exposed on globalThis for
// devtools); `finishBoot` records a final paint mark on the next frame and persists
// the session so the options page can show/export it. Disabled = a single flag
// read and nothing else, so normal use pays nothing.

const KEY_ENABLED = "tabsOutlinerProfileEnabled";
const KEY_LAST = "tabsOutlinerLastProfile";

const enabled = () => {
  try {
    return localStorage.getItem(KEY_ENABLED) === "1";
  } catch {
    return false;
  }
};

export const getEnabled = () => enabled();

export const setEnabled = (b) => () => {
  try {
    localStorage.setItem(KEY_ENABLED, b ? "1" : "0");
  } catch {}
};

export const nowMs = () => performance.now();

const buf = (globalThis.__tabsOutlinerProfile = globalThis.__tabsOutlinerProfile || { entries: [] });

const round2 = (ms) => Math.round(ms * 100) / 100;

export const clearBuffer = () => {
  buf.entries.length = 0;
};

export const record = (name) => (ms) => () => {
  if (enabled()) buf.entries.push({ name, ms: round2(ms) });
};

// Record a final paint mark on the next frame (after the browser has painted), then
// persist the labeled session for the options page and dump it to the console.
export const finishBoot = (label) => () => {
  if (!enabled()) return;
  requestAnimationFrame(() => {
    buf.entries.push({ name: "boot.paint", ms: round2(performance.now()) });
    const payload = { schema: "tabs-outliner-profile", label, at: new Date().toISOString(), entries: buf.entries.slice() };
    try {
      localStorage.setItem(KEY_LAST, JSON.stringify(payload));
    } catch {}
    try {
      console.table(buf.entries);
    } catch {}
  });
};

export const readLast = () => {
  try {
    return localStorage.getItem(KEY_LAST) || "";
  } catch {
    return "";
  }
};

export const clearLast = () => {
  try {
    localStorage.removeItem(KEY_LAST);
  } catch {}
};

// Download the last persisted profile as JSON (for sharing / side-by-side notes).
export const downloadProfile = () => {
  const data = readLast();
  if (!data) return;
  const url = URL.createObjectURL(new Blob([data], { type: "application/json" }));
  const a = document.createElement("a");
  a.href = url;
  a.download = "tabs-outliner-profile-" + new Date().toISOString().slice(0, 10) + ".json";
  document.body.appendChild(a);
  a.click();
  a.remove();
  setTimeout(() => URL.revokeObjectURL(url), 0);
};
