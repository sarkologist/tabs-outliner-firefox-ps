// Allow drops anywhere in the sidebar by preventing the default dragover/drop
// handling (which would otherwise reject the drop). The row's own onDrop handler
// still fires; we read the dragged id from component state, so no DataTransfer
// is needed.
export const allowDrops = () => {
  document.addEventListener("dragover", (e) => e.preventDefault());
  document.addEventListener("drop", (e) => e.preventDefault());
};

// Firefox eats the first click into an *unfocused* sidebar document: that click
// is spent focusing the sidebar instead of activating whatever was under it, so
// the action only lands on the second click. Here the sidebar is unfocused most
// of the time — activating a tab focuses its window/page (focusTabImpl), and
// ordinary browsing keeps focus in the content too — and nothing pulls focus
// back, so almost any click can need a second try. Reacquire focus the moment the
// pointer is over the sidebar, i.e. before the click resolves. Guarded by
// hasFocus() so it's a cheap no-op (one boolean read) whenever we already hold
// focus, and so it never yanks focus around while the sidebar is in use.
export const keepFocused = () => {
  const grab = () => {
    if (!document.hasFocus()) {
      try {
        window.focus();
      } catch (_) {
        // focus() can throw in unusual window states; a missed grab just leaves
        // the pre-existing double-click, never anything worse.
      }
    }
  };
  // pointerover (capture) fires as the pointer arrives over the row about to be
  // clicked — early enough that the click lands on an already-focused document.
  document.addEventListener("pointerover", grab, true);
  // backstop for "pointer already resting on the sidebar when a background tab
  // event stole focus, then the user presses": grab again at the press.
  document.addEventListener("pointerdown", grab, true);
};

// Download a string as a file via a Blob URL (no downloads permission needed).
export const downloadJson = (filename) => (content) => () => {
  const url = URL.createObjectURL(new Blob([content], { type: "application/json" }));
  const a = document.createElement("a");
  a.href = url;
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  a.remove();
  setTimeout(() => URL.revokeObjectURL(url), 0);
};

// Open a file picker, read the chosen file as text, and hand it to the callback.
export const pickJson = (cb) => () => {
  const input = document.createElement("input");
  input.type = "file";
  input.accept = "application/json,.json";
  input.addEventListener("change", () => {
    const file = input.files && input.files[0];
    if (!file) return;
    const reader = new FileReader();
    reader.onload = () => cb(String(reader.result))();
    reader.readAsText(file);
  });
  input.click();
};

export const getZoom = () => {
  const v = parseFloat(localStorage.getItem("zoom") || "1");
  return Number.isFinite(v) ? v : 1;
};

export const setZoom = (z) => () => localStorage.setItem("zoom", String(z));

// --- viewport virtualization helpers ---

// Pure: read scroll offset + visible height off a scroll event's target.
export const scrollMetrics = (e) => ({
  top: e.target.scrollTop,
  height: e.target.clientHeight,
});

export const treeViewportHeight = () => {
  const el = document.getElementById("tree");
  return el ? el.clientHeight : 600;
};

// Scroll the tree to a computed offset (revealing the active tab). Deferred two
// frames so it runs after Halogen has applied the new row layout — the
// virtualized #tree-inner height — otherwise the browser would clamp scrollTop
// to a stale, smaller scrollHeight and land in the wrong place.
export const scrollTreeTo = (top) => () => {
  requestAnimationFrame(() =>
    requestAnimationFrame(() => {
      const el = document.getElementById("tree");
      if (el) el.scrollTop = top;
    })
  );
};

export const onResize = (cb) => () => {
  window.addEventListener("resize", () => cb());
};

// Focus the search box — target of the "focus search" shortcut.
export const focusSearch = () => {
  const el = document.getElementById("search");
  if (el) el.focus();
};

// Open the extension's options page (the gear button; also reachable from
// about:addons -> Preferences once options_ui is declared in the manifest).
export const openOptions = () => {
  if (globalThis.browser && browser.runtime && browser.runtime.openOptionsPage) {
    browser.runtime.openOptionsPage();
  }
};
