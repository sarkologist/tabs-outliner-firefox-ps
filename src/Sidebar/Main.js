// Allow drops anywhere in the sidebar by preventing the default dragover/drop
// handling (which would otherwise reject the drop). The row's own onDrop handler
// still fires; we read the dragged id from component state, so no DataTransfer
// is needed.
export const allowDrops = () => {
  document.addEventListener("dragover", (e) => e.preventDefault());
  document.addEventListener("drop", (e) => e.preventDefault());
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

export const onResize = (cb) => () => {
  window.addEventListener("resize", () => cb());
};
