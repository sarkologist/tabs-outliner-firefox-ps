// localStorage-backed shortcut overrides plus the keydown plumbing shared by the
// sidebar and the options page. Both extension pages share an origin, so they
// share this localStorage; a write also fires a "storage" event in other docs.

const KEY = "shortcuts";

// Canonical combo string for a KeyboardEvent: active modifiers in a fixed order
// (Ctrl, Alt, Shift, Meta), then the key, joined with "+". Single-char keys are
// lower-cased so "Shift+N" and "shift+n" can't both exist. Must stay in lockstep
// with Model.Shortcuts, whose defaults are written in this same form.
const comboOf = (e) => {
  const parts = [];
  if (e.ctrlKey) parts.push("Ctrl");
  if (e.altKey) parts.push("Alt");
  if (e.shiftKey) parts.push("Shift");
  if (e.metaKey) parts.push("Meta");
  let k = e.key;
  if (k === " ") k = "Space";
  else if (k.length === 1) k = k.toLowerCase();
  parts.push(k);
  return parts.join("+");
};

const isModifier = (k) =>
  k === "Control" || k === "Shift" || k === "Alt" || k === "Meta";

const editableTarget = (t) =>
  !!t && (t.tagName === "INPUT" || t.tagName === "TEXTAREA" || t.isContentEditable);

export const getShortcuts = () => {
  try {
    const s = localStorage.getItem(KEY);
    if (!s) return {};
    const o = JSON.parse(s);
    if (!o || typeof o !== "object") return {};
    // keep only string values, so a corrupt entry can't break matching
    const out = {};
    for (const k of Object.keys(o)) if (typeof o[k] === "string") out[k] = o[k];
    return out;
  } catch (_) {
    return {};
  }
};

export const setShortcuts = (obj) => () => {
  localStorage.setItem(KEY, JSON.stringify(obj));
};

export const onShortcut = (handle) => () => {
  document.addEventListener("keydown", (e) => {
    // ignore auto-repeat (held key): one keypress = one action, so holding "e"
    // can't fire a stream of exports/imports/new-groups
    if (e.repeat || editableTarget(e.target) || isModifier(e.key)) return;
    if (handle(comboOf(e))()) e.preventDefault();
  });
};

export const captureCombo = (cb) => () => {
  const onKey = (e) => {
    if (isModifier(e.key)) return; // wait for a non-modifier key
    e.preventDefault();
    e.stopPropagation();
    window.removeEventListener("keydown", onKey, true);
    cb(comboOf(e))();
  };
  window.addEventListener("keydown", onKey, true);
};
