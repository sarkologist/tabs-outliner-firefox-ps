// WebExtension FFI. `api` is globalThis.browser (real in Firefox, fake in
// tests). The curried shapes match the PureScript foreign imports; functions
// returning Effect are `() => ...` thunks, and Sink callbacks are
// `(arg) => () => unit`, so we call them as `sink.fn(arg)()`.

export const getBrowser = () => globalThis.browser;

// Key under which we stash a tab's outliner node id via browser.sessions. The
// value survives a browser restart for any tab Firefox session-restores, giving
// startup re-match a STABLE identity to bind by (instead of guessing by url).
const NODE_KEY = "outlinerNode";

// Read a tab's stashed node id, tolerating a missing sessions API (older fakes)
// or a per-tab read failure — either yields null (re-match falls back to url).
const getTabKey = (api, tabId) => {
  const s = api && api.sessions;
  if (!s || typeof s.getTabValue !== "function") return Promise.resolve(null);
  return Promise.resolve(s.getTabValue(tabId, NODE_KEY)).then((v) => v ?? null, () => null);
};

export const getAllWindowsImpl = (api) => () =>
  Promise.resolve(api.windows.getAll({ populate: true })).then((wins) =>
    Promise.all(
      wins.map((w) =>
        Promise.all(
          (w.tabs ?? []).map((t) =>
            getTabKey(api, t.id).then((nodeKey) => ({
              tabId: t.id,
              windowId: t.windowId,
              index: t.index,
              url: t.url ?? null,
              title: t.title ?? "",
              active: !!t.active,
              favIconUrl: t.favIconUrl ?? null,
              nodeKey,
            }))
          )
        ).then((tabs) => ({ windowId: w.id, tabs }))
      )
    )
  );

// Stamp a tab with its outliner node id (best-effort; a missing sessions API or a
// failed write is swallowed — re-match degrades to url matching for that tab).
export const tagTabImpl = (api) => (tabId) => (value) => () => {
  const s = api && api.sessions;
  if (!s || typeof s.setTabValue !== "function") return Promise.resolve();
  return Promise.resolve(s.setTabValue(tabId, NODE_KEY, value)).catch(() => {});
};

// The window hosting this sidebar (`windows.getCurrent`). Guarded so a missing
// API (older test fakes) yields null rather than throwing during boot.
export const getCurrentWindowIdImpl = (api) => () => {
  const wins = api && api.windows;
  if (!wins || typeof wins.getCurrent !== "function") return Promise.resolve(null);
  return Promise.resolve(wins.getCurrent()).then((w) =>
    w && typeof w.id === "number" ? w.id : null
  );
};

export const subscribeImpl = (api) => (sink) => () => {
  const t = api.tabs;
  const w = api.windows;
  t.onCreated.addListener((tab) =>
    sink.tabOpened({
      tabId: tab.id,
      windowId: tab.windowId,
      index: tab.index,
      url: tab.url ?? null,
      title: tab.title ?? "",
      active: !!tab.active,
      favIconUrl: tab.favIconUrl ?? null,
    })()
  );
  t.onRemoved.addListener((tabId) => sink.tabClosed(tabId)());
  t.onUpdated.addListener((tabId, change, tab) =>
    sink.tabChanged({
      tabId,
      url: change.url ?? null,
      title: change.title ?? (tab && tab.title) ?? null,
      favIconUrl: change.favIconUrl ?? null,
    })()
  );
  t.onActivated.addListener((info) =>
    sink.tabActivated({ tabId: info.tabId, windowId: info.windowId })()
  );
  t.onMoved.addListener((tabId, info) =>
    sink.tabMoved({ tabId, windowId: info.windowId, toIndex: info.toIndex })()
  );
  t.onAttached.addListener((tabId, info) =>
    sink.tabAttached({ tabId, windowId: info.newWindowId, index: info.newPosition })()
  );
  // Dragging a tab OUT to a brand-new window (tab tear-off) is not reliably
  // reported by onAttached in Firefox — the new window can be born already
  // holding the tab, with no onCreated/onAttached to observe — so onAttached
  // alone misses the move. onDetached, however, always fires when a tab leaves a
  // window. Resolve where the tab actually landed and feed it through the same
  // attach path; resolveWindow mints the window node if it was never announced.
  // For an ordinary window-to-window move (which does fire onAttached) this is a
  // harmless idempotent re-home; a tab that vanished (detach then close) get()s
  // nothing, so we leave it for onRemoved.
  t.onDetached?.addListener((tabId) =>
    // Two-arg then: swallow only a tabs.get rejection (the tab was closed right
    // after detaching — onRemoved handles it), not a throw from the handler, which
    // should surface like every other listener's does.
    Promise.resolve(api.tabs.get(tabId)).then((tab) => {
      if (tab) sink.tabAttached({ tabId, windowId: tab.windowId, index: tab.index })();
    }, () => {})
  );
  w.onCreated.addListener((win) => sink.windowOpened(win.id)());
  w.onRemoved.addListener((winId) => sink.windowClosed(winId)());
};

export const focusTabImpl = (api) => (tabId) => () =>
  Promise.resolve(api.tabs.update(tabId, { active: true })).then(() =>
    Promise.resolve(api.tabs.get(tabId)).then((t) =>
      t ? api.windows.update(t.windowId, { focused: true }) : undefined
    )
  );

export const createTabImpl = (api) => (windowId) => (url) => () => {
  const props = {};
  if (windowId !== null) props.windowId = windowId;
  if (url !== null) props.url = url;
  return Promise.resolve(api.tabs.create(props));
};

export const createWindowImpl = (api) => (urls) => () =>
  Promise.resolve(api.windows.create({ url: urls }));

// Move an existing tab into another window at `index` (-1 = append). Fires
// tabs.onAttached.
export const moveTabToWindowImpl = (api) => (tabId) => (windowId) => (index) => () =>
  Promise.resolve(api.tabs.move(tabId, { windowId, index }));

// Create a new window holding existing tabs: the first tab opens the window
// (windows.onCreated, then its tabs.onAttached), and the rest move in after.
export const newWindowWithTabsImpl = (api) => (tabIds) => () => {
  if (tabIds.length === 0) return Promise.resolve();
  const [first, ...rest] = tabIds;
  return Promise.resolve(api.windows.create({ tabId: first })).then((w) =>
    Promise.all(rest.map((t) => api.tabs.move(t, { windowId: w.id, index: -1 })))
  );
};

export const removeTabImpl = (api) => (tabId) => () =>
  Promise.resolve(api.tabs.remove(tabId));
