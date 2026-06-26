// WebExtension FFI. `api` is globalThis.browser (real in Firefox, fake in
// tests). The curried shapes match the PureScript foreign imports; functions
// returning Effect are `() => ...` thunks, and Sink callbacks are
// `(arg) => () => unit`, so we call them as `sink.fn(arg)()`.

// Debug trace: route raw-event logs through the shared sink Effect/Trace.js
// installs on globalThis (same localStorage buffer + on/off flag). A no-op until
// the background's Trace module loads and while tracing is disabled.
const tlog = (msg) => {
  try {
    if (globalThis.__toTraceSink) globalThis.__toTraceSink(msg);
  } catch {}
};

export const getBrowser = () => globalThis.browser;

export const getAllWindowsImpl = (api) => () =>
  Promise.resolve(api.windows.getAll({ populate: true })).then((wins) =>
    wins.map((w) => ({
      windowId: w.id,
      tabs: (w.tabs ?? []).map((t) => ({
        tabId: t.id,
        windowId: t.windowId,
        index: t.index,
        url: t.url ?? null,
        title: t.title ?? "",
        active: !!t.active,
        favIconUrl: t.favIconUrl ?? null,
      })),
    }))
  );

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
  t.onCreated.addListener((tab) => {
    // RAW arrival time, before the background's queue/drainer — compare with the
    // EV dispatch line to see if buffering reorders relative to windows.onCreated.
    tlog("RAW tabs.onCreated tab=" + tab.id + " win=" + tab.windowId + " idx=" + tab.index + " url=" + (tab.url ?? "-"));
    sink.tabOpened({
      tabId: tab.id,
      windowId: tab.windowId,
      index: tab.index,
      url: tab.url ?? null,
      title: tab.title ?? "",
      active: !!tab.active,
      favIconUrl: tab.favIconUrl ?? null,
    })();
  });
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
  w.onCreated.addListener((win) => {
    tlog("RAW windows.onCreated win=" + win.id);
    sink.windowOpened(win.id)();
  });
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
  tlog("CALL tabs.create props=" + JSON.stringify(props));
  return Promise.resolve(api.tabs.create(props)).then((tab) => {
    tlog("DONE tabs.create -> tab=" + (tab && tab.id) + " win=" + (tab && tab.windowId));
  });
};

export const createWindowImpl = (api) => (urls) => () => {
  tlog("CALL windows.create urls=" + JSON.stringify(urls));
  return Promise.resolve(api.windows.create({ url: urls })).then((w) => {
    const tabs = w && w.tabs ? w.tabs.map((x) => ({ id: x.id, url: x.url })) : [];
    tlog("DONE windows.create -> win=" + (w && w.id) + " tabs=" + JSON.stringify(tabs));
  });
};

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
