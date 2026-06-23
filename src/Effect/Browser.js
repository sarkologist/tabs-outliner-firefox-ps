// WebExtension FFI. `api` is globalThis.browser (real in Firefox, fake in
// tests). The curried shapes match the PureScript foreign imports; functions
// returning Effect are `() => ...` thunks, and Sink callbacks are
// `(arg) => () => unit`, so we call them as `sink.fn(arg)()`.

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
  w.onCreated.addListener((win) => sink.windowOpened(win.id)());
  w.onRemoved.addListener((winId) => sink.windowClosed(winId)());
};

export const focusTabImpl = (api) => (windowId) => (tabId) => () =>
  Promise.resolve(api.tabs.update(tabId, { active: true })).then(() =>
    api.windows.update(windowId, { focused: true })
  );

export const createTabImpl = (api) => (windowId) => (url) => () => {
  const props = {};
  if (windowId !== null) props.windowId = windowId;
  if (url !== null) props.url = url;
  return Promise.resolve(api.tabs.create(props));
};

export const removeTabImpl = (api) => (tabId) => () =>
  Promise.resolve(api.tabs.remove(tabId));

export const restoreSessionImpl = (api) => (sessionId) => () =>
  Promise.resolve(api.sessions.restore(sessionId));
