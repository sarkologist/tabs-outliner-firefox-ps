// WebExtension FFI. `api` is globalThis.browser (real in Firefox, fake in
// tests). The curried shapes match the PureScript foreign imports; functions
// returning Effect are `() => ...` thunks, and Sink callbacks are
// `(arg) => () => unit`, so we call them as `sink.fn(arg)()`.

export const getBrowser = () => globalThis.browser;

export const initSidebarActionImpl = (api) => () => {
  const action = api && api.action;
  const sidebar = api && api.sidebarAction;
  if (!action?.onClicked || typeof sidebar?.open !== "function") return;
  action.onClicked.addListener(() => {
    Promise.resolve(sidebar.open()).catch(() => {});
  });
};

const BACKUP_ALARM = "tabs-outliner-automatic-backup";
const BACKUP_ENABLED_KEY = "tabsOutlinerAutomaticBackupsEnabled";
const BACKUP_LAST_SUCCESS_KEY = "tabsOutlinerAutomaticBackupLastSuccessfulAt";
const BACKUP_INTERVAL_MS = 24 * 60 * 60 * 1000;
const SIDEBAR_PATH = "sidebar/sidebar.html";
const FULL_SIZE_SIDEBAR_PATH = `${SIDEBAR_PATH}?view=window`;

const outlinerPopupWindowIds = new Set();
const pendingOutlinerPopupWindowIds = new Set();
const fullSizePopupFocusRecency = [];
const nonOutlinerWindowIds = new Set();
let outlinerPopupCreationDepth = 0;

// FIFO of container node ids awaiting the windows.onCreated of a restore/rehome
// window we just asked the browser to create. One entry (a `{ node }` wrapper) is
// pushed per window-creating call (in creation order) and popped by the next
// matching onCreated, so each new window binds to the exact node that requested
// it (a precise `windowBound` instead of the reducer guessing from its shared
// pending queue — the fix for two windows restored at once cross-wiring). A `null`
// node marks a create with no container to bind (a fresh window at the root); it
// pops in lockstep so the queue stays aligned with creation order, and yields a
// plain `windowOpened`. A failed create removes its own entry (see
// registerRestoreBind), so the queue only ever holds creates still awaiting their
// onCreated — the same head-pop exposure to an interleaved user-opened window that
// the reducer's pending-window fallback already had.
const restoreBindQueue = [];

// Firefox does not order a new window's windows.onCreated before its first
// tabs.onCreated/onAttached. If a created restore window's tab arrived first, the
// reducer would bind that window from its FIFO fallback and concurrent restores
// could cross-wire. So while a restore create is still awaiting its onCreated,
// hold tab/attach events for any not-yet-announced window and replay them the
// moment that window binds (see the windows.onCreated flush). Keyed by windowId.
const bufferedWindowTabs = new Map();
const bufferTabEventIfPending = (windowId, emit) => {
  if (restoreBindQueue.length > 0 && !nonOutlinerWindowIds.has(windowId) && !isKnownOrPendingOutlinerWindow(windowId)) {
    const arr = bufferedWindowTabs.get(windowId);
    if (arr) arr.push(emit);
    else bufferedWindowTabs.set(windowId, [emit]);
    return true;
  }
  return false;
};
const flushBufferedTabs = (windowId) => {
  const arr = bufferedWindowTabs.get(windowId);
  if (!arr) return;
  bufferedWindowTabs.delete(windowId);
  for (const emit of arr) emit();
};

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

const extensionUrl = (api, path) => {
  const rt = api && api.runtime;
  return rt && typeof rt.getURL === "function" ? rt.getURL(path) : `moz-extension://extension-id/${path}`;
};

const isOutlinerSidebarUrl = (api, url) =>
  typeof url === "string" && url.startsWith(extensionUrl(api, SIDEBAR_PATH));

const isOutlinerWindow = (api, win) =>
  (win?.tabs ?? []).some((tab) => isOutlinerSidebarUrl(api, tab.url));

const noteFullSizePopup = (windowId) => {
  if (typeof windowId !== "number") return;
  outlinerPopupWindowIds.add(windowId);
  pendingOutlinerPopupWindowIds.delete(windowId);
  const i = fullSizePopupFocusRecency.indexOf(windowId);
  if (i >= 0) fullSizePopupFocusRecency.splice(i, 1);
  fullSizePopupFocusRecency.push(windowId);
};

const forgetFullSizePopup = (windowId) => {
  outlinerPopupWindowIds.delete(windowId);
  pendingOutlinerPopupWindowIds.delete(windowId);
  const i = fullSizePopupFocusRecency.indexOf(windowId);
  if (i >= 0) fullSizePopupFocusRecency.splice(i, 1);
};

const isKnownOrPendingOutlinerWindow = (windowId) =>
  outlinerPopupWindowIds.has(windowId) || pendingOutlinerPopupWindowIds.has(windowId);

const isOutlinerPopupPlaceholderTab = (tab) => {
  if (!tab || outlinerPopupCreationDepth <= 0) return false;
  if (typeof tab.windowId !== "number" || nonOutlinerWindowIds.has(tab.windowId)) return false;
  const url = tab.url ?? "";
  return url === "" || url === "about:blank" || url === "about:newtab" || tab.title === "New Tab";
};

const shouldIgnoreTab = (api, tab) => {
  if (!tab) return false;
  if (isKnownOrPendingOutlinerWindow(tab.windowId)) return true;
  if (isOutlinerPopupPlaceholderTab(tab)) {
    pendingOutlinerPopupWindowIds.add(tab.windowId);
    return true;
  }
  if (isOutlinerSidebarUrl(api, tab.url)) {
    noteFullSizePopup(tab.windowId);
    return true;
  }
  return false;
};

export const getAllWindowsImpl = (api) => () =>
  Promise.resolve(api.windows.getAll({ populate: true })).then((wins) =>
    Promise.all(
      wins.filter((w) => {
        if (isOutlinerWindow(api, w)) {
          nonOutlinerWindowIds.delete(w.id);
          noteFullSizePopup(w.id);
          return false;
        }
        if (isKnownOrPendingOutlinerWindow(w.id)) return false;
        nonOutlinerWindowIds.add(w.id);
        return true;
      }).map((w) =>
        Promise.all(
          (w.tabs ?? []).map((t) =>
            getTabKey(api, t.id).then((nodeKey) => ({
              tabId: t.id,
              windowId: t.windowId,
              openerTabId: t.openerTabId ?? null,
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
  t.onCreated.addListener((tab) => {
    if (shouldIgnoreTab(api, tab)) return;
    const emit = () => sink.tabOpened({
      tabId: tab.id,
      windowId: tab.windowId,
      openerTabId: tab.openerTabId ?? null,
      index: tab.index,
      url: tab.url ?? null,
      title: tab.title ?? "",
      active: !!tab.active,
      favIconUrl: tab.favIconUrl ?? null,
    })();
    if (bufferTabEventIfPending(tab.windowId, emit)) return;
    emit();
  });
  t.onRemoved.addListener((tabId, info) => {
    if (info && isKnownOrPendingOutlinerWindow(info.windowId)) return;
    sink.tabClosed(tabId)();
  });
  t.onUpdated.addListener((tabId, change, tab) => {
    if (shouldIgnoreTab(api, tab) || isOutlinerSidebarUrl(api, change.url)) return;
    sink.tabChanged({
      tabId,
      url: change.url ?? null,
      title: change.title ?? (tab && tab.title) ?? null,
      favIconUrl: change.favIconUrl ?? null,
    })();
  });
  t.onActivated.addListener((info) => {
    if (isKnownOrPendingOutlinerWindow(info.windowId)) return;
    sink.tabActivated({ tabId: info.tabId, windowId: info.windowId })()
  });
  t.onMoved.addListener((tabId, info) => {
    if (isKnownOrPendingOutlinerWindow(info.windowId)) return;
    sink.tabMoved({ tabId, windowId: info.windowId, toIndex: info.toIndex })()
  });
  t.onAttached.addListener((tabId, info) => {
    if (isKnownOrPendingOutlinerWindow(info.newWindowId)) return;
    const emit = () => sink.tabAttached({ tabId, windowId: info.newWindowId, index: info.newPosition })();
    if (bufferTabEventIfPending(info.newWindowId, emit)) return;
    emit();
  });
  // Dragging a tab OUT to a brand-new window (tab tear-off) is not reliably
  // reported by onAttached in Firefox — the new window can be born already
  // holding the tab, with no onCreated/onAttached to observe — so onAttached
  // alone misses the move. onDetached, however, always fires when a tab leaves a
  // window. Resolve where the tab actually landed and feed it through the same
  // attach path; resolveWindow mints the window node if it was never announced.
  // For an ordinary window-to-window move (which does fire onAttached) this is a
  // harmless idempotent re-home; a tab that vanished (detach then close) get()s
  // nothing, so we leave it for onRemoved.
  t.onDetached?.addListener((tabId, info) => {
    if (info && isKnownOrPendingOutlinerWindow(info.oldWindowId)) return;
    // Two-arg then: swallow only a tabs.get rejection (the tab was closed right
    // after detaching — onRemoved handles it), not a throw from the handler, which
    // should surface like every other listener's does.
    Promise.resolve(api.tabs.get(tabId)).then((tab) => {
      // Not buffered: a user tear-off births a window that fires NO
      // windows.onCreated (only this onDetached), so there is no later flush — and
      // such a window is never one of our restore creates anyway (those always
      // fire onCreated). Buffering here would strand the event forever.
      if (tab && !shouldIgnoreTab(api, tab)) sink.tabAttached({ tabId, windowId: tab.windowId, index: tab.index })();
    }, () => {});
  });
  w.onCreated.addListener((win) => {
    // Firefox can briefly report the full-size popup as a normal window whose
    // first tab is "New Tab" before the extension URL is visible. The in-flight
    // create call is the only reliable early signal, so mark the window pending
    // regardless of the reported type.
    if (outlinerPopupCreationDepth > 0) {
      pendingOutlinerPopupWindowIds.add(win.id);
      return;
    }
    if (isKnownOrPendingOutlinerWindow(win.id)) return;
    if (win.type === "popup") {
      openFullSizeSidebarWindows(api).then((open) => {
        if (open.some((w) => w.windowId === win.id)) return;
        if (!isKnownOrPendingOutlinerWindow(win.id)) {
          nonOutlinerWindowIds.add(win.id);
          sink.windowOpened(win.id)();
          // an external popup opening mid-restore may have buffered a tab event;
          // now that it's announced, replay so it isn't stranded.
          flushBufferedTabs(win.id);
        }
      });
      return;
    }
    nonOutlinerWindowIds.add(win.id);
    // Pair this window to the container that asked us to create it, if any: pop
    // the next queued entry (FIFO, in creation order). Popping on every new-window
    // event (not gated by an in-flight flag) means the entry is consumed even if
    // the create promise already settled — otherwise a restore window would leak
    // its entry and fall through to a plain `windowOpened`.
    const entry = restoreBindQueue.length ? restoreBindQueue.shift() : null;
    const boundNode = entry ? entry.node : null;
    if (boundNode != null) sink.windowBound({ node: boundNode, windowId: win.id })();
    else sink.windowOpened(win.id)();
    // now that the window is announced/bound, replay any tab events that raced
    // ahead of this onCreated (see bufferTabEventIfPending)
    flushBufferedTabs(win.id);
  });
  w.onRemoved.addListener((winId) => {
    nonOutlinerWindowIds.delete(winId);
    if (isKnownOrPendingOutlinerWindow(winId)) {
      forgetFullSizePopup(winId);
      return;
    }
    sink.windowClosed(winId)();
  });
};

export const focusTabImpl = (api) => (tabId) => () =>
  Promise.resolve(api.tabs.update(tabId, { active: true })).then(() =>
    Promise.resolve(api.tabs.get(tabId)).then((t) =>
      t ? api.windows.update(t.windowId, { focused: true }) : undefined
    )
  );

export const createTabImpl = (api) => (windowId) => (index) => (url) => () => {
  const props = {};
  if (windowId !== null) props.windowId = windowId;
  if (index !== null) props.index = index;
  if (url !== null) props.url = url;
  return Promise.resolve(api.tabs.create(props));
};

// Register a create so its window's onCreated binds to `nodeKey` (or null → a
// plain new window). Returns a `dropOnFailure` callback for the create's reject
// path: a failed create fires no windows.onCreated to consume the entry, so it
// must be removed or it would offset every later restore's pairing. A SUCCESSFUL
// create is left entirely to windows.onCreated (which pops the entry and flushes
// any buffered tabs), so binding never races the create promise's settlement —
// the two are unordered — and buffered tabs can't replay early.
const registerRestoreBind = (nodeKey) => {
  const entry = { node: nodeKey };
  restoreBindQueue.push(entry);
  return () => {
    const i = restoreBindQueue.indexOf(entry);
    if (i >= 0) restoreBindQueue.splice(i, 1);
  };
};

// `type: "normal"` so the created window inherits the sidebar (see master's
// sidebar-for-command-windows change); nodeKey pairs it back to its container.
export const createWindowImpl = (api) => (nodeKey) => (urls) => () => {
  const dropOnFailure = registerRestoreBind(nodeKey);
  return Promise.resolve(api.windows.create({ type: "normal", url: urls })).catch((err) => { dropOnFailure(); throw err; });
};

// Move an existing tab into another window at `index` (-1 = append). Fires
// tabs.onAttached.
export const moveTabToWindowImpl = (api) => (tabId) => (windowId) => (index) => () =>
  Promise.resolve(api.tabs.move(tabId, { windowId, index }));

// Create a new window holding existing tabs: the first tab opens the window
// (windows.onCreated, then its tabs.onAttached), and the rest move in after.
export const newWindowWithTabsImpl = (api) => (nodeKey) => (tabIds) => () => {
  if (tabIds.length === 0) return Promise.resolve();
  // nodeKey is null for a fresh window at the root (nothing to bind); it still
  // registers so the queue stays aligned with creation order.
  const dropOnFailure = registerRestoreBind(nodeKey);
  const [first, ...rest] = tabIds;
  // Only a failed windows.create (no window, so no onCreated) drops the entry; if
  // the window is created its onCreated consumes it, even if a later tabs.move
  // rejects. `type: "normal"` so it inherits the sidebar.
  return Promise.resolve(api.windows.create({ type: "normal", tabId: first })).then(
    (w) => Promise.all(rest.map((t) => api.tabs.move(t, { windowId: w.id, index: -1 }))),
    (err) => { dropOnFailure(); throw err; }
  );
};

export const removeTabImpl = (api) => (tabId) => () =>
  Promise.resolve(api.tabs.remove(tabId));

const openFullSizeSidebarWindows = (api) =>
  Promise.resolve(api.windows.getAll({ populate: true, windowTypes: ["popup"] }))
    .catch(() => [])
    .then((wins) =>
      wins
        .filter((w) => isOutlinerWindow(api, w))
        .map((w) => {
          noteFullSizePopup(w.id);
          return { windowId: w.id, focused: !!w.focused };
        })
    );

const pickFullSizePopup = (open) => {
  const openIds = new Set(open.map((w) => w.windowId));
  for (let i = fullSizePopupFocusRecency.length - 1; i >= 0; i--) {
    const windowId = fullSizePopupFocusRecency[i];
    if (openIds.has(windowId)) return windowId;
  }
  const focused = open.find((w) => w.focused);
  if (focused) return focused.windowId;
  const ids = open.map((w) => w.windowId);
  return ids.length ? Math.max(...ids) : null;
};

const createFullSizeOutliner = (api) => {
  outlinerPopupCreationDepth++;
  return Promise.resolve(
    api.windows.create({
      url: extensionUrl(api, FULL_SIZE_SIDEBAR_PATH),
      type: "popup",
      state: "maximized",
      focused: true,
    })
  ).then(
    (win) => {
      noteFullSizePopup(win && win.id);
      return undefined;
    },
    (err) => {
      throw err;
    }
  ).finally(() => {
    outlinerPopupCreationDepth = Math.max(0, outlinerPopupCreationDepth - 1);
  });
};

export const openFullSizeOutlinerImpl = (api) => (sourceWindowId) => () =>
  openFullSizeSidebarWindows(api).then((open) => {
    const clickedFromFullSize =
      sourceWindowId !== null && open.some((w) => w.windowId === sourceWindowId);
    const target = clickedFromFullSize ? null : pickFullSizePopup(open);
    if (target === null) return createFullSizeOutliner(api);
    return Promise.resolve(api.windows.update(target, { focused: true })).then(
      () => {
        noteFullSizePopup(target);
      },
      () => {
        forgetFullSizePopup(target);
        return createFullSizeOutliner(api);
      }
    );
  });

export const getAutomaticBackupsEnabledImpl = (api) => () => {
  const local = api && api.storage && api.storage.local;
  if (!local || typeof local.get !== "function") return Promise.resolve(false);
  return Promise.resolve(local.get(BACKUP_ENABLED_KEY)).then(
    (stored) => stored && stored[BACKUP_ENABLED_KEY] === true,
    () => false
  );
};

export const setAutomaticBackupsEnabledImpl = (api) => (enabled) => () => {
  const local = api && api.storage && api.storage.local;
  if (!local || typeof local.set !== "function") return Promise.resolve();
  return Promise.resolve(local.set({ [BACKUP_ENABLED_KEY]: !!enabled })).then(() => undefined);
};

export const automaticBackupDueImpl = (api) => () => {
  const local = api && api.storage && api.storage.local;
  if (!local || typeof local.get !== "function") return Promise.resolve(true);
  return Promise.resolve(local.get(BACKUP_LAST_SUCCESS_KEY)).then(
    (stored) => {
      const raw = stored && stored[BACKUP_LAST_SUCCESS_KEY];
      const last = typeof raw === "string" ? Date.parse(raw) : NaN;
      return !Number.isFinite(last) || Date.now() - last >= BACKUP_INTERVAL_MS;
    },
    () => true
  );
};

export const recordAutomaticBackupSuccessImpl = (api) => () => {
  const local = api && api.storage && api.storage.local;
  if (!local || typeof local.set !== "function") return Promise.resolve();
  return Promise.resolve(local.set({ [BACKUP_LAST_SUCCESS_KEY]: new Date().toISOString() })).then(
    () => undefined
  );
};

export const ensureBackupAlarmImpl = (api) => () => {
  const alarms = api && api.alarms;
  if (!alarms || typeof alarms.create !== "function") return Promise.resolve();
  const create = () => Promise.resolve(
    alarms.create(BACKUP_ALARM, { periodInMinutes: 24 * 60 })
  ).then(() => undefined);
  if (typeof alarms.get !== "function") return create();
  return Promise.resolve(alarms.get(BACKUP_ALARM)).then(
    (alarm) => (alarm ? undefined : create()),
    () => create()
  );
};

export const clearBackupAlarmImpl = (api) => () => {
  const alarms = api && api.alarms;
  if (!alarms || typeof alarms.clear !== "function") return Promise.resolve();
  return Promise.resolve(alarms.clear(BACKUP_ALARM)).then(() => undefined);
};

export const onBackupAlarmImpl = (api) => (cb) => () => {
  const alarms = api && api.alarms;
  if (!alarms || !alarms.onAlarm || typeof alarms.onAlarm.addListener !== "function") return;
  alarms.onAlarm.addListener((alarm) => {
    if (alarm && alarm.name === BACKUP_ALARM) cb();
  });
};

const localDateSlug = (date) => {
  const y = date.getFullYear();
  const m = String(date.getMonth() + 1).padStart(2, "0");
  const d = String(date.getDate()).padStart(2, "0");
  return `${y}-${m}-${d}`;
};

export const backupFilename = () =>
  `grove-backups/grove-${localDateSlug(new Date())}.json`;

export const downloadBackupImpl = (api) => (filename) => (content) => () => {
  const downloads = api && api.downloads;
  if (!downloads || typeof downloads.download !== "function") return Promise.resolve();
  const blob = new Blob([content], { type: "application/json" });
  const url = URL.createObjectURL(blob);
  const changes = downloads.onChanged;
  const canObserve =
    changes &&
    typeof changes.addListener === "function" &&
    typeof changes.removeListener === "function";
  let revoked = false;
  const revoke = () => {
    if (!revoked) {
      revoked = true;
      URL.revokeObjectURL(url);
    }
  };
  if (!canObserve) {
    return Promise.resolve(
      downloads.download({ url, filename, saveAs: false, conflictAction: "uniquify" })
    ).then(() => revoke(), (err) => {
      revoke();
      throw err;
    });
  }
  return new Promise((resolve, reject) => {
    let downloadId = null;
    const pending = [];
    let settled = false;
    const cleanup = () => {
      changes.removeListener(listener);
      revoke();
    };
    const settle = (fn, value) => {
      if (settled) return;
      settled = true;
      cleanup();
      fn(value);
    };
    const finishFromDelta = (delta) => {
      const state = delta && delta.state && delta.state.current;
      if (!state) return false;
      if (downloadId === null) {
        pending.push(delta);
        return false;
      }
      if (delta.id !== downloadId) return false;
      if (state === "complete") {
        settle(resolve, undefined);
        return true;
      }
      if (state === "interrupted") {
        const detail = delta.error && (delta.error.current || delta.error);
        settle(reject, new Error("Automatic backup download interrupted" + (detail ? ": " + detail : "")));
        return true;
      }
      return false;
    };
    const listener = (delta) => {
      finishFromDelta(delta);
    };
    changes.addListener(listener);
    Promise.resolve(
      downloads.download({ url, filename, saveAs: false, conflictAction: "uniquify" })
    ).then((id) => {
      downloadId = id;
      pending.slice().some(finishFromDelta);
    }, (err) => settle(reject, err));
  });
};
