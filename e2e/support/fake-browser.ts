// A fake `globalThis.browser` for tests, injected via addInitScript BEFORE any
// app script runs. It implements the subset of the WebExtension API the app
// uses (windows/tabs/sessions/runtime), plus a `globalThis.__fake` driver so a
// test can emit live browser events. Because the app only ever touches
// globalThis.browser, the exact same compiled code runs here and in Firefox.
//
// NOTE: this function is serialized by Playwright (fn.toString()), so it must be
// fully self-contained — no imports, no closure over module scope.

export type Seed = {
  windows: Array<{
    id: number;
    tabs: Array<{ id: number; url?: string; title?: string; active?: boolean; favIconUrl?: string }>;
  }>;
};

export function installFakeBrowser(seed: Seed) {
  const wins = new Map<number, { id: number; tabIds: number[] }>();
  const tabs = new Map<number, any>();
  const msgListeners: Array<(msg: any, sender: any) => any> = [];
  let tabSeq = 100000;
  let winSeq = 900000;

  // WebExtensions commands API state (only the sidebar-toggle command we ship).
  const commandShortcuts = [
    { name: "_execute_sidebar_action", shortcut: "Ctrl+Shift+Y", _default: "Ctrl+Shift+Y" },
  ];
  // Mirror Firefox's commands grammar closely enough that the e2e can't pass for
  // shortcuts real Firefox would reject.
  const validateShortcut = (s: string) => {
    const parts = String(s).split("+");
    const key = parts[parts.length - 1];
    const mods = parts.slice(0, -1);
    const primary = ["Ctrl", "Alt", "Command", "MacCtrl"];
    const named = ["Comma", "Period", "Space", "Home", "End", "PageUp", "PageDown", "Insert", "Delete", "Up", "Down", "Left", "Right"];
    const fkey = /^F([1-9]|1[0-9])$/.test(key); // F1..F19
    // a couple of browser-reserved combos, so the rejection path is exercisable
    const reserved = ["Ctrl+Shift+Q", "Command+Shift+Q", "MacCtrl+Shift+Q"];
    if (reserved.includes(s)) return "This shortcut is reserved by the browser.";
    if (!(/^[A-Z0-9]$/.test(key) || named.includes(key) || fkey)) return "Invalid key.";
    if (mods.length > 2) return "Use at most two modifiers.";
    if (new Set(mods).size !== mods.length) return "Duplicate modifier.";
    if (mods.some((m) => !primary.includes(m) && m !== "Shift")) return "Invalid modifier.";
    if (!fkey && !mods.some((m) => primary.includes(m))) return "Shortcut must include Ctrl, Alt, Command, or MacCtrl.";
    return null;
  };

  const listener = () => {
    const ls: Array<(...a: any[]) => void> = [];
    return { addListener: (f: any) => ls.push(f), _emit: (...a: any[]) => ls.slice().forEach((f) => f(...a)) };
  };
  const ev = {
    tabCreated: listener(),
    tabRemoved: listener(),
    tabUpdated: listener(),
    tabActivated: listener(),
    tabMoved: listener(),
    tabAttached: listener(),
    winCreated: listener(),
    winRemoved: listener(),
  };

  const tabInfo = (t: any) => ({
    id: t.id,
    windowId: t.windowId,
    index: t.index,
    url: t.url,
    title: t.title,
    active: t.active,
    favIconUrl: t.favIconUrl,
  });
  const reindex = (windowId: number) => {
    const w = wins.get(windowId);
    if (w) w.tabIds.forEach((id, i) => (tabs.get(id).index = i));
  };

  for (const w of seed?.windows ?? []) {
    wins.set(w.id, { id: w.id, tabIds: [] });
    (w.tabs ?? []).forEach((t) => {
      tabs.set(t.id, {
        id: t.id,
        windowId: w.id,
        index: 0,
        url: t.url ?? null,
        title: t.title ?? "",
        active: !!t.active,
        favIconUrl: t.favIconUrl ?? null,
      });
      wins.get(w.id)!.tabIds.push(t.id);
    });
    reindex(w.id);
  }

  (globalThis as any).browser = {
    windows: {
      getAll: (opts: any = {}) =>
        Promise.resolve(
          [...wins.values()].map((w) => ({
            id: w.id,
            tabs: opts.populate ? w.tabIds.map((id) => tabInfo(tabs.get(id))) : undefined,
          }))
        ),
      update: (id: number, props: any) => {
        if (props && props.focused) driver.winFocusLog.push(id);
        return Promise.resolve({});
      },
      create: (props: any = {}) => {
        const id = ++winSeq;
        wins.set(id, { id, tabIds: [] });
        ev.winCreated._emit({ id });
        const urls = Array.isArray(props.url) ? props.url : props.url != null ? [props.url] : [];
        urls.forEach((u: string, i: number) =>
          driver.openTab({ id: ++tabSeq, windowId: id, url: u, title: u, active: i === 0 })
        );
        return Promise.resolve({ id, tabs: wins.get(id)!.tabIds.map((tid) => tabInfo(tabs.get(tid))) });
      },
      onCreated: ev.winCreated,
      onRemoved: ev.winRemoved,
    },
    tabs: {
      get: (id: number) => Promise.resolve(tabs.has(id) ? tabInfo(tabs.get(id)) : undefined),
      update: (id: number, props: any) => {
        const t = tabs.get(id);
        if (t && props && props.active) {
          driver.focusLog.push(id);
          driver.activateTab(id);
        }
        return Promise.resolve(t ? tabInfo(t) : {});
      },
      create: (props: any) => {
        const id = ++tabSeq;
        driver.openTab({ id, windowId: props.windowId ?? firstWindowId(), url: props.url, title: props.url ?? "", active: true });
        return Promise.resolve(tabInfo(tabs.get(id)));
      },
      remove: (id: number) => {
        driver.closeTab(id);
        return Promise.resolve();
      },
      onCreated: ev.tabCreated,
      onRemoved: ev.tabRemoved,
      onUpdated: ev.tabUpdated,
      onActivated: ev.tabActivated,
      onMoved: ev.tabMoved,
      onAttached: ev.tabAttached,
    },
    sessions: { restore: () => Promise.resolve({}) },
    runtime: {
      // real Firefox structured-clones messages across contexts; mirror that so
      // the harness reflects real serialization cost and no accidental aliasing
      sendMessage: (msg: any) => {
        const m = structuredClone(msg);
        for (const l of msgListeners.slice()) {
          const r = l(m, {});
          if (r !== undefined) return Promise.resolve(r).then((v) => structuredClone(v));
        }
        return Promise.reject(new Error("no receiver"));
      },
      onMessage: { addListener: (f: any) => msgListeners.push(f) },
    },
    commands: {
      getAll: () => Promise.resolve(commandShortcuts.map((c) => ({ name: c.name, shortcut: c.shortcut }))),
      update: ({ name, shortcut }: { name: string; shortcut: string }) => {
        const c = commandShortcuts.find((x) => x.name === name);
        if (!c) return Promise.reject(new Error("Unknown command: " + name));
        const err = validateShortcut(shortcut);
        if (err) return Promise.reject(new Error(err));
        c.shortcut = shortcut;
        return Promise.resolve();
      },
      reset: (name: string) => {
        const c = commandShortcuts.find((x) => x.name === name);
        if (c) c.shortcut = c._default;
        return Promise.resolve();
      },
    },
  };

  function firstWindowId() {
    return [...wins.keys()][0];
  }

  const driver: any = {
    focusLog: [] as number[],
    winFocusLog: [] as number[],
    // current shortcut bound to a command, for assertions
    commandShortcut: (name: string) => {
      const c = commandShortcuts.find((x) => x.name === name);
      return c ? c.shortcut : null;
    },
    // read-only view of the live windows + their tab urls, for assertions
    listWindows: () =>
      [...wins.values()].map((w) => ({
        id: w.id,
        tabs: w.tabIds.map((id) => ({ url: tabs.get(id).url, title: tabs.get(id).title })),
      })),
    openWindow: (id: number) => {
      if (!wins.has(id)) wins.set(id, { id, tabIds: [] });
      ev.winCreated._emit({ id });
    },
    closeWindow: (id: number) => {
      const w = wins.get(id);
      if (!w) return;
      w.tabIds.slice().forEach((tid) => tabs.delete(tid));
      wins.delete(id);
      ev.winRemoved._emit(id);
    },
    openTab: (t: { id: number; windowId: number; index?: number; url?: string; title?: string; active?: boolean }) => {
      if (!wins.has(t.windowId)) driver.openWindow(t.windowId);
      const w = wins.get(t.windowId)!;
      const index = t.index ?? w.tabIds.length;
      const tab = {
        id: t.id,
        windowId: t.windowId,
        index,
        url: t.url ?? null,
        title: t.title ?? "",
        active: !!t.active,
        favIconUrl: null,
      };
      tabs.set(t.id, tab);
      w.tabIds.splice(index, 0, t.id);
      reindex(t.windowId);
      ev.tabCreated._emit(tabInfo(tab));
    },
    closeTab: (id: number) => {
      const t = tabs.get(id);
      if (!t) return;
      const w = wins.get(t.windowId);
      if (w) w.tabIds = w.tabIds.filter((x) => x !== id);
      tabs.delete(id);
      if (w) reindex(t.windowId);
      ev.tabRemoved._emit(id, { windowId: t.windowId, isWindowClosing: false });
    },
    updateTab: (id: number, change: any) => {
      const t = tabs.get(id);
      if (t) Object.assign(t, change);
      ev.tabUpdated._emit(id, change, t ? tabInfo(t) : {});
    },
    activateTab: (id: number) => {
      const t = tabs.get(id);
      if (!t) return;
      for (const o of tabs.values()) if (o.windowId === t.windowId) o.active = o.id === id;
      ev.tabActivated._emit({ tabId: id, windowId: t.windowId });
    },
    moveTab: (id: number, toIndex: number) => {
      const t = tabs.get(id);
      if (!t) return;
      const w = wins.get(t.windowId)!;
      w.tabIds = w.tabIds.filter((x) => x !== id);
      w.tabIds.splice(toIndex, 0, id);
      reindex(t.windowId);
      ev.tabMoved._emit(id, { fromIndex: -1, toIndex, windowId: t.windowId });
    },
    attachTab: (id: number, newWindowId: number, newPosition: number) => {
      const t = tabs.get(id);
      if (!t) return;
      const old = wins.get(t.windowId);
      if (old) old.tabIds = old.tabIds.filter((x) => x !== id);
      if (!wins.has(newWindowId)) driver.openWindow(newWindowId);
      const nw = wins.get(newWindowId)!;
      nw.tabIds.splice(newPosition, 0, id);
      t.windowId = newWindowId;
      reindex(newWindowId);
      ev.tabAttached._emit(id, { newWindowId, newPosition });
    },
  };

  (globalThis as any).__fake = driver;
}
