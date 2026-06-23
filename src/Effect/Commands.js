// WebExtensions commands API for the sidebar-toggle command. Every call guards
// for the API being absent (non-Firefox / the test harness without a fake),
// resolving to a sentinel the PureScript side maps to "unavailable".

const SIDEBAR_CMD = "_execute_sidebar_action";

const cmdsApi = () =>
  globalThis.browser && browser.commands ? browser.commands : null;

export const isMac = () =>
  typeof navigator !== "undefined" &&
  /mac/i.test(navigator.platform || navigator.userAgent || "");

// Effect (Promise (Nullable String)): null => API/command unavailable; "" => unset.
export const getSidebarToggleImpl = () => {
  const c = cmdsApi();
  if (!c || !c.getAll) return Promise.resolve(null);
  return Promise.resolve(c.getAll())
    .then((list) => {
      const cmd = (list || []).find((x) => x && x.name === SIDEBAR_CMD);
      return cmd && typeof cmd.shortcut === "string" ? cmd.shortcut : "";
    })
    .catch(() => null);
};

// null => success; a string => the browser's rejection message.
export const setSidebarToggleImpl = (shortcut) => () => {
  const c = cmdsApi();
  if (!c || !c.update) return Promise.resolve("Editing shortcuts isn't available here.");
  return Promise.resolve(c.update({ name: SIDEBAR_CMD, shortcut }))
    .then(() => null)
    .catch((e) => String((e && e.message) || e));
};

export const resetSidebarToggleImpl = () => {
  const c = cmdsApi();
  if (!c || !c.reset) return Promise.resolve(null);
  return Promise.resolve(c.reset(SIDEBAR_CMD))
    .then(() => null)
    .catch((e) => String((e && e.message) || e));
};
