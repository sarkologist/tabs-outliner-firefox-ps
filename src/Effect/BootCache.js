const KEY = "tabsOutlinerBootWindow";

export const save = (json) => () => {
  try {
    localStorage.setItem(KEY, json);
  } catch {}
};

export const load = () => {
  try {
    return localStorage.getItem(KEY) || "";
  } catch {
    return "";
  }
};
