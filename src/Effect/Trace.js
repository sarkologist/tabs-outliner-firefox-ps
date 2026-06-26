// Debug tracing for diagnosing the restore flow. On by default; silence with
// `globalThis.__toTrace = false` in the background console (about:debugging ->
// Inspect). Each line is prefixed with a monotonic ms clock so event ORDER and
// the gaps between them are visible.
export const traceImpl = (msg) => () => {
  if (globalThis.__toTrace === false) return;
  const t =
    globalThis.performance && typeof performance.now === "function"
      ? Math.round(performance.now())
      : 0;
  console.log("[trace " + t + "] " + msg);
};
