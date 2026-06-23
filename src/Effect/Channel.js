// browser.runtime messaging. Requests and broadcasts share one onMessage bus,
// disambiguated by `kind`. A request listener returns a Promise<Json> which the
// WebExtension runtime delivers as the response (Firefox semantics); the fake
// runtime in tests implements the same contract.

const REQ = "req";
const PATCH = "patch";

export const requestImpl = (api) => (json) => () =>
  Promise.resolve(api.runtime.sendMessage({ kind: REQ, body: json }));

export const onRequestImpl = (api) => (handler) => () => {
  api.runtime.onMessage.addListener((msg) => {
    if (!msg || msg.kind !== REQ) return undefined;
    return handler(msg.body)(); // Effect (Promise Json) -> Promise Json
  });
};

export const broadcastImpl = (api) => (json) => () => {
  // No receiver (e.g. no sidebar open) rejects; that is fine.
  Promise.resolve(api.runtime.sendMessage({ kind: PATCH, body: json })).catch(() => {});
};

export const onBroadcastImpl = (api) => (handler) => () => {
  api.runtime.onMessage.addListener((msg) => {
    if (!msg || msg.kind !== PATCH) return undefined;
    handler(msg.body)();
    return undefined;
  });
};
