// IndexedDB FFI. Two stores: "nodes" (one record per node, key = NodeId, value
// = JSON string) and "meta" (the root list under key "roots"). A whole patch is
// one readwrite transaction, so the cost is O(changed records).

export const openImpl = (name) => () =>
  new Promise((resolve, reject) => {
    const req = indexedDB.open(name, 1);
    req.onupgradeneeded = () => {
      const db = req.result;
      if (!db.objectStoreNames.contains("nodes")) db.createObjectStore("nodes");
      if (!db.objectStoreNames.contains("meta")) db.createObjectStore("meta");
    };
    req.onsuccess = () => resolve(req.result);
    req.onerror = () => reject(req.error);
  });

export const writePatchImpl = (db) => (payload) => () =>
  new Promise((resolve, reject) => {
    const tx = db.transaction(["nodes", "meta"], "readwrite");
    const nodes = tx.objectStore("nodes");
    for (const p of payload.puts) nodes.put(p.json, p.id);
    for (const id of payload.deletes) nodes.delete(id);
    if (payload.roots !== null && payload.roots !== undefined) {
      tx.objectStore("meta").put(payload.roots, "roots");
    }
    tx.oncomplete = () => resolve();
    tx.onerror = () => reject(tx.error);
    tx.onabort = () => reject(tx.error);
  });

export const loadImpl = (db) => () =>
  new Promise((resolve, reject) => {
    const tx = db.transaction(["nodes", "meta"], "readonly");
    const getNodes = tx.objectStore("nodes").getAll();
    const getRoots = tx.objectStore("meta").get("roots");
    tx.oncomplete = () =>
      resolve({ nodes: getNodes.result ?? [], roots: getRoots.result ?? null });
    tx.onerror = () => reject(tx.error);
  });
