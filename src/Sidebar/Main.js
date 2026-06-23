// Allow drops anywhere in the sidebar by preventing the default dragover/drop
// handling (which would otherwise reject the drop). The row's own onDrop handler
// still fires; we read the dragged id from component state, so no DataTransfer
// is needed.
export const allowDrops = () => {
  document.addEventListener("dragover", (e) => e.preventDefault());
  document.addEventListener("drop", (e) => e.preventDefault());
};
