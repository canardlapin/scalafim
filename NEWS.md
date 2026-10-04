# Changes

## Unreleased

- Repeated surface world-coordinate picks can use an explicitly prepared
  immutable index, preserving original vertex ids under affine placement.
- Viewer models support validated layer reorder/replacement; Canvas controllers
  adopt edits while retaining session state and unchanged-layer caches.
