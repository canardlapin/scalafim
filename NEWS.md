# Changes

## Unreleased

- Repeated surface world-coordinate picks can use an explicitly prepared
  immutable index, preserving original vertex ids under affine placement.
- Viewer models support validated layer reorder/replacement; Canvas controllers
  adopt edits while retaining session state and unchanged-layer caches.
- The reference surface raster backend now advertises `Lighting` in its
  capabilities, matching the per-vertex ambient/diffuse Lambert shading it
  already applied from world-space normals. Its caveats state the light-vector
  convention and the absence of per-pixel, specular and shadow lighting.
  Backend admission that consults capabilities now treats it as lit-capable.
