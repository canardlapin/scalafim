# Changes

## Unreleased

- `Hrfs.bspline(..., convention = Hrfs.BsplineConvention.EndpointAnchored)`
  matches fmrihrf commit `18d418f`: exactly `nBasis` columns, uniform knots over
  the actual span, and zero at both endpoints. It requires `degree >= 1` and
  `nBasis >= max(1, degree - 1)` and has a distinct descriptor identity. The
  default `LegacyR` convention and its parity fixtures remain frozen to
  fmrihrf 0.4.0; existing identities are unchanged. Use `Complete` when the
  response basis must contain constants.

- Removed the unused `scalafim.locus.Aggregation` duplicate and the
  `cats-kernel` dependency it alone required from `locus-data`. Use the adopted
  `locus4s.data.Aggregation` with checked `PartialMap` aggregation instead.
- Repeated surface world-coordinate picks can use an explicitly prepared
  immutable index, preserving original vertex ids under affine placement.
- Viewer models support validated layer reorder/replacement; Canvas controllers
  adopt edits while retaining session state and unchanged-layer caches.
- The reference surface raster backend now advertises `Lighting` in its
  capabilities, matching the per-vertex ambient/diffuse Lambert shading it
  already applied from world-space normals. Its caveats state the light-vector
  convention and the absence of per-pixel, specular and shadow lighting.
  Backend admission that consults capabilities now treats it as lit-capable.
- `Hrfs.bspline(..., convention = Hrfs.BsplineConvention.Complete)` and
  `HrfFunctions.bsplineBasis` now offer a complete clamped B-spline basis. Its
  columns sum to one on the
  closed response window, so constant responses and whole-window means are
  representable. Interior knots are uniform over the actual span, including
  fractional spans; the output width is `max(nBasis, degree + 1)`. Values outside
  the window are zero. The descriptor records this choice. The default,
  `BsplineConvention.LegacyR`, retains the fmrihrf 0.4.0 basis and its knot
  conventions; no normalization
  policy changes. A complete basis generally has a nonzero response at onset.
  Legacy B-spline descriptor identities are unchanged.
