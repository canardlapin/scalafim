# Changes

## Unreleased

- GIFTI surface readers no longer silently choose the first coordinate transform.
  Explicit target/index selection retains placement provenance; ambiguous and
  unknown placement is rejected, and native coordinates are identified in the
  rich decoded result. Geometry-only readers use the documented unambiguous policy.

- Removed the unused `scalafim.locus.Aggregation` duplicate. Use the adopted
  `locus4s.data.Aggregation` with checked `PartialMap` aggregation instead.

- Repeated surface world-coordinate picks can use an explicitly prepared
  immutable index, preserving original vertex ids under affine placement.
- Viewer models support validated layer reorder/replacement; Canvas controllers
  adopt edits while retaining session state and unchanged-layer caches.

- GIFTI readers accept Float64 as an interoperability extension, preserving
  double precision for ASCII, binary and compressed payloads on JVM and Scala.js.
  Integer and label payload APIs explicitly reject Float64 conversion.

- `Hrfs.bspline(..., includeIntercept = true)` and `HrfFunctions.bsplineBasis`
  now offer a complete clamped B-spline basis. Its columns sum to one on the
  closed response window, so constant responses and whole-window means are
  representable. Interior knots are uniform over the actual span, including
  fractional spans; the output width is `max(nBasis, degree + 1)`. Values outside
  the window are zero. The descriptor records this choice. The default retains
  the legacy R-compatible basis and its knot conventions; no normalization
  policy changes. A complete basis generally has a nonzero response at onset.
