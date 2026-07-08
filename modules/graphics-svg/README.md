# scalafim-graphics-svg

`graphics-svg` is the first concrete backend for the renderer-neutral
`graphics` scene IR.

It renders `Scene` values to deterministic SVG strings without platform IO,
browser APIs, Java2D, or mutable device state. The module exists to exercise the
core scene contract before adding interactive Canvas or JVM raster backends.

Current scope:

- root SVG document options with explicit canvas size and optional title;
- points, lines, segments, rectangles, circles, text, and groups;
- basic graphical parameters: stroke, fill, alpha, line width/type, font family,
  and font size;
- nested SVG viewport wrappers with `viewBox`, `overflow`, and optional
  rotation;
- renderer-neutral `Axis` output: baselines, tick marks, and labels.

Unsupported units and unit expressions return typed `SvgRenderError` values.
That is intentional: a backend should expose missing layout semantics instead of
silently inventing device-specific behavior.
