# scalafim-graphics-canvas

`graphics-canvas` is the Scala.js Canvas 2D backend for the renderer-neutral
`graphics` device scene. It compiles a `Scene` into a deterministic
`CanvasProgram`, then interprets that program against a browser
`CanvasRenderingContext2D`.

The recorded command program is the primary conformance and debugging surface:
it makes save/restore nesting, transform-before-clip ordering, primitive
selection, paint, text alignment, and z-order testable without depending on
browser pixel rasterization. The backend contains no plot, scale, guide, or
layout semantics.

`BrowserGallery` in the Scala.js test sources exports a real-browser review
entry point. It renders every shared conformance scene through the production
Canvas interpreter and is intended for headless-Chrome or interactive visual
QA after linking the test configuration as a `NoModule` script.
