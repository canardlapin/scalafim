# scalafim-graphics-java2d

`graphics-java2d` is the JVM raster backend for the renderer-neutral
`graphics` device scene. It compiles scenes into deterministic Java2D commands
and interprets them against `java.awt.Graphics2D`.

Tests combine the shared renderer conformance contract with real
`BufferedImage` pixel assertions. The backend owns Java2D-specific stroke,
font-metric, clipping, transform, and alpha-compositing behavior; it contains no
plot, scale, guide, or layout semantics.

Shared raster images are materialized as cached ARGB `BufferedImage` values and
drawn with explicit nearest-neighbor or bilinear interpolation. Image-level
tests independently pin top-left row order, source alpha, grob alpha, and
z-order with later vector marks.

`tools/render_position_adjustment_qa.sh` renders the canonical dodge, stack,
and seeded-jitter scenes through Java2D beside independently generated ggplot2
references. The resulting comparison page is a review artifact, while numeric
position laws remain the automated JVM/Scala.js gate.
