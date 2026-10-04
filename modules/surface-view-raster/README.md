# surface-view-raster

Deterministic CPU reference interpreter for `SurfaceRenderPlan`, shared by JVM
and Scala.js. It provides exact depth buffering, culling, clipping, compositing,
native-equivalent picking receipts, and reproducible pixels. It is the semantic
oracle for concrete GPU/toolkit backends, not the preferred interactive path.

Lighting supports unlit colors and directional ambient/diffuse Lambert shading
using world-space vertex normals. The rasterizer interpolates the lit vertex
colors; it does not implement per-pixel normal shading, specular highlights,
or shadows. The directional vector `d` points from the surface toward the
light, so a face is fully lit when its normal equals `d`. Vertex normals are
derived from world-space triangle winding after the surface-to-world affine,
so they flip under a reflecting (negative-determinant) affine.
