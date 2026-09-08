package scalafim.examples.surfaceview

import scalafim.surface.*
import scalafim.surface.view.*

/** Established benchmark grid scaled to millimetres, with synthetic folding.
  * This is an ellipsoid workload, not anatomical data or a closed cortical mesh.
  * Its duplicated seam, polar samples and partial final row are preserved.
  */
final case class SurfaceMapBenchmarkFixture(surfaces: SurfaceSet, folding: SurfaceField[Double]):
  def example(mode: CorticalMapMode): SurfaceViewerExample =
    CorticalMapSemantics.build(surfaces, folding, mode)

object SurfaceMapBenchmarkFixture:
  def apply(vertices: Int): SurfaceMapBenchmarkFixture =
    val packet = SurfaceBenchmarkFixture.plan(vertices, 1).meshes.head
    val coordinates = packet.positions.unsafeArray.map(_.toDouble * 100)
    val indices = packet.indices.unsafeArray
    def geometry(kind: SurfaceKind, scale: Double, translate: Boolean): SurfaceGeometry =
      val positions = Array.tabulate(coordinates.length): i =>
        coordinates(i) * scale + (if translate then (if i % 3 == 0 then 5 else if i % 3 == 2 then 8 else 0) else 0)
      SurfaceGeometry(TriangleMesh.fromArrays(positions, indices), Hemisphere.Left, kind)
    val pial = geometry(SurfaceKind.Pial, 1, false)
    val surfaces = SurfaceSet.of(SurfaceKind.Pial, pial,
      SurfaceKind.White -> geometry(SurfaceKind.White, 0.9, false),
      SurfaceKind.Inflated -> geometry(SurfaceKind.Inflated, 1.12, true))
    val values = Array.tabulate(vertices): i =>
      1.5 * math.sin(coordinates(i * 3 + 1) / 30) * math.cos(coordinates(i * 3 + 2) / 20)
    SurfaceMapBenchmarkFixture(surfaces, SurfaceField(pial, Array.tabulate(vertices)(identity), values))
