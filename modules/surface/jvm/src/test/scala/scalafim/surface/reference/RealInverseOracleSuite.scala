package scalafim.surface.reference

import scalafim.image.WorldPoint
import scalafim.surface.*

/** The all-vertex oracle is complete, converged, inside the displacement support, and consistent with the
  * exact declared map ScalaFIM applies: ITK's solution, carried forward by ScalaFIM's reading of the same
  * composite, returns to the vertex.
  */
class RealInverseOracleSuite extends munit.FunSuite, RealAssetGate:
  private def distance(p: WorldPoint, q: WorldPoint): Double =
    math.sqrt(math.pow(p.x - q.x, 2) + math.pow(p.y - q.y, 2) + math.pow(p.z - q.z, 2))

  test("every vertex has a converged in-support solution"):
    for (h, solutions) <- RealInverseOracle.solutions do
      assertEquals(solutions.length, 32492, h)
      assert(solutions.forall(s => s.residualMm <= 1e-10 && s.inSupport), h)

  test("the committed subsample is reproduced within each vertex's recorded residual bound"):
    assert(RealInverseOracle.manifest("reproducesCommittedSubsampleMaxBoundRatio").num <= 1.0)

  test("ITK solutions round-trip through ScalaFIM's exact declared map at every vertex"):
    requireReal(RealAssets.evidencePresent, s"locked assets under ${RealAssets.root}")
    for h <- RealAssets.hemispheres do
      val geometry = h.surface.geometry
      val errors = RealInverseOracle.solutions(h.label).zipWithIndex.map: (s, i) =>
        val p = geometry.mesh.vertex(VertexId(i))
        val w = geometry.surfaceToWorld(Vector(p.x, p.y, p.z)).fold(e => fail(e.message), identity)
        val back = RealAssets.pointMap.map.forward(s.world).placed.getOrElse(fail(s"$h vertex $i not placed forward"))
        distance(back, WorldPoint(w(0), w(1), w(2)))
      // Both read the same float32 coordinates exactly; agreement is limited by the 1e-10 mm solve tolerance
      // and evaluation order. The real forward fixture already agrees with ITK to ~1e-13 mm.
      assert(errors.max <= 1e-6, s"${h.label}: max cross-implementation round trip ${errors.max} mm")
