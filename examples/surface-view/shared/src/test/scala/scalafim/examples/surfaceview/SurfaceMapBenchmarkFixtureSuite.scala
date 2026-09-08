package scalafim.examples.surfaceview

import munit.FunSuite
import scalafim.surface.*

class SurfaceMapBenchmarkFixtureSuite extends FunSuite:
  for (vertices, faces) <- Vector(32768 -> 64770, 163842 -> 326020) do
    test(s"$vertices benchmark preserves topology and exercises both tails, hiding and all categories"):
      val fixture = SurfaceMapBenchmarkFixture(vertices)
      val pial = fixture.surfaces.default
      assertEquals(pial.vertexCount, vertices)
      assertEquals(pial.faceCount, faces)
      assert(fixture.folding.geometry.hasSameMeshDomain(pial))
      assert(fixture.folding.indices.sameElements(Array.tabulate(vertices)(identity)))
      assert(fixture.folding.data.forall(x => x.isFinite && math.abs(x) <= 1.5))
      val coordinates = pial.mesh.coordinates
      assert(coordinates.forall(_.isFinite))
      for kind <- Vector(SurfaceKind.White, SurfaceKind.Inflated) do
        val geometry = fixture.surfaces.get(kind).get
        assert(geometry.mesh.faceIndices.sameElements(pial.mesh.faceIndices))
        assert(geometry.mesh.coordinates.forall(_.isFinite))
        assert(!geometry.mesh.coordinates.sameElements(coordinates))
      val values = Array.tabulate(vertices)(i => CorticalMapSemantics.scalar(coordinates(i * 3 + 1), coordinates(i * 3 + 2)))
      assert(values.count(_ < -1) > vertices / 10)
      assert(values.count(_ > 1) > vertices / 10)
      assert(values.count(x => math.abs(x) < 1) > vertices / 10)
      assertEquals(Array.tabulate(vertices)(i => CorticalMapSemantics.category(coordinates(i * 3 + 1))).toSet, Set(1, 2, 3))
      assertEqualsDouble(coordinates.map(math.abs).max, 100, 0.01)
