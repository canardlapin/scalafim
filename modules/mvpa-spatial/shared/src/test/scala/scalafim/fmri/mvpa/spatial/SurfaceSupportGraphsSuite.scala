package scalafim.fmri.mvpa.spatial

import multivar.core.SpaceRole
import munit.FunSuite
import mvpa4s.locus.SupportWeightPolicy
import mvpa4s.AxisRef
import mvpa4s.pattern.SupportTopology
import neurosurf4s.surface.{TriangleMesh, VertexId}

class SurfaceSupportGraphsSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(e => fail(e.toString), identity)
  private def axis(n: Int) = right(AxisRef.fromStableKeys("anatomy", SpaceRole.Observed,
    Vector.tabulate(n)(i => s"v$i"), "vertex", "BOLD", "native"))

  test("surface adjacency retains two nearby disconnected sulcal banks"):
    val mesh = TriangleMesh.fromRows(Vector(Vector(0.0, 0.0, 0.0), Vector(2.0, 0.0, 0.0), Vector(0.0, 2.0, 0.0),
      Vector(0.0, 0.0, 0.01), Vector(2.0, 0.0, 0.01), Vector(0.0, 2.0, 0.01)), Vector((0, 1, 2), (3, 4, 5)))
    val graph = right(SurfaceSupportGraphs.surface(axis(6), mesh, Vector.tabulate(6)(VertexId.apply), "mm", SupportWeightPolicy.InversePhysicalLength))
    assertEquals(graph.edges.size, 6)
    assert(graph.edges.forall(e => (e.left < 3) == (e.right < 3)))
    assertEqualsDouble(graph.edges.find(e => e.left == 0 && e.right == 1).get.weight, 0.5, 1e-12)
    assertEquals(graph.weightUnits, "1/mm")
    // Frozen legacy decimal topology ID, independently calculated by reference.py.
    assertEquals(graph.topology, SupportTopology.SurfaceTriangles("1580453993426280883"))

