package scalafim.fmri.mvpa.spatial

import multivar.core.SpaceRole
import munit.FunSuite
import scalafim.fmri.mvpa.AxisRef
import scalafim.fmri.mvpa.pattern.SupportTopology
import scalafim.surface.{TriangleMesh, VertexId}

class SupportGraphsSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(e => fail(e.toString), identity)
  private def axis(n: Int) = right(AxisRef.fromStableKeys("anatomy", SpaceRole.Observed,
    Vector.tabulate(n)(i => s"v$i"), "vertex", "BOLD", "native"))

  test("surface adjacency retains two nearby disconnected sulcal banks"):
    val mesh = TriangleMesh.fromRows(Vector(Vector(0.0, 0.0, 0.0), Vector(2.0, 0.0, 0.0), Vector(0.0, 2.0, 0.0),
      Vector(0.0, 0.0, 0.01), Vector(2.0, 0.0, 0.01), Vector(0.0, 2.0, 0.01)), Vector((0, 1, 2), (3, 4, 5)))
    val graph = right(SupportGraphs.surface(axis(6), mesh, Vector.tabulate(6)(VertexId.apply), "mm", SupportWeightPolicy.InversePhysicalLength))
    assertEquals(graph.edges.size, 6)
    assert(graph.edges.forall(e => (e.left < 3) == (e.right < 3)))
    assertEqualsDouble(graph.edges.find(e => e.left == 0 && e.right == 1).get.weight, 0.5, 1e-12)
    assertEquals(graph.weightUnits, "1/mm")
    assert(graph.topology.isInstanceOf[SupportTopology.SurfaceTriangles])

  test("volume uses face adjacency only and preserves anisotropic physical weights"):
    val coordinates = Vector((0, 0, 0), (1, 0, 0), (0, 1, 0), (0, 0, 1), (2, 2, 0))
    val graph = right(SupportGraphs.volume(axis(5), coordinates, Vector(2.0, 3.0, 4.0), "mm", "grid", SupportWeightPolicy.InversePhysicalLength))
    assertEquals(graph.edges.map(e => (e.left, e.right)), Vector((0, 1), (0, 2), (0, 3)))
    graph.edges.zip(Vector(0.5, 1.0 / 3.0, 0.25)).foreach((e, expected) => assertEqualsDouble(e.weight, expected, 1e-12))
    assert(SupportGraphs.volume(axis(5), coordinates, Vector(0.0, 3.0, 4.0), "mm", "grid").isLeft)

  test("anatomical adapters reject duplicate mappings"):
    assert(SupportGraphs.volume(axis(2), Vector((0, 0, 0), (0, 0, 0)), Vector(1.0, 1.0, 1.0), "mm", "grid").isLeft)
