package scalafim.surface.fixtures

import scalafim.surface.*

object SurfaceTestFixtures:

  /** Closed four-vertex tetrahedron used for topology, geodesic, and core mesh checks. */
  val tetraVertices: Vector[Vector[Double]] =
    Vector(
      Vector(0.0, 0.0, 0.0),
      Vector(1.0, 0.0, 0.0),
      Vector(0.0, 1.0, 0.0),
      Vector(0.0, 0.0, 1.0)
    )

  val tetraFaces: Vector[(Int, Int, Int)] =
    Vector(
      (0, 1, 2),
      (0, 1, 3),
      (0, 2, 3),
      (1, 2, 3)
    )

  val tetraMesh: TriangleMesh =
    TriangleMesh.fromRows(tetraVertices, tetraFaces)

  val tetraGeometry: SurfaceGeometry =
    SurfaceGeometry(tetraMesh, Hemisphere.Left, SurfaceKind.Pial)

  val tetraTopology: MeshTopology =
    MeshTopology.from(tetraMesh)

  /** Two triangles sharing an edge, labeled as two contiguous parcels. */
  val sheetGeometry: SurfaceGeometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Vector(
          Vector(0.0, 0.0, 0.0),
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0),
          Vector(1.0, 1.0, 0.0)
        ),
        Vector(
          (0, 1, 2),
          (1, 3, 2)
        )
      ),
      Hemisphere.Left,
      SurfaceKind.Pial
    )

  val sheetTopology: MeshTopology =
    MeshTopology.from(sheetGeometry.mesh)

  val sheetLabels: LabeledSurface =
    LabeledSurface.fromIndexed(
      sheetGeometry,
      Vector(VertexId(0), VertexId(1), VertexId(2), VertexId(3)),
      Vector(1, 1, 2, 2),
      Vector(LabelInfo(1, "A"), LabelInfo(2, "B"))
    )

  /** Two disconnected triangles used to exercise fragmented parcels and unreachable geodesics. */
  val disconnectedGeometry: SurfaceGeometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Vector(
          Vector(0.0, 0.0, 0.0),
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0),
          Vector(10.0, 0.0, 0.0),
          Vector(11.0, 0.0, 0.0),
          Vector(10.0, 1.0, 0.0)
        ),
        Vector(
          (0, 1, 2),
          (3, 4, 5)
        )
      )
    )

  val disconnectedTopology: MeshTopology =
    MeshTopology.from(disconnectedGeometry.mesh)

  val fragmentedLabels: LabeledSurface =
    LabeledSurface.fromIndexed(
      disconnectedGeometry,
      Vector(VertexId(0), VertexId(1), VertexId(3), VertexId(4)),
      Vector(9, 9, 9, 9),
      Vector(LabelInfo(9, "fragmented"))
    )

  /** Unit-sphere points with a sparse triangle fan for spherical-distance checks. */
  val sphericalTopology: MeshTopology =
    MeshTopology.from(
      TriangleMesh.fromRows(
        Vector(
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 1.0),
          Vector(-1.0, 0.0, 0.0)
        ),
        Vector(
          (0, 1, 2),
          (1, 2, 3)
        )
      )
    )

  /** Folded star fixture where Euclidean centroid and graph geodesic medoid diverge. */
  val skewedCentroidGeometry: SurfaceGeometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Vector(
          Vector(0.0, 0.0, 0.0),
          Vector(100.0, 0.0, 0.0),
          Vector(100.0, 10.0, 0.0),
          Vector(100.0, -10.0, 0.0),
          Vector(100.0, 20.0, 0.0),
          Vector(0.0, 100.0, 0.0),
          Vector(0.0, 110.0, 0.0),
          Vector(0.0, 120.0, 0.0),
          Vector(0.0, 130.0, 0.0)
        ),
        Vector(
          (0, 1, 5),
          (0, 2, 6),
          (0, 3, 7),
          (0, 4, 8)
        )
      )
    )

  val skewedCentroidTopology: MeshTopology =
    MeshTopology.from(skewedCentroidGeometry.mesh)

  val skewedCentroidParcel: ParcelUnit =
    ParcelUnit(
      ParcelKey(1),
      Vector(VertexId(0), VertexId(1), VertexId(2), VertexId(3), VertexId(4)),
      Some(LabelInfo(1, "star"))
    )
