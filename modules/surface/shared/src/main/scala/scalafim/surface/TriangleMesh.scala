package scalafim.surface

import narr.NArray
import scalafim.image.NArrayUtil

final case class TriangleMesh private (
  coordinates: NArray[Double],
  faceIndices: NArray[Int],
  vertexCount: Int,
  faceCount: Int
):
  lazy val topologyIdentity: MeshTopologyIdentity =
    MeshTopologyIdentity.from(vertexCount, faceIndices)

  /** Exact ordered-topology compatibility for fields and alternate surface
    * coordinates. The fingerprint is a cheap rejection path; the full index
    * comparison is the proof used at construction boundaries.
    */
  def hasSameTopology(other: TriangleMesh): Boolean =
    if vertexCount != other.vertexCount || faceCount != other.faceCount then false
    else if topologyIdentity != other.topologyIdentity then false
    else
      var index = 0
      var same = true
      while index < faceIndices.length && same do
        same = faceIndices(index) == other.faceIndices(index)
        index += 1
      same

  inline def vertex(id: VertexId): Point3D =
    val i = id.index
    require(i < vertexCount, "vertex id out of range")
    val off = i * 3
    Point3D(coordinates(off), coordinates(off + 1), coordinates(off + 2))

  inline def face(id: FaceId): Triangle =
    val i = id.index
    require(i < faceCount, "face id out of range")
    val off = i * 3
    Triangle(
      VertexId.unsafe(faceIndices(off)),
      VertexId.unsafe(faceIndices(off + 1)),
      VertexId.unsafe(faceIndices(off + 2))
    )

  def vertices: Vector[Point3D] =
    Vector.tabulate(vertexCount)(i => vertex(VertexId.unsafe(i)))

  def faces: Vector[Triangle] =
    Vector.tabulate(faceCount)(i => face(FaceId.unsafe(i)))

object TriangleMesh:

  def fromRows(vertices: Seq[Seq[Double]], faces: Seq[(Int, Int, Int)]): TriangleMesh =
    require(vertices.nonEmpty, "mesh must contain at least one vertex")
    require(faces.nonEmpty, "mesh must contain at least one face")
    require(vertices.forall(_.length == 3), "vertex rows must have exactly 3 coordinates")

    val coords = Array.ofDim[Double](vertices.length * 3)
    var row = 0
    vertices.foreach { point =>
      var col = 0
      point.foreach { value =>
        coords(row * 3 + col) = value
        col += 1
      }
      row += 1
    }

    val idx = Array.ofDim[Int](faces.length * 3)
    var i = 0
    faces.foreach { case (a, b, c) =>
      idx(i) = a
      idx(i + 1) = b
      idx(i + 2) = c
      i += 3
    }

    fromArrays(coords, idx)

  def fromArrays(coordinates: Array[Double], faceIndices: Array[Int]): TriangleMesh =
    require(coordinates.nonEmpty, "mesh must contain at least one vertex")
    require(faceIndices.nonEmpty, "mesh must contain at least one face")
    require(coordinates.length % 3 == 0, "coordinate array length must be a multiple of 3")
    require(faceIndices.length % 3 == 0, "face index array length must be a multiple of 3")
    require(coordinates.forall(_.isFinite), "vertex coordinates must be finite")
    require(faceIndices.forall(_ >= 0), "face indices must be non-negative")

    val vertexCount = coordinates.length / 3
    val faceCount = faceIndices.length / 3
    require(faceIndices.forall(_ < vertexCount), "face indices out of range")

    var f = 0
    while f < faceCount do
      val off = f * 3
      val a = faceIndices(off)
      val b = faceIndices(off + 1)
      val c = faceIndices(off + 2)
      require(a != b && a != c && b != c, "triangle faces must reference three distinct vertices")
      f += 1

    TriangleMesh(
      coordinates = NArrayUtil.fromArray(coordinates),
      faceIndices = NArrayUtil.fromArray(faceIndices),
      vertexCount = vertexCount,
      faceCount = faceCount
    )
