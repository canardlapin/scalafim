package scalafim.surface

import mesh4s.{Triangle as MeshTriangle, TriangleTopology, TopologyAudit, TopologyIssue}
import mesh4s.geometry.{PointField, PointFieldError}
import spatial4s.{CoordinateConvention, CoordinateQuantity, CoordinateUnit, D3, Frame}

enum MeshAdmissionError:
  case Shape(reason: String)
  case Topology(audit: TopologyAudit)
  case Coordinates(error: PointFieldError)

  def message: String = this match
    case Shape(reason) => reason
    case Topology(audit) => audit.message
    case Coordinates(error) => error.message

/** Read-only primitive access to the point field's own storage. */
final class MeshCoordinates private[surface] (private val field: PointField[?, D3, Frame[D3]]):
  val length: Int = field.space.size * 3
  def apply(offset: Int): Double = field.packedValue(offset)
  def toArray: Array[Double] = field.interleavedDoubles
  override def clone(): Array[Double] = toArray
  def toVector: Vector[Double] = Vector.tabulate(length)(apply)

/** Read-only face-major view of mesh4s's admitted triangle storage. */
final class MeshFaces private[surface] (private val packed: IArray[Int]):
  val length: Int = packed.length
  def apply(offset: Int): Int = packed(offset)
  def toArray: Array[Int] = Array.tabulate(length)(apply)
  override def clone(): Array[Int] = toArray
  def toVector: Vector[Int] = Vector.tabulate(length)(apply)

/** Neuro-surface composition over one strict mesh4s topology and point field.
  * Connectivity and the vertex domain have exactly one owner: `topology`.
  */
final class TriangleMesh private (
  val topology: TriangleTopology
)(private[surface] val points: PointField[topology.Vertex, D3, Frame[D3]]):
  val coordinates: MeshCoordinates = new MeshCoordinates(points)
  val faceIndices: MeshFaces = new MeshFaces(topology.packedFaces)
  def vertexCount: Int = topology.vertices.size
  def faceCount: Int = topology.faces.size

  lazy val topologyIdentity: MeshTopologyIdentity =
    MeshTopologyIdentity.from(vertexCount, faceIndices)

  override def equals(other: Any): Boolean = other match
    case that: TriangleMesh =>
      (this eq that) || (hasSameTopology(that) && sameCoordinates(that))
    case _ => false

  private def sameCoordinates(that: TriangleMesh): Boolean =
    var i = 0
    while i < coordinates.length do
      if java.lang.Double.doubleToLongBits(coordinates(i)) != java.lang.Double.doubleToLongBits(that.coordinates(i)) then
        return false
      i += 1
    true

  override def hashCode(): Int = structuralHash

  private lazy val structuralHash: Int =
    var positions = 1
    var faces = 1
    var i = 0
    while i < coordinates.length do
      val bits = java.lang.Double.doubleToLongBits(coordinates(i))
      positions = 31 * positions + (bits ^ (bits >>> 32)).toInt
      i += 1
    i = 0
    while i < faceIndices.length do
      faces = 31 * faces + faceIndices(i)
      i += 1
    31 * positions + faces

  def hasSameTopology(other: TriangleMesh): Boolean =
    (topology eq other.topology) ||
      (topologyIdentity == other.topologyIdentity && topology.sameConnectivity(other.topology))

  /** Coordinates for an alternate realization retain this exact topology owner. */
  def withCoordinates(values: Array[Double]): TriangleMesh =
    TriangleMesh.on(topology, values).fold(error => throw new IllegalArgumentException(error.message), identity)

  /** Reattach coordinates only after exact ordered-connectivity comparison.
    * This cold attachment boundary makes a defensive point-field copy.
    */
  def sharingTopology(owner: TriangleMesh): Either[MeshAdmissionError, TriangleMesh] =
    if topology eq owner.topology then Right(this)
    else if !hasSameTopology(owner) then Left(MeshAdmissionError.Shape("ordered triangle topology differs"))
    else TriangleMesh.on(owner.topology, coordinates.toArray)

  def vertex(id: VertexId): Point3D =
    val i = id.index
    require(i < vertexCount, "vertex id out of range")
    val off = i * 3
    Point3D(coordinates(off), coordinates(off + 1), coordinates(off + 2))

  def face(id: FaceId): Triangle =
    val i = id.index
    require(i < faceCount, "face id out of range")
    val off = i * 3
    Triangle(VertexId.unsafe(faceIndices(off)), VertexId.unsafe(faceIndices(off + 1)), VertexId.unsafe(faceIndices(off + 2)))

  def vertices: Vector[Point3D] = Vector.tabulate(vertexCount)(i => vertex(VertexId.unsafe(i)))
  def faces: Vector[Triangle] = Vector.tabulate(faceCount)(i => face(FaceId.unsafe(i)))

object TriangleMesh:
  // Storage coordinates have no declared physical quantity or conversion scale.
  // Each field gets its own ephemeral chart; topology sharing is not frame alignment.
  private val storageQuantity = CoordinateQuantity.custom("uninterpreted-surface-coordinates").toOption.get
  private val storageUnit = CoordinateUnit.custom("uninterpreted-surface-unit", "unknown", storageQuantity, None).toOption.get

  def fromRows(vertices: Seq[Seq[Double]], faces: Seq[(Int, Int, Int)]): TriangleMesh =
    require(vertices.forall(_.length == 3), "vertex rows must have exactly 3 coordinates")
    fromArrays(vertices.iterator.flatten.toArray, faces.iterator.flatMap((a, b, c) => Iterator(a, b, c)).toArray)

  def fromArrays(coordinates: Array[Double], faceIndices: Array[Int]): TriangleMesh =
    fromArraysEither(coordinates, faceIndices).fold(error => throw new IllegalArgumentException(constructionMessage(error)), identity)

  // Preserve the throwing constructor's established messages for basic invalid
  // arrays. The checked API retains the complete typed provider audit.
  private def constructionMessage(error: MeshAdmissionError): String = error match
    case MeshAdmissionError.Coordinates(_: PointFieldError.NonFiniteCoordinate) =>
      "requirement failed: vertex coordinates must be finite"
    case MeshAdmissionError.Topology(audit) =>
      audit.issues.head match
        case TopologyIssue.VertexOutOfBounds(_, _, vertex) if vertex < 0 =>
          "requirement failed: face indices must be non-negative"
        case TopologyIssue.VertexOutOfBounds(_, _, _) =>
          "requirement failed: face indices out of range"
        case TopologyIssue.RepeatedVertex(_, _) =>
          "requirement failed: triangle faces must reference three distinct vertices"
        case _ => audit.message
    case _ => error.message

  def fromArraysEither(coordinates: Array[Double], faceIndices: Array[Int]): Either[MeshAdmissionError, TriangleMesh] =
    if coordinates.isEmpty || coordinates.length % 3 != 0 then
      Left(MeshAdmissionError.Shape("coordinates must contain nonempty rows of three values"))
    else if faceIndices.isEmpty || faceIndices.length % 3 != 0 then
      Left(MeshAdmissionError.Shape("faces must contain nonempty rows of three indices"))
    else
      TriangleTopology.fromOrdinalFaces(
        coordinates.length / 3,
        faceIndices.grouped(3).map(row => MeshTriangle(row(0), row(1), row(2)))
      ).left.map(MeshAdmissionError.Topology.apply).flatMap(on(_, coordinates))

  /** Cold semantic adapter: build the topology on the restored vertex owner,
    * retaining the original persistent vertex key and exact face order.
    */
  private[surface] def onDomain[S](mesh: TriangleMesh, vertices: locus4s.FiniteSpace[S]):
      (TriangleMesh, TriangleTopology { type Vertex = S }) =
    require(vertices.size == mesh.vertexCount, "vertex domain size must match mesh")
    val faces = Iterator.tabulate(mesh.faceCount): face =>
      val offset = face * 3
      MeshTriangle(
        vertices.indexAtValidatedOrdinal(mesh.faceIndices(offset)),
        vertices.indexAtValidatedOrdinal(mesh.faceIndices(offset + 1)),
        vertices.indexAtValidatedOrdinal(mesh.faceIndices(offset + 2))
      )
    val owner = TriangleTopology.on(vertices).fromOrientedFaces(faces)
      .fold(audit => throw new IllegalStateException(audit.message), identity)
    val attached = on(owner, mesh.coordinates.toArray)
      .fold(error => throw new IllegalStateException(error.message), identity)
    (attached, owner)

  private def on(owner: TriangleTopology, values: Array[Double]): Either[MeshAdmissionError, TriangleMesh] =
    val localFrame = Frame.named[D3]("unplaced surface coordinates", storageUnit, CoordinateConvention.Unspecified).toOption.get
    PointField.fromInterleavedDoubles(owner.vertices, localFrame, values)
      .left.map(MeshAdmissionError.Coordinates.apply)
      .map(points => new TriangleMesh(owner)(points))
