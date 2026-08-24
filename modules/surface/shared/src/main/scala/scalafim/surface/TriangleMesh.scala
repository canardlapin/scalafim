package scalafim.surface

import mesh4s.OrientationError as MeshOrientationError
import mesh4s.TopologyAudit
import mesh4s.Triangle as MeshTriangle
import mesh4s.TriangleTable
import mesh4s.TriangleTopology as MeshTriangleTopology
import mesh4s.geometry.SurfaceRealization
import mesh4s.geometry.SurfaceRealizationError
import scalafim.image.PrimitiveBuffers
import spatial4s.D3
import spatial4s.Dimension.given
import spatial4s.Frame

enum TriangleMeshOrientationPolicy derives CanEqual:
  case Strict
  case Orient

enum TriangleMeshOrientationReceipt derives CanEqual:
  case Strict
  case Oriented(flippedFaces: Vector[FaceId])

enum TriangleMeshError derives CanEqual:
  case InvalidInput(reason: String)
  case TopologyRejected(audit: TopologyAudit)
  case OrientationRejected(error: MeshOrientationError)
  case RealizationRejected(error: SurfaceRealizationError)
  case ConnectivityMismatch

  def message: String =
    this match
      case InvalidInput(reason)       => reason
      case TopologyRejected(audit)    => audit.message
      case OrientationRejected(error) => error.message
      case RealizationRejected(error) => error.message
      case ConnectivityMismatch       => "mesh connectivity does not match the canonical topology"

/** Compatibility facade over one lawful mesh4s topology and one coordinate
  * realization.
  *
  * `topology` is the cell-domain and adjacency authority. The primitive arrays
  * remain available while renderer and codec consumers migrate to cached
  * renditions; they are not used to reconstruct a second topology.
  */
final class TriangleMesh private (
  private val packedCoordinates: Array[Double],
  private val packedFaceIndices: Array[Int],
  val topology: MeshTriangleTopology,
  val orientationReceipt: TriangleMeshOrientationReceipt,
  val nativeFrame: Frame[D3]
)(
  val realization: SurfaceRealization[topology.type, D3, Frame[D3]]
):
  val vertexCount: Int =
    topology.vertices.size

  val faceCount: Int =
    topology.faces.size

  val topologyIdentity: MeshTopologyIdentity =
    MeshTopologyIdentity.from(vertexCount, packedFaceIndices)

  /** Surface-local coordinates before `SurfaceGeometry.surfaceToWorld` maps
    * them into RAS+. A defensive copy preserves the mesh/realization invariant.
    */
  def coordinates: Array[Double] =
    packedCoordinates.clone()

  /** Ordered renderer/codec compatibility indices. mesh4s remains the
    * topology authority.
    */
  def faceIndices: Array[Int] =
    packedFaceIndices.clone()

  /** Exact ordered-connectivity compatibility for fields and alternate
    * coordinate realizations. The legacy fingerprint is a cheap rejection
    * path; mesh4s performs the full ordered triangle comparison.
    */
  def hasSameTopology(other: TriangleMesh): Boolean =
    vertexCount == other.vertexCount &&
      faceCount == other.faceCount &&
      topologyIdentity == other.topologyIdentity &&
      topology.sameConnectivity(other.topology)

  /** Rebind this coordinate realization onto an already-validated canonical
    * topology. Structural comparison occurs once at this ingestion boundary.
    */
  def shareTopologyFrom(canonical: TriangleMesh): Either[TriangleMeshError, TriangleMesh] =
    if topology eq canonical.topology then Right(this)
    else if !hasSameTopology(canonical) then Left(TriangleMeshError.ConnectivityMismatch)
    else
      TriangleMesh.create(
        packedCoordinates,
        canonical.packedFaceIndices,
        canonical.topology,
        orientationReceipt,
        nativeFrame
      )

  inline def vertex(id: VertexId): Point3D =
    val i = id.index
    require(i < vertexCount, "vertex id out of range")
    val off = i * 3
    Point3D(
      packedCoordinates(off),
      packedCoordinates(off + 1),
      packedCoordinates(off + 2)
    )

  inline def face(id: FaceId): Triangle =
    val i = id.index
    require(i < faceCount, "face id out of range")
    val meshFace = topology.faces.indexAtValidatedOrdinal(i)
    val vertices = topology.verticesOf(meshFace)
    Triangle(
      VertexId.unsafe(vertices.first.ordinal),
      VertexId.unsafe(vertices.second.ordinal),
      VertexId.unsafe(vertices.third.ordinal)
    )

  def vertices: Vector[Point3D] =
    Vector.tabulate(vertexCount)(i => vertex(VertexId.unsafe(i)))

  def faces: Vector[Triangle] =
    Vector.tabulate(faceCount)(i => face(FaceId.unsafe(i)))

object TriangleMesh:

  def fromRowsEither(
    vertices: Seq[Seq[Double]],
    faces: Seq[(Int, Int, Int)],
    orientation: TriangleMeshOrientationPolicy = TriangleMeshOrientationPolicy.Strict
  ): Either[TriangleMeshError, TriangleMesh] =
    if vertices.isEmpty then
      Left(TriangleMeshError.InvalidInput("mesh must contain at least one vertex"))
    else if faces.isEmpty then
      Left(TriangleMeshError.InvalidInput("mesh must contain at least one face"))
    else if !vertices.forall(_.length == 3) then
      Left(TriangleMeshError.InvalidInput("vertex rows must have exactly 3 coordinates"))
    else
      val coordinates = Array.ofDim[Double](vertices.length * 3)
      var row = 0
      val vertexIterator = vertices.iterator
      while vertexIterator.hasNext do
        val point = vertexIterator.next().iterator
        var column = 0
        while point.hasNext do
          coordinates(row * 3 + column) = point.next()
          column += 1
        row += 1

      val faceIndices = Array.ofDim[Int](faces.length * 3)
      var offset = 0
      val faceIterator = faces.iterator
      while faceIterator.hasNext do
        val (first, second, third) = faceIterator.next()
        faceIndices(offset) = first
        faceIndices(offset + 1) = second
        faceIndices(offset + 2) = third
        offset += 3

      fromArraysEither(coordinates, faceIndices, orientation)

  def fromRows(vertices: Seq[Seq[Double]], faces: Seq[(Int, Int, Int)]): TriangleMesh =
    fromRows(vertices, faces, TriangleMeshOrientationPolicy.Strict)

  def fromRows(
    vertices: Seq[Seq[Double]],
    faces: Seq[(Int, Int, Int)],
    orientation: TriangleMeshOrientationPolicy
  ): TriangleMesh =
    orThrow(fromRowsEither(vertices, faces, orientation))

  def fromArraysEither(
    coordinates: Array[Double],
    faceIndices: Array[Int],
    orientation: TriangleMeshOrientationPolicy = TriangleMeshOrientationPolicy.Strict
  ): Either[TriangleMeshError, TriangleMesh] =
    validateArrays(coordinates, faceIndices).flatMap: _ =>
      val vertexCount = coordinates.length / 3
      val table = TriangleTable(vertexCount, faceRows(faceIndices))
      orientation match
        case TriangleMeshOrientationPolicy.Strict =>
          MeshTriangleTopology
            .fromTable(table)
            .left
            .map(TriangleMeshError.TopologyRejected.apply)
            .flatMap: topology =>
              create(
                coordinates,
                faceIndices,
                topology,
                TriangleMeshOrientationReceipt.Strict
              )
        case TriangleMeshOrientationPolicy.Orient =>
          MeshTriangleTopology
            .orientAndBuild(table)
            .left
            .map(TriangleMeshError.OrientationRejected.apply)
            .flatMap: oriented =>
              val flippedFaces =
                oriented.flipped.toVector.zipWithIndex.collect:
                  case (true, face) => FaceId.unsafe(face)
              create(
                coordinates,
                faceIndicesFrom(oriented.topology),
                oriented.topology,
                TriangleMeshOrientationReceipt.Oriented(flippedFaces)
              )

  def fromArrays(coordinates: Array[Double], faceIndices: Array[Int]): TriangleMesh =
    fromArrays(coordinates, faceIndices, TriangleMeshOrientationPolicy.Strict)

  def fromArrays(
    coordinates: Array[Double],
    faceIndices: Array[Int],
    orientation: TriangleMeshOrientationPolicy
  ): TriangleMesh =
    orThrow(fromArraysEither(coordinates, faceIndices, orientation))

  private def create(
    coordinates: Array[Double],
    faceIndices: Array[Int],
    topology: MeshTriangleTopology,
    orientationReceipt: TriangleMeshOrientationReceipt,
    nativeFrame: Frame[D3] = freshNativeFrame()
  ): Either[TriangleMeshError, TriangleMesh] =
    val ownedCoordinates = PrimitiveBuffers.fromArray(coordinates)
    val ownedFaceIndices = PrimitiveBuffers.fromArray(faceIndices)
    SurfaceRealization
      .fromInterleavedDoubles(topology, nativeFrame, ownedCoordinates)
      .left
      .map(TriangleMeshError.RealizationRejected.apply)
      .map: realization =>
        new TriangleMesh(
          packedCoordinates = ownedCoordinates,
          packedFaceIndices = ownedFaceIndices,
          topology = topology,
          orientationReceipt = orientationReceipt,
          nativeFrame = nativeFrame
        )(realization)

  private def freshNativeFrame(): Frame[D3] =
    Frame
      .named[D3]("ScalaFIM surface-local coordinates")
      .fold(
        error => throw new IllegalStateException(error.message),
        identity
      )

  private def validateArrays(
    coordinates: Array[Double],
    faceIndices: Array[Int]
  ): Either[TriangleMeshError, Unit] =
    if coordinates.isEmpty then
      Left(TriangleMeshError.InvalidInput("mesh must contain at least one vertex"))
    else if faceIndices.isEmpty then
      Left(TriangleMeshError.InvalidInput("mesh must contain at least one face"))
    else if coordinates.length % 3 != 0 then
      Left(TriangleMeshError.InvalidInput("coordinate array length must be a multiple of 3"))
    else if faceIndices.length % 3 != 0 then
      Left(TriangleMeshError.InvalidInput("face index array length must be a multiple of 3"))
    else if !coordinates.forall(_.isFinite) then
      Left(TriangleMeshError.InvalidInput("vertex coordinates must be finite"))
    else if !faceIndices.forall(_ >= 0) then
      Left(TriangleMeshError.InvalidInput("face indices must be non-negative"))
    else
      val vertexCount = coordinates.length / 3
      if !faceIndices.forall(_ < vertexCount) then
        Left(TriangleMeshError.InvalidInput("face indices out of range"))
      else Right(())

  private def faceRows(faceIndices: Array[Int]): Vector[MeshTriangle[Int]] =
    Vector.tabulate(faceIndices.length / 3): face =>
      val offset = face * 3
      MeshTriangle(
        faceIndices(offset),
        faceIndices(offset + 1),
        faceIndices(offset + 2)
      )

  private def faceIndicesFrom(topology: MeshTriangleTopology): Array[Int] =
    val result = Array.ofDim[Int](topology.faces.size * 3)
    topology.faces.foreachIndex: face =>
      val vertices = topology.verticesOf(face)
      val offset = face.ordinal * 3
      result(offset) = vertices.first.ordinal
      result(offset + 1) = vertices.second.ordinal
      result(offset + 2) = vertices.third.ordinal
    result

  private def orThrow(
    result: Either[TriangleMeshError, TriangleMesh]
  ): TriangleMesh =
    result.fold(
      error => throw new IllegalArgumentException(s"requirement failed: ${error.message}"),
      identity
    )
