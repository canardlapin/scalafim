package scalafim.surface

import scalafim.image.PrimitiveBuffers
import scala.reflect.ClassTag
import scala.util.control.NonFatal

private[surface] object SurfaceData:

  def validateIndices(indices: Array[Int], vertexCount: Int): Unit =
    val seen = scala.collection.mutable.Set.empty[Int]
    var i = 0
    while i < indices.length do
      val idx = indices(i)
      require(idx >= 0 && idx < vertexCount, "surface vertex index out of range")
      require(!seen(idx), "surface vertex indices must be unique")
      seen += idx
      i += 1

  def vertexArray(ids: Seq[VertexId]): Array[Int] =
    val out = Array.ofDim[Int](ids.length)
    var i = 0
    ids.foreach { id =>
      out(i) = id.index
      i += 1
    }
    out

/** Compatibility facade whose mutable ingress arrays are copied into one
  * exact topology-owned field. It is intentionally not a case class: a
  * generated `copy` would reopen the unchecked-array construction path.
  */
final class SurfaceField[A] private (
  val geometry: SurfaceGeometry,
  private val ownedIndices: Array[Int],
  private val ownedData: Array[A],
  val label: String
):
  require(ownedData.length == ownedIndices.length, "field data length must match vertex indices")
  SurfaceData.validateIndices(ownedIndices, geometry.vertexCount)

  /** Exact locus4s field and support over `geometry.mesh.topology.vertices`. */
  val locus: SurfaceVertexField[A] =
    SurfaceVertexData.field(geometry, ownedIndices, ownedData)

  /** Dynamic-boundary compatibility copy in selection order. */
  def indices: Array[Int] =
    ownedIndices.clone()

  /** Dynamic-boundary compatibility copy in selection order. */
  def data: Array[A] =
    ownedData.clone()

  private[surface] def unsafeIndices: Array[Int] =
    ownedIndices

  private[surface] def unsafeData: Array[A] =
    ownedData

  def size: Int =
    ownedIndices.length

  def vertexIds: Vector[VertexId] =
    Vector.tabulate(ownedIndices.length)(i => VertexId.unsafe(ownedIndices(i)))

  def valueAt(vertex: VertexId): Option[A] =
    val row = locus.rowAtOrdinal(vertex.index)
    if row < 0 then None else Some(ownedData(row))

  def domainEither: Either[SurfaceError, SurfaceDomain] =
    geometry.domainEither

object SurfaceField:

  def apply[A](
    geometry: SurfaceGeometry,
    indices: Array[Int],
    data: Array[A],
    label: String = ""
  ): SurfaceField[A] =
    new SurfaceField(geometry, indices.clone(), data.clone(), label)

  def fromIndexed[A: ClassTag](
    geometry: SurfaceGeometry,
    indices: Seq[VertexId],
    data: Seq[A],
    label: String = ""
  ): SurfaceField[A] =
    new SurfaceField(
      geometry,
      PrimitiveBuffers.fromArray(SurfaceData.vertexArray(indices)),
      PrimitiveBuffers.fromArray(data.toArray),
      label
    )

  def fromIndexedEither[A: ClassTag](
    geometry: SurfaceGeometry,
    indices: Seq[VertexId],
    data: Seq[A],
    label: String = ""
  ): Either[SurfaceError, SurfaceField[A]] =
    try scala.util.Right(fromIndexed(geometry, indices, data, label))
    catch case NonFatal(error) => scala.util.Left(SurfaceError.InvalidField(SurfaceError.reason(error)))

  def full[A: ClassTag](
    geometry: SurfaceGeometry,
    data: Seq[A],
    label: String = ""
  ): SurfaceField[A] =
    require(data.length == geometry.vertexCount, "full surface field data must match geometry vertex count")
    val indices = Vector.tabulate(geometry.vertexCount)(VertexId.unsafe)
    fromIndexed(geometry, indices, data, label)

  def fullEither[A: ClassTag](
    geometry: SurfaceGeometry,
    data: Seq[A],
    label: String = ""
  ): Either[SurfaceError, SurfaceField[A]] =
    try scala.util.Right(full(geometry, data, label))
    catch case NonFatal(error) => scala.util.Left(SurfaceError.InvalidField(SurfaceError.reason(error)))

/** Compatibility matrix facade with copied storage and typed row ownership. */
final class SurfaceMatrix[A] private (
  val geometry: SurfaceGeometry,
  private val ownedIndices: Array[Int],
  private val ownedData: Array[A],
  val columns: Int,
  val label: String
):
  require(columns > 0, "surface matrix must have at least one column")
  require(ownedData.length == ownedIndices.length * columns, "matrix data length must equal vertices * columns")
  SurfaceData.validateIndices(ownedIndices, geometry.vertexCount)

  /** Typed row selection into the exact mesh vertex owner. */
  val locus: SurfaceVertexMatrix[A] =
    SurfaceVertexData.matrix(geometry, ownedIndices, ownedData, columns)

  def indices: Array[Int] =
    ownedIndices.clone()

  def data: Array[A] =
    ownedData.clone()

  private[surface] def unsafeIndices: Array[Int] =
    ownedIndices

  private[surface] def unsafeData: Array[A] =
    ownedData

  def rows: Int =
    ownedIndices.length

  def apply(row: Int, column: Int): A =
    require(row >= 0 && row < rows, "surface matrix row out of range")
    require(column >= 0 && column < columns, "surface matrix column out of range")
    ownedData(row * columns + column)

  def rowVertex(row: Int): VertexId =
    require(row >= 0 && row < rows, "surface matrix row out of range")
    VertexId.unsafe(ownedIndices(row))

object SurfaceMatrix:

  def apply[A](
    geometry: SurfaceGeometry,
    indices: Array[Int],
    data: Array[A],
    columns: Int,
    label: String = ""
  ): SurfaceMatrix[A] =
    new SurfaceMatrix(geometry, indices.clone(), data.clone(), columns, label)

  def fromRows[A: ClassTag](
    geometry: SurfaceGeometry,
    indices: Seq[VertexId],
    rows: Seq[Seq[A]],
    label: String = ""
  ): SurfaceMatrix[A] =
    require(rows.nonEmpty, "surface matrix must contain at least one row")
    require(rows.length == indices.length, "matrix row count must match vertex indices")
    val columns = rows.head.length
    require(columns > 0, "surface matrix must have at least one column")
    require(rows.forall(_.length == columns), "surface matrix rows must have equal length")
    new SurfaceMatrix(
      geometry,
      PrimitiveBuffers.fromArray(SurfaceData.vertexArray(indices)),
      PrimitiveBuffers.fromArray(rows.iterator.flatten.toArray),
      columns,
      label
    )

/** Compatibility ROI facade over an exact locus4s selection and region. */
final class SurfaceRoi[A] private (
  val geometry: SurfaceGeometry,
  private val ownedIndices: Array[Int],
  private val ownedData: Array[A],
  val label: String
):
  require(ownedData.length == ownedIndices.length, "ROI data length must match vertex indices")
  SurfaceData.validateIndices(ownedIndices, geometry.vertexCount)

  /** Typed region, selection, and section over the exact mesh vertex owner. */
  val locus: SurfaceVertexField[A] =
    SurfaceVertexData.field(geometry, ownedIndices, ownedData)

  def indices: Array[Int] =
    ownedIndices.clone()

  def data: Array[A] =
    ownedData.clone()

  private[surface] def unsafeIndices: Array[Int] =
    ownedIndices

  private[surface] def unsafeData: Array[A] =
    ownedData

  def size: Int =
    ownedIndices.length

  def vertexIds: Vector[VertexId] =
    Vector.tabulate(ownedIndices.length)(i => VertexId.unsafe(ownedIndices(i)))

  def valueAt(vertex: VertexId): Option[A] =
    val row = locus.rowAtOrdinal(vertex.index)
    if row < 0 then None else Some(ownedData(row))

object SurfaceRoi:

  def apply[A](
    geometry: SurfaceGeometry,
    indices: Array[Int],
    data: Array[A],
    label: String = ""
  ): SurfaceRoi[A] =
    new SurfaceRoi(geometry, indices.clone(), data.clone(), label)

  def fromField[A: ClassTag](
    field: SurfaceField[A],
    vertices: Seq[VertexId],
    label: String = ""
  ): SurfaceRoi[A] =
    val values = vertices.map { vertex =>
      field.valueAt(vertex).getOrElse {
        throw new IllegalArgumentException(s"ROI vertex ${vertex.index} is not present in field")
      }
    }
    new SurfaceRoi(
      field.geometry,
      PrimitiveBuffers.fromArray(SurfaceData.vertexArray(vertices)),
      PrimitiveBuffers.fromArray(values.toArray),
      label
    )

final case class LabelInfo(id: Int, name: String, color: Option[String] = None):
  require(name.nonEmpty, "label name must be non-empty")

/** Compatibility label facade whose support belongs to one exact topology. */
final class LabeledSurface private (
  val geometry: SurfaceGeometry,
  private val ownedIndices: Array[Int],
  private val ownedLabels: Array[Int],
  val table: Vector[LabelInfo],
  val label: String
):
  require(ownedLabels.length == ownedIndices.length, "label data length must match vertex indices")
  require(table.map(_.id).distinct.length == table.length, "label table ids must be unique")
  SurfaceData.validateIndices(ownedIndices, geometry.vertexCount)

  /** Typed label field and support over the exact mesh vertex owner. */
  val locus: SurfaceVertexField[Int] =
    SurfaceVertexData.field(geometry, ownedIndices, ownedLabels)

  def indices: Array[Int] =
    ownedIndices.clone()

  def labels: Array[Int] =
    ownedLabels.clone()

  private[surface] def unsafeIndices: Array[Int] =
    ownedIndices

  private[surface] def unsafeLabels: Array[Int] =
    ownedLabels

  lazy private val labelLookup: Map[Int, LabelInfo] =
    table.map(info => info.id -> info).toMap

  def size: Int =
    ownedIndices.length

  def info(id: Int): Option[LabelInfo] =
    labelLookup.get(id)

  def labelAt(vertex: VertexId): Option[Int] =
    val row = locus.rowAtOrdinal(vertex.index)
    if row < 0 then None else Some(ownedLabels(row))

  def parcelLabelAt(vertex: VertexId): Option[ParcelLabel] =
    labelAt(vertex).map(ParcelLabel(_))

  def domainEither: Either[SurfaceError, SurfaceDomain] =
    geometry.domainEither

object LabeledSurface:

  def apply(
    geometry: SurfaceGeometry,
    indices: Array[Int],
    labels: Array[Int],
    table: Vector[LabelInfo],
    label: String = ""
  ): LabeledSurface =
    new LabeledSurface(geometry, indices.clone(), labels.clone(), table, label)

  def fromIndexed(
    geometry: SurfaceGeometry,
    indices: Seq[VertexId],
    labels: Seq[Int],
    table: Seq[LabelInfo],
    label: String = ""
  ): LabeledSurface =
    new LabeledSurface(
      geometry,
      PrimitiveBuffers.fromArray(SurfaceData.vertexArray(indices)),
      PrimitiveBuffers.fromArray(labels.toArray),
      table.toVector,
      label
    )

  def fromIndexedEither(
    geometry: SurfaceGeometry,
    indices: Seq[VertexId],
    labels: Seq[Int],
    table: Seq[LabelInfo],
    label: String = ""
  ): Either[SurfaceError, LabeledSurface] =
    try scala.util.Right(fromIndexed(geometry, indices, labels, table, label))
    catch case NonFatal(error) => scala.util.Left(SurfaceError.InvalidLabels(SurfaceError.reason(error)))

final case class SurfaceSet private (
  surfaces: Map[SurfaceKind, SurfaceGeometry],
  defaultKind: SurfaceKind
):
  require(surfaces.nonEmpty, "surface set must contain at least one geometry")
  require(surfaces.contains(defaultKind), "surface set default must exist in the set")
  require(
    surfaces.forall((kind, geometry) => kind == geometry.kind),
    "surface set keys must match geometry kinds"
  )

  val hemisphere: Hemisphere =
    surfaces(defaultKind).hemisphere

  val vertexCount: Int =
    surfaces(defaultKind).vertexCount

  val topologyIdentity: MeshTopologyIdentity =
    surfaces(defaultKind).mesh.topologyIdentity

  val connectivityFingerprint =
    surfaces(defaultKind).mesh.connectivityFingerprint

  require(surfaces.values.forall(_.hemisphere == hemisphere), "all surface geometries must share a hemisphere")
  require(surfaces.values.forall(_.vertexCount == vertexCount), "all surface geometries must share a vertex count")
  require(
    surfaces.values.forall(geometry => geometry.mesh.topology eq surfaces(defaultKind).mesh.topology),
    "all surface geometries must share one canonical mesh topology owner"
  )
  require(
    surfaces.values.forall(_.surfaceToWorld == surfaces(defaultKind).surfaceToWorld),
    "all surface geometries must share a surface-to-world transform"
  )

  def default: SurfaceGeometry =
    surfaces(defaultKind)

  def get(kind: SurfaceKind): Option[SurfaceGeometry] =
    surfaces.get(kind)

  def meshDomainEither: Either[SurfaceError, SurfaceMeshDomain] =
    default.meshDomainEither

  def labels: Vector[String] =
    surfaces.keys.toVector.map(_.label)

object SurfaceSet:

  def apply(
    surfaces: Map[SurfaceKind, SurfaceGeometry],
    defaultKind: SurfaceKind
  ): SurfaceSet =
    require(surfaces.nonEmpty, "surface set must contain at least one geometry")
    require(surfaces.contains(defaultKind), "surface set default must exist in the set")
    require(
      surfaces.forall((kind, geometry) => kind == geometry.kind),
      "surface set keys must match geometry kinds"
    )
    val canonical = surfaces(defaultKind)
    require(
      surfaces.values.forall(_.hemisphere == canonical.hemisphere),
      "all surface geometries must share a hemisphere"
    )
    require(
      surfaces.values.forall(_.vertexCount == canonical.vertexCount),
      "all surface geometries must share a vertex count"
    )
    require(
      surfaces.values.forall(_.mesh.hasSameTopology(canonical.mesh)),
      "all surface geometries must share ordered triangle topology"
    )
    require(
      surfaces.values.forall(_.surfaceToWorld == canonical.surfaceToWorld),
      "all surface geometries must share a surface-to-world transform"
    )
    val canonicalized =
      surfaces.map: (kind, geometry) =>
        val mesh =
          geometry.mesh
            .shareTopologyFrom(canonical.mesh)
            .fold(
              error => throw new IllegalArgumentException(error.message),
              identity
            )
        kind ->
          (if mesh eq geometry.mesh then geometry
           else SurfaceGeometry(mesh, geometry.hemisphere, geometry.kind, geometry.surfaceToWorld))
    new SurfaceSet(canonicalized, defaultKind)

  def of(defaultKind: SurfaceKind, default: SurfaceGeometry, rest: (SurfaceKind, SurfaceGeometry)*): SurfaceSet =
    val entries = (defaultKind -> default) +: rest
    require(entries.map(_._1).distinct.length == entries.length, "surface set kinds must be unique")
    SurfaceSet(entries.toMap, defaultKind)

final case class HemispherePair[A](left: A, right: A)
