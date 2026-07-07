package scalafim.surface

import narr.NArray
import narr.nArray2NArr
import scalafim.image.NArrayUtil
import scala.reflect.ClassTag
import scala.util.control.NonFatal

private[surface] object SurfaceData:

  def validateIndices(indices: NArray[Int], vertexCount: Int): Unit =
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

final case class SurfaceField[A](
  geometry: SurfaceGeometry,
  indices: NArray[Int],
  data: NArray[A],
  label: String = ""
):
  require(data.length == indices.length, "field data length must match vertex indices")
  SurfaceData.validateIndices(indices, geometry.vertexCount)

  lazy private val indexLookup: Map[Int, Int] =
    val pairs =
      Vector.tabulate(indices.length)(i => indices(i) -> i)
    pairs.toMap

  def size: Int =
    indices.length

  def vertexIds: Vector[VertexId] =
    Vector.tabulate(indices.length)(i => VertexId.unsafe(indices(i)))

  def valueAt(vertex: VertexId): Option[A] =
    indexLookup.get(vertex.index).map(data(_))

  def domainEither: Either[SurfaceError, SurfaceDomain] =
    geometry.domainEither

object SurfaceField:

  def fromIndexed[A: ClassTag](
    geometry: SurfaceGeometry,
    indices: Seq[VertexId],
    data: Seq[A],
    label: String = ""
  ): SurfaceField[A] =
    SurfaceField(
      geometry = geometry,
      indices = NArrayUtil.fromArray(SurfaceData.vertexArray(indices)),
      data = NArrayUtil.fromArray(data.toArray),
      label = label
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

final case class SurfaceMatrix[A](
  geometry: SurfaceGeometry,
  indices: NArray[Int],
  data: NArray[A],
  columns: Int,
  label: String = ""
):
  require(columns > 0, "surface matrix must have at least one column")
  require(data.length == indices.length * columns, "matrix data length must equal vertices * columns")
  SurfaceData.validateIndices(indices, geometry.vertexCount)

  def rows: Int =
    indices.length

  def apply(row: Int, column: Int): A =
    require(row >= 0 && row < rows, "surface matrix row out of range")
    require(column >= 0 && column < columns, "surface matrix column out of range")
    data(row * columns + column)

  def rowVertex(row: Int): VertexId =
    require(row >= 0 && row < rows, "surface matrix row out of range")
    VertexId.unsafe(indices(row))

object SurfaceMatrix:

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
    SurfaceMatrix(
      geometry = geometry,
      indices = NArrayUtil.fromArray(SurfaceData.vertexArray(indices)),
      data = NArrayUtil.fromArray(rows.iterator.flatten.toArray),
      columns = columns,
      label = label
    )

final case class SurfaceRoi[A](
  geometry: SurfaceGeometry,
  indices: NArray[Int],
  data: NArray[A],
  label: String = ""
):
  require(data.length == indices.length, "ROI data length must match vertex indices")
  SurfaceData.validateIndices(indices, geometry.vertexCount)

  def size: Int =
    indices.length

  def vertexIds: Vector[VertexId] =
    Vector.tabulate(indices.length)(i => VertexId.unsafe(indices(i)))

object SurfaceRoi:

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
    SurfaceRoi(
      geometry = field.geometry,
      indices = NArrayUtil.fromArray(SurfaceData.vertexArray(vertices)),
      data = NArrayUtil.fromArray(values.toArray),
      label = label
    )

final case class LabelInfo(id: Int, name: String, color: Option[String] = None):
  require(name.nonEmpty, "label name must be non-empty")

final case class LabeledSurface(
  geometry: SurfaceGeometry,
  indices: NArray[Int],
  labels: NArray[Int],
  table: Vector[LabelInfo],
  label: String = ""
):
  require(labels.length == indices.length, "label data length must match vertex indices")
  require(table.map(_.id).distinct.length == table.length, "label table ids must be unique")
  SurfaceData.validateIndices(indices, geometry.vertexCount)

  lazy private val labelLookup: Map[Int, LabelInfo] =
    table.map(info => info.id -> info).toMap

  def size: Int =
    indices.length

  def info(id: Int): Option[LabelInfo] =
    labelLookup.get(id)

  def labelAt(vertex: VertexId): Option[Int] =
    var i = 0
    while i < indices.length do
      if indices(i) == vertex.index then return Some(labels(i))
      i += 1
    None

  def parcelLabelAt(vertex: VertexId): Option[ParcelLabel] =
    labelAt(vertex).map(ParcelLabel(_))

  def domainEither: Either[SurfaceError, SurfaceDomain] =
    geometry.domainEither

object LabeledSurface:

  def fromIndexed(
    geometry: SurfaceGeometry,
    indices: Seq[VertexId],
    labels: Seq[Int],
    table: Seq[LabelInfo],
    label: String = ""
  ): LabeledSurface =
    LabeledSurface(
      geometry = geometry,
      indices = NArrayUtil.fromArray(SurfaceData.vertexArray(indices)),
      labels = NArrayUtil.fromArray(labels.toArray),
      table = table.toVector,
      label = label
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

  val hemisphere: Hemisphere =
    surfaces.values.head.hemisphere

  val vertexCount: Int =
    surfaces.values.head.vertexCount

  require(surfaces.values.forall(_.hemisphere == hemisphere), "all surface geometries must share a hemisphere")
  require(surfaces.values.forall(_.vertexCount == vertexCount), "all surface geometries must share a vertex count")

  def default: SurfaceGeometry =
    surfaces(defaultKind)

  def get(kind: SurfaceKind): Option[SurfaceGeometry] =
    surfaces.get(kind)

  def labels: Vector[String] =
    surfaces.keys.toVector.map(_.label)

object SurfaceSet:

  def apply(
    surfaces: Map[SurfaceKind, SurfaceGeometry],
    defaultKind: SurfaceKind
  ): SurfaceSet =
    new SurfaceSet(surfaces, defaultKind)

  def of(defaultKind: SurfaceKind, default: SurfaceGeometry, rest: (SurfaceKind, SurfaceGeometry)*): SurfaceSet =
    SurfaceSet((Map(defaultKind -> default) ++ rest.toMap), defaultKind)

final case class HemispherePair[A](left: A, right: A)
