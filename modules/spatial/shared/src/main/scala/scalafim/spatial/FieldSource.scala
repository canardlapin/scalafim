package scalafim.spatial

import scalafim.linalg.DoubleMatrix

opaque type FieldSourceId = String

object FieldSourceId:
  def apply(value: String): Either[SpatialError, FieldSourceId] =
    val normalized = value.trim
    if normalized.isEmpty then Left(SpatialError.EmptyIdentifier("field source"))
    else Right(normalized)

  private[spatial] def unsafe(value: String): FieldSourceId =
    value

  extension (id: FieldSourceId)
    def value: String =
      id

opaque type FieldSourceRevision = String

object FieldSourceRevision:
  def apply(value: String): Either[SpatialError, FieldSourceRevision] =
    val normalized = value.trim
    if normalized.isEmpty then Left(SpatialError.EmptyIdentifier("field source revision"))
    else Right(normalized)

  private[spatial] def unsafe(value: String): FieldSourceRevision =
    value

  extension (revision: FieldSourceRevision)
    def value: String =
      revision

final case class FieldSourceDescriptor private (
  id: FieldSourceId,
  revision: FieldSourceRevision,
  label: String,
  domain: DomainId,
  geometry: SamplingGeometry,
  rows: Int,
  observations: Int
)

object FieldSourceDescriptor:
  def make(
    id: FieldSourceId,
    revision: FieldSourceRevision,
    label: String,
    domain: DomainId,
    geometry: SamplingGeometry,
    rows: Int,
    observations: Int
  ): Either[SpatialError, FieldSourceDescriptor] =
    if rows <= 0 then Left(SpatialError.NonPositiveDimension("field source rows", rows))
    else if observations <= 0 then Left(SpatialError.NonPositiveDimension("field source observations", observations))
    else
      Right(
        FieldSourceDescriptor(
          id,
          revision,
          label.trim,
          domain,
          geometry,
          rows,
          observations
        )
      )

  private[spatial] def unsafe(
    id: FieldSourceId,
    revision: FieldSourceRevision,
    label: String,
    domain: DomainId,
    geometry: SamplingGeometry,
    rows: Int,
    observations: Int
  ): FieldSourceDescriptor =
    FieldSourceDescriptor(id, revision, label.trim, domain, geometry, rows, observations)

final case class FieldSourceRequest private (
  sourceRows: Vector[Int],
  observations: Vector[Int]
):
  def rows: Int =
    sourceRows.length

  def columns: Int =
    observations.length

object FieldSourceRequest:
  def make(
    descriptor: FieldSourceDescriptor,
    sourceRows: Vector[Int],
    observations: Vector[Int]
  ): Either[SpatialError, FieldSourceRequest] =
    validateIndices("source row", sourceRows, descriptor.rows).flatMap { _ =>
      validateIndices("observation", observations, descriptor.observations).map { _ =>
        FieldSourceRequest(sourceRows, observations)
      }
    }

  private def validateIndices(
    axis: String,
    indices: Vector[Int],
    limit: Int
  ): Either[SpatialError, Unit] =
    val seen = scala.collection.mutable.HashSet.empty[Int]
    var i = 0
    while i < indices.length do
      val index = indices(i)
      if index < 0 || index >= limit then
        return Left(SpatialError.FieldSourceIndexOutOfBounds(axis, index, limit))
      if seen.contains(index) then
        return Left(SpatialError.DuplicateFieldSourceIndex(axis, index))
      seen += index
      i += 1
    Right(())

final case class FieldSourceBlock private (
  request: FieldSourceRequest,
  data: DoubleMatrix
)

object FieldSourceBlock:
  def make(
    source: FieldSourceId,
    request: FieldSourceRequest,
    data: DoubleMatrix
  ): Either[SpatialError, FieldSourceBlock] =
    if data.rows != request.rows || data.cols != request.columns then
      Left(
        SpatialError.FieldSourceBlockShapeMismatch(
          source,
          request.rows,
          data.rows,
          request.columns,
          data.cols
        )
      )
    else Right(FieldSourceBlock(request, data))

trait FieldSource:
  def descriptor: FieldSourceDescriptor

  /**
   * Checks availability, freshness, and backing geometry without reading a value block.
   * Implementations must release any resources before returning.
   */
  def validate(): Either[SpatialError, Unit]

  /** Reads exactly one compact block in request order and closes all acquired resources. */
  def read(request: FieldSourceRequest): Either[SpatialError, FieldSourceBlock]

private[spatial] final class UnavailableFieldSource(
  override val descriptor: FieldSourceDescriptor,
  detail: String
) extends FieldSource:
  override def validate(): Either[SpatialError, Unit] =
    Left(SpatialError.FieldSourceUnavailable(descriptor.id, detail))

  override def read(request: FieldSourceRequest): Either[SpatialError, FieldSourceBlock] =
    Left(SpatialError.FieldSourceUnavailable(descriptor.id, detail))
