package scalafim.spatial

import scalafim.linalg.{DoubleMatrix, LinearMapError}

import scala.collection.mutable

enum FieldDataRef:
  case DenseMatrix(override val label: String, data: DoubleMatrix)
  case External(override val label: String, override val rows: Int, override val cols: Int)

  def label: String =
    this match
      case DenseMatrix(label, _) => label
      case External(label, _, _) => label

  def rows: Int =
    this match
      case DenseMatrix(_, data) => data.rows
      case External(_, rows, _) => rows

  def cols: Int =
    this match
      case DenseMatrix(_, data) => data.cols
      case External(_, _, cols) => cols

  def materialized: Option[DoubleMatrix] =
    this match
      case DenseMatrix(_, data) => Some(data)
      case External(_, _, _) => None

  def isMaterialized: Boolean =
    materialized.isDefined

object FieldDataRef:
  def dense(label: String, data: DoubleMatrix): FieldDataRef =
    FieldDataRef.DenseMatrix(label.trim, data)

  def external(label: String, rows: Int, cols: Int): Either[SpatialError, FieldDataRef] =
    if rows < 0 || cols < 0 then Left(SpatialError.FieldShapeMismatch(rows, cols))
    else Right(FieldDataRef.External(label.trim, rows, cols))

final case class OperatorCacheKey(
  source: DomainId,
  target: DomainId,
  path: Vector[MorphismId],
  routing: RoutingPolicy,
  sampling: SamplingPolicy,
  roi: Option[Vector[Int]],
  allowInverses: Boolean,
  compiler: String,
  rows: Int,
  cols: Int
):
  def label: String =
    val pathLabel = if path.isEmpty then "identity" else path.map(_.value).mkString(">")
    val roiLabel = roi.fold("all")(_.mkString("[", ",", "]"))
    s"${source.value}->${target.value}|$pathLabel|$routing|$sampling|roi=$roiLabel|inv=$allowInverses|$compiler|${rows}x${cols}"

object OperatorCacheKey:
  def from(operator: SpatialOperator): OperatorCacheKey =
    OperatorCacheKey(
      source = operator.source,
      target = operator.target,
      path = operator.provenance.path,
      routing = operator.provenance.routing,
      sampling = operator.provenance.sampling,
      roi = operator.provenance.roi,
      allowInverses = operator.provenance.allowInverses,
      compiler = operator.provenance.compiler,
      rows = operator.rows,
      cols = operator.cols
    )

final case class FieldTransform(
  key: OperatorCacheKey,
  source: DomainId,
  target: DomainId,
  path: Vector[MorphismId],
  compiler: String
)

object FieldTransform:
  def from(operator: SpatialOperator): FieldTransform =
    val key = OperatorCacheKey.from(operator)
    FieldTransform(
      key = key,
      source = operator.source,
      target = operator.target,
      path = operator.provenance.path,
      compiler = operator.provenance.compiler
    )

final case class FieldProvenance(root: DomainId, transforms: Vector[FieldTransform]):
  def append(transform: FieldTransform): FieldProvenance =
    copy(transforms = transforms :+ transform)

object FieldProvenance:
  def root(domain: DomainId): FieldProvenance =
    FieldProvenance(domain, Vector.empty)

final case class Field private (
  data: FieldDataRef,
  domain: DomainId,
  root: DomainId,
  sampleCount: Int,
  observations: Int,
  pending: Vector[OperatorCacheKey],
  provenance: FieldProvenance
):
  require(sampleCount >= 0, "field sample count must be non-negative")
  require(observations >= 0, "field observation count must be non-negative")

  def isView: Boolean =
    pending.nonEmpty

  def isMaterializedRoot: Boolean =
    pending.isEmpty && data.isMaterialized

  def describe: FieldDescription =
    FieldDescription(domain, root, sampleCount, observations, pending, data.label)

object Field:
  def fromMatrix(domain: DomainId, data: DoubleMatrix, label: String = ""): Field =
    Field(
      data = FieldDataRef.dense(label, data),
      domain = domain,
      root = domain,
      sampleCount = data.rows,
      observations = data.cols,
      pending = Vector.empty,
      provenance = FieldProvenance.root(domain)
    )

  def external(
    domain: DomainId,
    rows: Int,
    cols: Int,
    label: String
  ): Either[SpatialError, Field] =
    FieldDataRef.external(label, rows, cols).map { ref =>
      Field(
        data = ref,
        domain = domain,
        root = domain,
        sampleCount = rows,
        observations = cols,
        pending = Vector.empty,
        provenance = FieldProvenance.root(domain)
      )
    }

  private[spatial] def viewOf(field: Field, operator: SpatialOperator): Field =
    val transform = FieldTransform.from(operator)
    Field(
      data = field.data,
      domain = operator.target,
      root = field.root,
      sampleCount = operator.rows,
      observations = field.observations,
      pending = field.pending :+ transform.key,
      provenance = field.provenance.append(transform)
    )

final case class FieldDescription(
  domain: DomainId,
  root: DomainId,
  sampleCount: Int,
  observations: Int,
  pending: Vector[OperatorCacheKey],
  label: String
)

trait OperatorCache:
  def get(key: OperatorCacheKey): Option[SpatialOperator]
  def put(key: OperatorCacheKey, operator: SpatialOperator): Either[SpatialError, Unit]
  def size: Int

final class InMemoryOperatorCache private (
  private val store: mutable.LinkedHashMap[OperatorCacheKey, SpatialOperator]
) extends OperatorCache:
  override def get(key: OperatorCacheKey): Option[SpatialOperator] =
    store.get(key)

  override def put(key: OperatorCacheKey, operator: SpatialOperator): Either[SpatialError, Unit] =
    if key.rows != operator.rows || key.cols != operator.cols then
      Left(SpatialError.OperatorAssemblyFailed("operator cache key shape does not match operator"))
    else if key.source != operator.source || key.target != operator.target then
      Left(SpatialError.OperatorAssemblyFailed("operator cache key domains do not match operator"))
    else
      store.update(key, operator)
      Right(())

  override def size: Int =
    store.size

  def keys: Vector[OperatorCacheKey] =
    store.keys.toVector

object InMemoryOperatorCache:
  def empty: InMemoryOperatorCache =
    new InMemoryOperatorCache(mutable.LinkedHashMap.empty)

trait FieldRuntime:
  def view(field: Field, operator: SpatialOperator): Either[SpatialError, Field]
  def data(field: Field): Either[SpatialError, DoubleMatrix]

final class CachedFieldRuntime private (cache: OperatorCache) extends FieldRuntime:
  override def view(field: Field, operator: SpatialOperator): Either[SpatialError, Field] =
    if field.domain != operator.source then Left(SpatialError.FieldDomainMismatch(field.domain, operator.source))
    else if field.sampleCount != operator.cols then Left(SpatialError.FieldShapeMismatch(operator.cols, field.sampleCount))
    else
      val key = OperatorCacheKey.from(operator)
      cache.put(key, operator).map(_ => Field.viewOf(field, operator))

  override def data(field: Field): Either[SpatialError, DoubleMatrix] =
    field.data.materialized match
      case None =>
        Left(SpatialError.FieldDataUnavailable(field.data.label))
      case Some(rootData) =>
        if rootData.rows != field.data.rows then Left(SpatialError.FieldShapeMismatch(field.data.rows, rootData.rows))
        else
          var current = rootData
          var i = 0
          var error = Option.empty[SpatialError]
          while i < field.pending.length && error.isEmpty do
            val key = field.pending(i)
            cache.get(key) match
              case None =>
                error = Some(SpatialError.OperatorCacheMiss(key.label))
              case Some(operator) =>
                operator.forward(current) match
                  case Left(err) =>
                    error = Some(linearError(err))
                  case Right(next) =>
                    current = next
            i += 1

          error match
            case Some(err) => Left(err)
            case None =>
              if current.rows != field.sampleCount then Left(SpatialError.FieldShapeMismatch(field.sampleCount, current.rows))
              else Right(current)

  private def linearError(error: LinearMapError): SpatialError =
    SpatialError.OperatorAssemblyFailed(error.message)

object CachedFieldRuntime:
  def apply(cache: OperatorCache): CachedFieldRuntime =
    new CachedFieldRuntime(cache)
