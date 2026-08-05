package scalafim.spatial

import gale.linalg.{DMat, LinAlgError}

import scala.collection.mutable

opaque type FieldRevision = Long

object FieldRevision:
  private[spatial] def unsafe(value: Long): FieldRevision =
    value

  extension (revision: FieldRevision)
    def value: Long =
      revision

enum FieldDataRef:
  case DenseMatrix(override val label: String, data: DMat)
  case Source(source: FieldSource)

  def label: String =
    this match
      case DenseMatrix(label, _) => label
      case Source(source) => source.descriptor.label

  def rows: Int =
    this match
      case DenseMatrix(_, data) => data.rows
      case Source(source) => source.descriptor.rows

  def cols: Int =
    this match
      case DenseMatrix(_, data) => data.cols
      case Source(source) => source.descriptor.observations

  def materialized: Option[DMat] =
    this match
      case DenseMatrix(_, data) => Some(data)
      case Source(_) => None

  def fieldSource: Option[FieldSource] =
    this match
      case DenseMatrix(_, _) => None
      case Source(source) => Some(source)

  def isMaterialized: Boolean =
    materialized.isDefined

object FieldDataRef:
  def dense(label: String, data: DMat): FieldDataRef =
    FieldDataRef.DenseMatrix(label.trim, data)

  def source(source: FieldSource): FieldDataRef =
    FieldDataRef.Source(source)

final case class OperatorCacheKey(signature: OperatorSignature):
  def source: DomainId =
    signature.source

  def target: DomainId =
    signature.target

  def path: Vector[MorphismId] =
    signature.recipe.path

  def routing: RoutingPolicy =
    signature.recipe.routing

  def sampling: SamplingPolicy =
    signature.recipe.sampling

  def rowSelection: RowSelection =
    signature.recipe.rowSelection

  def roi: Option[Vector[Int]] =
    signature.recipe.roi

  def allowInverses: Boolean =
    signature.recipe.allowInverses

  def compiler: String =
    signature.recipe.compiler

  def rows: Int =
    signature.shape.rows

  def cols: Int =
    signature.shape.cols

  def label: String =
    signature.label

object OperatorCacheKey:
  def apply(
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
  ): OperatorCacheKey =
    val shape = OperatorShape.unsafe(rows, cols)
    val recipe =
      OperatorRecipe.unsafe(
        path,
        routing,
        sampling,
        RowSelection.unsafeFromRoi(roi),
        allowInverses,
        compiler
      )
    new OperatorCacheKey(OperatorSignature.unsafe(source, target, shape, recipe))

  def from(operator: SpatialOperator): OperatorCacheKey =
    new OperatorCacheKey(operator.signature)

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

final case class FieldProvenance(
  root: DomainId,
  transforms: Vector[FieldTransform],
  materializations: Vector[FieldMaterializationLineage] = Vector.empty
):
  def append(transform: FieldTransform): FieldProvenance =
    copy(transforms = transforms :+ transform)

object FieldProvenance:
  def root(domain: DomainId): FieldProvenance =
    FieldProvenance(domain, Vector.empty, Vector.empty)

  private[spatial] def materializedFrom(
    field: Field,
    execution: Option[ExecutionExplanation]
  ): FieldProvenance =
    FieldProvenance(
      root = field.domain,
      transforms = Vector.empty,
      materializations = field.provenance.materializations :+ FieldMaterializationLineage.from(field, execution)
    )

final case class LegacyFieldExecution private (operators: Vector[OperatorCacheKey]):
  def append(operator: OperatorCacheKey): LegacyFieldExecution =
    copy(operators = operators :+ operator)

  def isEmpty: Boolean =
    operators.isEmpty

object LegacyFieldExecution:
  val empty: LegacyFieldExecution =
    LegacyFieldExecution(Vector.empty)

final case class Field private (
  data: FieldDataRef,
  revision: FieldRevision,
  plan: ViewPlan,
  legacyExecution: LegacyFieldExecution,
  provenance: FieldProvenance
):
  def rootRevision: FieldRevision =
    revision

  def rootId: FieldRootId =
    plan.rootId

  def domain: DomainId =
    plan.currentDomain

  def root: DomainId =
    plan.rootDomain

  def sampleCount: Int =
    plan.sampleCount

  def observations: Int =
    plan.observations

  def pending: Vector[OperatorCacheKey] =
    legacyExecution.operators

  def isView: Boolean =
    plan.isView

  def isMaterializedRoot: Boolean =
    plan.isRoot && legacyExecution.isEmpty && data.isMaterialized

  def describe: FieldDescription =
    FieldDescription(rootId, domain, root, sampleCount, observations, plan, pending, data.label)

object Field:
  private var nextRootSequence = 0L
  private var nextRevisionSequence = 0L

  def fromMatrix(domain: DomainId, data: DMat, label: String = ""): Field =
    fromMatrix(nextRootId("dense", domain, label), domain, data, label)

  def fromMatrix(
    rootId: FieldRootId,
    domain: DomainId,
    data: DMat,
    label: String
  ): Field =
    Field(
      data = FieldDataRef.dense(label, data),
      revision = nextRevision(),
      plan = ViewPlan.unsafeRoot(rootId, domain, data.rows, data.cols),
      legacyExecution = LegacyFieldExecution.empty,
      provenance = FieldProvenance.root(domain)
    )

  def fromSource(domain: Domain, source: FieldSource): Either[SpatialError, Field] =
    fromSource(nextRootId("source", domain.id, source.descriptor.label), domain, source)

  def fromSource(
    rootId: FieldRootId,
    domain: Domain,
    source: FieldSource
  ): Either[SpatialError, Field] =
    val descriptor = source.descriptor
    if descriptor.domain != domain.id then
      Left(SpatialError.FieldSourceDomainMismatch(descriptor.id, domain.id, descriptor.domain))
    else if descriptor.geometry != domain.geometry then
      Left(SpatialError.FieldSourceGeometryMismatch(descriptor.id))
    else if descriptor.rows != domain.nElements then
      Left(SpatialError.FieldSourceShapeMismatch(descriptor.id, domain.nElements, descriptor.rows))
    else
      Right(
        Field(
          data = FieldDataRef.source(source),
          revision = nextRevision(),
          plan = ViewPlan.unsafeRoot(rootId, domain.id, descriptor.rows, descriptor.observations),
          legacyExecution = LegacyFieldExecution.empty,
          provenance = FieldProvenance.root(domain.id)
        )
      )

  private[spatial] def viewOf(
    field: Field,
    operator: SpatialOperator
  ): Either[ViewPlanError, Field] =
    val transform = FieldTransform.from(operator)
    field.plan.reexpressLegacy(operator).map { nextPlan =>
      Field(
        data = field.data,
        revision = field.revision,
        plan = nextPlan,
        legacyExecution = field.legacyExecution.append(transform.key),
        provenance = field.provenance.append(transform)
      )
    }

  private[spatial] def withPlan(field: Field, plan: ViewPlan): Field =
    require(plan.rootId == field.rootId, "planned field must preserve root identity")
    require(plan.rootDomain == field.root, "planned field must preserve root domain")
    require(plan.rootSampleCount == field.plan.rootSampleCount, "planned field must preserve root sample count")
    require(plan.rootObservations == field.plan.rootObservations, "planned field must preserve root observations")
    Field(
      data = field.data,
      revision = field.revision,
      plan = plan,
      legacyExecution = field.legacyExecution,
      provenance = field.provenance
    )

  private[spatial] def materializedFrom(
    view: Field,
    data: DMat,
    execution: Option[ExecutionExplanation] = None
  ): Either[SpatialError, Field] =
    if data.rows != view.sampleCount || data.cols != view.observations then
      Left(
        SpatialError.FieldMaterializedShapeMismatch(
          view.sampleCount,
          data.rows,
          view.observations,
          data.cols
        )
      )
    else
      val materialized =
        fromMatrix(
          domain = view.domain,
          data = data,
          label = s"${view.data.label}:materialized"
        )
      Right(
        materialized.copy(
          provenance = FieldProvenance.materializedFrom(view, execution)
        )
      )

  private def nextRootId(kind: String, domain: DomainId, label: String): FieldRootId =
    val sequence =
      this.synchronized {
        nextRootSequence += 1L
        nextRootSequence
      }
    val normalizedLabel = label.trim.replaceAll("[^A-Za-z0-9._-]", "-")
    FieldRootId.unsafe(s"$kind:${domain.value}:$normalizedLabel:$sequence")

  private def nextRevision(): FieldRevision =
    this.synchronized {
      nextRevisionSequence += 1L
      FieldRevision.unsafe(nextRevisionSequence)
    }

final case class FieldDescription(
  rootId: FieldRootId,
  domain: DomainId,
  root: DomainId,
  sampleCount: Int,
  observations: Int,
  plan: ViewPlan,
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
    else if key.signature != operator.signature then
      Left(SpatialError.OperatorAssemblyFailed("operator cache key signature does not match operator"))
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
  def data(field: Field): Either[SpatialError, DMat]

  def materialize(field: Field): Either[SpatialError, Field] =
    data(field).flatMap(data => Field.materializedFrom(field, data))

final class CachedFieldRuntime private (cache: OperatorCache) extends FieldRuntime:
  override def view(field: Field, operator: SpatialOperator): Either[SpatialError, Field] =
    if field.domain != operator.source then Left(SpatialError.FieldDomainMismatch(field.domain, operator.source))
    else if field.sampleCount != operator.cols then Left(SpatialError.FieldShapeMismatch(operator.cols, field.sampleCount))
    else
      val key = OperatorCacheKey.from(operator)
      cache
        .put(key, operator)
        .flatMap(_ => Field.viewOf(field, operator).left.map(viewPlanError))

  override def data(field: Field): Either[SpatialError, DMat] =
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

  private def linearError(error: LinAlgError): SpatialError =
    SpatialError.OperatorAssemblyFailed(error.getMessage)

  private def viewPlanError(error: ViewPlanError): SpatialError =
    SpatialError.OperatorAssemblyFailed(error.message)

object CachedFieldRuntime:
  def apply(cache: OperatorCache): CachedFieldRuntime =
    new CachedFieldRuntime(cache)
