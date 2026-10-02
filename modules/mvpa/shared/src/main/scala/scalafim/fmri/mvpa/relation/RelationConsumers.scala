package scalafim.fmri.mvpa.relation

import gale.backend.Backend.given
import gale.linalg.{DMat, QROptions, QRPivoting}
import multivar.core.{SemanticSpace, ValueIdentity}
import scalafim.fmri.mvpa.{AxisDescriptor, AxisDigest, AxisRef, Column, EvidenceError, RsaScoreFailure, RsaScoreKernels}

/** Bounds numerical adapter buffers separately from query-reuse metadata.
  * Resident operators, provider-private scratch and object overhead are excluded.
  * These counts are not a certified process peak-memory bound.
  */
final case class RelationConsumerBudget(maximumValues: Long, maximumWorkspaceCells: Long):
  require(maximumValues >= 0L && maximumWorkspaceCells >= 0L)

/** The exact RDM computation, including its typed pairing and metric admission. */
final case class FitRequirements(preparationRevision: Option[String] = None, noiseRevision: Option[String] = None):
  require(preparationRevision.forall(_.trim.nonEmpty) && noiseRevision.forall(_.trim.nonEmpty))
enum RelationCacheRefusal:
  case RefitRequired(requirements: FitRequirements, actualPreparation: Vector[String], actualNoise: Vector[String])
  case Evidence(error: EvidenceError)
final case class RelationRdmRequest[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, K](
    pairing: PairingDesign[P, K], metric: CrossClosure[N, N], metricAdmission: MetricAdmission,
    declaredErrorIndependence: Vector[ConditionalErrorIndependence], policy: IdentityRdmPolicy, fit: FitRequirements = FitRequirements()
)

/** A method-owned retained geometry.  Its constructor is private so callers
  * cannot attach arbitrary dependencies to an RDM and call it reusable.
  */
final class CachedRelationRdm private[relation] (
    private[relation] val retained: RetainedQuery[RelationRdm]
)

final case class RelationRdmReceipt(product: QueryProductId, program: QueryReuseProgram, comparison: QueryReuseProgram, retainedGeometry: Boolean = true)
/** Scientific scoring is reported separately from cache admission.  In
  * particular, a reusable geometry can quite legitimately yield a singular
  * comparison model; that is not evidence that the geometry cache was wrong.
  */
enum RelationRsaOutcome:
  case Defined(value: Double)
  case MissingEffects(ordinals: Vector[Int], reasons: Vector[String])
  case SingularControls(rank: Int, controls: Int, rankTolerance: Double)
  case ZeroResidual(relativeResidualTolerance: Double)
  case ZeroVariance
  case InsufficientComparisons(actual: Int, required: Int)
  case Execution(error: EvidenceError)
final case class RelationRsaScore(model: String, outcome: RelationRsaOutcome, receipt: RelationRdmReceipt)
enum RelationRsaRefusal:
  case RefitRequired(requirements: FitRequirements, actualPreparation: Vector[String], actualNoise: Vector[String])
  case Cache(decision: QueryReuseDecision)
  case Setup(error: EvidenceError)
final case class FirstOrderReceipt(partition: Int, program: QueryReuseProgram)
final case class FirstOrderRelationResult[C <: SemanticSpace, N <: SemanticSpace](pattern: FirstOrderPattern[C, N], receipt: FirstOrderReceipt)

enum RectangularRelationOutcome:
  case Defined(value: Double)
  case MissingEffects(left: Vector[(Int, String)], right: Vector[(Int, String)])
  case ZeroVariance
  case InsufficientComparisons(actual: Int)
final case class RectangularRelationScore(model: String, outcome: RectangularRelationOutcome, identity: String)

enum RelationRsaMethod:
  case Pearson
  case Spearman
  /** An empty control family is the intercept-only Pearson comparison. */
  case PartialPearson(controls: Vector[SquareRelationModel], policy: PartialRsaPolicy = PartialRsaPolicy())

/** Explicit, serializable numerical policy for the Gale QR residualization.
  * Both tolerances are relative and therefore remain meaningful after the
  * positive scale normalization applied by partial RSA.
  */
final case class PartialRsaPolicy(rankTolerance: Double = 1e-10, relativeResidualTolerance: Double = 1e-12):
  require(rankTolerance >= 0.0 && rankTolerance.isFinite, "partial RSA rank tolerance must be finite and non-negative")
  require(relativeResidualTolerance >= 0.0 && relativeResidualTolerance.isFinite, "partial RSA residual tolerance must be finite and non-negative")

enum SamplewiseUndefined:
  case InsufficientComparisons(actual: Int)
  case ZeroVariance
  case Execution(error: EvidenceError)
enum SamplewiseScore:
  case Defined(value: Double)
  case Undefined(reason: SamplewiseUndefined)
final case class SamplewiseRelationScores(rows: Vector[SamplewiseScore])
final case class SamplewiseRowScore(sample: Int, item: String, block: String, score: SamplewiseScore)
final case class TypedSamplewiseRelationScores(rows: Vector[SamplewiseRowScore], program: QueryReuseProgram)

/** An admitted ordinary sample geometry.  It is intentionally distinct from a
  * signed crossvalidated RDM: row correlation consumes this direct geometry. */
final class SamplewiseGeometry[S <: SemanticSpace] private[relation] (
    val samples: multivar.core.SpaceEvidence[S], val sampleAxis: AxisDescriptor, val values: DMat,
    val origins: RelationOrigins, val valueIdentity: ValueIdentity, val admission: SamplewiseGeometryAdmission
)
enum SamplewiseGeometryAdmission:
  case OrdinaryDissimilarity(methodReceipt: String, normalization: String)
  case SignedCrossvalidated
object SamplewiseGeometry:
  def apply[SK](samples: AxisRef[SK], values: DMat, origins: RelationOrigins, identity: ValueIdentity, admission: SamplewiseGeometryAdmission): Either[EvidenceError, SamplewiseGeometry[samples.Id]] =
    val admitted = admission match
      case SamplewiseGeometryAdmission.OrdinaryDissimilarity(receipt, normalization) => receipt.trim.nonEmpty && normalization.trim.nonEmpty
      case SamplewiseGeometryAdmission.SignedCrossvalidated => false
    if !admitted then Left(EvidenceError.InvalidSource("samplewise RSA requires a declared ordinary dissimilarity; signed crossvalidated geometry is not admitted"))
    else if !RelationAccess.admitsRetained(origins.access) then Left(EvidenceError.InvalidSource("samplewise retained geometry requires owned replay"))
    else if values.rows != samples.size || values.cols != samples.size then Left(EvidenceError.ShapeMismatch("samplewise geometry", samples.size * samples.size, values.rows * values.cols))
    else if (0 until values.rows).exists(row => (0 until values.cols).exists(column => !values(row, column).isFinite)) then Left(EvidenceError.InvalidSource("samplewise geometry has non-finite values"))
    else if (0 until values.rows).exists(row => values(row, row) != 0.0 || (0 until values.cols).exists(column => values(row, column) < 0.0 || values(row, column) != values(column, row))) then Left(EvidenceError.InvalidSource("ordinary dissimilarity must be nonnegative, symmetric and have zero diagonal"))
    else Right(new SamplewiseGeometry(samples.evidence, samples.descriptor, values, origins, identity, admission))

final class SquareRelationModel private[relation] (
    val name: String, val effectKeys: Vector[String], private val pairs: Map[(String, String), Double],
    val valueIdentity: ValueIdentity, val normalization: String, val preparation: String, val noise: String
):
  private[relation] def at(a: String, b: String): Option[Double] = pairs.get(if a < b then a -> b else b -> a)
  /** These are model provenance declarations.  They are included in the
    * comparison receipt; Pearson and Spearman declare only positive-scale
    * invariance, so a differently prepared/noise-normalized model is never
    * silently treated as an interchangeable scientific requirement. */


object SquareRelationModel:
  def apply(name: String, keys: Vector[String], pairs: Map[(String, String), Double], identity: ValueIdentity,
      normalization: String, preparation: String, noise: String): Either[EvidenceError, SquareRelationModel] =
    val expected = (for right <- keys.indices; left <- right + 1 until keys.size yield if keys(left) < keys(right) then keys(left) -> keys(right) else keys(right) -> keys(left)).toSet
    if name.trim.isEmpty || normalization.trim.isEmpty || preparation.trim.isEmpty || noise.trim.isEmpty || keys.distinct.size != keys.size then Left(EvidenceError.InvalidSource("model metadata or labels invalid"))
    else if pairs.keySet != expected || pairs.values.exists(value => !value.isFinite) then Left(EvidenceError.InvalidSource("model pairs invalid"))
    else Right(new SquareRelationModel(name, keys, pairs, identity, normalization, preparation, noise))

/** Rectangular models bind their nominal endpoint spaces and their actual axes.
  * Same-sized foreign endpoint spaces cannot pass this check.
  */
final class RectangularRelationModel[L <: SemanticSpace, R <: SemanticSpace] private[relation] (
    val name: String, val leftAxis: AxisDescriptor, val rightAxis: AxisDescriptor,
    val leftKeys: Vector[String], val rightKeys: Vector[String], val values: DMat,
    val valueIdentity: ValueIdentity
)
object RectangularRelationModel:
  def apply[LK, RK](name: String, left: scalafim.fmri.mvpa.AxisRef[LK], right: scalafim.fmri.mvpa.AxisRef[RK], values: DMat,
      identity: ValueIdentity): Either[EvidenceError, RectangularRelationModel[left.Id, right.Id]] =
    if name.trim.isEmpty || values.rows != left.size || values.cols != right.size then Left(EvidenceError.ShapeMismatch("rectangular model", left.size * right.size, values.rows * values.cols))
    else if (0 until values.rows).exists(row => (0 until values.cols).exists(column => !values(row, column).isFinite)) then Left(EvidenceError.InvalidSource("rectangular model has non-finite values"))
    else Right(new RectangularRelationModel(name, left.descriptor, right.descriptor, left.toRecord.stableKeys, right.toRecord.stableKeys, values, identity))

object RelationConsumers:
  private val rdmProduct = QueryProductId("relation-rdm")

  /** Evaluates within the caller's live relation scope and returns only
    * detached scores/metadata. Requires owned or active scoped replay; a
    * one-shot declaration is insufficient. No source is retained or relabeled.
    * Query and comparison admission precede source evaluation.
    */
  def rsaDirect[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, K](
      relations: RelationSet[P, E, N], request: RelationRdmRequest[P, E, N, K],
      model: SquareRelationModel, method: RelationRsaMethod, reuse: QueryReuseBudget, budget: RelationConsumerBudget
  ): Either[RelationRsaRefusal, RelationRsaScore] =
    val output = relationCells(relations)
    val workspace = BigInt(rdmWorkspace(relations, request)) + 16 * BigInt(output)
    val controlWorkspace = method match
      case RelationRsaMethod.PartialPearson(controls, _) => partialWorkspace(output.min(Int.MaxValue).toInt, controls.size)
      case _ => BigInt(0)
    if !fits(relations, request.fit) then Left(RelationRsaRefusal.RefitRequired(request.fit, relations.relations.map(_.origins.source.preparationRevision), relations.relations.map(_.origins.source.noiseRevision)))
    else if output > budget.maximumValues || output > Int.MaxValue || workspace + controlWorkspace > budget.maximumWorkspaceCells || workspace + controlWorkspace > Int.MaxValue then Left(RelationRsaRefusal.Setup(EvidenceError.InvalidSource("direct RSA output/workspace budget or Int capacity refusal")))
    else
      for
        _ <- relations.relations.foldLeft[Either[EvidenceError, Unit]](Right(()))((admitted, relation) => admitted.flatMap(_ => RelationAccess.admitImmediate(relation.origins.access))).left.map(RelationRsaRefusal.Setup.apply)
        program <- rdmProgram(relations, request, reuse).left.map(RelationRsaRefusal.Setup.apply)
        comparison <- comparisonProgram(program, model, method, reuse).left.map(RelationRsaRefusal.Setup.apply)
        rdm <- RelationRdm.compute(relations, request.pairing, request.metric, request.metricAdmission, request.declaredErrorIndependence, request.policy).left.map(RelationRsaRefusal.Setup.apply)
      yield RelationRsaScore(model.name, score(rdm, model, method, budget), RelationRdmReceipt(rdmProduct, program, comparison, false))

  def cache[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, K](
      relations: RelationSet[P, E, N], request: RelationRdmRequest[P, E, N, K], reuse: QueryReuseBudget,
      consumer: RelationConsumerBudget
  ): Either[RelationCacheRefusal, CachedRelationRdm] =
    val outputCells = relationCells(relations)
    // The RDM itself is retained.  Before applying a relation operator charge
    // its output, retained copy and live effect/neural/pair contribution
    // vectors.  This is deliberately an Int-capacity preflight too: Gale
    // matrices cannot represent a larger dimension even when Long arithmetic
    // would otherwise succeed.
    val workspace = rdmWorkspace(relations, request)
    if !fits(relations, request.fit) then Left(refit(relations, request.fit))
    else if relations.relations.exists(relation => !RelationAccess.admitsRetained(relation.origins.access)) then Left(RelationCacheRefusal.Evidence(EvidenceError.InvalidSource("cached RDM requires owned replay relation values")))
    else if outputCells > reuse.maximumRetainedCells then Left(RelationCacheRefusal.Evidence(EvidenceError.InvalidSource("RDM retained cells exceed query-reuse budget")))
    else if outputCells > consumer.maximumValues || outputCells > Int.MaxValue then Left(RelationCacheRefusal.Evidence(EvidenceError.InvalidSource("RDM output exceeds consumer budget or Int capacity")))
    else if workspace > consumer.maximumWorkspaceCells || workspace > Int.MaxValue then Left(RelationCacheRefusal.Evidence(EvidenceError.InvalidSource("RDM workspace exceeds consumer budget or Int capacity")))
    else
      for
        program <- rdmProgram(relations, request, reuse).left.map(RelationCacheRefusal.Evidence.apply)
        rdm <- RelationRdm.compute(relations, request.pairing, request.metric, request.metricAdmission, request.declaredErrorIndependence, request.policy).left.map(RelationCacheRefusal.Evidence.apply)
        retained <- RetainedQuery(rdmProduct, program, rdm, rdm.cells.size.toLong, reuse).left.map(RelationCacheRefusal.Evidence.apply)
      yield new CachedRelationRdm(retained)

  /** Reconstructs dependencies from the current relation set before touching
    * retained geometry.  Preparation/noise changes therefore refuse before an
    * old RDM is read; they cannot be authorized by replacing a display string.
    */
  def rsa[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, K](
      current: RelationSet[P, E, N], request: RelationRdmRequest[P, E, N, K], cached: CachedRelationRdm,
      model: SquareRelationModel, method: RelationRsaMethod, reuse: QueryReuseBudget, budget: RelationConsumerBudget
  ): Either[RelationRsaRefusal, RelationRsaScore] =
    if !fits(current, request.fit) then Left(RelationRsaRefusal.RefitRequired(request.fit, current.relations.map(_.origins.source.preparationRevision), current.relations.map(_.origins.source.noiseRevision)))
    else rdmProgram(current, request, reuse).left.map(RelationRsaRefusal.Setup.apply).flatMap: program =>
      // `use` is deliberately before any RDM/model value traversal.  A changed
      // pairing, metric, policy, metric admission or fitted relation rejects
      // geometry.  A changed model/control only changes the child program.
      cached.retained.use(program, reuse)(scala.Predef.identity).left.map(RelationRsaRefusal.Cache.apply).flatMap: rdm =>
        comparisonProgram(program, model, method, reuse).left.map(RelationRsaRefusal.Setup.apply).map: comparison =>
          RelationRsaScore(model.name, score(rdm, model, method, budget), RelationRdmReceipt(rdmProduct, program, comparison))

  /** A first-order consumer selects one explicit partition and checks support
    * of every non-zero coefficient before applying the query. */
  def firstOrder[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, K, C <: SemanticSpace](
      relations: RelationSet[P, E, N], partition: PartitionCoordinate[P, K], query: FirstOrderQuery[E, C], budget: RelationConsumerBudget
  ): Either[EvidenceError, FirstOrderPattern[C, N]] =
    firstOrderWithReceipt(relations, partition, query, QueryReuseBudget(8, 64, budget.maximumWorkspaceCells), budget).map(_.pattern)

  def firstOrderWithReceipt[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, K, C <: SemanticSpace](
      relations: RelationSet[P, E, N], partition: PartitionCoordinate[P, K], query: FirstOrderQuery[E, C], reuse: QueryReuseBudget, budget: RelationConsumerBudget
  ): Either[EvidenceError, FirstOrderRelationResult[C, N]] =
    val e = BigInt(relations.effectAxis.size)
    val c = BigInt(query.contrastAxis.size)
    val n = BigInt(relations.neuralAxis.size)
    val workspace = 4 * e * e + 4 * e * c + 16 * (e + c + n)
    if partition.selection.parentAxis != relations.partitionAxis then Left(EvidenceError.AxisMismatch("first-order partition", relations.partitionAxis.stableKey, partition.selection.parentAxis.stableKey))
    else if query.effectAxis != relations.effectAxis then Left(EvidenceError.AxisMismatch("first-order effects", relations.effectAxis.stableKey, query.effectAxis.stableKey))
    else if !RelationAccess.admitsRetained(relations.relations(partition.ordinal).origins.access) then Left(EvidenceError.InvalidSource("first-order retained consumer requires owned replay"))
    else if c * n > budget.maximumValues || workspace > budget.maximumWorkspaceCells || Vector(e * e, e * c, c * n, workspace).exists(_ > Int.MaxValue) then Left(EvidenceError.InvalidSource("first-order coefficient/output budget or Int capacity refusal"))
    else
      val product = QueryProductId("first-order-pattern")
      val dependencies = Vector(QueryDependency.relation(relations.relations(partition.ordinal)), QueryDependency.Scope("selected-partition", framed("partition", Vector(relations.partitionAxis.stableKey, java.lang.Integer.toString(partition.ordinal)))), QueryDependency.Scope("contrast-axis", query.contrastAxis.stableKey), QueryDependency.Parameter("contrast-coefficients", identityDigest(query.weights.valueIdentity)))
      QueryReuseProgram(Vector(QueryNode(product, Vector.empty, dependencies, "exact", "first-order-query")), reuse).flatMap: program =>
        query.weights(DMat.eye(relations.effectAxis.size)).left.map(EvidenceError.SemanticFailure.apply).flatMap: coefficients =>
          var contrast = 0
          var refusal: Option[EvidenceError] = None
          while contrast < coefficients.rows && refusal.isEmpty do
            var effect = 0
            while effect < coefficients.cols && refusal.isEmpty do
              if !coefficients(contrast, effect).isFinite then refusal = Some(EvidenceError.InvalidSource(s"contrast $contrast has unknown non-finite support"))
              else if coefficients(contrast, effect) != 0.0 then relations.relations(partition.ordinal).estimability(effect) match
                case EffectEstimability.Estimable => ()
                case EffectEstimability.NotEstimable(reason) => refusal = Some(EvidenceError.InvalidSource(s"contrast $contrast uses non-estimable effect $effect: $reason"))
              effect += 1
            contrast += 1
          refusal.toLeft(()).flatMap(_ => query(relations.relations(partition.ordinal)).map(pattern => FirstOrderRelationResult(pattern, FirstOrderReceipt(partition.ordinal, program))))

  /** Preferred rectangular route: endpoint descriptors are supplied with the
    * nominal form, and model labels are aligned explicitly in its orientation.
    * Each form column is applied separately, avoiding a hidden square identity.
    */
  def rectangular[LK, RK](left: AxisRef[LK], right: AxisRef[RK], form: EffectForm[left.Id, right.Id], model: RectangularRelationModel[left.Id, right.Id], budget: RelationConsumerBudget): Either[EvidenceError, RectangularRelationScore] =
    val cells = BigInt(left.size) * right.size
    val workspace = 2 * cells + form.oneColumnAdapterCells
    if !RelationAccess.admitsRetained(form.leftOrigins.access) || !RelationAccess.admitsRetained(form.rightOrigins.access) then Left(EvidenceError.InvalidSource("rectangular replay requires owned relation endpoints"))
    else if form.leftAxis != left.descriptor || form.rightAxis != right.descriptor then Left(EvidenceError.InvalidSource("rectangular form endpoints do not match the supplied axes"))
    else if model.leftAxis != left.descriptor || model.rightAxis != right.descriptor then Left(EvidenceError.AxisMismatch("rectangular model endpoints", s"${left.descriptor.stableKey}/${right.descriptor.stableKey}", s"${model.leftAxis.stableKey}/${model.rightAxis.stableKey}"))
    else if cells > budget.maximumValues || workspace > budget.maximumWorkspaceCells || cells > Int.MaxValue || workspace > Int.MaxValue then Left(EvidenceError.InvalidSource("rectangular consumer budget refusal"))
    else
      val missingLeft = form.leftEstimability.zipWithIndex.collect:
        case (EffectEstimability.NotEstimable(reason), index) => index -> reason
      val missingRight = form.rightEstimability.zipWithIndex.collect:
        case (EffectEstimability.NotEstimable(reason), index) => index -> reason
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.rectangular-relation-comparison.v1")
        writer.string(left.descriptor.stableKey)
        writer.string(right.descriptor.stableKey)
        writer.string(identityDigest(form.values.valueIdentity))
        def origins(value: RelationOrigins): Unit =
          writer.string(value.source.acquisitionRevision)
          writer.string(value.source.responseRevision)
          writer.string(value.source.readoutRevision)
          writer.string(value.source.preparationRevision)
          writer.string(value.source.noiseRevision)
          writer.string(value.support.identityDigest)
          RelationAccess.writeFramed(writer, value.access)
        def estimability(values: Vector[EffectEstimability]): Unit =
          writer.intLE(values.size)
          values.foreach:
            case EffectEstimability.Estimable => writer.string("estimable")
            case EffectEstimability.NotEstimable(reason) => writer.string("not-estimable"); writer.string(reason)
        origins(form.leftOrigins)
        origins(form.rightOrigins)
        estimability(form.leftEstimability)
        estimability(form.rightEstimability)
        writer.string(model.name)
        writer.string(identityDigest(model.valueIdentity))
        for row <- 0 until model.values.rows; column <- 0 until model.values.cols do writer.string(java.lang.Double.toHexString(model.values(row, column)))
        writer.string("pearson; column-major aligned enumeration")
      if missingLeft.nonEmpty || missingRight.nonEmpty then return Right(RectangularRelationScore(model.name, RectangularRelationOutcome.MissingEffects(missingLeft, missingRight), identity))
      val leftIndices = left.toRecord.stableKeys.map(key => model.leftKeys.indexOf(key))
      val rightIndices = right.toRecord.stableKeys.map(key => model.rightKeys.indexOf(key))
      if leftIndices.contains(-1) || rightIndices.contains(-1) || leftIndices.distinct.size != leftIndices.size || rightIndices.distinct.size != rightIndices.size then Left(EvidenceError.InvalidSource("rectangular model endpoint labels do not align"))
      else
        val observed = Vector.newBuilder[Double]
        val target = Vector.newBuilder[Double]
        var column = 0
        var failure: Option[EvidenceError] = None
        while column < right.size && failure.isEmpty do
          form.values(DMat.tabulate(right.size, 1)((row, _) => if row == column then 1.0 else 0.0)) match
            case Left(error) => failure = Some(EvidenceError.SemanticFailure(error))
            case Right(value) =>
              var row = 0
              while row < left.size do
                observed += value(row, 0)
                target += model.values(leftIndices(row), rightIndices(column))
                row += 1
          column += 1
        failure.toLeft(()).flatMap: _ =>
          RsaScoreKernels.pearson(observed.result(), target.result()) match
            case Right(value) => Right(RectangularRelationScore(model.name, RectangularRelationOutcome.Defined(value), identity))
            case Left(RsaScoreFailure.ZeroVariance) => Right(RectangularRelationScore(model.name, RectangularRelationOutcome.ZeroVariance, identity))
            case Left(RsaScoreFailure.InsufficientData(actual)) => Right(RectangularRelationScore(model.name, RectangularRelationOutcome.InsufficientComparisons(actual), identity))
            case Left(error) => Left(scoreFailure(error))

  /** Block-excluded row correlations keep repeated item identities and every
    * undefined row.  The matrices are ordinary sample geometry, not an RDM. */
  private[relation] def samplewise(observed: Vector[Vector[Double]], model: Vector[Vector[Double]], items: Vector[String], blocks: Vector[String], method: RelationRsaMethod, budget: RelationConsumerBudget): Either[EvidenceError, SamplewiseRelationScores] =
    val n = observed.size
    if n < 2 || model.size != n || items.size != n || blocks.size != n || observed.exists(_.size != n) || model.exists(_.size != n) then Left(EvidenceError.InvalidSource("samplewise shapes invalid"))
    else if BigInt(n) * n > budget.maximumValues || 16L * n > budget.maximumWorkspaceCells then Left(EvidenceError.InvalidSource("samplewise budget refusal"))
    else Right(SamplewiseRelationScores(Vector.tabulate(n): row =>
      val keep = (0 until n).filter(column => column != row && blocks(column) != blocks(row))
      if keep.size < 2 then SamplewiseScore.Undefined(SamplewiseUndefined.InsufficientComparisons(keep.size))
      else rowScore(keep.map(observed(row)).toVector, keep.map(model(row)).toVector, method) match
        case Right(value) => SamplewiseScore.Defined(value)
        case Left(error) => SamplewiseScore.Undefined(samplewiseFailure(error))
    ))

  /** Typed samplewise RSA binds repeated item and block membership columns to
    * the admitted geometry's sample axis. */
  def samplewise[S <: SemanticSpace](geometry: SamplewiseGeometry[S], items: Column[S, String], blocks: Column[S, String], model: SquareRelationModel, method: RelationRsaMethod, reuse: QueryReuseBudget, budget: RelationConsumerBudget): Either[EvidenceError, TypedSamplewiseRelationScores] =
    val n = geometry.values.rows
    if items.rowAxis != geometry.sampleAxis || blocks.rowAxis != geometry.sampleAxis then Left(EvidenceError.AxisMismatch("samplewise columns", geometry.sampleAxis.stableKey, if items.rowAxis != geometry.sampleAxis then items.rowAxis.stableKey else blocks.rowAxis.stableKey))
    else if n < 2 || BigInt(n) * n > budget.maximumValues || 16L * n > budget.maximumWorkspaceCells then Left(EvidenceError.InvalidSource("samplewise budget refusal"))
    else if items.values.exists(key => !model.effectKeys.contains(key)) then Left(EvidenceError.InvalidSource("samplewise item is absent from model membership, including repeated items"))
    else if blocks.values.exists(_.trim.isEmpty) then Left(EvidenceError.InvalidSource("samplewise block keys must be nonempty"))
    else if method.isInstanceOf[RelationRsaMethod.PartialPearson] then Left(EvidenceError.InvalidSource("samplewise partial correlation requires an admitted row-local control geometry"))
    else
      val dependencies = Vector(
        QueryDependency.Scope("sample-axis", geometry.sampleAxis.stableKey),
        QueryDependency.Parameter("sample-geometry", geometryDigest(geometry)),
        QueryDependency.Parameter("sample-items", columnDigest(items)),
        QueryDependency.Parameter("sample-blocks", columnDigest(blocks)),
        QueryDependency.Parameter("samplewise-model", modelDigest(model)),
        QueryDependency.Parameter("samplewise-scorer", method match
          case RelationRsaMethod.Pearson => "pearson"
          case RelationRsaMethod.Spearman => "spearman-average-ranks"
          case RelationRsaMethod.PartialPearson(_, _) => "unreachable")
      )
      QueryReuseProgram(Vector(QueryNode(QueryProductId("samplewise-rsa"), Vector.empty, dependencies, "exact", "samplewise-ordinary-geometry")), reuse).flatMap: program =>
        val output = Vector.newBuilder[SamplewiseRowScore]
        var row = 0
        while row < n do
          val keep = (0 until n).filter(column => column != row && blocks.values(column) != blocks.values(row))
          val score =
            if keep.size < 2 then SamplewiseScore.Undefined(SamplewiseUndefined.InsufficientComparisons(keep.size))
            else
              val observed = keep.map(column => geometry.values(row, column)).toVector
              val target = keep.map: column =>
                if items.values(row) == items.values(column) then 0.0
                else model.at(items.values(row), items.values(column)).getOrElse(Double.NaN)
              rowScore(observed, target.toVector, method) match
                case Right(value) => SamplewiseScore.Defined(value)
                case Left(error) => SamplewiseScore.Undefined(samplewiseFailure(error))
          output += SamplewiseRowScore(row, items.values(row), blocks.values(row), score)
          row += 1
        Right(TypedSamplewiseRelationScores(output.result(), program))

  private def samplewiseFailure(error: EvidenceError): SamplewiseUndefined = error match
    case EvidenceError.InvalidSource("zero residual variance") => SamplewiseUndefined.ZeroVariance
    case other => SamplewiseUndefined.Execution(other)

  private def rdmProgram[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, K](relations: RelationSet[P, E, N], request: RelationRdmRequest[P, E, N, K], budget: QueryReuseBudget): Either[EvidenceError, QueryReuseProgram] =
    val dependencies = relations.relations.map(QueryDependency.relation) ++ Vector(
      QueryDependency.metric(request.metric, relations.neuralAxis, relations.neuralAxis),
      QueryDependency.Scope("partitions", relations.partitionAxis.stableKey),
      QueryDependency.Scope("effects", relations.effectAxis.stableKey),
      QueryDependency.Parameter("pairing", pairingDigest(request.pairing)),
      QueryDependency.Parameter("normalization", if request.policy.normalizeByFeatures then "feature-mean" else "raw"),
      QueryDependency.Parameter("metric-admission", admissionDigest(request.metricAdmission)),
      QueryDependency.Parameter("conditional-errors", framed("conditional-errors", request.declaredErrorIndependence.map(conditionalDigest)) )
    )
    QueryReuseProgram(Vector(QueryNode(rdmProduct, Vector.empty, dependencies, "exact", "relation-rdm.compute")), budget)

  private def comparisonProgram(parent: QueryReuseProgram, model: SquareRelationModel, method: RelationRsaMethod, budget: QueryReuseBudget): Either[EvidenceError, QueryReuseProgram] =
    val controls = method match
      case RelationRsaMethod.PartialPearson(values, _) => values
      case _ => Vector.empty
    val scorer = method match
      case RelationRsaMethod.Pearson => "pearson"
      case RelationRsaMethod.Spearman => "spearman-average-ranks"
      case RelationRsaMethod.PartialPearson(_, _) => "partial-pearson-gale-qr"
    val dependencies = Vector(
      QueryDependency.Parameter("aligned-model", modelDigest(model)),
      QueryDependency.Parameter("model-provenance", framed("model-provenance", Vector(model.normalization, model.preparation, model.noise))),
      QueryDependency.Parameter("ordered-controls", framed("controls", controls.map(modelDigest))),
      QueryDependency.Parameter("scorer", scorer),
      QueryDependency.Parameter("scorer-compatibility", method match
        case RelationRsaMethod.Pearson | RelationRsaMethod.Spearman => "positive-scale-invariant-model-comparison"
        case RelationRsaMethod.PartialPearson(_, _) => "positive-scale-invariant-after-admitted-control-residualization"),
      QueryDependency.Parameter("partial-qr-policy", method match
        case RelationRsaMethod.PartialPearson(_, policy) => framed("partial-qr-policy", Vector(java.lang.Double.toHexString(policy.rankTolerance), java.lang.Double.toHexString(policy.relativeResidualTolerance)))
        case _ => "not-applicable"),
      QueryDependency.Parameter("missing-policy", "refuse-not-estimable-and-zero-residual")
    )
    QueryReuseProgram(parent.nodes :+ QueryNode(QueryProductId("relation-rsa-comparison"), Vector(rdmProduct), dependencies, "exact", "relation-rsa"), budget)

  private def score(rdm: RelationRdm, model: SquareRelationModel, method: RelationRsaMethod, budget: RelationConsumerBudget): RelationRsaOutcome =
    val n = rdm.cells.size
    if n.toLong > budget.maximumValues || 16L * n > budget.maximumWorkspaceCells || n > Int.MaxValue then RelationRsaOutcome.Execution(EvidenceError.InvalidSource("RSA output/workspace exceeds consumer budget or Int capacity"))
    else estimated(rdm) match
      case Left(outcome) => outcome
      case Right(observed) => align(rdm.effectKeys, model) match
        case Left(error) => RelationRsaOutcome.Execution(error)
        case Right(target) => method match
          case RelationRsaMethod.Pearson => correlationOutcome(RsaScoreKernels.pearson(observed, target), observed.size, 2)
          case RelationRsaMethod.Spearman => correlationOutcome(RsaScoreKernels.spearman(observed, target), observed.size, 2)
          case RelationRsaMethod.PartialPearson(controls, policy) =>
            // Charge the aligned nuisance vectors before `align` can allocate
            // them. This covers controls, normalized responses, the design,
            // QR factor storage, and both residual columns.
            val workspace = partialWorkspace(observed.size, controls.size)
            if workspace > budget.maximumWorkspaceCells || workspace > Int.MaxValue then RelationRsaOutcome.Execution(EvidenceError.InvalidSource("partial Pearson control alignment/workspace exceeds budget or Int capacity"))
            else partialGaleQR(observed, target, controls.map(align(rdm.effectKeys, _)), controls.size, policy)

  private def rowScore(left: Vector[Double], right: Vector[Double], method: RelationRsaMethod): Either[EvidenceError, Double] = method match
    case RelationRsaMethod.Pearson => pearson(left, right)
    case RelationRsaMethod.Spearman => RsaScoreKernels.spearman(left, right).left.map(scoreFailure)
    case RelationRsaMethod.PartialPearson(_, _) => Left(EvidenceError.InvalidSource("samplewise partial correlation has no row-local admitted controls"))

  private def estimated(rdm: RelationRdm): Either[RelationRsaOutcome, Vector[Double]] = rdm.cells.foldLeft[Either[RelationRsaOutcome, Vector[Double]]](Right(Vector.empty)): (acc, cell) =>
    for values <- acc; value <- cell match
      case RelationRdmCell.Estimated(value) => Right(value)
      case RelationRdmCell.NotEstimable(ordinals, reasons) => Left(RelationRsaOutcome.MissingEffects(ordinals, reasons))
    yield values :+ value

  private def align(keys: Vector[String], model: SquareRelationModel): Either[EvidenceError, Vector[Double]] =
    if keys.toSet != model.effectKeys.toSet then Left(EvidenceError.InvalidSource("foreign model labels"))
    else
      val values = for right <- keys.indices; left <- right + 1 until keys.size yield model.at(keys(left), keys(right))
      values.foldLeft[Either[EvidenceError, Vector[Double]]](Right(Vector.empty)): (acc, value) =>
        for xs <- acc; next <- value.toRight(EvidenceError.InvalidSource("model pair missing")) yield xs :+ next

  /** Domain adapter over Gale's pivoted QR.  The rank and residual tolerance
    * are explicit scientific policy, while factorization remains Gale-owned.
    */
  private[relation] def partialGaleQR(observed: Vector[Double], target: Vector[Double], controls: Vector[Either[EvidenceError, Vector[Double]]], controlCount: Int, policy: PartialRsaPolicy): RelationRsaOutcome =
    val required = controlCount + 2 // intercept plus each nuisance needs one residual degree of freedom
    if controls.size != controlCount then RelationRsaOutcome.Execution(EvidenceError.ShapeMismatch("partial RSA controls", controlCount, controls.size))
    else if target.size != observed.size then RelationRsaOutcome.Execution(EvidenceError.ShapeMismatch("partial RSA target", observed.size, target.size))
    else if observed.size < required then RelationRsaOutcome.InsufficientComparisons(observed.size, required)
    else controls.foldLeft[Either[EvidenceError, Vector[Vector[Double]]]](Right(Vector.empty))((acc, control) => for xs <- acc; next <- control yield xs :+ next) match
      case Left(error) => RelationRsaOutcome.Execution(error)
      case Right(values) =>
        values.find(_.size != observed.size) match
          case Some(value) => RelationRsaOutcome.Execution(EvidenceError.ShapeMismatch("partial RSA control", observed.size, value.size))
          case None => partialCenteredInputs(observed, target, values, policy, required)

  private def partialCenteredInputs(observed: Vector[Double], target: Vector[Double], controls: Vector[Vector[Double]], policy: PartialRsaPolicy, required: Int): RelationRsaOutcome =
        (centerAndScale(observed), centerAndScale(target), controls.foldLeft[Either[RsaScoreFailure, Vector[Vector[Double]]]](Right(Vector.empty))((acc, value) => for xs <- acc; next <- centerAndScale(value) yield xs :+ next)) match
          case (Left(failure), _, _) => outcomeForFailure(failure, observed.size, required, policy.relativeResidualTolerance)
          case (_, Left(failure), _) => outcomeForFailure(failure, observed.size, required, policy.relativeResidualTolerance)
          case (_, _, Left(failure)) => outcomeForFailure(failure, observed.size, required, policy.relativeResidualTolerance)
          case (Right(normalObserved), Right(normalTarget), Right(normalControls)) =>
            partialQrResiduals(normalObserved, normalTarget, normalControls, policy, required)

  private def partialQrResiduals(observed: Vector[Double], target: Vector[Double], controls: Vector[Vector[Double]], policy: PartialRsaPolicy, required: Int): RelationRsaOutcome =
        // Gale's rank tolerance is absolute. Centered control columns are
        // unit-L2 here (as is the intercept), so the declared relative
        // tolerance is exactly the absolute tolerance Gale receives.
        val intercept = 1.0 / math.sqrt(observed.size.toDouble)
        val unitControls = controls.map(unitL2)
        val design = DMat.tabulate(observed.size, controls.size + 1)((row, column) => if column == 0 then intercept else unitControls(column - 1)(row))
        val qr = design.qr(QROptions(pivoting = QRPivoting.Column, rankTolerance = Some(policy.rankTolerance)))
        val rank = qr.diagnostics.rank.getOrElse(0)
        if rank != controls.size + 1 then RelationRsaOutcome.SingularControls(rank, controls.size, policy.rankTolerance)
        // DMat.dense is row-major: interleave response values by row rather
        // than concatenating whole columns, which silently swapped RHS rows.
        else qr.residualize(DMat.dense(observed.size, 2, Vector.tabulate(observed.size * 2)(index => if index % 2 == 0 then observed(index / 2) else target(index / 2)))) match
          case Left(error) => RelationRsaOutcome.Execution(EvidenceError.InvalidSource(s"partial Pearson QR residualization failed: $error"))
          case Right(residuals) =>
            val left = Vector.tabulate(observed.size)(i => residuals(i, 0))
            val right = Vector.tabulate(observed.size)(i => residuals(i, 1))
            if stableL2(left) <= policy.relativeResidualTolerance * stableL2(observed) || stableL2(right) <= policy.relativeResidualTolerance * stableL2(target) then RelationRsaOutcome.ZeroResidual(policy.relativeResidualTolerance)
            else correlationOutcome(RsaScoreKernels.pearson(left, right), observed.size, required, policy.relativeResidualTolerance)

  private def correlationOutcome(value: Either[RsaScoreFailure, Double], actual: Int, required: Int, tolerance: Double = 0.0): RelationRsaOutcome = value match
    case Right(score) => RelationRsaOutcome.Defined(score)
    case Left(failure) => outcomeForFailure(failure, actual, required, tolerance)

  private def outcomeForFailure(failure: RsaScoreFailure, actual: Int, required: Int, tolerance: Double): RelationRsaOutcome = failure match
    case RsaScoreFailure.InsufficientData(_) => RelationRsaOutcome.InsufficientComparisons(actual, required)
    case RsaScoreFailure.ZeroVariance if tolerance > 0.0 => RelationRsaOutcome.ZeroResidual(tolerance)
    case RsaScoreFailure.ZeroVariance => RelationRsaOutcome.ZeroVariance
    case RsaScoreFailure.NonFiniteInput(index) => RelationRsaOutcome.Execution(EvidenceError.InvalidSource(s"non-finite RSA comparison at $index"))
    case RsaScoreFailure.ShapeMismatch(left, right) => RelationRsaOutcome.Execution(EvidenceError.ShapeMismatch("RSA comparison", left, right))

  /** First-anchor centering avoids overflow from a large common offset.  The
    * centered values are max-scaled before their mean is removed, so this
    * preserves the centered span and makes the operation scale invariant. */
  private def centerAndScale(values: Vector[Double]): Either[RsaScoreFailure, Vector[Double]] =
    var index = 0
    var rawScale = 0.0
    while index < values.size do
      if !values(index).isFinite then return Left(RsaScoreFailure.NonFiniteInput(index))
      rawScale = math.max(rawScale, math.abs(values(index)))
      index += 1
    if rawScale == 0.0 then Right(Vector.fill(values.size)(0.0))
    else
      // Preserve representable differences beside a large common offset;
      // scale first only when opposite-sign subtraction would overflow.
      val offset = values.map: value =>
        val difference = value - values.head
        if difference.isFinite then difference / rawScale else value / rawScale - values.head / rawScale
      val scale = offset.foldLeft(0.0)((maximum, value) => math.max(maximum, math.abs(value)))
      if scale == 0.0 then return Right(Vector.fill(values.size)(0.0))
      val anchored = offset.map(_ / scale)
      val mean = anchored.sum / anchored.size
      Right(anchored.map(_ - mean))

  private def unitL2(values: Vector[Double]): Vector[Double] =
    val norm = stableL2(values)
    if norm == 0.0 then values else values.map(_ / norm)

  /** Stable L2 norm, used for the documented relative residual policy. */
  private def stableL2(values: Vector[Double]): Double =
    var scale = 0.0
    var squares = 0.0
    values.foreach: value =>
      val magnitude = math.abs(value)
      if magnitude > scale then
        val ratio = if scale == 0.0 then 0.0 else scale / magnitude
        squares = 1.0 + squares * ratio * ratio
        scale = magnitude
      else if magnitude != 0.0 then
        val ratio = magnitude / scale
        squares += ratio * ratio
    scale * math.sqrt(squares)

  private def partialWorkspace(rows: Int, controls: Int): BigInt =
    // aligned controls, normalized controls/observed/target, design, QR
    // reflectors and two RHS/residual columns; every term is Int-addressable.
    16 * BigInt(rows) * (BigInt(controls) + 8) + 8 * (BigInt(controls) + 1) * (BigInt(controls) + 1)

  private def pearson(left: IndexedSeq[Double], right: IndexedSeq[Double]): Either[EvidenceError, Double] =
    RsaScoreKernels.pearson(left, right).left.map(scoreFailure)

  private def scoreFailure(value: RsaScoreFailure): EvidenceError = value match
    case RsaScoreFailure.InsufficientData(actual) => EvidenceError.InvalidSource(s"insufficient RSA comparisons: $actual")
    case RsaScoreFailure.NonFiniteInput(index) => EvidenceError.InvalidSource(s"non-finite RSA comparison at $index")
    case RsaScoreFailure.ZeroVariance => EvidenceError.InvalidSource("zero residual variance")
    case RsaScoreFailure.ShapeMismatch(left, right) => EvidenceError.ShapeMismatch("RSA comparison", left, right)

  private def relationCells[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace](relations: RelationSet[P, E, N]): Long = relations.effectAxis.size.toLong * (relations.effectAxis.size - 1L) / 2L
  private def rdmWorkspace[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, K](relations: RelationSet[P, E, N], request: RelationRdmRequest[P, E, N, K]): Long =
    val effects = relations.effectAxis.size.toLong
    val neural = relations.neuralAxis.size.toLong
    val edges = request.pairing.edges.size.toLong
    // output + retained output + two effect and neural vectors + one
    // contribution per admitted pairing edge.  Saturate so overflow is refused.
    val total = BigInt(relationCells(relations)) * 2 + effects * 16 + neural * 16 + edges * 8
    if total > Long.MaxValue then Long.MaxValue else total.longValue
  private def fits[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace](relations: RelationSet[P, E, N], requirement: FitRequirements): Boolean =
    relations.relations.forall: relation =>
      requirement.preparationRevision.forall(_ == relation.origins.source.preparationRevision) && requirement.noiseRevision.forall(_ == relation.origins.source.noiseRevision)
  private def refit[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace](relations: RelationSet[P, E, N], requirement: FitRequirements): RelationCacheRefusal =
    RelationCacheRefusal.RefitRequired(requirement, relations.relations.map(_.origins.source.preparationRevision), relations.relations.map(_.origins.source.noiseRevision))
  private def framed(tag: String, values: Vector[String]): String = AxisDigest.sha256Hex: writer =>
    writer.string(tag)
    values.foreach(writer.string)
  private def identityDigest(value: ValueIdentity): String = AxisDigest.sha256Hex: writer =>
    def write(identity: ValueIdentity): Unit = identity match
      case ValueIdentity.Source(id) => writer.string("source"); writer.string(id.value)
      case ValueIdentity.Adjoint(of) => writer.string("adjoint"); write(of)
      case ValueIdentity.Composition(first, second) => writer.string("composition"); write(first); write(second)
      case ValueIdentity.Derived(operation, inputs) => writer.string("derived"); writer.string(operation); writer.intLE(inputs.size); inputs.foreach(write)
    write(value)
  private def modelDigest(model: SquareRelationModel): String = AxisDigest.sha256Hex: writer =>
    writer.string("square-relation-model")
    writer.string(model.name)
    writer.string(identityDigest(model.valueIdentity))
    writer.string(model.normalization)
    writer.string(model.preparation)
    writer.string(model.noise)
    writer.intLE(model.effectKeys.size)
    model.effectKeys.foreach(writer.string)
    var left = 0
    while left < model.effectKeys.size do
      var right = left + 1
      while right < model.effectKeys.size do
        writer.string(model.effectKeys(left))
        writer.string(model.effectKeys(right))
        writer.string(java.lang.Double.toHexString(model.at(model.effectKeys(left), model.effectKeys(right)).getOrElse(Double.NaN)))
        right += 1
      left += 1
  private def pairingDigest[P <: SemanticSpace, K](pairing: PairingDesign[P, K]): String =
    val reducer = pairing.reducer match
      case EdgeReducer.WeightedSum => "weighted-sum"
      case EdgeReducer.WeightedMean => "weighted-mean"
    AxisDigest.sha256Hex: writer =>
      writer.string("pairing")
      writer.string(pairing.partitions.stableKey)
      writer.string(reducer)
      writer.intLE(pairing.edges.size)
      pairing.edges.foreach: edge =>
        writer.intLE(edge.left.ordinal)
        writer.intLE(edge.right.ordinal)
        writer.string(java.lang.Double.toHexString(edge.weight))
        writer.string(edge.generalizesOver.stableKey)
  private def geometryDigest[S <: SemanticSpace](geometry: SamplewiseGeometry[S]): String = AxisDigest.sha256Hex: writer =>
    writer.string("samplewise-geometry")
    geometry.admission match
      case SamplewiseGeometryAdmission.OrdinaryDissimilarity(receipt, normalization) =>
        writer.string("ordinary-dissimilarity")
        writer.string(receipt)
        writer.string(normalization)
      case SamplewiseGeometryAdmission.SignedCrossvalidated => writer.string("unadmitted-signed-crossvalidated")
    writer.string(geometry.sampleAxis.stableKey)
    writer.string(identityDigest(geometry.valueIdentity))
    writer.string(geometry.origins.source.acquisitionRevision)
    writer.string(geometry.origins.source.responseRevision)
    writer.string(geometry.origins.source.readoutRevision)
    writer.string(geometry.origins.source.preparationRevision)
    writer.string(geometry.origins.source.noiseRevision)
    RelationAccess.writeFramed(writer, geometry.origins.access)
    writer.string(geometry.origins.support.identityDigest)
    writer.intLE(geometry.values.rows)
    writer.intLE(geometry.values.cols)
    var row = 0
    while row < geometry.values.rows do
      var column = 0
      while column < geometry.values.cols do
        writer.string(java.lang.Double.toHexString(geometry.values(row, column)))
        column += 1
      row += 1
  private def columnDigest[S <: SemanticSpace](column: Column[S, String]): String = AxisDigest.sha256Hex: writer =>
    writer.string("samplewise-column")
    writer.string(column.rowAxis.stableKey)
    writer.string(identityDigest(column.valueIdentity))
    writer.intLE(column.values.size)
    column.values.foreach(writer.string)
  private def conditionalDigest(value: ConditionalErrorIndependence): String =
    framed("conditional-error-independence", Vector(conditionalOriginDigest(value.left), conditionalOriginDigest(value.right), value.leftEvidence.identityDigest, value.rightEvidence.identityDigest, value.conditionedMetric.map(_.identityDigest).getOrElse("none"), value.reasoning))
  private def conditionalOriginDigest(value: RelationOrigins): String = AxisDigest.sha256Hex: writer =>
    writer.string("conditional-endpoint-origin-v1")
    writer.string(value.source.acquisitionRevision)
    writer.string(value.source.responseRevision)
    writer.string(value.source.readoutRevision)
    writer.string(value.source.preparationRevision)
    writer.string(value.source.noiseRevision)
    RelationAccess.writeFramed(writer, value.access)
    writer.string(value.support.identityDigest)
  private def admissionDigest(admission: MetricAdmission): String = admission match
    case MetricAdmission.Fixed(origins) => framed("fixed", Vector(origins.identityDigest))
    case MetricAdmission.IndependentlySourced(origins, conditional) => framed("independent", Vector(origins.identityDigest, conditionalDigest(conditional)))
    case MetricAdmission.EndpointLearned(origins, conditional) => framed("endpoint", Vector(origins.identityDigest, conditionalDigest(conditional)))
