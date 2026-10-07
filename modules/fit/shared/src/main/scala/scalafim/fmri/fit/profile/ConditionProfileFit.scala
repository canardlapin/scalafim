package scalafim.fmri.fit.profile

import scalafim.dataset.DatasetSeriesReader
import scalafim.fmri.design.ColumnId
import scalafim.fmri.design.event.ConvolvedTerm
import scalafim.fmri.design.hrf.{HrfKernelBasis, KernelBasisProvenance}
import scalafim.fmri.fit.{BasisExpandedRetention, BasisExpandedRetentionPlan, ChunkSize, EstimateExecutionOutcome, FitError, ResponsePreparationIdentity, ResponsePreparationProvenance, TaskBasisStructure}
import scalafim.fmri.hrf.family.{FamilySummaryError, JetLayout, NormalizationRule, ShapePoint, ShapeSummary}
import scalafim.fmri.model.FitPlan
import scala.util.control.NonFatal

/** The condition-only ProfileHrf policy composed over an existing fixed
  * `FitPlan` whose task term was convolved with `basis.kernel`: the ordinary
  * OLS engine retains the basis-expanded sufficient statistics, and this
  * policy adds the post-solve. No new fit strategy; no fabricated design.
  */
final case class ConditionProfilePolicy(
    basis: HrfKernelBasis,
    structure: TaskBasisStructure,
    nodesPerAxis: Vector[Int],
    budget: DecodeBudget,
    prior: Option[ShapePrior],
    noiseVariance: Double,
    output: OutputRequest,
    admission: ObservedFamilyAdmission,
    blockSize: Int = 256)

final case class ConditionVoxelResult(
    voxel: Int,
    coordinates: Vector[Double],
    summaries: ShapeSummary,
    amplitudes: Vector[QueryValue],
    queries: Vector[QueryValue],
    status: DecodeStatus,
    conditionalSd: Vector[Double],
    residualEnergy: Double,
    noisePlugin: Double,
    newtonSteps: Int)

private[profile] final case class ConditionVoxelReadout(
    voxel: Int,
    coordinates: Vector[Double],
    amplitudes: Vector[QueryValue],
    queries: Vector[QueryValue],
    status: DecodeStatus,
    conditionalSd: Vector[Double],
    residualEnergy: Double,
    noisePlugin: Double,
    newtonSteps: Int):
  def withSummary(summary: ShapeSummary): ConditionVoxelResult =
    ConditionVoxelResult(voxel, coordinates, summary, amplitudes, queries, status, conditionalSd, residualEnergy, noisePlugin, newtonSteps)

/** One block's payload: released to the sink, never retained by the runner. */
final case class ConditionProfileBlock(ordinal: Int, results: Vector[ConditionVoxelResult])

final case class ConditionProfileReceipt(ordinal: Int, voxels: Int, accepted: Int)

/** The exact inputs of one condition-profile post-solve, held as typed values
  * and encoded structurally by [[canonical]]. Every policy field that can change
  * the numerics is recorded; [[ConditionProfileProvenance.unencodedPolicyFields]]
  * lists the ones that cannot, with the reason.
  */
final case class ConditionProfileProvenance(
    basis: String,
    structure: Vector[Vector[String]],
    preparation: ResponsePreparationProvenance,
    nodesPerAxis: Vector[Int],
    budget: DecodeBudget,
    prior: Option[ShapePrior],
    output: OutputRequest,
    noiseVariance: Double):
  /** Destructured positionally: a new field fails compilation until encoded. */
  def canonical: String =
    this match
      case ConditionProfileProvenance(basis, structure, preparation, nodesPerAxis, budget, prior, output, noiseVariance) =>
        val conditions = structure.map(condition => KernelBasisProvenance.record("condition", condition*))
        s"condition-profile/v2|basis=${KernelBasisProvenance.field(basis)}|" +
          s"structure=${KernelBasisProvenance.record("conditions", conditions*)}|" +
          s"preparation=${KernelBasisProvenance.field(ResponsePreparationIdentity.provenance(preparation))}|" +
          s"nodesPerAxis=${KernelBasisProvenance.record("nodes", nodesPerAxis.map(_.toString)*)}|" +
          s"budget=${KernelBasisProvenance.field(ConditionProfileProvenance.budgetCanonical(budget))}|" +
          s"prior=${ConditionProfileProvenance.priorCanonical(prior)}|" +
          s"output=${ConditionProfileProvenance.outputCanonical(output)}|" +
          s"noiseVariance=${KernelBasisProvenance.number(noiseVariance)}"

object ConditionProfileProvenance:

  /** The policy fields recorded by [[ConditionProfileProvenance.canonical]], by name. */
  private[profile] val encodedPolicyFields: Vector[String] =
    Vector("basis", "structure", "nodesPerAxis", "budget", "prior", "noiseVariance", "output")

  /** Policy fields deliberately absent from the identity, with the reason. */
  private[profile] val unencodedPolicyFields: Map[String, String] =
    Map(
      "admission" -> "gates whether preparation is admitted; it cannot change any accepted result",
      "blockSize" -> "batching only; each voxel's post-solve is independent of the block it is read in"
    )

  private[profile] def of(policy: ConditionProfilePolicy, preparation: ResponsePreparationProvenance): ConditionProfileProvenance =
    ConditionProfileProvenance(
      basis = policy.basis.provenance.canonical,
      structure = policy.structure.conditions.map(_.map(_.value)),
      preparation = preparation,
      nodesPerAxis = policy.nodesPerAxis,
      budget = policy.budget,
      prior = policy.prior,
      output = policy.output,
      noiseVariance = policy.noiseVariance
    )

  private def numbers(values: Vector[Double]): String =
    KernelBasisProvenance.record("values", values.map(KernelBasisProvenance.number)*)

  private[profile] def budgetCanonical(budget: DecodeBudget): String =
    budget match
      case DecodeBudget(
            coarseStride,
            maxNewtonSteps,
            maxJets,
            maxExactEvaluations,
            weakSdLimit,
            ambiguityEnergy,
            maxCandidateAttempts,
            stationarityStepTolerance,
            initialization
          ) =>
        val initializationSuffix = initialization match
          case DecodeInitialization.BankNode => "|initialization=bank-node"
          case DecodeInitialization.ChartCenterProbe => "|initialization=chart-center-probe"
          case DecodeInitialization.BoundedMultistart => "|initialization=bounded-multistart/v1"
        s"decode-budget/v2|coarseStride=$coarseStride|maxNewtonSteps=$maxNewtonSteps|" +
          s"maxJets=$maxJets|maxExactEvaluations=$maxExactEvaluations|" +
          s"weakSdLimit=${numbers(weakSdLimit)}|" +
          s"ambiguityEnergy=${KernelBasisProvenance.number(ambiguityEnergy)}|" +
          s"maxCandidateAttempts=$maxCandidateAttempts|" +
          s"stationarityStepTolerance=${KernelBasisProvenance.number(stationarityStepTolerance)}" + initializationSuffix

  private[profile] def priorCanonical(prior: Option[ShapePrior]): String =
    KernelBasisProvenance.option(prior.map { case ShapePrior(mean, precision) =>
      KernelBasisProvenance.record("shape_prior", s"mean=${numbers(mean)}", s"precision=${numbers(precision)}")
    })

  private def normalization(rule: NormalizationRule): String =
    rule match
      case NormalizationRule.Unnormalised => "unnormalised"
      case NormalizationRule.UnitPeak => "unit_peak"
      case NormalizationRule.UnitIntegral => "unit_integral"
      case NormalizationRule.Density => "density"
      case NormalizationRule.PositiveComponentArea => "positive_component_area"

  /** Destructured positionally: a new `SignedQuery` field fails compilation until encoded. */
  private[profile] def signedQuery(value: SignedQuery): String =
    value match
      case SignedQuery(label, weights, absoluteTolerance) => query(label, weights, absoluteTolerance)

  private def query(label: String, weights: Vector[Double], absoluteTolerance: Double): String =
    KernelBasisProvenance.record(
      "query",
      s"label=$label",
      s"weights=${numbers(weights)}",
      s"absoluteTolerance=${KernelBasisProvenance.number(absoluteTolerance)}"
    )

  /** Condition outputs are encoded in full. Trial outputs are refused by
    * [[ConditionProfileFit.prepare]] before any provenance exists; they are
    * still encoded totally, with the trial axis by its ordered trial and
    * condition counts, because that axis is bound by reference identity.
    */
  private[profile] def outputCanonical(output: OutputRequest): String =
    output match
      case OutputRequest.ConditionAmplitudes(rule) =>
        KernelBasisProvenance.record("condition_amplitudes", normalization(rule))
      case OutputRequest.ConditionQueries(queries, rule) =>
        KernelBasisProvenance.record(
          "condition_queries",
          normalization(rule),
          KernelBasisProvenance.record("queries", queries.map(signedQuery)*)
        )
      case OutputRequest.TrialAmplitudes(rule) =>
        KernelBasisProvenance.record("trial_amplitudes", normalization(rule))
      case OutputRequest.TrialQueries(queries, rule) =>
        KernelBasisProvenance.record(
          "trial_queries",
          normalization(rule),
          KernelBasisProvenance.record(
            "queries",
            queries.map(q =>
              KernelBasisProvenance.record(
                "trial_query",
                query(q.label, q.weights, q.absoluteTolerance),
                s"trials=${q.axis.trialIds.length}",
                s"conditions=${q.axis.conditionIds.length}"
              )
            )*
          )
        )

/** Prepared from the plan only; no response is read until [[run]]. */
final class ConditionProfilePreparation private[profile] (
    val plan: FitPlan,
    val policy: ConditionProfilePolicy,
    val retention: BasisExpandedRetentionPlan,
    val gram: Array[Double]):

  def conditions: Int = policy.structure.conditionCount
  def basisRank: Int = policy.structure.basisSize

  val provenance: ConditionProfileProvenance =
    ConditionProfileProvenance.of(policy, retention.preparation)

  /** A worker owning its own objective, decoder and counters. */
  final class Worker:
    val objective: GramConditionObjective =
      new GramConditionObjective(new GramConditionJets(gram, conditions, basisRank, policy.basis.family.dimension), policy.basis, NodeGrid(policy.basis.family.chart, policy.nodesPerAxis))
    private val decoder = new ShapeDecoder(objective, policy.budget, policy.prior, policy.noiseVariance)
    val counters: DecoderCounters = new DecoderCounters
    private val scale = new Array[Double](policy.basis.family.jetComponents)
    private val x = new Array[Double](conditions * basisRank)
    private val residualDf = retention.residualDf - policy.basis.family.dimension

    def fitEither(voxel: Int, crossProducts: gale.linalg.DVec, responseEnergy: Double): Either[FamilySummaryError, ConditionVoxelResult] =
      try
        val raw = fitRaw(voxel, crossProducts, responseEnergy)
        policy.basis.family.summariesEither(ShapePoint.unsafe(raw.coordinates)).map(raw.withSummary)
      catch
        case NonFatal(error) => Left(FamilySummaryError.EvaluationFailed(Option(error.getMessage).getOrElse(error.toString)))

    def fit(voxel: Int, crossProducts: gale.linalg.DVec, responseEnergy: Double): ConditionVoxelResult =
      fitEither(voxel, crossProducts, responseEnergy).fold(error => throw new IllegalArgumentException(error.message), identity)

    private[profile] def fitRaw(voxel: Int, crossProducts: gale.linalg.DVec, responseEnergy: Double): ConditionVoxelReadout =
      crossProducts.copyTo(x)
      objective.pointAt(x, responseEnergy)
      val decode = decoder.decode(counters)
      val family = policy.basis.family
      family.scaleJetInto(policy.output.rule, decode.point, scale)
      val amplitudes = decode.amplitudes.map(_ / scale(JetLayout.Value))
      val tolerance = policy.output match
        case OutputRequest.ConditionQueries(queries, _) => queries.map(_.absoluteTolerance).minOption.getOrElse(1e-6)
        case _ => 1e-6
      val amplitudeValues = amplitudes.indices.map(i => QueryEvaluation.audit(s"condition_${i + 1}", amplitudes(i), tolerance)).toVector
      val queryValues = policy.output match
        case OutputRequest.ConditionQueries(queries, _) => QueryEvaluation.evaluate(amplitudes, queries)
        case _ => Vector.empty
      ConditionVoxelReadout(
        voxel = voxel,
        coordinates = decode.coordinates,
        amplitudes = amplitudeValues,
        queries = queryValues,
        status = decode.status,
        conditionalSd = decode.conditionalSd,
        residualEnergy = decode.energy,
        noisePlugin = if residualDf > 0 then decode.energy / residualDf else Double.NaN,
        newtonSteps = decode.newtonSteps
      )

  /** Stream the retained blocks, post-solve each voxel, deliver to the sink in order. */
  def run(
      reader: DatasetSeriesReader,
      sink: BlockSink[ConditionProfileBlock, ConditionProfileReceipt],
      cancelled: () => Boolean = () => false
  ): Either[FitError, (Vector[ConditionProfileReceipt], DecoderCounters)] =
    val worker = new Worker
    val receipts = Vector.newBuilder[ConditionProfileReceipt]
    var ordinal = 0
    retention
      .foreachBlock(
        reader,
        block =>
          val product = block.product
          val results = Vector.newBuilder[ConditionVoxelResult]
          var v = 0
          var failure: Option[FitError] = None
          while v < block.voxelIndices.length && failure.isEmpty do
            worker.fitEither(block.voxelIndices(v), product.crossProducts.col(v), product.responseSquares(v)) match
              case Left(error) => failure = Some(FitError.InvalidFitAxis("condition profile summary", error.message))
              case Right(result) => results += result
            v += 1
          failure match
            case Some(error) => Left(error)
            case None =>
              val payload = ConditionProfileBlock(ordinal, results.result())
              ordinal += 1
              sink.accept(VoxelBlock(payload.ordinal, block.voxelIndices.headOption.getOrElse(0), block.voxelIndices.length), payload) match
                case Left(detail) => Left(FitError.InvalidFitAxis("condition profile sink", detail))
                case Right(receipt) =>
                  receipts += receipt
                  Right(())
        ,
        cancelled
      )
      .flatMap:
        case EstimateExecutionOutcome.Completed(_, _) => Right((receipts.result(), worker.counters))
        case EstimateExecutionOutcome.Cancelled(chunksDone, voxelsDone) =>
          Left(FitError.InvalidFitAxis("condition profile",
            s"cancelled after $chunksDone chunks / $voxelsDone voxels; previously delivered blocks remain partial"))

object ConditionProfileFit:

  /** Group the plan's structural task columns by the term's condition tags,
    * ordered by basis index, to declare the retained task structure.
    */
  def structureFor(plan: FitPlan, term: ConvolvedTerm): Either[FitError, TaskBasisStructure] =
    val byLabel = plan.structuralColumns.map(col => col.label -> col.id).toMap
    val conditions = term.columnConditions.flatten.distinct
    val grouped = conditions.map { cond =>
      val columns = term.columnNames.indices
        .filter(i => term.columnConditions(i).contains(cond))
        .sortBy(i => term.columnBasisIx(i).getOrElse(i))
        .map(i => term.columnNames(i))
      columns.map(name => byLabel.get(name).toRight(FitError.MissingStructuralIdentity(s"task column '$name'")))
    }
    val resolved = grouped.map(cols => cols.foldLeft[Either[FitError, Vector[ColumnId]]](Right(Vector.empty)) { (acc, c) => acc.flatMap(v => c.map(v :+ _)) })
    resolved.foldLeft[Either[FitError, Vector[Vector[ColumnId]]]](Right(Vector.empty)) { (acc, r) => acc.flatMap(v => r.map(v :+ _)) }
      .flatMap(TaskBasisStructure.make)

  def prepare(plan: FitPlan, policy: ConditionProfilePolicy): Either[FitError, ConditionProfilePreparation] =
    if !policy.output.isConditionNative then prepareRaw(plan, policy)
    else
      val summaryAdmission =
        try policy.basis.family.validateSummaryGrid
        catch
          case NonFatal(error) => Left(FamilySummaryError.EvaluationFailed(Option(error.getMessage).getOrElse(error.toString)))
      summaryAdmission.left.map(error => FitError.InvalidFitAxis("condition profile summary", error.message))
        .flatMap(_ => prepareRaw(plan, policy))

  /** The unified raw profile payload emits coordinates and amplitudes only. */
  private[profile] def prepareRaw(plan: FitPlan, policy: ConditionProfilePolicy): Either[FitError, ConditionProfilePreparation] =
    val c = policy.structure.conditionCount
    if policy.structure.basisSize != policy.basis.rank then
      Left(FitError.InvalidFitAxis("condition profile", s"structure declares ${policy.structure.basisSize} basis columns per condition but the kernel basis has rank ${policy.basis.rank}"))
    else if !policy.basis.family.supports(policy.output.rule) then
      Left(FitError.InvalidFitAxis("condition profile", s"family does not support ${policy.output.rule.label}"))
    else if !(policy.noiseVariance > 0.0 && policy.noiseVariance.isFinite) then
      Left(FitError.InvalidFitAxis("condition profile", s"noise variance must be finite and positive, got ${policy.noiseVariance}"))
    else
      for
        _ <- policy.output.validateFor(c).left.map(err => FitError.InvalidFitAxis("condition profile output", err.message))
        _ <- policy.admission.admits(plan, policy.structure, policy.basis).left.map(err => FitError.InvalidFitAxis("condition profile observed-family admission", err.message))
        size <- ChunkSize(policy.blockSize)
        retention <- BasisExpandedRetention.prepare(plan, policy.structure, size)
      yield
        val cm = c * policy.structure.basisSize
        val gram = new Array[Double](cm * cm)
        retention.gram.copyRowMajorTo(gram)
        new ConditionProfilePreparation(plan, policy, retention, gram)
