package scalafim.fmri.fit.profile

import scalafim.dataset.DatasetSeriesReader
import scalafim.fmri.design.ColumnId
import scalafim.fmri.design.event.ConvolvedTerm
import scalafim.fmri.design.hrf.HrfKernelBasis
import scalafim.fmri.fit.{BasisExpandedRetention, BasisExpandedRetentionPlan, ChunkSize, FitError, TaskBasisStructure}
import scalafim.fmri.hrf.family.{JetLayout, ShapeSummary}
import scalafim.fmri.model.FitPlan

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

/** One block's payload: released to the sink, never retained by the runner. */
final case class ConditionProfileBlock(ordinal: Int, results: Vector[ConditionVoxelResult])

final case class ConditionProfileReceipt(ordinal: Int, voxels: Int, accepted: Int)

final case class ConditionProfileProvenance(
    basis: String,
    structure: Vector[Vector[String]],
    preparation: String,
    nodesPerAxis: Vector[Int],
    budget: DecodeBudget,
    output: String,
    noiseVariance: Double):
  def canonical: String =
    s"condition-profile/v1|basis=$basis|structure=${structure.map(_.mkString("+")).mkString(";")}|preparation=$preparation|nodes=${nodesPerAxis.mkString("x")}|budget=$budget|output=$output|sigma2=$noiseVariance"

/** Prepared from the plan only; no response is read until [[run]]. */
final class ConditionProfilePreparation private[profile] (
    val plan: FitPlan,
    val policy: ConditionProfilePolicy,
    val retention: BasisExpandedRetentionPlan,
    val gram: Array[Double]):

  def conditions: Int = policy.structure.conditionCount
  def basisRank: Int = policy.structure.basisSize

  val provenance: ConditionProfileProvenance =
    ConditionProfileProvenance(
      basis = policy.basis.provenance.canonical,
      structure = policy.structure.conditions.map(_.map(_.value)),
      preparation = retention.preparation.toString,
      nodesPerAxis = policy.nodesPerAxis,
      budget = policy.budget,
      output = policy.output.toString,
      noiseVariance = policy.noiseVariance
    )

  /** A worker owning its own objective, decoder and counters. */
  final class Worker:
    val objective: GramConditionObjective =
      new GramConditionObjective(new GramConditionJets(gram, conditions, basisRank, policy.basis.family.dimension), policy.basis, NodeGrid(policy.basis.family.chart, policy.nodesPerAxis))
    private val decoder = new ShapeDecoder(objective, policy.budget, policy.prior, policy.noiseVariance)
    val counters: DecoderCounters = new DecoderCounters
    private val scale = new Array[Double](policy.basis.family.jetComponents)
    private val x = new Array[Double](conditions * basisRank)
    private val residualDf = retention.residualDf - policy.basis.family.dimension

    def fit(voxel: Int, crossProducts: gale.linalg.DVec, responseEnergy: Double): ConditionVoxelResult =
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
      ConditionVoxelResult(
        voxel = voxel,
        coordinates = decode.coordinates,
        summaries = family.summaries(decode.point),
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
          while v < block.voxelIndices.length do
            results += worker.fit(block.voxelIndices(v), product.crossProducts.col(v), product.responseSquares(v))
            v += 1
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
      .map(_ => (receipts.result(), worker.counters))

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
        size <- ChunkSize(policy.blockSize)
        retention <- BasisExpandedRetention.prepare(plan, policy.structure, size)
      yield
        val cm = c * policy.structure.basisSize
        val gram = new Array[Double](cm * cm)
        retention.gram.copyRowMajorTo(gram)
        new ConditionProfilePreparation(plan, policy, retention, gram)
