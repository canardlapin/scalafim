package scalafim.fmri.fit.profile

import gale.linalg.{DMat, QROptions, QRPivoting}
import scalafim.fmri.ar.{WhiteningPlan, WhiteningTransform}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, HrfKernelBasis}
import scala.util.control.NonFatal
import scalafim.fmri.hrf.family.{FamilySummaryError, JetLayout, NormalizationRule, ParametricHrfFamily, ShapePoint, ShapeSummary}

enum CompactConditionError:
  case RuntimePreparation(detail: String)
  case Summary(error: FamilySummaryError)
  case Whitening(detail: String)
  case RankDeficient(rank: Int, columns: Int)
  case Normalization(rule: NormalizationRule)
  case Admission(detail: String)

  def message: String =
    this match
      case RuntimePreparation(detail) => s"compact runtime preparation failed: $detail"
      case Summary(error) => error.message
      case Whitening(detail) => s"whitening failed: $detail"
      case RankDeficient(rank, columns) => s"the projected expanded design has rank $rank of $columns; identify the shape-invariant directions before fitting"
      case Normalization(rule) => s"the family does not support ${rule.label} normalisation"
      case Admission(detail) => s"observed-family admission refused: $detail"

/** Response-independent preparation of the compact condition backend: the
  * whitened nuisance basis `qF`, and the rank-revealing `U R` of the
  * whitened, nuisance-projected expanded design with `R` un-permuted so that
  * `D(theta) = R (I_C kron c(theta))`. Retains `(U, qF, R)`; never a
  * trial-sized object. One shared geometry: whitening and mask are fixed.
  */
final class CompactConditionPreparation private (
    val expanded: ExpandedConditionDesign,
    val whitening: Option[WhiteningPlan],
    val rows: Int,
    val rank: Int,
    val nuisanceRank: Int,
    val qF: Array[Double],
    val u: Array[Double],
    val rHat: Array[Double]):

  def basis: HrfKernelBasis = expanded.basis
  def family: ParametricHrfFamily = basis.family
  def conditions: Int = expanded.conditionCount

  /** Whiten a row-major `rows x cols` matrix through the shared plan (identity when none). */
  def whiten(cols: Int, rowMajor: Array[Double]): Either[CompactConditionError, Array[Double]] =
    whitening match
      case None => Right(rowMajor)
      case Some(plan) =>
        WhiteningTransform.matrix(plan, CompactCondition.toDMat(rows, cols, rowMajor)) match
          case Left(err) => Left(CompactConditionError.Whitening(err.toString))
          case Right(w) =>
            val out = new Array[Double](rows * cols)
            w.copyRowMajorTo(out)
            Right(out)

  /** `z = U' wy`, `qy = qF' wy`; returns `e = ||wy||^2 - ||qy||^2`. */
  def project(wy: Array[Double], offset: Int, z: Array[Double], qy: Array[Double]): Double =
    val k = rank
    val f = nuisanceRank
    java.util.Arrays.fill(z, 0.0)
    java.util.Arrays.fill(qy, 0.0)
    var total = 0.0
    var t = 0
    while t < rows do
      val y = wy(offset + t)
      total += y * y
      val ub = t * k
      var i = 0
      while i < k do
        z(i) += u(ub + i) * y
        i += 1
      val qb = t * f
      i = 0
      while i < f do
        qy(i) += qF(qb + i) * y
        i += 1
      t += 1
    var q2 = 0.0
    var i = 0
    while i < f do
      q2 += qy(i) * qy(i)
      i += 1
    total - q2

object CompactConditionPreparation:

  /** Opt in when the consumer emits summaries; ordinary preparation is reusable geometry. */
  def prepareWithSummaries(
      expanded: ExpandedConditionDesign,
      admission: ObservedFamilyAdmission,
      whitening: Option[WhiteningPlan],
      nuisance: Option[DMat],
      sourceTerm: scalafim.fmri.design.event.EventTerm,
      frame: scalafim.fmri.hrf.design.SamplingFrame,
      precision: scalafim.fmri.hrf.Seconds
  ): Either[CompactConditionError, CompactConditionPreparation] =
    val summaryAdmission =
      try expanded.basis.family.validateSummaryGrid
      catch
        case NonFatal(error) => Left(FamilySummaryError.EvaluationFailed(Option(error.getMessage).getOrElse(error.toString)))
    summaryAdmission.left.map(CompactConditionError.Summary.apply)
      .flatMap(_ => prepare(expanded, admission, whitening, nuisance, sourceTerm, frame, precision))

  def prepare(
      expanded: ExpandedConditionDesign,
      admission: ObservedFamilyAdmission,
      whitening: Option[WhiteningPlan],
      nuisance: Option[DMat],
      sourceTerm: scalafim.fmri.design.event.EventTerm,
      frame: scalafim.fmri.hrf.design.SamplingFrame,
      precision: scalafim.fmri.hrf.Seconds
  ): Either[CompactConditionError, CompactConditionPreparation] =
    val rows = expanded.rows
    val cm = expanded.columns
    def whitenD(m: DMat): Either[CompactConditionError, DMat] =
      whitening match
        case None => Right(m)
        case Some(plan) => WhiteningTransform.matrix(plan, m).left.map(err => CompactConditionError.Whitening(err.toString))
    val options = QROptions(pivoting = QRPivoting.Column, rankTolerance = Some(1e-10))
    def nuisanceBasis: Either[CompactConditionError, (Array[Double], Int)] =
      nuisance match
        case None => Right((new Array[Double](0), 0))
        case Some(f) =>
          whitenD(f).map { wf =>
            val qr = wf.qr(options)
            val rf = qr.diagnostics.rank.getOrElse(wf.cols)
            val q = new Array[Double](rows * rf)
            qr.q.slice(0, rows, 0, rf).copyRowMajorTo(q)
            (q, rf)
          }
    admission.admits(expanded, sourceTerm, frame, precision, whitening, nuisance).left.map(err => CompactConditionError.Admission(err.message)).flatMap { _ =>
      for
        nb <- nuisanceBasis
        wa <- whitenD(CompactCondition.toDMat(rows, cm, expanded.term.data.data))
      yield {
        val (qF, rf) = nb
        val waArr = new Array[Double](rows * cm)
        wa.copyRowMajorTo(waArr)
        // A_proj = (I - qF qF') W A
        val coeffs = new Array[Double](rf * cm)
        var t = 0
        while t < rows do
          var i = 0
          while i < rf do
            val q = qF(t * rf + i)
            var j = 0
            while j < cm do
              coeffs(i * cm + j) += q * waArr(t * cm + j)
              j += 1
            i += 1
          t += 1
        val projected = new Array[Double](rows * cm)
        t = 0
        while t < rows do
          var j = 0
          while j < cm do
            var acc = waArr(t * cm + j)
            var i = 0
            while i < rf do
              acc -= qF(t * rf + i) * coeffs(i * cm + j)
              i += 1
            projected(t * cm + j) = acc
            j += 1
          t += 1
        val qr = CompactCondition.toDMat(rows, cm, projected).qr(options)
        val k = qr.diagnostics.rank.getOrElse(cm)
        val u = new Array[Double](rows * k)
        qr.q.slice(0, rows, 0, k).copyRowMajorTo(u)
        val rPerm = new Array[Double](k * cm)
        qr.r.slice(0, k, 0, cm).copyRowMajorTo(rPerm)
        val perm = qr.columnPermutation.toArray
        val rHat = new Array[Double](k * cm)
        var i = 0
        while i < k do
          var j = 0
          while j < cm do
            rHat(i * cm + perm(j)) = rPerm(i * cm + j)
            j += 1
          i += 1
        new CompactConditionPreparation(expanded, whitening, rows, k, rf, qF, u, rHat)
      }
    }

/** The capability is sealed here so arbitrary objective callbacks cannot
  * introduce comparison evidence unrelated to their returned criterion.
  */
private[profile] sealed trait PairedProfileObjective extends ShapeObjective:
  private[profile] def enablePairedComparison(): Unit
  private[profile] def captureAcceptedProfile(coordinates: Array[Double], jet: ProfileJetBuffer): Unit
  private[profile] def pairedCandidateAvailable(coordinates: Array[Double], jet: ProfileJetBuffer): Boolean
  private[profile] def comparePairedCandidate(coordinates: Array[Double], jet: ProfileJetBuffer)
    : Either[CompactComparisonFailure, PairedShapeDecrease]

/** Compact condition energies and jets over an owned response and fixed factor.
  * Optional owned model snapshots bind paired comparisons to successful full
  * jets. Response epochs are local to this objective, not global response IDs.
  */
final class CompactConditionObjective(val prep: CompactConditionPreparation, val grid: NodeGrid) extends PairedProfileObjective:
  private val family = prep.family
  private val basis = prep.basis
  private val k = prep.rank
  private val c = prep.conditions
  private val m = basis.rank
  private val d = family.dimension
  private val comps = family.jetComponents
  private val jets = new CompactConditionJets(prep.rHat, k, c, m, d)
  private val reduction = new ProfileReduction(d, c)
  private val kernelScratch = new Array[Double](comps * basis.fineCount)
  private val coeff = new Array[Double](comps * m)
  private val coords = new Array[Double](d)
  private val stacked = new Array[Double](grid.count * c * k)
  private val bank = new Array[Double](grid.count * jets.designJetSize)
  private val gram = new Array[Double](c * c)
  private val tmp = new Array[Double](c)
  private val z: Array[Double] = new Array[Double](k)
  private var e: Double = 0.0
  private var responseEpoch: Long = 0L
  private var evaluationGeneration: Long = 0L
  private var pairedState: Option[CompactPairedState] = None

  private[profile] def comparisonWorkspaceReceipt(enabled: Boolean): CompactComparisonWorkspaceReceipt =
    CompactComparisonWorkspaceReceipt.estimate(prep, enabled)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private[profile] def enablePairedComparison(): Unit =
    if pairedState.isEmpty then
      val receipt = comparisonWorkspaceReceipt(true)
      require(receipt.enabled, "paired allocation must be admitted")
      pairedState = Some(new CompactPairedState(k, c, d))

  private[profile] def captureAcceptedProfile(coordinates: Array[Double], jet: ProfileJetBuffer): Unit =
    pairedState.foreach(_.capture(coordinates, jet, z, e, responseEpoch))

  private[profile] def pairedCandidateAvailable(coordinates: Array[Double], jet: ProfileJetBuffer): Boolean =
    pairedState.exists(_.available(coordinates, jet, z, e, responseEpoch))

  private[profile] def comparePairedCandidate(coordinates: Array[Double], jet: ProfileJetBuffer)
      : Either[CompactComparisonFailure, PairedShapeDecrease] =
    pairedState match
      case None => Left(CompactComparisonFailure.SnapshotMismatch)
      case Some(state) => state.compare(coordinates, jet, z, e, responseEpoch)

  private def beginPoint(): Long =
    pairedState.foreach(_.currentFullJet = false)
    evaluationGeneration += 1L
    evaluationGeneration

  private def stampPoint(point: ShapePoint, out: ProfileJetBuffer, epoch: Long, generation: Long, ok: Boolean): Boolean =
    if responseEpoch != epoch || evaluationGeneration != generation then
      pairedState.foreach(_.currentFullJet = false)
      false
    else
      if ok then pairedState.foreach: state =>
        jets.copyValueDesignInto(state.currentDesign)
        state.stamp(point.coordinates, out, epoch)
      ok

  locally {
    var node = 0
    while node < grid.count do
      grid.coordinatesInto(node, coords)
      basis.coefficientJetInto(ShapePoint.unsafe(coords.toVector), kernelScratch, coeff, comps)
      jets.assemble(new Array[Double](k), 0.0, coeff, comps)
      jets.exportDesign(bank, node * jets.designJetSize)
      // orthonormal projector rows: q_kk = L^-1 d_kk with G = L L'
      val design = jets.valueDesign
      var i = 0
      while i < c do
        var j = 0
        while j < c do
          var acc = 0.0
          var kk = 0
          while kk < k do
            acc += design(kk * c + i) * design(kk * c + j)
            kk += 1
          gram(i * c + j) = acc
          j += 1
        i += 1
      require(SmallCholesky.factorInPlace(c, gram), s"node $node has a singular compact Gram")
      var kk = 0
      while kk < k do
        i = 0
        while i < c do
          tmp(i) = design(kk * c + i)
          i += 1
        i = 0
        while i < c do
          var s = tmp(i)
          var j = 0
          while j < i do
            s -= gram(i * c + j) * tmp(j)
            j += 1
          tmp(i) = s / gram(i * c + i)
          i += 1
        i = 0
        while i < c do
          stacked((node * c + i) * k + kk) = tmp(i)
          i += 1
        kk += 1
      node += 1
  }

  def amplitudeCount: Int = c

  /** Own a compact response for one voxel and invalidate previous comparison
    * snapshots. Caller arrays cannot change the criterion after this boundary.
    */
  def pointAt(response: Array[Double], energy: Double): Unit =
    require(response.length == k, "compact response has the wrong rank")
    System.arraycopy(response, 0, z, 0, k)
    e = energy
    responseEpoch += 1L
    pairedState.foreach: state =>
      state.previousValid = false
      state.currentFullJet = false

  def scoreNode(node: Int): Double =
    var fit = 0.0
    var i = 0
    while i < c do
      val row = (node * c + i) * k
      var acc = 0.0
      var kk = 0
      while kk < k do
        acc += stacked(row + kk) * z(kk)
        kk += 1
      fit += acc * acc
      i += 1
    e - fit

  private def reduceInto(out: ProfileJetBuffer): Boolean =
    reduction.reduce(jets.s, jets.b, jets.g, out)

  def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
    val epoch = responseEpoch
    val generation = beginPoint()
    val point = grid.point(node)
    jets.assembleLoaded(z, e, bank, node * jets.designJetSize, comps)
    stampPoint(point, out, epoch, generation, reduceInto(out))

  def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean =
    val epoch = responseEpoch
    val generation = beginPoint()
    val point = ShapePoint.unsafe(coordinates.toVector)
    basis.coefficientJetInto(point, kernelScratch, coeff, comps)
    jets.assemble(z, e, coeff, comps)
    val ok = reduceInto(out)
    var axis = 0
    while axis < d do
      if java.lang.Double.doubleToRawLongBits(point(axis)) != java.lang.Double.doubleToRawLongBits(coordinates(axis)) then
        pairedState.foreach(_.currentFullJet = false)
        return false
      axis += 1
    stampPoint(point, out, epoch, generation, ok)

  def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
    val epoch = responseEpoch
    val generation = beginPoint()
    val point = ShapePoint.unsafe(coordinates.toVector)
    basis.coefficientJetInto(point, kernelScratch, coeff, 1)
    jets.assemble(z, e, coeff, 1)
    reduceInto(out)
    pairedState.foreach(_.currentFullJet = false)
    if responseEpoch == epoch && evaluationGeneration == generation &&
      point.coordinates.indices.forall(axis => java.lang.Double.doubleToRawLongBits(point(axis)) ==
        java.lang.Double.doubleToRawLongBits(coordinates(axis))) then out.energy else Double.PositiveInfinity

/** One voxel's condition fit: decoded shape, signed amplitudes in the
  * requested normalisation, nuisance projections retained for recovery,
  * a labelled selection-dependent noise plug-in and shape summaries.
  */
final case class CompactConditionFit(
    decode: ShapeDecodeResult,
    amplitudes: Vector[Double],
    normalization: NormalizationRule,
    nuisanceProjection: Vector[Double],
    residualEnergy: Double,
    noisePlugin: Double,
    summaries: ShapeSummary)

private[profile] final case class CompactConditionReadout(
    decode: ShapeDecodeResult,
    amplitudes: Vector[Double],
    normalization: NormalizationRule,
    nuisanceProjection: Vector[Double],
    residualEnergy: Double,
    noisePlugin: Double):
  def withSummary(summary: ShapeSummary): CompactConditionFit =
    CompactConditionFit(decode, amplitudes, normalization, nuisanceProjection, residualEnergy, noisePlugin, summary)

/** Per-voxel driver: project a whitened response, decode, read out. */
final class CompactConditionRuntime private (
    val prep: CompactConditionPreparation,
    grid: NodeGrid,
    budget: DecodeBudget,
    prior: Option[ShapePrior],
    noiseVariance: Double,
    normalization: NormalizationRule,
    emitSummaries: Boolean):
  def this(prep: CompactConditionPreparation, grid: NodeGrid, budget: DecodeBudget, prior: Option[ShapePrior], noiseVariance: Double, normalization: NormalizationRule) =
    this(prep, grid, budget, prior, noiseVariance, normalization, true)

  if emitSummaries then prep.family.validateSummaryGrid.fold(error => throw new IllegalArgumentException(error.message), identity)
  require(prep.family.supports(normalization), s"family does not support ${normalization.label}")
  val comparisonWorkspaceReceipt: CompactComparisonWorkspaceReceipt = CompactComparisonWorkspaceReceipt.estimate(prep,
    CompactComparisonWorkspaceReceipt.enabled(prior, budget))
    .fold(error => throw new IllegalArgumentException(error.message), identity)
  val objective: CompactConditionObjective = new CompactConditionObjective(prep, grid)
  private val decoder = new ShapeDecoder(objective, budget, prior, noiseVariance)
  private val z = new Array[Double](prep.rank)
  private val qy = new Array[Double](prep.nuisanceRank)
  private val scale = new Array[Double](prep.family.jetComponents)
  private val residualDf = prep.rows - prep.nuisanceRank - prep.conditions - prep.family.dimension

  /** Node energies scanned for the last fitted voxel, for pooling. */
  def lastNodeEnergies: Array[Double] = decoder.lastNodeEnergies

  /** Fit one voxel from its whitened response column starting at `offset`. */
  def fitEither(whitened: Array[Double], offset: Int, counters: DecoderCounters): Either[FamilySummaryError, CompactConditionFit] =
    try
      val raw = fitRaw(whitened, offset, counters)
      prep.family.summariesEither(raw.decode.point).map(raw.withSummary)
    catch
      case NonFatal(error) => Left(FamilySummaryError.EvaluationFailed(Option(error.getMessage).getOrElse(error.toString)))

  def fit(whitened: Array[Double], offset: Int, counters: DecoderCounters): CompactConditionFit =
    fitEither(whitened, offset, counters).fold(error => throw new IllegalArgumentException(error.message), identity)

  private[profile] def fitRaw(whitened: Array[Double], offset: Int, counters: DecoderCounters): CompactConditionReadout =
    val e = prep.project(whitened, offset, z, qy)
    objective.pointAt(z, e)
    val decode = decoder.decode(counters)
    prep.family.scaleJetInto(normalization, decode.point, scale)
    val amplitudes = decode.amplitudes.map(_ / scale(JetLayout.Value))
    CompactConditionReadout(
      decode = decode,
      amplitudes = amplitudes,
      normalization = normalization,
      nuisanceProjection = qy.toVector,
      residualEnergy = decode.energy,
      noisePlugin = if residualDf > 0 then decode.energy / residualDf else Double.NaN
    )

object CompactConditionRuntime:
  def prepare(prep: CompactConditionPreparation, grid: NodeGrid, budget: DecodeBudget, prior: Option[ShapePrior], noiseVariance: Double, normalization: NormalizationRule): Either[CompactConditionError, CompactConditionRuntime] =
    try
      prep.family.validateSummaryGrid.left.map(CompactConditionError.Summary.apply).flatMap: _ =>
        if !prep.family.supports(normalization) then Left(CompactConditionError.Normalization(normalization))
        else Right(new CompactConditionRuntime(prep, grid, budget, prior, noiseVariance, normalization, false))
    catch
      case NonFatal(error) => Left(CompactConditionError.RuntimePreparation(Option(error.getMessage).getOrElse(error.toString)))

  private[profile] def raw(prep: CompactConditionPreparation, grid: NodeGrid, budget: DecodeBudget, prior: Option[ShapePrior], noiseVariance: Double, normalization: NormalizationRule): CompactConditionRuntime =
    new CompactConditionRuntime(prep, grid, budget, prior, noiseVariance, normalization, false)

private[profile] object CompactCondition:
  def toDMat(rows: Int, cols: Int, rowMajor: Array[Double]): DMat =
    val b = DMat.newBuilder(rows, cols)
    var i = 0
    while i < rows * cols do
      b.writeLinear(i, rowMajor(i))
      i += 1
    b.result()
