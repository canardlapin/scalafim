package scalafim.fmri.fit.profile

import gale.linalg.{DMat, QROptions, QRPivoting}
import scalafim.fmri.ar.{WhiteningPlan, WhiteningTransform}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, HrfKernelBasis}
import scalafim.fmri.hrf.family.{JetLayout, NormalizationRule, ParametricHrfFamily, ShapePoint, ShapeSummary}

enum CompactConditionError:
  case Whitening(detail: String)
  case RankDeficient(rank: Int, columns: Int)
  case Normalization(rule: NormalizationRule)

  def message: String =
    this match
      case Whitening(detail) => s"whitening failed: $detail"
      case RankDeficient(rank, columns) => s"the projected expanded design has rank $rank of $columns; identify the shape-invariant directions before fitting"
      case Normalization(rule) => s"the family does not support ${rule.label} normalisation"

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

  def prepare(
      expanded: ExpandedConditionDesign,
      whitening: Option[WhiteningPlan],
      nuisance: Option[DMat]
  ): Either[CompactConditionError, CompactConditionPreparation] =
    val rows = expanded.rows
    val cm = expanded.columns
    def whitenD(m: DMat): Either[CompactConditionError, DMat] =
      whitening match
        case None => Right(m)
        case Some(plan) => WhiteningTransform.matrix(plan, m).left.map(err => CompactConditionError.Whitening(err.toString))
    val options = QROptions(pivoting = QRPivoting.Column, rankTolerance = Some(1e-10))
    val nuisanceBasis: Either[CompactConditionError, (Array[Double], Int)] =
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
    for
      nb <- nuisanceBasis
      wa <- whitenD(CompactCondition.toDMat(rows, cm, expanded.term.data.data))
    yield
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

/** The compact condition backend as a [[ShapeObjective]]: a node bank of
  * orthonormal node projectors (for exact node scores) and full design jets
  * (so the node jet costs only small Gram products), continuous jets and
  * exact energies through [[CompactConditionJets]] and [[ProfileReduction]].
  * Point it at a voxel with [[pointAt]]; it holds only `z` and `e`.
  */
final class CompactConditionObjective(val prep: CompactConditionPreparation, val grid: NodeGrid) extends ShapeObjective:
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
  private var z: Array[Double] = new Array[Double](k)
  private var e: Double = 0.0

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

  /** Point the objective at a voxel's compact response. */
  def pointAt(response: Array[Double], energy: Double): Unit =
    z = response
    e = energy

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
    jets.assembleLoaded(z, e, bank, node * jets.designJetSize, comps)
    reduceInto(out)

  def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean =
    basis.coefficientJetInto(ShapePoint.unsafe(coordinates.toVector), kernelScratch, coeff, comps)
    jets.assemble(z, e, coeff, comps)
    reduceInto(out)

  def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
    basis.coefficientJetInto(ShapePoint.unsafe(coordinates.toVector), kernelScratch, coeff, 1)
    jets.assemble(z, e, coeff, 1)
    reduceInto(out)
    out.energy

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

/** Per-voxel driver: project a whitened response, decode, read out. */
final class CompactConditionRuntime(
    val prep: CompactConditionPreparation,
    grid: NodeGrid,
    budget: DecodeBudget,
    prior: Option[ShapePrior],
    noiseVariance: Double,
    normalization: NormalizationRule):
  require(prep.family.supports(normalization), s"family does not support ${normalization.label}")
  val objective: CompactConditionObjective = new CompactConditionObjective(prep, grid)
  private val decoder = new ShapeDecoder(objective, budget, prior, noiseVariance)
  private val z = new Array[Double](prep.rank)
  private val qy = new Array[Double](prep.nuisanceRank)
  private val scale = new Array[Double](prep.family.jetComponents)
  private val residualDf = prep.rows - prep.nuisanceRank - prep.conditions - prep.family.dimension

  /** Fit one voxel from its whitened response column starting at `offset`. */
  def fit(whitened: Array[Double], offset: Int, counters: DecoderCounters): CompactConditionFit =
    val e = prep.project(whitened, offset, z, qy)
    objective.pointAt(z, e)
    val decode = decoder.decode(counters)
    prep.family.scaleJetInto(normalization, decode.point, scale)
    val amplitudes = decode.amplitudes.map(_ / scale(JetLayout.Value))
    CompactConditionFit(
      decode = decode,
      amplitudes = amplitudes,
      normalization = normalization,
      nuisanceProjection = qy.toVector,
      residualEnergy = decode.energy,
      noisePlugin = if residualDf > 0 then decode.energy / residualDf else Double.NaN,
      summaries = prep.family.summaries(decode.point)
    )

private[profile] object CompactCondition:
  def toDMat(rows: Int, cols: Int, rowMajor: Array[Double]): DMat =
    val b = DMat.newBuilder(rows, cols)
    var i = 0
    while i < rows * cols do
      b.writeLinear(i, rowMajor(i))
      i += 1
    b.result()
