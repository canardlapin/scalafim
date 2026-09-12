package scalafim.fmri.fit.profile

import gale.linalg.{BandedCholesky, CholeskyOptions, DMat, DMatBuilder}
import scalafim.fmri.ar.{WhiteningPlan, WhiteningTransform}
import scalafim.fmri.design.hrf.{ExpandedTrialDesign, HrfKernelBasis, TrialMembership}
import scalafim.fmri.hrf.family.{JetLayout, ShapePoint}

enum TrialBandedError:
  case InvalidLambda(value: Double)
  case NuisanceRows(expected: Int, actual: Int)
  case ResponseLength(expectedAtLeast: Int, actual: Int)
  case UnobservedTrial(trial: Int)
  case Whitening(detail: String)
  case Factorisation(detail: String)
  case ReleaseRank(node: Option[Int], columns: Int)
  case InvalidNode(node: Int, count: Int)
  case ReadoutRhs(expected: Int, actual: Int)

  def message: String =
    this match
      case InvalidLambda(value) => s"lambda must be finite and > 0, got $value"
      case NuisanceRows(expected, actual) => s"nuisance has $actual rows; expected $expected"
      case ResponseLength(expected, actual) => s"response storage has $actual entries; expected at least $expected"
      case UnobservedTrial(trial) => s"trial ${trial + 1} has no observed support after whitening"
      case Whitening(detail) => s"whitening failed: $detail"
      case Factorisation(detail) => s"banded trial factorisation failed: $detail"
      case ReleaseRank(node, columns) =>
        val where = node.fold("continuous shape")(n => s"reference node $n")
        s"the $where nuisance/condition release is rank deficient ($columns columns)"
      case InvalidNode(node, count) => s"reference node $node is outside 0 until $count"
      case ReadoutRhs(expected, actual) => s"trial readout RHS has length $actual; expected $expected"

/** Response-independent accounting for the packed trial geometry. */
final case class TrialBandedPreparationReceipt(
    rows: Int,
    trials: Int,
    basisRank: Int,
    conditions: Int,
    nuisanceColumns: Int,
    bandwidth: Int,
    gramBlocks: Int,
    retainedDoubles: Long):
  def estimatedBytes: Long = retainedDoubles * 8L

/** Per-worker counters. Shared node factors are preparation, while continuous
  * and exact-readout factors are charged where they occur.
  */
final case class TrialBandedWorkSnapshot(
    voxels: Long,
    trialBasisScores: Long,
    bankValueEvaluations: Long,
    jetEvaluations: Long,
    amplitudeCorrections: Long,
    bandedSolveCalls: Long,
    bandedRightHandSides: Long,
    continuousFactors: Long,
    exactReadoutFactors: Long)

final class TrialBandedWork private[profile] ():
  private[profile] var voxels: Long = 0L
  private[profile] var trialBasisScores: Long = 0L
  private[profile] var bankValueEvaluations: Long = 0L
  private[profile] var jetEvaluations: Long = 0L
  private[profile] var amplitudeCorrections: Long = 0L
  private[profile] var bandedSolveCalls: Long = 0L
  private[profile] var bandedRightHandSides: Long = 0L
  private[profile] var continuousFactors: Long = 0L
  private[profile] var exactReadoutFactors: Long = 0L

  def snapshot: TrialBandedWorkSnapshot =
    TrialBandedWorkSnapshot(voxels, trialBasisScores, bankValueEvaluations, jetEvaluations, amplitudeCorrections,
      bandedSolveCalls, bandedRightHandSides, continuousFactors, exactReadoutFactors)

/** Encoded response in the sufficient statistics consumed by TrialBanded.
  * Construction is tied to one preparation so incompatible geometries cannot
  * be pointed at an objective accidentally.
  */
final class TrialBandedResponse private[profile] (
    private[profile] val owner: TrialBandedPreparation,
    private[profile] val trialBasisScores: Array[Double],
    private[profile] val nuisanceScores: Array[Double],
    private[profile] var energyValue: Double):
  def responseEnergy: Double = energyValue

enum TrialReadoutFactorMode:
  case PreparedNode(node: Int)
  case ExactShape(coordinates: Vector[Double])

final case class TrialBandedReadout(
    penalizedEnergy: Double,
    conditionMeans: Vector[Double],
    trialAmplitudes: Vector[Double],
    factorMode: TrialReadoutFactorMode)

/** Shared, response-independent trial geometry. The `m(m+1)/2` blocks store
  * lower symmetric bands directly; no dense `N x N` Gram is constructed.
  */
final class TrialBandedPreparation private[profile] (
    val basis: HrfKernelBasis,
    val membership: TrialMembership,
    val rows: Int,
    val whitening: Option[WhiteningPlan],
    val lambda: Double,
    val bandwidth: Int,
    private[profile] val sparseDesign: Array[Double],
    private[profile] val trialOffsets: Array[Int],
    private[profile] val whitenedNuisance: Array[Double],
    private[profile] val gramBlocksData: Array[Double],
    private[profile] val basisNuisanceCross: Array[Double],
    private[profile] val nuisanceGram: Array[Double],
    private[profile] val starts: Array[Int],
    private[profile] val ends: Array[Int]):

  val trials: Int = membership.trials
  val basisRank: Int = basis.rank
  val conditions: Int = membership.conditionCount
  val nuisanceColumns: Int = if rows == 0 then 0 else whitenedNuisance.length / rows
  val bandWidth: Int = bandwidth + 1
  val packedBandSize: Int = trials * bandWidth
  val gramBlockCount: Int = basisRank * (basisRank + 1) / 2

  val receipt: TrialBandedPreparationReceipt =
    TrialBandedPreparationReceipt(
      rows,
      trials,
      basisRank,
      conditions,
      nuisanceColumns,
      bandwidth,
      gramBlockCount,
      gramBlocksData.length.toLong + sparseDesign.length + whitenedNuisance.length +
        basisNuisanceCross.length + nuisanceGram.length + trialOffsets.length + starts.length + ends.length
    )

  /** Copy one cross-basis Gram block as `bands(i,d)=B(i,i-d)`. */
  def gramBlock(basisA: Int, basisB: Int): DMat =
    require(basisA >= 0 && basisA < basisRank && basisB >= 0 && basisB < basisRank)
    val index = TrialBandedPreparation.pairIndex(math.min(basisA, basisB), math.max(basisA, basisB))
    val out = DMatBuilder.zeros(trials, bandWidth)
    val offset = index * packedBandSize
    var i = 0
    while i < packedBandSize do
      out.writeLinear(i, gramBlocksData(offset + i))
      i += 1
    out.result()

  /** Apply the preparation's shared whitening to a row-major response block. */
  def whitenResponses(columns: Int, rowMajor: Array[Double]): Either[TrialBandedError, Array[Double]] =
    if rowMajor.length != rows * columns then Left(TrialBandedError.ResponseLength(rows * columns, rowMajor.length))
    else
      whitening match
        case None => Right(java.util.Arrays.copyOf(rowMajor, rowMajor.length))
        case Some(plan) =>
          WhiteningTransform.matrix(plan, TrialBandedPreparation.toDMat(rows, columns, rowMajor)) match
            case Left(error) => Left(TrialBandedError.Whitening(error.toString))
            case Right(matrix) =>
              val out = new Array[Double](rowMajor.length)
              matrix.copyRowMajorTo(out)
              Right(out)

  /** Encode one already-whitened response column. The output contains exactly
    * `N*m` trial-basis scores, `F'y`, and `y'y`.
    */
  def encodeWhitened(response: Array[Double], offset: Int = 0): Either[TrialBandedError, TrialBandedResponse] =
    encodeWhitenedInto(response, offset, newResponseBuffer)

  /** Allocate one reusable response buffer for a worker. */
  def newResponseBuffer: TrialBandedResponse =
    new TrialBandedResponse(this, new Array[Double](trials * basisRank), new Array[Double](nuisanceColumns), 0.0)

  /** Allocation-free response encoding after [[newResponseBuffer]]. */
  def encodeWhitenedInto(
      response: Array[Double],
      offset: Int,
      out: TrialBandedResponse
  ): Either[TrialBandedError, TrialBandedResponse] =
    if offset < 0 || response.length - offset < rows then Left(TrialBandedError.ResponseLength(offset + rows, response.length))
    else
      require(out.owner eq this, "response buffer belongs to a different TrialBanded preparation")
      java.util.Arrays.fill(out.trialBasisScores, 0.0)
      java.util.Arrays.fill(out.nuisanceScores, 0.0)
      var energy = 0.0
      var t = 0
      while t < rows do
        val y = response(offset + t)
        energy += y * y
        var f = 0
        while f < nuisanceColumns do
          out.nuisanceScores(f) += whitenedNuisance(t * nuisanceColumns + f) * y
          f += 1
        t += 1
      var trial = 0
      while trial < trials do
        t = starts(trial)
        while t <= ends(trial) do
          val base = trialOffsets(trial) + (t - starts(trial)) * basisRank
          var p = 0
          while p < basisRank do
            out.trialBasisScores(p * trials + trial) += sparseDesign(base + p) * response(offset + t)
            p += 1
          t += 1
        trial += 1
      out.energyValue = energy
      Right(out)

  def objective(grid: NodeGrid): Either[TrialBandedError, TrialBandedObjective] =
    TrialBandedObjective.make(this, grid)

object TrialBandedPreparation:

  def prepare(
      expanded: ExpandedTrialDesign,
      whitening: Option[WhiteningPlan],
      nuisance: Option[DMat],
      lambda: Double
  ): Either[TrialBandedError, TrialBandedPreparation] =
    if !(lambda > 0.0 && lambda.isFinite) then Left(TrialBandedError.InvalidLambda(lambda))
    else if nuisance.exists(_.rows != expanded.rows) then
      Left(TrialBandedError.NuisanceRows(expanded.rows, nuisance.map(_.rows).getOrElse(0)))
    else
      val rows = expanded.rows
      val n = expanded.trials
      val m = expanded.rank
      val rawDesign = expanded.term.data.data
      val f = nuisance.map(_.cols).getOrElse(0)
      val rawNuisance = nuisance.fold(new Array[Double](0)) { matrix =>
        val out = new Array[Double](rows * f)
        matrix.copyRowMajorTo(out)
        out
      }
      def whiten(cols: Int, values: Array[Double]): Either[TrialBandedError, Array[Double]] =
        if cols == 0 then Right(new Array[Double](0))
        else
          whitening match
            case None => Right(java.util.Arrays.copyOf(values, values.length))
            case Some(plan) =>
              WhiteningTransform.matrix(plan, toDMat(rows, cols, values)) match
                case Left(error) => Left(TrialBandedError.Whitening(error.toString))
                case Right(matrix) =>
                  val out = new Array[Double](values.length)
                  matrix.copyRowMajorTo(out)
                  Right(out)
      for
        x <- whiten(n * m, rawDesign)
        nuisanceData <- whiten(f, rawNuisance)
        prepared <- build(expanded, whitening, lambda, x, nuisanceData)
      yield prepared

  private def build(
      expanded: ExpandedTrialDesign,
      whitening: Option[WhiteningPlan],
      lambda: Double,
      x: Array[Double],
      nuisance: Array[Double]): Either[TrialBandedError, TrialBandedPreparation] =
    val rows = expanded.rows
    val n = expanded.trials
    val m = expanded.rank
    val cols = n * m
    val f = if rows == 0 then 0 else nuisance.length / rows
    val starts = Array.fill(n)(rows)
    val ends = Array.fill(n)(-1)
    var t = 0
    while t < rows do
      var p = 0
      while p < m do
        val base = t * cols + p * n
        var trial = 0
        while trial < n do
          if x(base + trial) != 0.0 then
            starts(trial) = math.min(starts(trial), t)
            ends(trial) = math.max(ends(trial), t)
          trial += 1
        p += 1
      t += 1
    var trial = 0
    while trial < n do
      if ends(trial) < 0 then return Left(TrialBandedError.UnobservedTrial(trial))
      trial += 1
    var bandwidth = 0
    var i = 0
    while i < n do
      var j = 0
      while j < i do
        if starts(i) <= ends(j) && starts(j) <= ends(i) then bandwidth = math.max(bandwidth, i - j)
        j += 1
      i += 1
    val width = bandwidth + 1
    val bandSize = n * width
    val blockCount = m * (m + 1) / 2
    val blocks = new Array[Double](blockCount * bandSize)
    var q = 0
    while q < m do
      var p = 0
      while p <= q do
        val blockOffset = pairIndex(p, q) * bandSize
        i = 0
        while i < n do
          var delta = 0
          while delta <= math.min(i, bandwidth) do
            val other = i - delta
            val from = math.max(starts(i), starts(other))
            val until = math.min(ends(i), ends(other))
            var sum = 0.0
            t = from
            while t <= until do
              val row = t * cols
              if p == q then sum += x(row + p * n + i) * x(row + p * n + other)
              else
                sum += x(row + p * n + i) * x(row + q * n + other) +
                  x(row + q * n + i) * x(row + p * n + other)
              t += 1
            blocks(blockOffset + i * width + delta) = sum
            delta += 1
          i += 1
        p += 1
      q += 1
    val xf = new Array[Double](m * n * f)
    var p = 0
    while p < m do
      i = 0
      while i < n do
        var nuisanceCol = 0
        while nuisanceCol < f do
          var sum = 0.0
          t = starts(i)
          while t <= ends(i) do
            sum += x(t * cols + p * n + i) * nuisance(t * f + nuisanceCol)
            t += 1
          xf((p * n + i) * f + nuisanceCol) = sum
          nuisanceCol += 1
        i += 1
      p += 1
    val ff = new Array[Double](f * f)
    i = 0
    while i < f do
      var j = 0
      while j <= i do
        var sum = 0.0
        t = 0
        while t < rows do
          sum += nuisance(t * f + i) * nuisance(t * f + j)
          t += 1
        ff(i * f + j) = sum
        ff(j * f + i) = sum
        j += 1
      i += 1
    val offsets = new Array[Int](n + 1)
    trial = 0
    while trial < n do
      offsets(trial + 1) = offsets(trial) + (ends(trial) - starts(trial) + 1) * m
      trial += 1
    val sparse = new Array[Double](offsets(n))
    trial = 0
    while trial < n do
      t = starts(trial)
      while t <= ends(trial) do
        p = 0
        while p < m do
          sparse(offsets(trial) + (t - starts(trial)) * m + p) = x(t * cols + p * n + trial)
          p += 1
        t += 1
      trial += 1
    Right(new TrialBandedPreparation(expanded.basis, expanded.membership, rows, whitening, lambda, bandwidth, sparse, offsets, nuisance, blocks, xf, ff, starts, ends))

  private[profile] def pairIndex(p: Int, q: Int): Int = q * (q + 1) / 2 + p

  private[profile] def toDMat(rows: Int, cols: Int, values: Array[Double]): DMat =
    val out = DMatBuilder.zeros(rows, cols)
    var i = 0
    while i < values.length do
      out.writeLinear(i, values(i))
      i += 1
    out.result()

private final case class TrialBandedReference(
    coefficients: Array[Double],
    factor: BandedCholesky,
    aJets: Array[Double],
    cJets: Array[Double],
    wcJets: Array[Double],
    hJets: Array[Double],
    releaseLower: Array[Double],
    logDetK: Double)

/** Trial-sized profile objective. Its node factors and releases are immutable
  * and shared; each worker owns only response statistics and primitive scratch.
  */
final class TrialBandedObjective private (
    val preparation: TrialBandedPreparation,
    val grid: NodeGrid,
    private val references: Vector[TrialBandedReference]) extends ShapeObjective:

  private val n = preparation.trials
  private val m = preparation.basisRank
  private val c = preparation.conditions
  private val f = preparation.nuisanceColumns
  private val k = f + c
  private val d = preparation.basis.family.dimension
  private val comps = JetLayout.components(d)
  private val bandSize = preparation.packedBandSize
  private val responseB = new Array[Double](comps * n)
  private val zTy = new Array[Double](comps * k)
  private val wbJets = new Array[Double](comps * n)
  private val s = new Array[Double](comps)
  private val b = new Array[Double](comps * k)
  private val g = new Array[Double](comps * k * k)
  private val scoreBuilder = DMatBuilder.zeros(n, 1)
  private val readoutBuilder = DMatBuilder.zeros(n, 1)
  private val jetSolveBuilder = DMatBuilder.zeros(n, 1)
  private val scoreRelease = new Array[Double](k)
  private val scoreSolved = new Array[Double](k)
  private val releaseReduction = new ProfileReduction(d, k)
  private val releaseOut = new ProfileJetBuffer(d, k)
  private val kernelScratch = new Array[Double](comps * preparation.basis.fineCount)
  private val coefficients = new Array[Double](comps * m)
  private var response: TrialBandedResponse | Null = null
  val work: TrialBandedWork = new TrialBandedWork

  /** Primitive-array accounting for one prepared bank plus this worker. It
    * excludes VM/object headers and the caller-owned response/output buffers.
    */
  private def referenceDoubles(components: Int): Long =
    components.toLong * m + components.toLong * bandSize + 2L * components * n * k +
      components.toLong * k * k + bandSize + k.toLong * k

  val estimatedReferenceBytes: Long = 8L * referenceDoubles(comps)
  val estimatedValueReferenceBytes: Long = 8L * referenceDoubles(1)
  val estimatedValueBuildBytes: Long = estimatedValueReferenceBytes + 8L * n * (k + c)

  val estimatedSharedBytes: Long =
    preparation.receipt.estimatedBytes + references.length * estimatedReferenceBytes

  val estimatedWorkerBytes: Long =
    val worker = responseB.length.toLong + zTy.length + wbJets.length +
      s.length + b.length + g.length + n.toLong + n.toLong + n.toLong +
      scoreRelease.length + scoreSolved.length + comps.toLong * preparation.basis.fineCount + coefficients.length
    8L * worker

  val estimatedEngineBytes: Long = estimatedSharedBytes + estimatedWorkerBytes

  /** A new mutable worker over the same immutable preparation and reference
    * factors. No Gram block, factor or response-independent jet is rebuilt.
    */
  def newWorker(): TrialBandedObjective = new TrialBandedObjective(preparation, grid, references)

  def amplitudeCount: Int = c

  def pointAt(encoded: TrialBandedResponse): Unit =
    require(encoded.owner eq preparation, "response statistics belong to a different TrialBanded preparation")
    response = encoded
    work.voxels += 1
    work.trialBasisScores += n.toLong * m

  private def currentResponse: TrialBandedResponse =
    if response == null then throw new IllegalStateException("pointAt must be called before scoring")
    response.nn

  def scoreNode(node: Int): Double =
    if node < 0 || node >= references.length then Double.PositiveInfinity
    else
      work.bankValueEvaluations += 1
      scoreReference(references(node), currentResponse, null)

  def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
    if node < 0 || node >= references.length then false
    else
      work.jetEvaluations += 1
      fullJet(references(node), currentResponse, out)

  def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean =
    buildReference(coordinates, comps, None) match
      case Left(_) => false
      case Right(reference) =>
        work.continuousFactors += 1
        work.jetEvaluations += 1
        fullJet(reference, currentResponse, out)

  def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
    buildReference(coordinates, 1, None) match
      case Left(_) =>
        out.energy = Double.PositiveInfinity
        out.curvature = CurvatureStatus.GramNotPositiveDefinite
        Double.PositiveInfinity
      case Right(reference) =>
        work.continuousFactors += 1
        scoreReference(reference, currentResponse, out)

  def logDetAtNode(node: Int): Either[TrialBandedError, Double] =
    if node < 0 || node >= references.length then Left(TrialBandedError.InvalidNode(node, references.length))
    else Right(references(node).logDetK)

  def logDetAt(coordinates: Array[Double]): Either[TrialBandedError, Double] =
    buildReference(coordinates, 1, None).map { reference =>
      work.continuousFactors += 1
      reference.logDetK
    }

  /** Solve the trial ridge system for later conditional readout. Exact-shape
    * factorisation is deliberately named and counted; PHRF-11 owns the final
    * amplitude/query contract.
    */
  def solveReadoutSystem(mode: TrialReadoutFactorMode, rhs: Array[Double]): Either[TrialBandedError, Vector[Double]] =
    if rhs.length != n then Left(TrialBandedError.ReadoutRhs(n, rhs.length))
    else
      val reference =
        mode match
          case TrialReadoutFactorMode.PreparedNode(node) =>
            if node < 0 || node >= references.length then Left(TrialBandedError.InvalidNode(node, references.length))
            else Right(references(node))
          case TrialReadoutFactorMode.ExactShape(coords) =>
            if coords.length != d then Left(TrialBandedError.Factorisation(s"${coords.length} coordinates for dimension $d"))
            else
              buildReference(coords.toArray, 1, None).map { reference =>
                work.exactReadoutFactors += 1
                reference
              }
      reference.flatMap { ref =>
        val builder = DMatBuilder.zeros(n, 1)
        var i = 0
        while i < n do
          builder.writeLinear(i, rhs(i))
          i += 1
        ref.factor.solveInPlace(builder).left.map(error => TrialBandedError.Factorisation(error.getMessage)).map { _ =>
          work.bandedSolveCalls += 1
          work.bandedRightHandSides += 1
          Vector.tabulate(n)(i => builder(i, 0))
        }
      }

  /** Unnormalised conditional trial readout for this backend. The public
    * normalization/query surface remains PHRF-11; this method closes the
    * backend equation and makes exact-per-voxel factor work measurable.
    */
  def readout(mode: TrialReadoutFactorMode): Either[TrialBandedError, TrialBandedReadout] =
    val amplitudes = new Array[Double](n)
    readoutInto(mode, amplitudes).map { energy =>
      TrialBandedReadout(energy, Vector.tabulate(c)(i => scoreSolved(f + i)), amplitudes.toVector, mode)
    }

  /** Conditional readout into a caller-owned trial array. Prepared-node mode
    * reuses all numerical storage; exact-shape mode deliberately constructs
    * and counts one value-only reference factor.
    */
  def readoutInto(mode: TrialReadoutFactorMode, trialAmplitudes: Array[Double]): Either[TrialBandedError, Double] =
    if trialAmplitudes.length != n then Left(TrialBandedError.ReadoutRhs(n, trialAmplitudes.length))
    else
      work.amplitudeCorrections += 1
      val reference =
        mode match
          case TrialReadoutFactorMode.PreparedNode(node) =>
            if node < 0 || node >= references.length then Left(TrialBandedError.InvalidNode(node, references.length))
            else Right(references(node))
          case TrialReadoutFactorMode.ExactShape(coords) =>
            if coords.length != d then Left(TrialBandedError.Factorisation(s"${coords.length} coordinates for dimension $d"))
            else
              buildReference(coords.toArray, 1, None).map { reference =>
                work.exactReadoutFactors += 1
                reference
              }
      reference.flatMap { ref =>
        val energy = scoreReference(ref, currentResponse, null)
        if !energy.isFinite then Left(TrialBandedError.Factorisation("nonfinite profiled energy during readout"))
        else
          var i = 0
          while i < n do
            var value = responseB(i)
            var j = 0
            while j < k do
              value -= ref.cJets(i * k + j) * scoreSolved(j)
              j += 1
            readoutBuilder.writeLinear(i, value)
            i += 1
          ref.factor.solveInPlace(readoutBuilder).left.map(error => TrialBandedError.Factorisation(error.getMessage)).map { _ =>
            work.bandedSolveCalls += 1
            work.bandedRightHandSides += 1
            i = 0
            while i < n do
              trialAmplitudes(i) = scoreSolved(f + preparation.membership.conditionOfTrial(i)) + readoutBuilder(i, 0)
              i += 1
            energy
          }
      }

  private def buildReference(coordinates: Array[Double], activeComponents: Int, node: Option[Int]): Either[TrialBandedError, TrialBandedReference] =
    preparation.basis.coefficientJetInto(ShapePoint.unsafe(coordinates.toVector), kernelScratch, coefficients, activeComponents)
    TrialBandedObjective.reference(preparation, java.util.Arrays.copyOf(coefficients, activeComponents * m), activeComponents, node).map { reference =>
      work.bandedSolveCalls += activeComponents + 1L
      work.bandedRightHandSides += activeComponents.toLong * k + c
      reference
    }

  private def contractResponse(ref: TrialBandedReference, encoded: TrialBandedResponse, activeComponents: Int): Unit =
    java.util.Arrays.fill(responseB, 0.0)
    java.util.Arrays.fill(zTy, 0.0)
    var comp = 0
    while comp < activeComponents do
      var trial = 0
      while trial < n do
        var sum = 0.0
        var p = 0
        while p < m do
          sum += ref.coefficients(comp * m + p) * encoded.trialBasisScores(p * n + trial)
          p += 1
        responseB(comp * n + trial) = sum
        trial += 1
      var nuisanceCol = 0
      while nuisanceCol < f do
        if comp == JetLayout.Value then zTy(nuisanceCol) = encoded.nuisanceScores(nuisanceCol)
        nuisanceCol += 1
      trial = 0
      while trial < n do
        val condition = preparation.membership.conditionOfTrial(trial)
        zTy(comp * k + f + condition) += responseB(comp * n + trial)
        trial += 1
      comp += 1

  private def scoreReference(ref: TrialBandedReference, encoded: TrialBandedResponse, out: ProfileJetBuffer | Null): Double =
    contractResponse(ref, encoded, 1)
    var i = 0
    while i < n do
      scoreBuilder.writeLinear(i, responseB(i))
      i += 1
    ref.factor.solveInPlace(scoreBuilder) match
      case Left(_) => Double.PositiveInfinity
      case Right(_) =>
        work.bandedSolveCalls += 1
        work.bandedRightHandSides += 1
        var ridgeFit = 0.0
        i = 0
        while i < n do
          ridgeFit += responseB(i) * scoreBuilder(i, 0)
          i += 1
        var j = 0
        while j < k do
          var correction = 0.0
          i = 0
          while i < n do
            correction += ref.cJets(i * k + j) * scoreBuilder(i, 0)
            i += 1
          scoreRelease(j) = zTy(j) - correction
          j += 1
        System.arraycopy(scoreRelease, 0, scoreSolved, 0, k)
        SmallCholesky.solveInPlace(k, ref.releaseLower, scoreSolved)
        var releasedFit = 0.0
        j = 0
        while j < k do
          releasedFit += scoreRelease(j) * scoreSolved(j)
          j += 1
        val energy = encoded.responseEnergy - ridgeFit - releasedFit
        if out != null then
          out.nn.energy = energy
          java.util.Arrays.fill(out.nn.gradient, 0.0)
          java.util.Arrays.fill(out.nn.hessian, 0.0)
          j = 0
          while j < c do
            out.nn.amplitudes(j) = scoreSolved(f + j)
            j += 1
          out.nn.curvature = CurvatureStatus.Indefinite
        energy

  private def fullJet(ref: TrialBandedReference, encoded: TrialBandedResponse, out: ProfileJetBuffer): Boolean =
    contractResponse(ref, encoded, comps)
    java.util.Arrays.fill(wbJets, 0.0)
    if !solveResponseComponent(ref, JetLayout.Value, -1, -1) then return false
    var p = 0
    while p < d do
      if !solveResponseComponent(ref, JetLayout.first(p), p, -1) then return false
      p += 1
    p = 0
    while p < d do
      var q = p
      while q < d do
        if !solveResponseComponent(ref, JetLayout.second(d, p, q), p, q) then return false
        q += 1
      p += 1
    java.util.Arrays.fill(s, 0.0)
    java.util.Arrays.fill(b, 0.0)
    System.arraycopy(ref.hJets, 0, g, 0, g.length)
    assembleReducedComponent(ref, encoded, JetLayout.Value, 1,
      JetLayout.Value, JetLayout.Value, 0, 0, 0, 0, 0, 0)
    p = 0
    while p < d do
      assembleReducedComponent(ref, encoded, JetLayout.first(p), 2,
        JetLayout.first(p), JetLayout.Value,
        JetLayout.Value, JetLayout.first(p),
        0, 0, 0, 0)
      p += 1
    p = 0
    while p < d do
      var q = p
      while q < d do
        assembleReducedComponent(ref, encoded, JetLayout.second(d, p, q), 4,
          JetLayout.second(d, p, q), JetLayout.Value,
          JetLayout.first(p), JetLayout.first(q),
          JetLayout.first(q), JetLayout.first(p),
          JetLayout.Value, JetLayout.second(d, p, q))
        q += 1
      p += 1
    if !releaseReduction.reduce(s, b, g, releaseOut) then
      out.energy = Double.PositiveInfinity
      out.curvature = CurvatureStatus.GramNotPositiveDefinite
      false
    else
      out.energy = releaseOut.energy
      System.arraycopy(releaseOut.gradient, 0, out.gradient, 0, d)
      System.arraycopy(releaseOut.hessian, 0, out.hessian, 0, d * d)
      var i = 0
      while i < c do
        out.amplitudes(i) = releaseOut.amplitudes(f + i)
        i += 1
      out.curvature = releaseOut.curvature
      true

  /** Solve the differentiated `A w = b` system for one response RHS. */
  private def solveResponseComponent(ref: TrialBandedReference, comp: Int, axisP: Int, axisQ: Int): Boolean =
    val target = comp * n
    var i = 0
    while i < n do
      var value = responseB(target + i)
      if comp != JetLayout.Value then
        value -= bandVectorProduct(ref.aJets, comp, wbJets, 0, i)
        if axisQ >= 0 then
          value -= bandVectorProduct(ref.aJets, JetLayout.first(axisP), wbJets, JetLayout.first(axisQ) * n, i)
          value -= bandVectorProduct(ref.aJets, JetLayout.first(axisQ), wbJets, JetLayout.first(axisP) * n, i)
      jetSolveBuilder.writeLinear(i, value)
      i += 1
    ref.factor.solveInPlace(jetSolveBuilder) match
      case Left(_) => false
      case Right(_) =>
        work.bandedSolveCalls += 1
        work.bandedRightHandSides += 1
        i = 0
        while i < n do
          wbJets(target + i) = jetSolveBuilder(i, 0)
          i += 1
        true

  private def bandVectorProduct(bands: Array[Double], comp: Int, vector: Array[Double], vectorOffset: Int, row: Int): Double =
    val width = preparation.bandWidth
    val from = math.max(0, row - preparation.bandwidth)
    val until = math.min(n - 1, row + preparation.bandwidth)
    var sum = 0.0
    var j = from
    while j <= until do
      val high = math.max(row, j)
      val delta = math.abs(row - j)
      sum += bands(comp * bandSize + high * width + delta) * vector(vectorOffset + j)
      j += 1
    sum

  /** Assemble one component of `s = y'y - b'A^-1b` and
    * `b_release = Z'y - b'A^-1C`. The product rule has one, two or four
    * terms for value, first and second derivatives respectively.
    */
  private def assembleReducedComponent(
      ref: TrialBandedReference,
      encoded: TrialBandedResponse,
      outComponent: Int,
      termCount: Int,
      b0: Int,
      w0: Int,
      b1: Int,
      w1: Int,
      b2: Int,
      w2: Int,
      b3: Int,
      w3: Int
  ): Unit =
    var quadratic = responseSolutionDot(b0, w0)
    if termCount > 1 then quadratic += responseSolutionDot(b1, w1)
    if termCount > 2 then
      quadratic += responseSolutionDot(b2, w2)
      quadratic += responseSolutionDot(b3, w3)
    s(outComponent) = (if outComponent == JetLayout.Value then encoded.responseEnergy else 0.0) - quadratic
    var col = 0
    while col < k do
      var cross = responseReleaseDot(ref, b0, w0, col)
      if termCount > 1 then cross += responseReleaseDot(ref, b1, w1, col)
      if termCount > 2 then
        cross += responseReleaseDot(ref, b2, w2, col)
        cross += responseReleaseDot(ref, b3, w3, col)
      b(outComponent * k + col) = zTy(outComponent * k + col) - cross
      col += 1

  private def responseSolutionDot(bComponent: Int, wComponent: Int): Double =
    val bBase = bComponent * n
    val wBase = wComponent * n
    var sum = 0.0
    var i = 0
    while i < n do
      sum += responseB(bBase + i) * wbJets(wBase + i)
      i += 1
    sum

  private def responseReleaseDot(ref: TrialBandedReference, bComponent: Int, wComponent: Int, col: Int): Double =
    val bBase = bComponent * n
    val wBase = wComponent * n * k
    var sum = 0.0
    var i = 0
    while i < n do
      sum += responseB(bBase + i) * ref.wcJets(wBase + i * k + col)
      i += 1
    sum

object TrialBandedObjective:

  def make(preparation: TrialBandedPreparation, grid: NodeGrid): Either[TrialBandedError, TrialBandedObjective] =
    val basis = preparation.basis
    if grid.dimension != basis.family.dimension then
      Left(TrialBandedError.Factorisation(s"grid dimension ${grid.dimension} does not match family dimension ${basis.family.dimension}"))
    else
      val comps = basis.family.jetComponents
      val scratch = new Array[Double](comps * basis.fineCount)
      val coefficients = new Array[Double](comps * basis.rank)
      val coords = new Array[Double](grid.dimension)
      val refs = Vector.newBuilder[TrialBandedReference]
      var node = 0
      while node < grid.count do
        grid.coordinatesInto(node, coords)
        basis.coefficientJetInto(ShapePoint.unsafe(coords.toVector), scratch, coefficients, comps)
        reference(preparation, java.util.Arrays.copyOf(coefficients, coefficients.length), comps, Some(node)) match
          case Left(error) => return Left(error)
          case Right(ref) => refs += ref
        node += 1
      Right(new TrialBandedObjective(preparation, grid, refs.result()))

  private def reference(
      prep: TrialBandedPreparation,
      coefficients: Array[Double],
      activeComponents: Int,
      node: Option[Int]): Either[TrialBandedError, TrialBandedReference] =
    val n = prep.trials
    val m = prep.basisRank
    val c = prep.conditions
    val f = prep.nuisanceColumns
    val k = f + c
    val d = prep.basis.family.dimension
    val width = prep.bandWidth
    val bandSize = prep.packedBandSize
    val aJets = new Array[Double](activeComponents * bandSize)
    def addBlock(component: Int, p: Int, q: Int, weight: Double): Unit =
      if weight != 0.0 then
        val block = TrialBandedPreparation.pairIndex(math.min(p, q), math.max(p, q)) * bandSize
        val target = component * bandSize
        var i = 0
        while i < bandSize do
          aJets(target + i) += weight * prep.gramBlocksData(block + i)
          i += 1
    var q = 0
    while q < m do
      var p = 0
      while p <= q do
        val c0p = coefficients(p)
        val c0q = coefficients(q)
        addBlock(JetLayout.Value, p, q, c0p * c0q)
        var axis = 0
        while axis < d && JetLayout.first(axis) < activeComponents do
          val cp = coefficients(JetLayout.first(axis) * m + p)
          val cq = coefficients(JetLayout.first(axis) * m + q)
          addBlock(JetLayout.first(axis), p, q, cp * c0q + c0p * cq)
          axis += 1
        axis = 0
        while axis < d do
          var axisQ = axis
          while axisQ < d do
            val component = JetLayout.second(d, axis, axisQ)
            if component < activeComponents then
              val c2p = coefficients(component * m + p)
              val c2q = coefficients(component * m + q)
              val c1pp = coefficients(JetLayout.first(axis) * m + p)
              val c1pq = coefficients(JetLayout.first(axisQ) * m + p)
              val c1qp = coefficients(JetLayout.first(axis) * m + q)
              val c1qq = coefficients(JetLayout.first(axisQ) * m + q)
              addBlock(component, p, q, c2p * c0q + c1pp * c1qq + c1pq * c1qp + c0p * c2q)
            axisQ += 1
          axis += 1
        p += 1
      q += 1
    var i = 0
    while i < n do
      aJets(i * width) += prep.lambda
      i += 1
    val factorBuilder = DMatBuilder.zeros(n, width)
    i = 0
    while i < bandSize do
      factorBuilder.writeLinear(i, aJets(i))
      i += 1
    factorBuilder.consumeBandedCholesky(CholeskyOptions()) match
      case Left(error) => Left(TrialBandedError.Factorisation(error.getMessage))
      case Right(factor) =>
        val cJets = new Array[Double](activeComponents * n * k)
        val dJets = new Array[Double](activeComponents * k * k)
        var comp = 0
        while comp < activeComponents do
          i = 0
          while i < n do
            var nuisanceCol = 0
            while nuisanceCol < f do
              var sum = 0.0
              var basisIx = 0
              while basisIx < m do
                sum += coefficients(comp * m + basisIx) * prep.basisNuisanceCross((basisIx * n + i) * f + nuisanceCol)
                basisIx += 1
              cJets(comp * n * k + i * k + nuisanceCol) = sum
              nuisanceCol += 1
            var j = math.max(0, i - prep.bandwidth)
            while j <= math.min(n - 1, i + prep.bandwidth) do
              val high = math.max(i, j)
              val delta = math.abs(i - j)
              var value = aJets(comp * bandSize + high * width + delta)
              if comp == JetLayout.Value && i == j then value -= prep.lambda
              val condition = prep.membership.conditionOfTrial(j)
              cJets(comp * n * k + i * k + f + condition) += value
              j += 1
            i += 1
          if comp == JetLayout.Value then
            i = 0
            while i < f do
              var j = 0
              while j < f do
                dJets(i * k + j) = prep.nuisanceGram(i * f + j)
                j += 1
              i += 1
          i = 0
          while i < n do
            val condition = prep.membership.conditionOfTrial(i)
            var nuisanceCol = 0
            while nuisanceCol < f do
              val value = cJets(comp * n * k + i * k + nuisanceCol)
              dJets(comp * k * k + nuisanceCol * k + f + condition) += value
              dJets(comp * k * k + (f + condition) * k + nuisanceCol) += value
              nuisanceCol += 1
            var conditionCol = 0
            while conditionCol < c do
              dJets(comp * k * k + (f + condition) * k + f + conditionCol) +=
                cJets(comp * n * k + i * k + f + conditionCol)
              conditionCol += 1
            i += 1
          comp += 1
        val wcJets = new Array[Double](activeComponents * n * k)
        val solveC = DMatBuilder.zeros(n, k)
        def bandMatrixProduct(component: Int, solutionComponent: Int, row: Int, col: Int): Double =
          val from = math.max(0, row - prep.bandwidth)
          val until = math.min(n - 1, row + prep.bandwidth)
          val solutionBase = solutionComponent * n * k
          var sum = 0.0
          var trial = from
          while trial <= until do
            val high = math.max(row, trial)
            val delta = math.abs(row - trial)
            sum += aJets(component * bandSize + high * width + delta) * wcJets(solutionBase + trial * k + col)
            trial += 1
          sum
        def solveCComponent(component: Int, axisP: Int, axisQ: Int): Either[TrialBandedError, Unit] =
          val target = component * n * k
          var row = 0
          while row < n do
            var col = 0
            while col < k do
              var value = cJets(target + row * k + col)
              if component != JetLayout.Value then
                value -= bandMatrixProduct(component, JetLayout.Value, row, col)
                if axisQ >= 0 then
                  value -= bandMatrixProduct(JetLayout.first(axisP), JetLayout.first(axisQ), row, col)
                  value -= bandMatrixProduct(JetLayout.first(axisQ), JetLayout.first(axisP), row, col)
              solveC.writeLinear(row * k + col, value)
              col += 1
            row += 1
          factor.solveInPlace(solveC).left.map(error => TrialBandedError.Factorisation(error.getMessage)).map { _ =>
            row = 0
            while row < n do
              var col = 0
              while col < k do
                wcJets(target + row * k + col) = solveC(row, col)
                col += 1
              row += 1
          }
        var solveFailure: TrialBandedError | Null = null
        solveCComponent(JetLayout.Value, -1, -1) match
          case Left(error) => solveFailure = error
          case Right(_) => ()
        var axis = 0
        while axis < d && solveFailure == null && JetLayout.first(axis) < activeComponents do
          solveCComponent(JetLayout.first(axis), axis, -1) match
            case Left(error) => solveFailure = error
            case Right(_) => ()
          axis += 1
        axis = 0
        while axis < d && solveFailure == null do
          var axisQ = axis
          while axisQ < d && solveFailure == null do
            val component = JetLayout.second(d, axis, axisQ)
            if component < activeComponents then
              solveCComponent(component, axis, axisQ) match
                case Left(error) => solveFailure = error
                case Right(_) => ()
            axisQ += 1
          axis += 1
        if solveFailure != null then Left(solveFailure.nn)
        else
          val hJets = java.util.Arrays.copyOf(dJets, dJets.length)
          def subtractCross(outComponent: Int, cComponent: Int, wComponent: Int): Unit =
            val outBase = outComponent * k * k
            val cBase = cComponent * n * k
            val wBase = wComponent * n * k
            var row = 0
            while row < k do
              var col = 0
              while col < k do
                var correction = 0.0
                var trial = 0
                while trial < n do
                  correction += cJets(cBase + trial * k + row) * wcJets(wBase + trial * k + col)
                  trial += 1
                hJets(outBase + row * k + col) -= correction
                col += 1
              row += 1
          def symmetrise(component: Int): Unit =
            val base = component * k * k
            var row = 0
            while row < k do
              var col = 0
              while col < row do
                val mean = 0.5 * (hJets(base + row * k + col) + hJets(base + col * k + row))
                hJets(base + row * k + col) = mean
                hJets(base + col * k + row) = mean
                col += 1
              row += 1
          subtractCross(JetLayout.Value, JetLayout.Value, JetLayout.Value)
          symmetrise(JetLayout.Value)
          axis = 0
          while axis < d && JetLayout.first(axis) < activeComponents do
            val first = JetLayout.first(axis)
            subtractCross(first, first, JetLayout.Value)
            subtractCross(first, JetLayout.Value, first)
            symmetrise(first)
            axis += 1
          axis = 0
          while axis < d do
            var axisQ = axis
            while axisQ < d do
              val second = JetLayout.second(d, axis, axisQ)
              if second < activeComponents then
                subtractCross(second, second, JetLayout.Value)
                subtractCross(second, JetLayout.first(axis), JetLayout.first(axisQ))
                subtractCross(second, JetLayout.first(axisQ), JetLayout.first(axis))
                subtractCross(second, JetLayout.Value, second)
                symmetrise(second)
              axisQ += 1
            axis += 1
          val release = java.util.Arrays.copyOf(hJets, k * k)
          denseLower(release, k) match
            case None => Left(TrialBandedError.ReleaseRank(node, k))
            case Some((releaseLower, _)) =>
              constrainedLogDet(prep, factor) match
                case Left(error) => Left(error)
                case Right(logDet) => Right(TrialBandedReference(coefficients, factor, aJets, cJets, wcJets, hJets, releaseLower, logDet))

  private def constrainedLogDet(prep: TrialBandedPreparation, factor: BandedCholesky): Either[TrialBandedError, Double] =
    val n = prep.trials
    val c = prep.conditions
    val mSolve = DMatBuilder.zeros(n, c)
    var i = 0
    while i < n do
      mSolve(i, prep.membership.conditionOfTrial(i)) = 1.0
      i += 1
    factor.solveInPlace(mSolve) match
      case Left(error) => Left(TrialBandedError.Factorisation(error.getMessage))
      case Right(_) =>
        val small = new Array[Double](c * c)
        i = 0
        while i < n do
          val row = prep.membership.conditionOfTrial(i)
          var j = 0
          while j < c do
            small(row * c + j) += mSolve(i, j)
            j += 1
          i += 1
        denseLower(small, c) match
          case None => Left(TrialBandedError.ReleaseRank(None, c))
          case Some((_, smallLogDet)) =>
            val membershipLogDet = (0 until c).map(cond => math.log(prep.membership.trialsOf(cond).length.toDouble)).sum
            Right(factor.logDet - (n - c) * math.log(prep.lambda) + smallLogDet - membershipLogDet)

  /** Gale performs the rank decision; the copied lower triangle feeds the
    * allocation-free small release solve used for every voxel.
    */
  private def denseLower(matrix: Array[Double], size: Int): Option[(Array[Double], Double)] =
    TrialBandedPreparation.toDMat(size, size, matrix).cholesky.toOption.map { factor =>
      val lower = new Array[Double](size * size)
      factor.lower.copyRowMajorTo(lower)
      var logDet = 0.0
      var i = 0
      while i < size do
        logDet += 2.0 * math.log(lower(i * size + i))
        i += 1
      (lower, logDet)
    }
