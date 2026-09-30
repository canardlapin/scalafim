package scalafim.fmri.fit.profile

import scalafim.fmri.hrf.family.{JetLayout, ShapeChartError, ShapePoint}

/** Stage-1 evidence scope. An original-family certificate needs bounds for
  * basis approximation and normal-operator rounding, which this solve lacks.
  */
private[profile] enum TrialConditionalEvidence:
  case PreparedBasisResidual
  case CertifiedOriginalEquations

private[profile] enum TrialConditionalError:
  case InvalidReferenceNode(node: Int, count: Int)
  case InvalidShape(error: ShapeChartError)
  case ForeignResponse
  case CertificateUnavailable
  case ReferenceSolve(error: TrialBandedError)
  case NonFiniteAssembly(stage: String)

  def message: String =
    this match
      case InvalidReferenceNode(node, count) => s"reference node $node is outside 0 until $count"
      case InvalidShape(error) => error.message
      case ForeignResponse => "response statistics belong to another trial preparation"
      case CertificateUnavailable => "an original-family readout certificate is unavailable"
      case ReferenceSolve(error) => error.message
      case NonFiniteAssembly(stage) => s"nonfinite conditional readout at $stage"

/** Actual, attempted work on one successful solve. Banded attempts are also
  * recorded on the worker when a solve fails before this receipt can return.
  */
private[profile] final case class TrialConditionalWorkReceipt(
    referenceInverseAttempts: Long,
    referenceInverseFailures: Long,
    bandedSolveAttempts: Long,
    bandedSolveFailures: Long,
    bandedRightHandSideAttempts: Long,
    factorAttempts: Long,
    continuousFactors: Long,
    exactReadoutFactorAttempts: Long,
    residualCorrections: Int)

/** Coefficients in native amplitude units. The residual is the Euclidean norm
  * of the prepared-basis original normal equations at `actualCoordinates`.
  * It is empirical equation evidence, not an original-family error bound.
  */
private[profile] final case class TrialConditionalResult(
    trialAmplitudes: Vector[Double],
    nuisanceCoefficients: Vector[Double],
    conditionMeans: Vector[Double],
    actualCoordinates: Vector[Double],
    referenceNode: Int,
    preparedBasisResidualNorm: Double,
    work: TrialConditionalWorkReceipt)

/** One mutable worker over an immutable TrialBanded node bank. The normal
  * operator uses only packed trial Gram blocks and basis/nuisance crosses; it
  * never builds or factors a continuous-shape reference.
  */
private[profile] final class TrialConditionalSolve(val worker: TrialBandedObjective):
  private val prep = worker.preparation
  private val n = prep.trials
  private val f = prep.nuisanceColumns
  private val c = prep.conditions
  private val m = prep.basisRank
  private val d = prep.basis.family.dimension
  private val size = n + f
  private val referenceCoordinates = new Array[Double](d)
  private val referenceCoefficients = new Array[Double]((1 + d) * m)
  private val actualCoefficients = new Array[Double](m)
  private val directionalCoefficients = new Array[Double](m)
  private val kernelScratch = new Array[Double](prep.basis.family.jetComponents * prep.basis.fineCount)
  private val rhsReference = new Array[Double](size)
  private val rhsDirectional = new Array[Double](size)
  private val rhsActual = new Array[Double](size)
  private val action = new Array[Double](size)
  private val inverseRhs = new Array[Double](size)
  private val base = new Array[Double](size)
  private val direction = new Array[Double](size)
  private val predictor = new Array[Double](size)
  private val correction = new Array[Double](size)
  private val residual = new Array[Double](size)
  private val conditionCounts = Array.tabulate(c)(i => prep.membership.trialsOf(i).length)
  private val conditionSums = new Array[Double](c)

  def solve(
      encoded: TrialBandedResponse,
      referenceNode: Int,
      actualCoordinates: Vector[Double],
      evidence: TrialConditionalEvidence = TrialConditionalEvidence.PreparedBasisResidual
  ): Either[TrialConditionalError, TrialConditionalResult] =
    val work = worker.work
    work.readoutAttempts += 1L
    def refuse(error: TrialConditionalError): Either[TrialConditionalError, TrialConditionalResult] =
      work.readoutFailures += 1L
      Left(error)

    if !(encoded.owner eq prep) then return refuse(TrialConditionalError.ForeignResponse)
    if referenceNode < 0 || referenceNode >= worker.grid.count then
      return refuse(TrialConditionalError.InvalidReferenceNode(referenceNode, worker.grid.count))
    val actualPoint = prep.basis.family.chart.point(actualCoordinates) match
      case Left(error) => return refuse(TrialConditionalError.InvalidShape(error))
      case Right(point) => point
    if evidence == TrialConditionalEvidence.CertifiedOriginalEquations then
      return refuse(TrialConditionalError.CertificateUnavailable)

    val before = work.snapshot
    worker.grid.coordinatesInto(referenceNode, referenceCoordinates)
    prep.basis.coefficientJetInto(
      ShapePoint.unsafe(referenceCoordinates.toVector), kernelScratch, referenceCoefficients, 1 + d)
    prep.basis.coefficientJetInto(actualPoint, kernelScratch, actualCoefficients, 1)
    if !finite(referenceCoefficients) || !finite(actualCoefficients) then
      return refuse(TrialConditionalError.NonFiniteAssembly("basis coefficients"))
    var p = 0
    while p < m do
      var value = 0.0
      var axis = 0
      while axis < d do
        value += (actualCoordinates(axis) - referenceCoordinates(axis)) *
          referenceCoefficients(JetLayout.first(axis) * m + p)
        axis += 1
      directionalCoefficients(p) = value
      p += 1
    if !finite(directionalCoefficients) then
      return refuse(TrialConditionalError.NonFiniteAssembly("directional coefficients"))

    assembleRhs(referenceCoefficients, encoded, rhsReference, directional = false)
    assembleRhs(directionalCoefficients, encoded, rhsDirectional, directional = true)
    assembleRhs(actualCoefficients, encoded, rhsActual, directional = false)
    if !finite(rhsReference) || !finite(rhsDirectional) || !finite(rhsActual) then
      return refuse(TrialConditionalError.NonFiniteAssembly("response contraction"))

    worker.solveConditionalReference(referenceNode, rhsReference, base) match
      case Left(error) => return refuse(TrialConditionalError.ReferenceSolve(error))
      case Right(_) => ()
    normalAction(referenceCoefficients, directionalCoefficients, base, action, directional = true)
    var i = 0
    while i < size do
      inverseRhs(i) = rhsDirectional(i) - action(i)
      i += 1
    if !finite(inverseRhs) then return refuse(TrialConditionalError.NonFiniteAssembly("directional RHS"))
    worker.solveConditionalReference(referenceNode, inverseRhs, direction) match
      case Left(error) => return refuse(TrialConditionalError.ReferenceSolve(error))
      case Right(_) => ()
    i = 0
    while i < size do
      predictor(i) = base(i) + direction(i)
      i += 1
    normalAction(actualCoefficients, directionalCoefficients, predictor, action, directional = false)
    i = 0
    while i < size do
      inverseRhs(i) = rhsActual(i) - action(i)
      i += 1
    if !finite(inverseRhs) then return refuse(TrialConditionalError.NonFiniteAssembly("correction RHS"))
    worker.solveConditionalReference(referenceNode, inverseRhs, correction) match
      case Left(error) => return refuse(TrialConditionalError.ReferenceSolve(error))
      case Right(_) => ()
    i = 0
    while i < size do
      predictor(i) += correction(i)
      i += 1
    if !finite(predictor) then return refuse(TrialConditionalError.NonFiniteAssembly("corrected coefficients"))

    normalAction(actualCoefficients, directionalCoefficients, predictor, action, directional = false)
    var residualSquared = 0.0
    i = 0
    while i < size do
      residual(i) = rhsActual(i) - action(i)
      residualSquared += residual(i) * residual(i)
      i += 1
    val residualNorm = math.sqrt(residualSquared)
    if !residualNorm.isFinite then return refuse(TrialConditionalError.NonFiniteAssembly("normal residual"))

    java.util.Arrays.fill(conditionSums, 0.0)
    i = 0
    while i < n do
      conditionSums(prep.membership.conditionOfTrial(i)) += predictor(i)
      i += 1
    work.amplitudeCorrections += 1L
    val after = work.snapshot
    val receipt = TrialConditionalWorkReceipt(
      after.attempted.conditionalInverseAttempts - before.attempted.conditionalInverseAttempts,
      after.attempted.conditionalInverseFailures - before.attempted.conditionalInverseFailures,
      after.attempted.solveAttempts - before.attempted.solveAttempts,
      after.attempted.solveFailures - before.attempted.solveFailures,
      after.attempted.rightHandSideAttempts - before.attempted.rightHandSideAttempts,
      after.attempted.factorAttempts - before.attempted.factorAttempts,
      after.continuousFactors - before.continuousFactors,
      after.attempted.exactReadoutFactorAttempts - before.attempted.exactReadoutFactorAttempts,
      residualCorrections = 1)
    Right(TrialConditionalResult(
      Vector.tabulate(n)(predictor(_)),
      Vector.tabulate(f)(j => predictor(n + j)),
      Vector.tabulate(c)(j => conditionSums(j) / conditionCounts(j)),
      actualCoordinates,
      referenceNode,
      residualNorm,
      receipt))

  /** Trial/basis sufficient statistics supply X(theta)'y. F'y is shape fixed. */
  private def assembleRhs(
      coefficients: Array[Double],
      encoded: TrialBandedResponse,
      out: Array[Double],
      directional: Boolean
  ): Unit =
    var trial = 0
    while trial < n do
      var sum = 0.0
      var p = 0
      while p < m do
        sum += coefficients(p) * encoded.trialBasisScores(p * n + trial)
        p += 1
      out(trial) = sum
      trial += 1
    var j = 0
    while j < f do
      out(n + j) = if directional then 0.0 else encoded.nuisanceScores(j)
      j += 1

  /** Apply G(theta) or its first directional shape derivative. The penalty
    * projector is fixed across shapes, so it appears only in the value action.
    */
  private def normalAction(
      coefficients: Array[Double],
      derivative: Array[Double],
      vector: Array[Double],
      out: Array[Double],
      directional: Boolean
  ): Unit =
    java.util.Arrays.fill(out, 0.0)
    val bandSize = prep.packedBandSize
    val width = prep.bandWidth
    var q = 0
    while q < m do
      var p = 0
      while p <= q do
        val weight =
          if directional then derivative(p) * coefficients(q) + coefficients(p) * derivative(q)
          else coefficients(p) * coefficients(q)
        if weight != 0.0 then
          val block = TrialBandedPreparation.pairIndex(p, q) * bandSize
          var i = 0
          while i < n do
            var delta = 0
            while delta <= math.min(i, prep.bandwidth) do
              val j = i - delta
              val value = weight * prep.gramBlocksData(block + i * width + delta)
              out(i) += value * vector(j)
              if i != j then out(j) += value * vector(i)
              delta += 1
            i += 1
        p += 1
      q += 1

    var trial = 0
    while trial < n do
      var nuisance = 0
      while nuisance < f do
        var cross = 0.0
        var basisIx = 0
        while basisIx < m do
          val coefficient = if directional then derivative(basisIx) else coefficients(basisIx)
          cross += coefficient * prep.basisNuisanceCross((basisIx * n + trial) * f + nuisance)
          basisIx += 1
        out(trial) += cross * vector(n + nuisance)
        out(n + nuisance) += cross * vector(trial)
        nuisance += 1
      trial += 1

    if !directional then
      java.util.Arrays.fill(conditionSums, 0.0)
      trial = 0
      while trial < n do
        conditionSums(prep.membership.conditionOfTrial(trial)) += vector(trial)
        trial += 1
      trial = 0
      while trial < n do
        val condition = prep.membership.conditionOfTrial(trial)
        out(trial) += prep.lambda * (vector(trial) - conditionSums(condition) / conditionCounts(condition))
        trial += 1
      var nuisance = 0
      while nuisance < f do
        var other = 0
        while other < f do
          out(n + nuisance) += prep.nuisanceGram(nuisance * f + other) * vector(n + other)
          other += 1
        nuisance += 1

  private def finite(values: Array[Double]): Boolean =
    var i = 0
    while i < values.length do
      if !values(i).isFinite then return false
      i += 1
    true
