package scalafim.fmri.fit.profile

import scalafim.fmri.hrf.family.{JetLayout, ShapeChartError, ShapePoint}

/** Stage-1 evidence scope. An original-family certificate needs bounds for
  * basis approximation and normal-operator rounding, which this solve lacks.
  */
private[profile] enum TrialConditionalEvidence:
  case PreparedBasisResidual
  case CertifiedOriginalEquations

private[profile] enum TrialConditionalMode:
  case CorrectedReference
  case ExactShape

private[profile] enum TrialConditionalError:
  case InvalidReferenceNode(node: Int, count: Int)
  case InvalidShape(error: ShapeChartError)
  case ForeignResponse
  case CertificateUnavailable
  case ReferenceSolve(error: TrialBandedError)
  case NonFiniteAssembly(stage: String)
  case OutputLength(expected: Int, actual: Int)

  def message: String =
    this match
      case InvalidReferenceNode(node, count) => s"reference node $node is outside 0 until $count"
      case InvalidShape(error) => error.message
      case ForeignResponse => "response statistics belong to another trial preparation"
      case CertificateUnavailable => "an original-family readout certificate is unavailable"
      case ReferenceSolve(error) => error.message
      case NonFiniteAssembly(stage) => s"nonfinite conditional readout at $stage"
      case OutputLength(expected, actual) => s"conditional output has length $actual; expected $expected"

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
    residualCorrections: Int,
    normalActionApplications: Long)

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

/** Metadata returned when the coefficient vector stays in caller-owned scratch. */
private[profile] final case class TrialConditionalSummary(
    nuisanceCoefficients: Vector[Double],
    conditionMeans: Vector[Double],
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
  private var normalActionApplications = 0L

  /** The production predictor before its single residual correction. This
    * reads existing worker scratch after a solve, for the cubic-law test.
    */
  private[profile] def lastFirstOrderPredictor: Vector[Double] =
    Vector.tabulate(size)(i => base(i) + direction(i))

  def solve(
      encoded: TrialBandedResponse,
      referenceNode: Int,
      actualCoordinates: Vector[Double],
      evidence: TrialConditionalEvidence = TrialConditionalEvidence.PreparedBasisResidual
  ): Either[TrialConditionalError, TrialConditionalResult] =
    val output = new Array[Double](size)
    solveInto(encoded, referenceNode, actualCoordinates, output, evidence).map: summary =>
      TrialConditionalResult(
        Vector.tabulate(n)(output(_)), summary.nuisanceCoefficients,
        summary.conditionMeans, actualCoordinates, referenceNode,
        summary.preparedBasisResidualNorm, summary.work)

  /** Corrected mode keeps its full trial vector in caller-owned scratch, so a
    * query-only caller need retain only J projected output values. Exact mode
    * is explicit and charges one actual-shape factor rather than falling back.
    */
  def solveInto(
      encoded: TrialBandedResponse,
      referenceNode: Int,
      actualCoordinates: Vector[Double],
      output: Array[Double],
      evidence: TrialConditionalEvidence = TrialConditionalEvidence.PreparedBasisResidual,
      mode: TrialConditionalMode = TrialConditionalMode.CorrectedReference
  ): Either[TrialConditionalError, TrialConditionalSummary] =
    val work = worker.work
    work.conditionalReadoutAttempts += 1L
    def refuse(error: TrialConditionalError): Either[TrialConditionalError, TrialConditionalSummary] =
      work.conditionalReadoutFailures += 1L
      Left(error)
    if output.length != size then return refuse(TrialConditionalError.OutputLength(size, output.length))
    if !(encoded.owner eq prep) then return refuse(TrialConditionalError.ForeignResponse)
    if referenceNode < 0 || referenceNode >= worker.grid.count then
      return refuse(TrialConditionalError.InvalidReferenceNode(referenceNode, worker.grid.count))
    val actualPoint = prep.basis.family.chart.point(actualCoordinates) match
      case Left(error) => return refuse(TrialConditionalError.InvalidShape(error))
      case Right(point) => point
    if evidence == TrialConditionalEvidence.CertifiedOriginalEquations then
      return refuse(TrialConditionalError.CertificateUnavailable)

    val before = work.snapshot
    val beforeNormalActions = normalActionApplications
    prep.basis.coefficientJetInto(actualPoint, kernelScratch, actualCoefficients, 1)
    if !finite(actualCoefficients) then
      return refuse(TrialConditionalError.NonFiniteAssembly("actual basis coefficients"))
    if mode == TrialConditionalMode.ExactShape then
      assembleRhs(actualCoefficients, encoded, rhsActual, directional = false)
      if !finite(rhsActual) then return refuse(TrialConditionalError.NonFiniteAssembly("response contraction"))
      worker.solveConditionalExact(actualCoordinates, rhsActual, predictor) match
        case Left(error) => return refuse(TrialConditionalError.ReferenceSolve(error))
        case Right(_) => return finish(output, before, beforeNormalActions, residualCorrections = 0)

    worker.grid.coordinatesInto(referenceNode, referenceCoordinates)
    prep.basis.coefficientJetInto(
      ShapePoint.unsafe(referenceCoordinates.toVector), kernelScratch, referenceCoefficients, 1 + d)
    if !finite(referenceCoefficients) then
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
    correctAndFinish(output, referenceNode, before, beforeNormalActions)

  private def correctAndFinish(
      output: Array[Double],
      referenceNode: Int,
      before: TrialBandedWorkSnapshot,
      beforeNormalActions: Long
  ): Either[TrialConditionalError, TrialConditionalSummary] =
    val work = worker.work
    def refuse(error: TrialConditionalError): Either[TrialConditionalError, TrialConditionalSummary] =
      work.conditionalCorrectionFailures += 1L
      work.conditionalReadoutFailures += 1L
      Left(error)
    work.conditionalCorrectionAttempts += 1L
    normalAction(actualCoefficients, directionalCoefficients, predictor, action, directional = false)
    var i = 0
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
    finish(output, before, beforeNormalActions, residualCorrections = 1)

  private def finish(
      output: Array[Double],
      before: TrialBandedWorkSnapshot,
      beforeNormalActions: Long,
      residualCorrections: Int
  ): Either[TrialConditionalError, TrialConditionalSummary] =
    val work = worker.work
    def refuse(error: TrialConditionalError): Either[TrialConditionalError, TrialConditionalSummary] =
      work.conditionalReadoutFailures += 1L
      Left(error)
    normalAction(actualCoefficients, directionalCoefficients, predictor, action, directional = false)
    var residualSquared = 0.0
    var i = 0
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
    val completedWork = receipt(before, beforeNormalActions, residualCorrections)
    System.arraycopy(predictor, 0, output, 0, size)
    Right(TrialConditionalSummary(
      Vector.tabulate(f)(j => predictor(n + j)),
      Vector.tabulate(c)(j => conditionSums(j) / conditionCounts(j)),
      residualNorm,
      completedWork))

  private def receipt(
      before: TrialBandedWorkSnapshot,
      beforeNormalActions: Long,
      residualCorrections: Int
  ): TrialConditionalWorkReceipt =
    val after = worker.work.snapshot
    TrialConditionalWorkReceipt(
      after.attempted.conditionalInverseAttempts - before.attempted.conditionalInverseAttempts,
      after.attempted.conditionalInverseFailures - before.attempted.conditionalInverseFailures,
      after.attempted.solveAttempts - before.attempted.solveAttempts,
      after.attempted.solveFailures - before.attempted.solveFailures,
      after.attempted.rightHandSideAttempts - before.attempted.rightHandSideAttempts,
      after.attempted.factorAttempts - before.attempted.factorAttempts,
      after.continuousFactors - before.continuousFactors,
      after.attempted.exactReadoutFactorAttempts - before.attempted.exactReadoutFactorAttempts,
      residualCorrections = residualCorrections,
      normalActionApplications = normalActionApplications - beforeNormalActions)

  /** Reverse the complete frozen corrected composition. `out` addresses the
    * whitened response rows; applying W-transpose for original rows belongs to
    * the AR boundary. The operator as a whole is not self-adjoint.
    */
  def transposeInto(
      referenceNode: Int,
      actualCoordinates: Vector[Double],
      coefficientWeights: Array[Double],
      out: Array[Double],
      mode: TrialConditionalMode = TrialConditionalMode.CorrectedReference
  ): Either[TrialConditionalError, TrialConditionalWorkReceipt] =
    val work = worker.work
    work.conditionalReadoutAttempts += 1L
    def refuse(error: TrialConditionalError): Either[TrialConditionalError, TrialConditionalWorkReceipt] =
      work.conditionalReadoutFailures += 1L
      Left(error)
    def refuseCorrection(error: TrialConditionalError): Either[TrialConditionalError, TrialConditionalWorkReceipt] =
      work.conditionalCorrectionFailures += 1L
      work.conditionalReadoutFailures += 1L
      Left(error)
    if coefficientWeights.length != size then
      return refuse(TrialConditionalError.OutputLength(size, coefficientWeights.length))
    if out.length != prep.rows then
      return refuse(TrialConditionalError.OutputLength(prep.rows, out.length))
    if !finite(coefficientWeights) then
      return refuse(TrialConditionalError.NonFiniteAssembly("transpose coefficient weights"))
    if referenceNode < 0 || referenceNode >= worker.grid.count then
      return refuse(TrialConditionalError.InvalidReferenceNode(referenceNode, worker.grid.count))
    val actualPoint = prep.basis.family.chart.point(actualCoordinates) match
      case Left(error) => return refuse(TrialConditionalError.InvalidShape(error))
      case Right(point) => point
    val before = work.snapshot
    val beforeNormalActions = normalActionApplications
    prep.basis.coefficientJetInto(actualPoint, kernelScratch, actualCoefficients, 1)
    if !finite(actualCoefficients) then
      return refuse(TrialConditionalError.NonFiniteAssembly("actual basis coefficients"))
    if mode == TrialConditionalMode.ExactShape then
      worker.solveConditionalExact(actualCoordinates, coefficientWeights, base) match
        case Left(error) => return refuse(TrialConditionalError.ReferenceSolve(error))
        case Right(_) => ()
      responseTranspose(out, exact = true)
      if !finite(out) then return refuse(TrialConditionalError.NonFiniteAssembly("exact transpose rows"))
      return Right(receipt(before, beforeNormalActions, residualCorrections = 0))

    worker.grid.coordinatesInto(referenceNode, referenceCoordinates)
    prep.basis.coefficientJetInto(
      ShapePoint.unsafe(referenceCoordinates.toVector), kernelScratch, referenceCoefficients, 1 + d)
    if !finite(referenceCoefficients) then
      return refuse(TrialConditionalError.NonFiniteAssembly("reference basis coefficients"))
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
      return refuse(TrialConditionalError.NonFiniteAssembly("directional basis coefficients"))

    // z0 = R q; z1 = R (q - Gtheta z0); z2 = R G1 z1.
    // Btheta' z0 + B0' (z1 - z2) + B1' z1 is the exact transpose.
    worker.solveConditionalReference(referenceNode, coefficientWeights, base) match
      case Left(error) => return refuse(TrialConditionalError.ReferenceSolve(error))
      case Right(_) => ()
    normalAction(actualCoefficients, directionalCoefficients, base, action, directional = false)
    var i = 0
    while i < size do
      inverseRhs(i) = coefficientWeights(i) - action(i)
      i += 1
    if !finite(inverseRhs) then return refuse(TrialConditionalError.NonFiniteAssembly("transpose directional RHS"))
    worker.solveConditionalReference(referenceNode, inverseRhs, direction) match
      case Left(error) => return refuse(TrialConditionalError.ReferenceSolve(error))
      case Right(_) => ()
    normalAction(referenceCoefficients, directionalCoefficients, direction, action, directional = true)
    if !finite(action) then return refuse(TrialConditionalError.NonFiniteAssembly("transpose correction RHS"))
    work.conditionalCorrectionAttempts += 1L
    worker.solveConditionalReference(referenceNode, action, correction) match
      case Left(error) => return refuseCorrection(TrialConditionalError.ReferenceSolve(error))
      case Right(_) => ()
    responseTranspose(out, exact = false)
    if !finite(out) then return refuseCorrection(TrialConditionalError.NonFiniteAssembly("transpose rows"))
    Right(receipt(before, beforeNormalActions, residualCorrections = 1))

  /** Contract the whitened, sparse basis columns and nuisance columns with
    * the three coefficient-space adjoint terms. No dense time-by-trial matrix.
    */
  private def responseTranspose(out: Array[Double], exact: Boolean): Unit =
    java.util.Arrays.fill(out, 0.0)
    var trial = 0
    while trial < n do
      var row = prep.starts(trial)
      while row <= prep.ends(trial) do
        val source = prep.trialOffsets(trial) + (row - prep.starts(trial)) * m
        var value = 0.0
        var p = 0
        while p < m do
          val weight =
            if exact then actualCoefficients(p) * base(trial)
            else actualCoefficients(p) * base(trial) +
              referenceCoefficients(p) * (direction(trial) - correction(trial)) +
              directionalCoefficients(p) * direction(trial)
          value += prep.sparseDesign(source + p) * weight
          p += 1
        out(row) += value
        row += 1
      trial += 1
    var row = 0
    while row < prep.rows do
      var nuisance = 0
      while nuisance < f do
        val weight = if exact then base(n + nuisance)
          else base(n + nuisance) + direction(n + nuisance) - correction(n + nuisance)
        out(row) += prep.whitenedNuisance(row * f + nuisance) * weight
        nuisance += 1
      row += 1

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
    normalActionApplications += 1L
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
