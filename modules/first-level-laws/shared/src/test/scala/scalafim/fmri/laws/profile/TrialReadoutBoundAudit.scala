package scalafim.fmri.laws.profile

import gale.linalg.DMat
import gale.spectral.{MatrixEnclosure, RealInterval, ValidatedSvd}
import scalafim.fmri.ar.WhiteningTransform
import scalafim.fmri.design.hrf.{KernelBasisCompilation, TrialDesignLowering}
import scalafim.fmri.fit.profile.*
import scalafim.fmri.hrf.family.NormalizationRule
import scalafim.fmri.model.ProfileHrfSource
import scalafim.scenarios.*

/** Fixed-shape oracle court. The spectral and residual enclosures below apply
  * to the explicitly materialised finite augmented observation array, treating
  * its entries and the response as exact binary numbers. They do NOT enclose
  * family evaluation, convolution, whitening or continuous-time integration.
  * Dense per-shape validation is charged diagnostic work, never a fast-path
  * implementation of CertifiedOriginalEquations.
  */
object TrialReadoutBoundAudit:
  final case class Variant(horizon: Double, maximumRank: Int, subspace: Int = 96)
  final case class PreparationRefusal(variant: Variant, detail: String)
  final case class Record(horizon: Double, maximumRank: Int, shape: Int, mode: ProfileTrialReadoutMode,
      preparedResidual: Double, preparedQrMaxError: Double, originalQrMaxError: Double,
      originalQrError2: Double, finiteNormalResidualUpper: Double,
      finiteCoefficientError2Upper: Double, signedQueryError: Double, signedQueryErrorUpper: Double,
      signedQueryArithmeticUpper: Double,
      finiteSigmaLower: Double, residualGateEmits: Boolean, queryRetainedTrialValues: Int,
      referenceInverseAttempts: Long, exactFactorAttempts: Long)
  final case class Geometry(horizon: Double, maximumRank: Int, subspace: Int, rank: Int, bandwidth: Int,
      preparationBytes: Long, bankAndPreparationBytes: Long,
      searchReferenceBytes: Long, fullReferenceBytes: Long, preparationSeconds: Double, onsets: Vector[Double])
  final case class Result(records: Vector[Record], geometry: Vector[Geometry], preparationRefusals: Vector[PreparationRefusal], scenarios: Vector[ScenarioResult])

  private def interval(x: Double): RealInterval = RealInterval.exact(x).fold(e => throw new IllegalArgumentException(e.toString), identity)
  private def sum(a: RealInterval, b: RealInterval): RealInterval = a.add(b).fold(e => throw new IllegalArgumentException(e.toString), identity)
  private def product(a: RealInterval, b: RealInterval): RealInterval = a.multiply(b).fold(e => throw new IllegalArgumentException(e.toString), identity)
  private def difference(a: RealInterval, b: RealInterval): RealInterval = a.subtract(b).fold(e => throw new IllegalArgumentException(e.toString), identity)

  /** Interval normal residual of this finite augmented least-squares model.
    * No inverse is formed and every matrix contraction is enclosed by Gale.
    */
  private def finiteResidual(augmented: DMat, response: Array[Double], candidate: Vector[Double]): RealInterval =
    val residual = Array.tabulate(augmented.rows): row =>
      var value = interval(response(row))
      var col = 0
      while col < augmented.cols do
        value = difference(value, product(interval(augmented(row, col)), interval(candidate(col))))
        col += 1
      value
    var squared = interval(0.0)
    var col = 0
    while col < augmented.cols do
      var value = interval(0.0)
      var row = 0
      while row < augmented.rows do
        value = sum(value, product(interval(augmented(row, col)), residual(row)))
        row += 1
      squared = sum(squared, value.square.fold(e => throw new IllegalArgumentException(e.toString), identity))
      col += 1
    // Roundoff may put the accumulated lower endpoint just below zero.
    RealInterval.checked(math.max(0.0, squared.lower), squared.upper).flatMap(_.sqrt).fold(e => throw new IllegalArgumentException(e.toString), identity)

  def run(variants: Vector[Variant] = Vector(Variant(48.0, 32), Variant(96.0, 32), Variant(192.0, 32),
      Variant(96.0, 24), Variant(192.0, 15), Variant(192.0, 15, 48)),
      shapes: Vector[Int] = Vector(0, 1, 2, 3), completed: String => Unit = _ => ()): Result =
    require(variants.nonEmpty && variants.forall(v => v.horizon.isFinite && v.horizon >= 48.0 && v.horizon <= 600.0 &&
      v.maximumRank >= 1 && v.maximumRank <= 32 && v.subspace >= v.maximumRank))
    require(shapes.nonEmpty && shapes.forall(i => i >= 0 && i < TrialQualificationAudit.shapes.length))
    val records = Vector.newBuilder[Record]
    val geometries = Vector.newBuilder[Geometry]
    val refusals = Vector.newBuilder[PreparationRefusal]
    variants.foreach: variant =>
      val horizon = variant.horizon
      val started = System.nanoTime()
      val attempted = try Right(DecodedTrialCheckpoint.fixture(DecodedTrialCheckpoint.Config(
        DecodedTrialCheckpoint.Geometry.B0Dense, voxels = 1, trials = 30, blockSize = 1,
        compilation = KernelBasisCompilation.BlockedPartial(variant.subspace),
        trialPreparation = TrialPreparationPolicy(TrialDesignLowering.Blocked(16)), horizonSeconds = Some(horizon),
        basisMaxRank = variant.maximumRank)))
      catch case error: IllegalArgumentException => Left(error.getMessage)
      attempted match
        case Left(error) =>
          refusals += PreparationRefusal(variant, error)
          completed(s"refused horizon=$horizon rank=${variant.maximumRank} subspace=${variant.subspace}: $error")
        case Right(f) =>
          val outputs = f.prepare.flatMap(_.trialOutputs).fold(e => throw new IllegalArgumentException(e.message), identity)
          val axis = outputs.axis
          val prep = axis.preparation
          val grid = NodeGrid(f.plan.basis.family.chart, Vector(2, 2, 2))
          val bank = prep.objective(grid).fold(e => throw new IllegalArgumentException(e.message), identity)
          val onsets = f.plan.source match
            case ProfileHrfSource.TrialEvents(_, drive, _, _) => drive.schedule.onsets.map(_.value)
            case _ => throw new IllegalArgumentException("trial fixture required")
          geometries += Geometry(horizon, variant.maximumRank, variant.subspace, prep.basisRank, prep.bandwidth, prep.receipt.estimatedBytes,
            bank.estimatedSharedBytes, bank.estimatedValueReferenceBytes, bank.estimatedReferenceBytes,
            (System.nanoTime() - started) / 1e9, onsets)
          val nuisance = WhiteningTransform.matrix(f.whitening,
            DMat.tabulate(f.rows, f.nuisance)((t, j) => f.baseline.designMatrix(t, j))).fold(e => throw new IllegalArgumentException(e.toString), identity)
          shapes.foreach: shape =>
            val chart = f.plan.basis.family.chart
            val unit = TrialQualificationAudit.shapes(shape)
            val actual = unit.indices.map(i => chart.lower(i) + unit(i) * chart.width(i)).toVector
            val direct = TrialQualificationAudit.directDesign(f, actual, fullWindow = true)
            val raw = Array.tabulate(f.rows): t =>
              var value = 0.2 + 0.03 * f.baseline.designMatrix(t, 1)
              var j = 0
              while j < f.trials do
                value += direct(t * f.trials + j) * (Vector(1.2, -0.8, 0.4)(j % 3) + 0.25 * math.sin(0.7 * j))
                j += 1
              value
            val current = f.copy(rawBlock = raw)
            val originalOracle = current.oracleDesign(0, direct)
            val preparedOracle = current.oracle(0, actual)
            val original = WhiteningTransform.matrix(f.whitening,
              DMat.tabulate(f.rows, f.trials)((t, j) => direct(t * f.trials + j))).fold(e => throw new IllegalArgumentException(e.toString), identity)
            val whitened = WhiteningTransform.matrix(f.whitening, DMat.tabulate(f.rows, 1)((t, _) => raw(t)))
              .fold(e => throw new IllegalArgumentException(e.toString), identity)
            val augmented = DMat.tabulate(f.rows + f.trials, f.trials + f.nuisance): (t, j) =>
              if t < f.rows then
                if j < f.trials then original(t, j) else nuisance(t, j - f.trials)
              else if j >= f.trials then 0.0
              else (if t - f.rows == j then 1.0 else 0.0) -
                (if (t - f.rows) % 3 == j % 3 then 3.0 / f.trials else 0.0)
            val sigma = (for
              input <- MatrixEnclosure.exact(augmented)
              candidate <- augmented.svd
              bounds <- ValidatedSvd.enclosure(input, candidate)
            yield bounds.lower(augmented.cols - 1)).fold(e => throw new IllegalArgumentException(e.toString), identity)
            require(sigma > 0.0, "finite observation array must have certified full column rank")
            val rhs = Array.tabulate(augmented.rows)(t => if t < f.rows then whitened(t, 0) else 0.0)
            val response = ProfileTrialResponse.make(axis, axis.selectedResponseRows, ProfileTrialResponseDomain.Original, raw)
              .fold(e => throw new IllegalArgumentException(e.message), identity)
            val node = grid.indexOf(unit.map(v => if v > 0.5 then 1 else 0).toArray)
            val request = OutputRequest.TrialAmplitudes(NormalizationRule.Unnormalised)
            val weights = Vector.tabulate(f.trials)(j => if j % 2 == 0 then 1.0 else -1.0)
            val query = ProfileTrialSignedQuery.make("alternating signed sum", axis, weights, 1e-6)
              .fold(e => throw new IllegalArgumentException(e.message), identity)
            Vector(ProfileTrialReadoutMode.ExactShape, ProfileTrialReadoutMode.CorrectedReference).foreach: mode =>
              def frozen(evidence: ProfileTrialEvidenceRequest) = ProfileTrialReadout.freeze(bank, axis, actual, node,
                request.rule, mode, evidence).fold(e => throw new IllegalArgumentException(e.message), identity)
              val operator = frozen(ProfileTrialEvidenceRequest.PreparedBasisResidual)
              val readout = operator.newWorker().evaluate(response, request).fold(e => throw new IllegalArgumentException(e.message), identity)
              val candidate = readout.trialAmplitudes.get ++ readout.nuisanceCoefficients
              val residual = finiteResidual(augmented, rhs, candidate)
              val squaredSigma = interval(sigma).square.fold(e => throw new IllegalArgumentException(e.toString), identity)
              val coefficientBound = interval(residual.upper).divide(squaredSigma).fold(e => throw new IllegalArgumentException(e.toString), identity).upper
              val queryNorm = interval(f.trials.toDouble).sqrt.fold(e => throw new IllegalArgumentException(e.toString), identity)
              val queryResult = operator.newWorker().evaluate(response, OutputRequest.TrialQueries(Vector(query), request.rule))
                .fold(e => throw new IllegalArgumentException(e.message), identity)
              val queryInterval = weights.indices.foldLeft(interval(0.0)): (total, j) =>
                sum(total, product(interval(weights(j)), interval(candidate(j))))
              val queryRoundoff = difference(queryInterval, interval(queryResult.queries.head.value))
              val queryArithmeticUpper = math.max(math.abs(queryRoundoff.lower), math.abs(queryRoundoff.upper))
              val queryBound = sum(product(interval(coefficientBound), queryNorm), interval(queryArithmeticUpper)).upper
              val gated = frozen(ProfileTrialEvidenceRequest.PreparedBasisResidualAtMost(ProfileTrialResidualLimit(1e-8)))
                .newWorker().evaluate(response, request)
              gated.left.foreach:
                case _: ProfileTrialReadoutError.PreparedResidualExceeded => ()
                case error => throw new IllegalArgumentException(error.message)
              records += Record(horizon, variant.maximumRank, shape, mode, readout.evidence.preparedBasisNormalResidualNorm,
                candidate.zip(preparedOracle).map((a, b) => math.abs(a - b)).max,
                candidate.zip(originalOracle).map((a, b) => math.abs(a - b)).max,
                math.sqrt(candidate.zip(originalOracle).map((a, b) => (a - b) * (a - b)).sum),
                residual.upper, coefficientBound,
                math.abs(queryResult.queries.head.value - weights.zip(originalOracle).map((a, b) => a * b).sum), queryBound,
                queryArithmeticUpper,
                sigma, gated.isRight, queryResult.work.retainedTrialAmplitudeValues,
                readout.work.referenceInverseAttempts, readout.work.exactReadoutFactorAttempts)
            completed(s"horizon=$horizon shape=$shape")
    val values = records.result()
    val caveats = Vector(
      ScenarioCaveat("original-family-enclosure", CaveatKind.PublicApiGap, CaveatSeverity.Blocking,
        "fit/profile", Some("PHRF-11"), "finite rounded arrays do not enclose the original observation pipeline"),
      ScenarioCaveat("fixed-shape-exploration", CaveatKind.DiagnosticsGap, CaveatSeverity.Blocking,
        "first-level-laws", Some("PHRF-14"), "fixed-shape exploratory horizon probes are not adaptive scientific qualification"))
    val scenarios = values.map: r =>
      ScenarioResult(s"phrf-finite-bound-${r.horizon}-${r.maximumRank}-${r.shape}-${r.mode}", Vector(
        ScenarioHarness.fact("finite coefficient error enclosed", r.originalQrError2 <= r.finiteCoefficientError2Upper,
          s"observed=${r.originalQrError2}; upper=${r.finiteCoefficientError2Upper}"),
        ScenarioHarness.fact("signed query error enclosed", r.signedQueryError <= r.signedQueryErrorUpper,
          s"observed=${r.signedQueryError}; upper=${r.signedQueryErrorUpper}")), caveats)
    val refused = refusals.result()
    val refusalScenarios = refused.map: r =>
      ScenarioResult(s"phrf-horizon-${r.variant.horizon}-${r.variant.maximumRank}-${r.variant.subspace}", Vector(
        ScenarioHarness.fact("preparation admitted", false, r.detail)), caveats)
    Result(values, geometries.result(), refused, scenarios ++ refusalScenarios)
