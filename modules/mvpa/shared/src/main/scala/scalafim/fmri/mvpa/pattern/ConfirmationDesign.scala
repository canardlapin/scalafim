package scalafim.fmri.mvpa.pattern

import gale.linalg.DMat
import scalafim.fmri.mvpa.{AxisDigest, AxisRef}
import scalafim.fmri.mvpa.analysis.{EvidenceExposure, ExposureControl, ExposureScope}

/** This admits a fixed-discovery C1 design only. It deliberately returns no
  * statistic, p-value, calibration, or registry result. */
enum ConfirmationDesignError:
  case UnavailableClaim(claim: String, action: String)
  case Invalid(field: String)
  case AxisMismatch(field: String)
  case OverlappingUnits(keys: Vector[String])
  case Exposure(reference: String, detail: String)
  case Nuisance(field: String)
  case ErrorLaw(detail: String)
  case Budget(requiredCells: BigInt, allowedCells: Long)

enum ConfirmationClaim:
  case FixedDiscoveryC1
  case CompleteSelectionAwareC2
  case PopulationMeanG1
  case NewSubjectG2
  case PrevalenceG3

enum TemporalCovarianceStatus:
  case KnownGaussian
  case EstimatedUncalibrated

enum ConfirmationErrorLaw:
  case IndependentGaussian
  case DependentTime(covariance: BoundConfirmationCovariance[?])
  case RepeatedSubjects(bound: BoundRepeatedGaussian[?, ?])

final class BoundConfirmationCovariance[S] private (val rows: AxisRef[S], val covariance: ResidualCovariance[S], val sourceScope: String, val status: TemporalCovarianceStatus, val identity: String)
object BoundConfirmationCovariance:
  def apply[S](rows: AxisRef[S], covariance: ResidualCovariance[S], sourceScope: String, status: TemporalCovarianceStatus): Either[ConfirmationDesignError, BoundConfirmationCovariance[S]] =
    if covariance.neuralAxis.descriptor != rows.descriptor || sourceScope.trim.isEmpty then Left(ConfirmationDesignError.ErrorLaw("temporal covariance must be bound to the actual confirmation rows and source scope"))
    else
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.confirmation-time-covariance.v1"); writer.string(rows.descriptor.coordinateSignature.value); writer.string(sourceScope); writer.string(status.toString)
        writer.intLE(covariance.diagonalValues.size); covariance.diagonalValues.foreach(value => writer.string(java.lang.Double.toHexString(value)))
        val loadings = covariance.loadingsMatrix
        writer.intLE(loadings.rows); writer.intLE(loadings.cols)
        var row = 0
        while row < loadings.rows do
          var column = 0
          while column < loadings.cols do
            writer.string(java.lang.Double.toHexString(loadings(row, column)))
            column += 1
          row += 1
      Right(new BoundConfirmationCovariance(rows, covariance, sourceScope, status, identity))

/** Known-Gaussian repeated-measure capability. It validates covariance block
  * independence; it is neither a CV partition nor a resampling calibration. */
final class BoundRepeatedGaussian[S, U] private (val samples: ConfirmationUnits[S, U], val covariance: BoundConfirmationCovariance[S], val relativeTolerance: Double, val identity: String)
object BoundRepeatedGaussian:
  def apply[S, U](samples: ConfirmationUnits[S, U], covariance: BoundConfirmationCovariance[S], relativeTolerance: Double = 1e-12,
      maximumWorkspaceCells: Long = 10000000L, maximumCrossSubjectPairs: Long = 10000000L): Either[ConfirmationDesignError, BoundRepeatedGaussian[S, U]] =
    val n = samples.rows.size; val rank = covariance.covariance.rank
    val pairs = BigInt(n) * (n - 1) / 2
    val cells = BigInt(n) * rank + n
    if covariance.rows.descriptor != samples.rows.descriptor then Left(ConfirmationDesignError.AxisMismatch("repeated covariance rows"))
    else if covariance.status == TemporalCovarianceStatus.EstimatedUncalibrated then Left(ConfirmationDesignError.UnavailableClaim("repeated Gaussian", "estimated covariance requires separate calibration"))
    else if samples.unitKeys.size < 2 then Left(ConfirmationDesignError.ErrorLaw("repeated Gaussian confirmation requires at least two actual subjects"))
    else if !relativeTolerance.isFinite || relativeTolerance < 0.0 || relativeTolerance > 1e-12 then Left(ConfirmationDesignError.Invalid("repeated covariance numerical tolerance must lie in [0, 1e-12]"))
    else if cells > maximumWorkspaceCells || BigInt(n) * rank > Int.MaxValue || pairs > maximumCrossSubjectPairs then Left(ConfirmationDesignError.Budget(cells.max(pairs), math.min(maximumWorkspaceCells, maximumCrossSubjectPairs)))
    else
      val diagonal = covariance.covariance.diagonalValues
      val loadings = covariance.covariance.loadingsMatrix
      var left = 0
      while left < n do
        var right = left + 1
        while right < n do
          if samples.rowUnitOrdinals(left) != samples.rowUnitOrdinals(right) then
            var cross = 0.0
            var k = 0
            while k < rank do
              // Normalize before multiplication: d_i*d_j can overflow even
              // when the covariance and its whitening are well-scaled.
              cross += (loadings(left, k) / math.sqrt(diagonal(left))) *
                (loadings(right, k) / math.sqrt(diagonal(right)))
              k += 1
            if !cross.isFinite || math.abs(cross) > relativeTolerance then
              return Left(ConfirmationDesignError.ErrorLaw("known Gaussian covariance has nonfinite or nonzero cross-subject dependence"))
          right += 1
        left += 1
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.bound-repeated-gaussian.v1"); writer.string(covariance.identity); writer.string(samples.rows.descriptor.coordinateSignature.value); writer.string(samples.units.descriptor.coordinateSignature.value)
        writer.intLE(samples.rowUnitOrdinals.size); samples.rowUnitOrdinals.foreach(writer.intLE); writer.string(java.lang.Double.toHexString(relativeTolerance))
      Right(new BoundRepeatedGaussian(samples, covariance, relativeTolerance, identity))

final class ConfirmationNuisance[S] private (val rows: AxisRef[S], val matrix: DMat, val plannedCells: Long)
object ConfirmationNuisance:
  def apply[S](rows: AxisRef[S], matrix: DMat, maximumWorkspaceCells: Long = 10000000L): Either[ConfirmationDesignError, ConfirmationNuisance[S]] =
    if matrix.rows != rows.size || matrix.cols <= 0 || !ResidualCovariance.finite(matrix) then Left(ConfirmationDesignError.Nuisance("finite confirmation-bound nuisance matrix"))
    else
      val n = rows.size; val q = matrix.cols; val cells = BigInt(n) * q + BigInt(q) * q + BigInt(2) * q
      if cells > maximumWorkspaceCells || Vector(BigInt(n) * q, BigInt(q) * q).exists(_ > Int.MaxValue) then Left(ConfirmationDesignError.Budget(cells, maximumWorkspaceCells))
      else if n <= q then Left(ConfirmationDesignError.Nuisance("confirmation nuisance residual degrees of freedom"))
      else if !TargetGeometry.fullColumnRank(matrix, 1e-12) then Left(ConfirmationDesignError.Nuisance("nuisance design must be full column rank at Gale tolerance 1e-12"))
      else Right(new ConfirmationNuisance(rows, matrix, cells.toLong))

/** Retained row-to-unit mapping. Unit stable keys must identify the same
  * independent subject or unit globally across discovery and confirmation;
  * callers must not rename a shared unit to manufacture independence.
  * Nominal row descriptors detect exact row reuse, but do not authenticate
  * the physical origin of renamed foreign axes or subsets. */
final class ConfirmationUnits[S, U] private (
    val rows: AxisRef[S], val units: AxisRef[U], val rowUnitOrdinals: Vector[Int]
):
  lazy val unitKeys: Vector[String] = rowUnitOrdinals.distinct.sorted.flatMap(units.index.stableKeyAt(_).toOption)
object ConfirmationUnits:
  def apply[S, U](rows: AxisRef[S], units: AxisRef[U], rowUnitOrdinals: Vector[Int]): Either[ConfirmationDesignError, ConfirmationUnits[S, U]] =
    if rowUnitOrdinals.size != rows.size || rowUnitOrdinals.exists(index => index < 0 || index >= units.size) then Left(ConfirmationDesignError.Invalid("row-to-unit mapping"))
    else Right(new ConfirmationUnits(rows, units, rowUnitOrdinals))

enum ProjectionKind:
  /** Caller-declared linear projection with retained numeric matrix and axes.
    * It does not assert raw, calibrated, or posterior score semantics. */
  case DeclaredLinearProjection
final class FrozenProjection[I, O] private (val input: AxisRef[I], val output: AxisRef[O], val matrix: DMat, val kind: ProjectionKind, val identity: String)
object FrozenProjection:
  def apply[I, O](input: AxisRef[I], output: AxisRef[O], matrix: DMat, kind: ProjectionKind): Either[ConfirmationDesignError, FrozenProjection[I, O]] =
    if matrix.rows != input.size || matrix.cols != output.size || !ResidualCovariance.finite(matrix) then Left(ConfirmationDesignError.Invalid("frozen projection axes or values"))
    else
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.frozen-projection.v1"); writer.string(input.descriptor.coordinateSignature.value); writer.string(output.descriptor.coordinateSignature.value); writer.string(kind.toString)
        writer.intLE(matrix.rows); writer.intLE(matrix.cols)
        var row = 0
        while row < matrix.rows do
          var column = 0
          while column < matrix.cols do
            writer.string(java.lang.Double.toHexString(matrix(row, column)))
            column += 1
          row += 1
      Right(new FrozenProjection(input, output, matrix, kind, identity))

final class DiscoverySnapshot[S, U] private (
    val samples: ConfirmationUnits[S, U], val artifact: PatternArtifact,
    val brainProjection: FrozenProjection[?, ?], val targetProjection: FrozenProjection[?, ?],
    val spatialSupportIdentity: String, val rotationIdentity: String,
    val preprocessingReceipt: String, val identity: String
)
object DiscoverySnapshot:
  def apply[S, U](samples: ConfirmationUnits[S, U], artifact: PatternArtifact,
      brainProjection: FrozenProjection[?, ?], targetProjection: FrozenProjection[?, ?],
      spatialSupportIdentity: String, rotationIdentity: String, preprocessingReceipt: String): Either[ConfirmationDesignError, DiscoverySnapshot[S, U]] =
    if Vector(spatialSupportIdentity, rotationIdentity, preprocessingReceipt).exists(_.trim.isEmpty) then Left(ConfirmationDesignError.Invalid("discovery freeze receipt"))
    else if samples.rows.descriptor != artifact.trainingBinding.declaredSampleAxis then Left(ConfirmationDesignError.AxisMismatch("discovery samples / artifact training binding"))
    else if !brainProjectionEndpoint(brainProjection, artifact) || !targetProjectionEndpoint(targetProjection, artifact) then Left(ConfirmationDesignError.AxisMismatch("frozen projection endpoints or score kind"))
    else
      val f = artifact.factors
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.confirmation-discovery.v1")
        writer.string(samples.rows.descriptor.coordinateSignature.value)
        writer.string(samples.units.descriptor.coordinateSignature.value)
        writer.intLE(samples.rowUnitOrdinals.size); samples.rowUnitOrdinals.foreach(writer.intLE)
        writer.string(spatialSupportIdentity); writer.string(rotationIdentity); writer.string(preprocessingReceipt)
        writer.string(brainProjection.identity); writer.string(targetProjection.identity)
        writer.string(artifact.trainingBinding.fingerprintDigest)
        writer.string(f.neuralAxis.descriptor.coordinateSignature.value); writer.string(f.targetAxis.descriptor.coordinateSignature.value); writer.string(f.componentAxis.descriptor.coordinateSignature.value)
        def matrix(value: DMat): Unit =
          writer.intLE(value.rows); writer.intLE(value.cols)
          var row = 0
          while row < value.rows do
            var column = 0
            while column < value.cols do
              writer.string(java.lang.Double.toHexString(value(row, column)))
              column += 1
            row += 1
        def values(vector: Vector[Double]): Unit =
          writer.intLE(vector.size)
          vector.foreach(value => writer.string(java.lang.Double.toHexString(value)))
        def strings(vector: Vector[String]): Unit =
          writer.intLE(vector.size)
          vector.foreach(writer.string)
        matrix(f.neuralByComponent); matrix(f.targetByComponent)
        f.gauge match
          case GaugeEvidence.PendingNumericalCheck => writer.string("pending-rank")
          case GaugeEvidence.DeclaredSolverDiagnostic(solver, rank) =>
            writer.string("declared-rank"); writer.string(solver); writer.intLE(rank)
          case GaugeEvidence.VerifiedByGaleGramCholesky(tolerance) =>
            writer.string("verified-rank"); writer.string(java.lang.Double.toHexString(tolerance))
        writer.string(f.coordinateGauge.toString)
        strings(artifact.trainingLineage)
        values(artifact.diagnostics.objective)
        writer.string(artifact.diagnostics.solver)
        strings(artifact.diagnostics.notes)
        writer.string(artifact.interpretation.toString)
        artifact.centering match
          case CenteringPolicy.CenteredBeforeFit(neural, target) => writer.string("centered"); writer.string(neural); writer.string(target)
          case CenteringPolicy.ExplicitIntercept(intercept, receipt) =>
            writer.string("intercept"); writer.string(intercept.axis.descriptor.coordinateSignature.value); values(intercept.values); writer.string(receipt)
        writer.string(artifact.degenerateTarget.toString)
        artifact.residualCovariance match
          case ResidualCovarianceCapability.NotFitted => writer.string("not-fitted")
          case ResidualCovarianceCapability.DiagonalPlusLowRank(axis, rank) => writer.string("diagonal-low-rank"); writer.string(axis.coordinateSignature.value); writer.string(rank.toString)
          case ResidualCovarianceCapability.ProviderBacked(axis, name) => writer.string("provider"); writer.string(axis.coordinateSignature.value); writer.string(name)
        writer.string(artifact.trainingBinding.declaredSampleAxis.coordinateSignature.value); writer.string(artifact.trainingBinding.source); writer.string(artifact.trainingBinding.fingerprintDigest)
        artifact.target match
          case TargetGeometry.Categorical(value) =>
            writer.string("categorical"); writer.string(value.conditions.descriptor.coordinateSignature.value)
            matrix(value.contrast); values(value.priors.values)
          case TargetGeometry.Continuous(value) =>
            writer.string("continuous")
            values(value.priorScale.values)
            values(value.metricDiagonal.values)
            writer.intLE(value.blockWeights.size)
            value.blockWeights.foreach: (name, weights) =>
              writer.string(name); values(weights.values)
      Right(new DiscoverySnapshot(samples, artifact, brainProjection, targetProjection, spatialSupportIdentity, rotationIdentity, preprocessingReceipt, identity))

  private def brainProjectionEndpoint(value: FrozenProjection[?, ?], artifact: PatternArtifact): Boolean =
    value.kind == ProjectionKind.DeclaredLinearProjection && value.input.descriptor == artifact.factors.neuralAxis.descriptor && value.output.descriptor == artifact.factors.componentAxis.descriptor
  private def targetProjectionEndpoint(value: FrozenProjection[?, ?], artifact: PatternArtifact): Boolean =
    value.kind == ProjectionKind.DeclaredLinearProjection && value.input.descriptor == artifact.factors.targetAxis.descriptor && value.output.descriptor == artifact.factors.componentAxis.descriptor

final class ConfirmationSnapshot[S, U] private (
    val samples: ConfirmationUnits[S, U], val preprocessingReceipt: String, val identity: String
)
object ConfirmationSnapshot:
  def apply[S, U](samples: ConfirmationUnits[S, U], preprocessingReceipt: String): Either[ConfirmationDesignError, ConfirmationSnapshot[S, U]] =
    if preprocessingReceipt.trim.isEmpty then Left(ConfirmationDesignError.Invalid("confirmation preprocessing receipt"))
    else
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.confirmation-snapshot.v1")
        writer.string(samples.rows.descriptor.coordinateSignature.value); writer.string(samples.units.descriptor.coordinateSignature.value)
        writer.intLE(samples.rowUnitOrdinals.size); samples.rowUnitOrdinals.foreach(writer.intLE)
        writer.string(preprocessingReceipt)
      Right(new ConfirmationSnapshot(samples, preprocessingReceipt, identity))

final case class C1Contract(
    conditioning: String, nullHypothesis: String, estimandAndGeneralization: String,
    multiplicityFamily: Vector[String], approximation: String, frozenStages: Vector[String], confirmationStages: Vector[String]
):
  require(Vector(conditioning, nullHypothesis, estimandAndGeneralization, approximation).forall(_.trim.nonEmpty))
  require(multiplicityFamily.nonEmpty && multiplicityFamily.forall(_.trim.nonEmpty))
  require(frozenStages.nonEmpty && confirmationStages.nonEmpty && frozenStages.forall(_.trim.nonEmpty) && confirmationStages.forall(_.trim.nonEmpty))

final class ConfirmationDesign[S, U] private[pattern] (
    val discovery: DiscoverySnapshot[?, ?], val confirmation: ConfirmationSnapshot[S, U],
    val contract: C1Contract, val nuisance: ConfirmationNuisance[S], val errorLaw: ConfirmationErrorLaw,
    val identity: String
)

object ConfirmationDesign:
  def admit[S, U](claim: ConfirmationClaim, discovery: DiscoverySnapshot[?, ?], confirmation: ConfirmationSnapshot[S, U],
      exposure: EvidenceExposure, contract: C1Contract, nuisance: ConfirmationNuisance[S], errorLaw: ConfirmationErrorLaw): Either[ConfirmationDesignError, ConfirmationDesign[S, U]] =
    claim match
      case ConfirmationClaim.CompleteSelectionAwareC2 => Left(ConfirmationDesignError.UnavailableClaim("C2", "repeat every selection-dependent stage in each calibrated replicate"))
      case ConfirmationClaim.PopulationMeanG1 => Left(ConfirmationDesignError.UnavailableClaim("G1", "supply subject-bound effects and a qualified group model"))
      case ConfirmationClaim.NewSubjectG2 => Left(ConfirmationDesignError.UnavailableClaim("G2", "supply held-out subjects and a subject-generalization estimand"))
      case ConfirmationClaim.PrevalenceG3 => Left(ConfirmationDesignError.UnavailableClaim("G3", "supply a calibrated prevalence procedure"))
      case ConfirmationClaim.FixedDiscoveryC1 =>
        val overlap = discovery.samples.unitKeys.toSet.intersect(confirmation.samples.unitKeys.toSet).toVector.sorted
        // Local ordinal keys across foreign nominal domains do not identify
        // shared rows. Actual independent-unit IDs remain separately checked.
        val rowOverlap = discovery.samples.rows.descriptor == confirmation.samples.rows.descriptor
        if contract.multiplicityFamily.distinct.size != contract.multiplicityFamily.size then Left(ConfirmationDesignError.Invalid("duplicate multiplicity family member"))
        else if overlap.nonEmpty || rowOverlap then Left(ConfirmationDesignError.OverlappingUnits(overlap ++ (if rowOverlap then Vector("discovery/confirmation rows") else Vector.empty)))
        else if exposure.reference.evidenceIdentity != confirmation.identity then Left(ConfirmationDesignError.Exposure("reference", "exposure evidence identity does not match the actual confirmation snapshot"))
        else if nuisance.rows.descriptor != confirmation.samples.rows.descriptor then Left(ConfirmationDesignError.AxisMismatch("nuisance confirmation rows"))
        else if exposure.events.exists(_.request.scope.canOverlap(ExposureScope.Holdout)) then Left(ConfirmationDesignError.Exposure("confirmation", "an existing confirmation score, timing, plot, payload, or selection exposure blocks the original path"))
        else ExposureControl.untouchedConfirmation(exposure, ExposureScope.Holdout).left.map(value => ConfirmationDesignError.Exposure("confirmation", value.toString)).flatMap: _ =>
          errorLaw match
            case ConfirmationErrorLaw.IndependentGaussian if confirmation.samples.rowUnitOrdinals.distinct.size != confirmation.samples.rows.size => Left(ConfirmationDesignError.ErrorLaw("independent Gaussian confirmation requires exactly one row per actual unit"))
            case ConfirmationErrorLaw.IndependentGaussian => build(discovery, confirmation, contract, nuisance, errorLaw)
            case ConfirmationErrorLaw.DependentTime(bound) if bound.rows.descriptor != confirmation.samples.rows.descriptor => Left(ConfirmationDesignError.ErrorLaw("bound temporal covariance does not name the confirmation rows"))
            case ConfirmationErrorLaw.DependentTime(bound) if bound.status == TemporalCovarianceStatus.EstimatedUncalibrated => Left(ConfirmationDesignError.UnavailableClaim("dependent-time", "estimated temporal covariance requires a separately calibrated reference"))
            case ConfirmationErrorLaw.DependentTime(_) => build(discovery, confirmation, contract, nuisance, errorLaw)
            case ConfirmationErrorLaw.RepeatedSubjects(bound) if bound.samples.rows.descriptor != confirmation.samples.rows.descriptor || bound.samples.units.descriptor != confirmation.samples.units.descriptor || bound.samples.rowUnitOrdinals != confirmation.samples.rowUnitOrdinals => Left(ConfirmationDesignError.ErrorLaw("bound repeated Gaussian units or mapping do not match confirmation"))
            case ConfirmationErrorLaw.RepeatedSubjects(_) => build(discovery, confirmation, contract, nuisance, errorLaw)

  private def build[S, U](discovery: DiscoverySnapshot[?, ?], confirmation: ConfirmationSnapshot[S, U], contract: C1Contract,
      nuisance: ConfirmationNuisance[S], errorLaw: ConfirmationErrorLaw): Either[ConfirmationDesignError, ConfirmationDesign[S, U]] =
    val identity = AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.confirmation-design.c1.v1")
      writer.string(discovery.identity)
      writer.string(confirmation.identity)
      writer.string(contract.conditioning)
      writer.string(contract.nullHypothesis)
      writer.string(contract.estimandAndGeneralization)
      writer.string(contract.approximation)
      def strings(values: Vector[String]): Unit =
        writer.intLE(values.size)
        values.foreach(writer.string)
      strings(contract.multiplicityFamily)
      strings(contract.frozenStages)
      strings(contract.confirmationStages)
      errorLaw match
        case ConfirmationErrorLaw.IndependentGaussian => writer.string("independent-gaussian")
        case ConfirmationErrorLaw.DependentTime(bound) => writer.string("dependent-time"); writer.string(bound.identity); writer.string(bound.status.toString)
        case ConfirmationErrorLaw.RepeatedSubjects(bound) => writer.string("repeated-subjects"); writer.string(bound.identity)
      writer.intLE(nuisance.matrix.rows)
      writer.intLE(nuisance.matrix.cols)
      var row = 0
      while row < nuisance.matrix.rows do
        var column = 0
        while column < nuisance.matrix.cols do
          writer.string(java.lang.Double.toHexString(nuisance.matrix(row, column)))
          column += 1
        row += 1
    Right(new ConfirmationDesign(discovery, confirmation, contract, nuisance, errorLaw, identity))
