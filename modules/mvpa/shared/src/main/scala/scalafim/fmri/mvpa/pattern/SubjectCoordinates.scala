package scalafim.fmri.mvpa.pattern

import gale.backend.Backend.given
import gale.linalg.DMat
import gale.spectral.{Eigen, EigenSelection, EigenVectors}
import gale.spectral.SpectralBackend.given
import multivar.core.{SemanticSpace, ValueIdentity}
import scalafim.fmri.mvpa.{AxisDescriptor, AxisDigest, AxisRef, Column, ColumnIdentity, EvidenceIdentity}
import scalafim.fmri.mvpa.analysis.{EvidenceExposure, ExposureScope}
import scalafim.fmri.mvpa.measurement.{MeasurementDescriptor, MeasurementLeg}

opaque type SubjectCoordinateKey = String
object SubjectCoordinateKey:
  def apply(value: String): Either[SubjectCoordinateError, SubjectCoordinateKey] =
    if value.trim.isEmpty || value != value.trim || value.exists(_.isControl) then Left(SubjectCoordinateError.Invalid("subject key"))
    else Right(value)
  extension (key: SubjectCoordinateKey) def value: String = key

enum SubjectCoordinateError:
  case Invalid(detail: String)
  case AxisMismatch(detail: String)
  case Independence(detail: String)
  case Unavailable(detail: String)
  case Budget(cells: BigInt, allowedCells: Long, work: BigInt, allowedWork: Long)
  case Condition(number: Double, limit: Double)
  case Numerical(detail: String)

  def message: String = this match
    case Invalid(detail) => s"invalid subject coordinates: $detail"
    case AxisMismatch(detail) => s"subject coordinate binding mismatch: $detail"
    case Independence(detail) => s"shared discovery is not separated: $detail"
    case Unavailable(detail) => s"subject comparison unavailable: $detail"
    case Budget(cells, allowedCells, work, allowedWork) => s"subject transport requires $cells cells/$work products; limits $allowedCells/$allowedWork"
    case Condition(number, limit) => s"task coordinate condition $number exceeds $limit"
    case Numerical(detail) => s"subject coordinate numerical failure: $detail"

/** This is df provenance, not a new reference distribution or group admission. */
enum SubjectCoordinateDf:
  case Known(value: Double, method: String)
  case Estimated(value: Double, method: String)
  case Approximate(value: Double, method: String)
  case Unknown(reason: String)
  case NotApplicable(reason: String)

enum SubjectCoordinateDfRole:
  case Residual, Effective, Reference, Unspecified

/** Covariance origin is immutable source metadata, independently of df.
  * Knowing n-r residual df never makes an estimated variance known. */
enum SubjectCovarianceOrigin:
  case Known(method: String)
  case Estimated(method: String)
  case Approximate(method: String)
  case Unknown(reason: String)

  private[pattern] def valid: Boolean = this match
    case Known(method) => method.trim.nonEmpty
    case Estimated(method) => method.trim.nonEmpty
    case Approximate(method) => method.trim.nonEmpty
    case Unknown(reason) => reason.trim.nonEmpty

/** Stability is an explicitly declared discovery diagnostic, not inferred from
  * the apparent agreement of confirmation maps. */
enum SubjectStabilityKind:
  case StableAxes, StableSubspace, UnstableSolution

final class SubjectAxisStability private (
    val kind: SubjectStabilityKind, val discoveryIdentity: String,
    val exposure: EvidenceExposure, val receipt: String, val identity: String
)

object SubjectAxisStability:
  def freeze(discovery: DiscoverySnapshot[?, ?], exposure: EvidenceExposure,
      kind: SubjectStabilityKind, receipt: String): Either[SubjectCoordinateError, SubjectAxisStability] =
    if exposure.reference.evidenceIdentity != discovery.identity then Left(SubjectCoordinateError.AxisMismatch("stability discovery exposure"))
    else if exposure.initialScope != ExposureScope.Training || exposure.events.exists(_.request.scope.canOverlap(ExposureScope.Holdout)) then
      Left(SubjectCoordinateError.Independence("stability must be assessed in discovery"))
    else if receipt.trim.isEmpty then Left(SubjectCoordinateError.Invalid("discovery stability receipt"))
    else
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.subject-axis-stability.v1")
        writer.string(discovery.identity); writer.string(exposure.identity.text); writer.string(kind.toString); writer.string(receipt)
      Right(new SubjectAxisStability(kind, discovery.identity, exposure, receipt, identity))

enum SubjectComparisonKind:
  case SharedComponentCoefficients
  case TaskLinkedForwardOperator

/** Physical numeric value units are supplied separately from axis coordinate
  * units (for example anatomical mm). The operator convention names the
  * original target-value denominator explicitly; this boundary performs no
  * implicit unit conversion. */
final class SubjectCoordinateValueUnits private (
    val coefficients: String, val taskOperator: String, val receipt: String
):
  val coefficientCovariance: String = s"($coefficients)^2"
  val taskOperatorCovariance: String = s"($taskOperator)^2"
  private[pattern] def writeFramed(writer: AxisDigest.Writer): Unit =
    Vector(coefficients, coefficientCovariance, taskOperator, taskOperatorCovariance, receipt).foreach(writer.string)

object SubjectCoordinateValueUnits:
  def apply(coefficients: String, taskOperator: String, receipt: String): Either[SubjectCoordinateError, SubjectCoordinateValueUnits] =
    if Vector(coefficients, taskOperator, receipt).exists(_.trim.isEmpty) then Left(SubjectCoordinateError.Invalid("physical value unit labels/receipt"))
    else Right(new SubjectCoordinateValueUnits(coefficients, taskOperator, receipt))

final case class SubjectCoordinatePolicy(
    maximumOwnedCells: Long = 1_000_000L,
    /** Bounds the planned domain matrix products (two scalar operations per
      * multiply/accumulate). Gale spectral/factorization work and arbitrary
      * measurement-provider work are excluded, not falsely bounded by n^3. */
    maximumScalarProducts: Long = 1_000_000_000L,
    maximumCondition: Double = 1e8,
    relativeProjectionTolerance: Double = 1e-10,
    relativeCovarianceTolerance: Double = 1e-12
):
  require(maximumOwnedCells >= 0L && maximumScalarProducts >= 0L)
  require(maximumCondition.isFinite && maximumCondition >= 1.0)
  require(relativeProjectionTolerance.isFinite && relativeProjectionTolerance > 0.0 && relativeProjectionTolerance <= 1e-6)
  require(relativeCovarianceTolerance.isFinite && relativeCovarianceTolerance > 0.0 && relativeCovarianceTolerance <= 1e-6)

/** Shared task coordinates come from an identified discovery snapshot. The
  * account records declared exposure; it cannot authenticate renamed foreign
  * rows or omitted external access. No confirmation payload is accepted here. */
final class SharedTaskCoordinates private (
    val discovery: DiscoverySnapshot[?, ?], val exposure: EvidenceExposure, val featureAxis: AxisDescriptor,
    val stability: SubjectAxisStability, val valueUnits: SubjectCoordinateValueUnits, val identity: String
):
  val taskAxis: AxisDescriptor = discovery.targetProjection.input.descriptor
  val componentAxis: AxisDescriptor = discovery.targetProjection.output.descriptor

object SharedTaskCoordinates:
  def freeze(discovery: DiscoverySnapshot[?, ?], exposure: EvidenceExposure, commonFeatures: AxisRef[?],
      stability: SubjectAxisStability, valueUnits: SubjectCoordinateValueUnits): Either[SubjectCoordinateError, SharedTaskCoordinates] =
    if exposure.reference.evidenceIdentity != discovery.identity then Left(SubjectCoordinateError.AxisMismatch("shared discovery exposure account"))
    else if exposure.initialScope != ExposureScope.Training || exposure.events.exists(_.request.scope.canOverlap(ExposureScope.Holdout)) then
      Left(SubjectCoordinateError.Independence("shared coordinates/alignment must be selected only in discovery"))
    else if stability.discoveryIdentity != discovery.identity then Left(SubjectCoordinateError.AxisMismatch("shared discovery stability receipt"))
    else
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.shared-task-coordinates.v1")
        writer.string(discovery.identity)
        writer.string(exposure.identity.text)
        writer.string(commonFeatures.descriptor.stableKey)
        writer.string(stability.identity)
        valueUnits.writeFramed(writer)
      Right(new SharedTaskCoordinates(discovery, exposure, commonFeatures.descriptor, stability, valueUnits, identity))

/** Freeze the actual admitted spatial map with discovery-only selection
  * evidence. An operator fingerprint alone proves neither independent selection
  * nor that an analyst has not optimized a map against confirmation values. */
final class SubjectSpatialAlignment[N <: SemanticSpace, K, L <: SemanticSpace] private (
    val measurement: MeasurementLeg[N, K, L], val discoveryIdentity: String,
    val sharedIdentity: String, val exposure: EvidenceExposure, val identity: String
)

object SubjectSpatialAlignment:
  def freeze[N <: SemanticSpace, K, L <: SemanticSpace](
      discovery: DiscoverySnapshot[?, ?], shared: SharedTaskCoordinates,
      measurement: MeasurementLeg[N, K, L], exposure: EvidenceExposure
  ): Either[SubjectCoordinateError, SubjectSpatialAlignment[N, K, L]] =
    if exposure.reference.evidenceIdentity != discovery.identity || exposure.reference.result.text != measurement.descriptor.semanticId then
      Left(SubjectCoordinateError.AxisMismatch("spatial selection account must bind subject discovery and the actual measurement semantic id"))
    else if exposure.initialScope != ExposureScope.Training || exposure.events.exists(_.request.scope.canOverlap(ExposureScope.Holdout)) then
      Left(SubjectCoordinateError.Independence("spatial alignment must be selected before confirmation, in discovery"))
    else if measurement.source.descriptor != discovery.brainProjection.input.descriptor || measurement.local.descriptor != shared.featureAxis then
      Left(SubjectCoordinateError.AxisMismatch("spatial selection endpoints"))
    else
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.subject-spatial-alignment.v1")
        writer.string(discovery.identity); writer.string(shared.identity)
        writer.string(measurement.descriptor.semanticId); writer.string(exposure.identity.text)
      Right(new SubjectSpatialAlignment(measurement, discovery.identity, shared.identity, exposure, identity))

/** Coefficients in X = Y C B^T, with feature-major/component-minor vec(B)
  * covariance. A scalar SE map is insufficient. Cross-feature covariances are
  * explicit even for independently estimated voxels; zeros require a source
  * contract, never an implicit diagonal approximation.
  *
  * The row-bound subject column must name one actual declared subject. Source,
  * value and covariance declarations are retained, not authenticated. M5.02
  * owns adapters from qualified confirmation procedures into this boundary. */
final class SubjectCoefficientEstimate private (
    val subject: SubjectCoordinateKey, val subjectColumn: ColumnIdentity,
    val featureAxis: AxisDescriptor, val componentAxis: AxisDescriptor,
    val design: ConfirmationDesign[?, ?], val coefficients: DMat, val covariance: DMat,
    val brainEvidence: EvidenceIdentity, val targetEvidence: EvidenceIdentity,
    val coefficientSource: ValueIdentity, val covarianceSource: ValueIdentity,
    val degreesOfFreedom: SubjectCoordinateDf, val uncertaintyReceipt: String,
    val covarianceOrigin: SubjectCovarianceOrigin,
    val degreesOfFreedomRole: SubjectCoordinateDfRole,
    val stability: SubjectAxisStability, val valueUnits: SubjectCoordinateValueUnits, val identity: String
)

object SubjectCoefficientEstimate:
  def bind[K](
      subjects: Column[?, SubjectCoordinateKey], features: AxisRef[K], design: ConfirmationDesign[?, ?],
      coefficients: DMat, fullJointCovariance: DMat,
      brainEvidence: EvidenceIdentity, targetEvidence: EvidenceIdentity,
      coefficientSource: ValueIdentity, covarianceSource: ValueIdentity,
      degreesOfFreedom: SubjectCoordinateDf, uncertaintyReceipt: String, stability: SubjectAxisStability,
      valueUnits: SubjectCoordinateValueUnits, covarianceOrigin: SubjectCovarianceOrigin, degreesOfFreedomRole: SubjectCoordinateDfRole,
      policy: SubjectCoordinatePolicy = SubjectCoordinatePolicy()
  ): Either[SubjectCoordinateError, SubjectCoefficientEstimate] =
    val components = design.discovery.targetProjection.output.descriptor
    val n = BigInt(features.size) * components.size
    val cells = 8 * n * n + 2 * n
    val work = BigInt(0) // PSD validation is a Gale capability; no domain matrix product here.
    val rows = design.confirmation.samples.rows.descriptor
    for
      _ <- if subjects.rowAxis == rows && subjects.values.nonEmpty && subjects.values.distinct.size == 1 then Right(())
        else Left(SubjectCoordinateError.AxisMismatch("confirmation row-bound subjects must identify exactly one subject"))
      _ <- if brainEvidence.rows == rows && targetEvidence.rows == rows && brainEvidence.columns == features.descriptor &&
          targetEvidence.columns == design.discovery.targetProjection.input.descriptor then Right(())
        else Left(SubjectCoordinateError.AxisMismatch("confirmation brain/target evidence rows, features or ordered task keys"))
      _ <- if stability.discoveryIdentity == design.discovery.identity then Right(()) else Left(SubjectCoordinateError.AxisMismatch("subject stability discovery"))
      _ <- if uncertaintyReceipt.trim.nonEmpty && validDf(degreesOfFreedom) && covarianceOrigin.valid then Right(())
        else Left(SubjectCoordinateError.Invalid("uncertainty, df or discovery stability receipt"))
      _ <- if !degreesOfFreedom.isInstanceOf[SubjectCoordinateDf.Approximate] || degreesOfFreedomRole == SubjectCoordinateDfRole.Effective then Right(())
        else Left(SubjectCoordinateError.Invalid("approximate df must be identified as effective"))
      _ <- if coefficients.rows == features.size && coefficients.cols == components.size &&
          fullJointCovariance.rows == n && fullJointCovariance.cols == n && n > 0 then Right(())
        else Left(SubjectCoordinateError.Invalid("coefficient/full joint covariance dimensions"))
      _ <- SubjectCoordinates.budget(cells, work, Vector(n * n, n), policy)
      _ <- if ResidualCovariance.finite(coefficients) then Right(()) else Left(SubjectCoordinateError.Numerical("nonfinite coefficients"))
      _ <- SubjectCoordinates.covariance(fullJointCovariance, policy)
    yield
      val mean = DMat.tabulate(coefficients.rows, coefficients.cols)(coefficients.apply)
      val joint = DMat.tabulate(fullJointCovariance.rows, fullJointCovariance.cols)(fullJointCovariance.apply)
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.subject-coefficient-estimate.v2")
        writer.string(subjects.values.head.value)
        writer.string(subjects.identity.toString)
        writer.string(design.identity)
        brainEvidence.writeFramed(writer); targetEvidence.writeFramed(writer)
        writer.string(coefficientSource.toString); writer.string(covarianceSource.toString)
        writer.string(degreesOfFreedom.toString); writer.string(uncertaintyReceipt); writer.string(stability.identity)
        writer.string(covarianceOrigin.toString)
        writer.string(degreesOfFreedomRole.toString)
        valueUnits.writeFramed(writer)
        SubjectCoordinates.writeMatrix(writer, mean); SubjectCoordinates.writeMatrix(writer, joint)
      new SubjectCoefficientEstimate(subjects.values.head, subjects.identity, features.descriptor, components, design, mean, joint,
        brainEvidence, targetEvidence, coefficientSource, covarianceSource, degreesOfFreedom, uncertaintyReceipt, covarianceOrigin, degreesOfFreedomRole, stability, valueUnits, identity)

  private def validDf(value: SubjectCoordinateDf): Boolean = value match
    case SubjectCoordinateDf.Known(df, method) => df.isFinite && df > 0.0 && method.trim.nonEmpty
    case SubjectCoordinateDf.Estimated(df, method) => df.isFinite && df > 0.0 && method.trim.nonEmpty
    case SubjectCoordinateDf.Approximate(df, method) => df.isFinite && df > 0.0 && method.trim.nonEmpty
    case SubjectCoordinateDf.Unknown(reason) => reason.trim.nonEmpty
    case SubjectCoordinateDf.NotApplicable(reason) => reason.trim.nonEmpty

/** Arithmetic transport only. This neither estimates a population effect nor
  * changes calibration status, df, subject differences or the original fit. */
final class SubjectCoordinates private (
    val source: SubjectCoefficientEstimate, val shared: SharedTaskCoordinates,
    val measurement: MeasurementDescriptor, val kind: SubjectComparisonKind,
    val spatialAlignmentIdentity: String,
    val featureAxis: AxisDescriptor, val taskAxis: AxisDescriptor,
    val estimates: DMat, val covariance: DMat,
    val taskCoefficientTransform: DMat, val coordinateCondition: Option[Double],
    val relativeProjectionResidual: Option[Double],
    val plannedOwnedCells: Long, val plannedScalarProducts: Long,
    val policy: SubjectCoordinatePolicy, val identity: String
):
  val subject: SubjectCoordinateKey = source.subject
  val degreesOfFreedom: SubjectCoordinateDf = source.degreesOfFreedom
  val covarianceOrigin: SubjectCovarianceOrigin = source.covarianceOrigin
  val degreesOfFreedomRole: SubjectCoordinateDfRole = source.degreesOfFreedomRole
  val brainEvidence: EvidenceIdentity = source.brainEvidence
  val targetEvidence: EvidenceIdentity = source.targetEvidence
  val coefficientUnits: String = if kind == SubjectComparisonKind.SharedComponentCoefficients then source.valueUnits.coefficients else source.valueUnits.taskOperator
  val covarianceUnits: String = if kind == SubjectComparisonKind.SharedComponentCoefficients then source.valueUnits.coefficientCovariance else source.valueUnits.taskOperatorCovariance
  val covarianceOrdering: String = "feature-major/task-coordinate-minor"

object SubjectCoordinates:
  def transport[N <: SemanticSpace, K, L <: SemanticSpace](
      source: SubjectCoefficientEstimate, shared: SharedTaskCoordinates,
      alignment: SubjectSpatialAlignment[N, K, L], kind: SubjectComparisonKind,
      policy: SubjectCoordinatePolicy = SubjectCoordinatePolicy()
  ): Either[SubjectCoordinateError, SubjectCoordinates] =
    val measurement = alignment.measurement
    val local = source.design.discovery.targetProjection
    val common = shared.discovery.targetProjection
    val outputTask = if kind == SubjectComparisonKind.SharedComponentCoefficients then common.output.descriptor else common.input.descriptor
    val p = BigInt(source.featureAxis.size); val r = BigInt(source.componentAxis.size)
    val m = BigInt(measurement.local.size); val c = BigInt(outputTask.size); val q = BigInt(local.input.size)
    // Includes both bounded block covariance stages, outputs, input snapshots,
    // materialized measurement, task SVD/solve and validation storage. Provider
    // internal scratch/object overhead are excluded; no dense Kronecker map.
    val cells = 8 * ((p * r).pow(2) + (m * r).pow(2) + (m * c).pow(2)) +
      8 * (p * p + m * p + m * m + r * r + r * c + q * q) + 4 * (p * r + m * r + m * c + q * r)
    val work = 2 * r * r * (m * p * p + m * m * p) +
      2 * m * m * (c * r * r + c * c * r) + 2 * (m * p * r + m * r * c) +
      (if kind == SubjectComparisonKind.SharedComponentCoefficients then 2 * q * r * r else BigInt(0))
    val confirmation = source.design.confirmation.samples
    val overlap = shared.discovery.samples.unitKeys.toSet.intersect(confirmation.unitKeys.toSet)
    for
      // Metadata refusals precede payload reads/operator applications.
      _ <- if local.input.descriptor == shared.taskAxis then Right(())
        else Left(SubjectCoordinateError.AxisMismatch("ordered original task axes; equal dimensions are insufficient"))
      _ <- if overlap.isEmpty && shared.discovery.samples.rows.descriptor != confirmation.rows.descriptor then Right(())
        else Left(SubjectCoordinateError.Independence("shared discovery overlaps this subject's confirmation units/rows"))
      _ <- if alignment.discoveryIdentity == source.design.discovery.identity && alignment.sharedIdentity == shared.identity then Right(())
        else Left(SubjectCoordinateError.AxisMismatch("frozen spatial selection must bind this subject discovery and shared coordinates"))
      _ <- if source.featureAxis == measurement.source.descriptor then Right(())
        else Left(SubjectCoordinateError.AxisMismatch("admitted measurement source"))
      _ <- if shared.featureAxis == measurement.local.descriptor then Right(())
        else Left(SubjectCoordinateError.AxisMismatch("admitted measurement destination must be the shared anatomical coordinate axis"))
      _ <- if measurement.local.descriptor.units == source.featureAxis.units && measurement.local.descriptor.scale == source.featureAxis.scale &&
          (kind != SubjectComparisonKind.SharedComponentCoefficients ||
            (local.output.descriptor.units == common.output.descriptor.units && local.output.descriptor.scale == common.output.descriptor.scale)) then Right(())
        else Left(SubjectCoordinateError.AxisMismatch("units or scale conversion is not declared by this transport"))
      _ <- if (kind == SubjectComparisonKind.SharedComponentCoefficients && source.valueUnits.coefficients == shared.valueUnits.coefficients &&
          source.valueUnits.coefficientCovariance == shared.valueUnits.coefficientCovariance) ||
          (kind == SubjectComparisonKind.TaskLinkedForwardOperator && source.valueUnits.taskOperator == shared.valueUnits.taskOperator &&
            source.valueUnits.taskOperatorCovariance == shared.valueUnits.taskOperatorCovariance) then Right(())
        else Left(SubjectCoordinateError.AxisMismatch("physical coefficient/covariance value units do not match the shared convention"))
      _ <- (kind, source.stability.kind, shared.stability.kind) match
        case (SubjectComparisonKind.SharedComponentCoefficients, SubjectStabilityKind.StableAxes, SubjectStabilityKind.StableAxes) => Right(())
        case (SubjectComparisonKind.TaskLinkedForwardOperator, SubjectStabilityKind.StableAxes, _) => Right(())
        case (SubjectComparisonKind.TaskLinkedForwardOperator, SubjectStabilityKind.StableSubspace, _) => Right(())
        case _ => Left(SubjectCoordinateError.Unavailable("unstable axes require an explicit stable-subspace task-operator comparison"))
      _ <- budget(cells, work, Vector((p * r).pow(2), (m * r).pow(2), (m * c).pow(2), p * p, m * p, q * q), policy)
      task <- kind match
        case SubjectComparisonKind.TaskLinkedForwardOperator => Right((local.matrix.t, None, None))
        case SubjectComparisonKind.SharedComponentCoefficients => taskAlignment(local.matrix, common.matrix, policy)
      spatial <- measurement.leg(DMat.eye(source.featureAxis.size)).left.map(error => SubjectCoordinateError.Numerical(error.message))
      _ <- if ResidualCovariance.finite(spatial) then Right(()) else Left(SubjectCoordinateError.Numerical("nonfinite spatial map"))
      mean = spatial * source.coefficients * task._1
      joint = transportCovariance(source.covariance, spatial, task._1)
      _ <- if ResidualCovariance.finite(mean) then Right(()) else Left(SubjectCoordinateError.Numerical("nonfinite transported mean"))
      _ <- covariance(joint, policy)
    yield
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.subject-coordinates.v1")
        writer.string(source.identity); writer.string(shared.identity); writer.string(measurement.descriptor.semanticId)
        writer.string(alignment.identity)
        writer.string(outputTask.stableKey); writer.string(kind.toString); writer.string(policy.toString)
        writeMatrix(writer, task._1); writeMatrix(writer, mean); writeMatrix(writer, joint)
      new SubjectCoordinates(source, shared, measurement.descriptor, kind, alignment.identity, measurement.local.descriptor, outputTask,
        mean, joint, task._1, task._2, task._3, cells.toLong, work.toLong, policy, identity)

  /** Cs R = Cshared, so Bs changes to Bs R^-T. This checks discovery
    * projections, never optimizes agreement between confirmation maps. */
  private def taskAlignment(local: DMat, shared: DMat, policy: SubjectCoordinatePolicy): Either[SubjectCoordinateError, (DMat, Option[Double], Option[Double])] =
    if local.cols != shared.cols || local.rows < local.cols then Left(SubjectCoordinateError.Unavailable("shared component comparison requires equal full task rank"))
    else
      for
        localSvd <- local.svd.flatMap(_.requireConverged).left.map(e => SubjectCoordinateError.Numerical(e.toString))
        localCondition <- admittedCondition(Vector.tabulate(localSvd.singularValues.length)(localSvd.singularValues.apply), policy)
        r <- local.qr.solveLeastSquares(shared).left.map(e => SubjectCoordinateError.Numerical(e.toString))
        rSvd <- r.svd.flatMap(_.requireConverged).left.map(e => SubjectCoordinateError.Numerical(e.toString))
        rCondition <- admittedCondition(Vector.tabulate(rSvd.singularValues.length)(rSvd.singularValues.apply), policy)
        inverse <- r.solve(DMat.eye(r.rows)).left.map(e => SubjectCoordinateError.Numerical(e.toString))
        difference = local * r - shared
        residual = maxAbs(difference) / math.max(maxAbs(shared), java.lang.Double.MIN_NORMAL)
        _ <- if residual.isFinite && residual <= policy.relativeProjectionTolerance then Right(())
          else Left(SubjectCoordinateError.AxisMismatch(s"discovery task subspaces do not agree (relative residual $residual)"))
        _ <- if ResidualCovariance.finite(inverse) then Right(()) else Left(SubjectCoordinateError.Numerical("nonfinite inverse task transform"))
      yield (inverse.t, Some(math.max(localCondition, rCondition)), Some(residual))

  private def admittedCondition(values: Vector[Double], policy: SubjectCoordinatePolicy): Either[SubjectCoordinateError, Double] =
    val condition = if values.isEmpty || values.last <= 0.0 then Double.PositiveInfinity else values.head / values.last
    if !condition.isFinite || condition > policy.maximumCondition then Left(SubjectCoordinateError.Condition(condition, policy.maximumCondition))
    else Right(condition)

  /** Two bounded Gale block products preserve every cross-feature/component
    * cell without constructing M ⊗ A^T. For vec-row(B), that map has entries
    * M(outFeature,inFeature) A(inComponent,outComponent). */
  private def transportCovariance(input: DMat, spatial: DMat, task: DMat): DMat =
    val p = spatial.cols; val m = spatial.rows; val r = task.rows; val c = task.cols
    val afterSpatial = DMat.newBuilder(m * r, m * r)
    var k = 0
    while k < r do
      var l = 0
      while l < r do
        val block = DMat.tabulate(p, p)((i, j) => input(i * r + k, j * r + l))
        val transformed = spatial * block * spatial.t
        var i = 0
        while i < m do
          var j = 0
          while j < m do
            afterSpatial(i * r + k, j * r + l) = transformed(i, j)
            j += 1
          i += 1
        l += 1
      k += 1
    val spatialCovariance = afterSpatial.result()
    val result = DMat.newBuilder(m * c, m * c)
    var i = 0
    while i < m do
      var j = 0
      while j < m do
        val block = DMat.tabulate(r, r)((k, l) => spatialCovariance(i * r + k, j * r + l))
        val transformed = task.t * block * task
        var k = 0
        while k < c do
          var l = 0
          while l < c do
            result(i * c + k, j * c + l) = transformed(k, l)
            l += 1
          k += 1
        j += 1
      i += 1
    result.result()

  private[pattern] def covariance(matrix: DMat, policy: SubjectCoordinatePolicy): Either[SubjectCoordinateError, Unit] =
    if matrix.rows != matrix.cols || matrix.rows <= 0 || !ResidualCovariance.finite(matrix) then Left(SubjectCoordinateError.Numerical("nonfinite or non-square covariance"))
    else
      val scale = math.max(maxAbs(matrix), java.lang.Double.MIN_NORMAL)
      val asymmetric = (0 until matrix.rows).exists(i => (0 until i).exists(j => math.abs(matrix(i, j) - matrix(j, i)) > policy.relativeCovarianceTolerance * scale))
      if asymmetric then Left(SubjectCoordinateError.Numerical("covariance is not symmetric"))
      else
        Eigen.eigSymmetric(matrix, EigenSelection.All, EigenVectors.ValuesOnly).flatMap(_.requireConverged).left.map(e => SubjectCoordinateError.Numerical(e.toString)).flatMap: decomposition =>
          if decomposition.eigenvalues(0) < -policy.relativeCovarianceTolerance * scale then Left(SubjectCoordinateError.Numerical("covariance is not positive semidefinite"))
          else Right(()) // Retain singular covariance and original cells; no jitter or eigenvalue clipping.

  private[pattern] def budget(cells: BigInt, work: BigInt, arrays: Vector[BigInt], policy: SubjectCoordinatePolicy): Either[SubjectCoordinateError, Unit] =
    if cells > policy.maximumOwnedCells || work > policy.maximumScalarProducts || arrays.exists(_ > Int.MaxValue) then
      Left(SubjectCoordinateError.Budget(cells, policy.maximumOwnedCells, work, policy.maximumScalarProducts))
    else Right(())

  private def maxAbs(matrix: DMat): Double =
    var maximum = 0.0; var i = 0
    while i < matrix.rows do
      var j = 0
      while j < matrix.cols do
        maximum = math.max(maximum, math.abs(matrix(i, j)))
        j += 1
      i += 1
    maximum

  private[pattern] def writeMatrix(writer: AxisDigest.Writer, matrix: DMat): Unit =
    writer.intLE(matrix.rows); writer.intLE(matrix.cols)
    var i = 0
    while i < matrix.rows do
      var j = 0
      while j < matrix.cols do
        writer.string(java.lang.Double.toHexString(matrix(i, j)))
        j += 1
      i += 1
