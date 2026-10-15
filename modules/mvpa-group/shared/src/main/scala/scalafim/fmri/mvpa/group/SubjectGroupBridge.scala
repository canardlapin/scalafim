package scalafim.fmri.mvpa.group

import gale.backend.Backend.given
import gale.linalg.DMat
import gale.spectral.{Eigen, EigenSelection, EigenVectors}
import gale.spectral.SpectralBackend.given
import multivar.core.{SemanticSpace, ValueId, ValueIdentity}
import scalafim.dataset.SubjectId
import scalafim.estimates.{DfRole, PoolingScope}
import scalafim.fmri.group.*
import scalafim.fmri.mvpa.{AxisDescriptor, AxisDigest, AxisRef, Column}
import scalafim.fmri.mvpa.pattern.*

enum SubjectGroupError:
  case Invalid(detail: String)
  case Binding(detail: String)
  case Unavailable(detail: String)
  case Budget(requiredCells: BigInt, allowedCells: Long)
  case Numerical(detail: String)
  case Coordinates(cause: SubjectCoordinateError)
  case Native(cause: GroupError)

  def message: String = this match
    case Invalid(detail) => s"invalid subject group input: $detail"
    case Binding(detail) => s"subject group binding mismatch: $detail"
    case Unavailable(detail) => s"subject group capability unavailable: $detail"
    case Budget(required, allowed) => s"subject group bridge requires $required cells, allowed $allowed"
    case Numerical(detail) => s"subject group numerical failure: $detail"
    case Coordinates(cause) => cause.message
    case Native(cause) => cause.message

/** Independent of whether df itself is known. This declaration must name the
  * actual M5.01 covariance value source and uncertainty receipt. */
final case class SubjectGroupUncertainty(
    coordinateIdentity: String, covarianceSource: ValueIdentity, uncertaintyReceipt: String,
    origin: GroupVarianceOrigin, fit: GroupFitProvenance, pooling: PoolingScope
)

enum LoadingJointGaussianModel:
  /** Cov(vec-row(E)) = K_rows ⊗ Sigma_features, with one homogeneous
    * feature covariance for every row. Marginal Gaussian row laws alone do
    * not establish this matrix-normal model. */
  case HomogeneousSeparable
  case GeneralJoint
  case Unknown

/** A scientific declaration tied to the actual fitted loading source.
  * It is separate from covariance origin, and does not authenticate a caller's
  * model assumption. The constructor cannot infer a joint law from voxel SEs. */
final class LoadingSeparableGaussian private (
    val loadingIdentity: String, val receipt: String, val identity: String
)
object LoadingSeparableGaussian:
  def declare(result: VoxelLoadingResult, model: LoadingJointGaussianModel, receipt: String): Either[SubjectGroupError, LoadingSeparableGaussian] =
    if model != LoadingJointGaussianModel.HomogeneousSeparable then
      Left(SubjectGroupError.Unavailable("loading covariance requires an explicit homogeneous separable joint Gaussian row/feature model; general or unknown joint laws are unsupported"))
    else if receipt.trim.isEmpty then Left(SubjectGroupError.Invalid("separable joint Gaussian model receipt"))
    else
      val loading = SubjectGroupBridge.loadingIdentity(result)
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.loading-separable-Gaussian.v1")
        writer.string(loading); writer.string(model.toString); writer.string(receipt)
      Right(new LoadingSeparableGaussian(loading, receipt, identity))

/** A separately supplied residual feature covariance, including cross-feature
  * cells. No feature independence or zero covariance is guessed from voxel SEs.
  * Its source/content declaration is not physical-origin authentication. */
final class LoadingResidualCovariance private (
    val loadingIdentity: String, val values: DMat, val valueSource: ValueIdentity,
    val receipt: String, val jointLaw: LoadingSeparableGaussian, val identity: String
)

object LoadingResidualCovariance:
  def bind(result: VoxelLoadingResult, jointLaw: LoadingSeparableGaussian, covariance: DMat, source: ValueIdentity, receipt: String,
      maximumOwnedCells: Long = 1_000_000L): Either[SubjectGroupError, LoadingResidualCovariance] =
    val p = result.estimates.rows
    val cells = 2 * BigInt(p) * p
    for
      _ <- if receipt.trim.nonEmpty && covariance.rows == p && covariance.cols == p && p > 0 then Right(())
        else Left(SubjectGroupError.Invalid("residual feature covariance dimensions/source receipt"))
      _ <- SubjectGroupBridge.budget(cells, maximumOwnedCells)
      _ <- if jointLaw.loadingIdentity == SubjectGroupBridge.loadingIdentity(result) then Right(())
        else Left(SubjectGroupError.Binding("joint Gaussian model belongs to another actual loading source"))
      _ <- if result.residualScales.size == p && result.residualDegreesOfFreedom > 0 then Right(())
        else Left(SubjectGroupError.Binding("loading residual scales/df"))
      _ <- if finite(covariance) && (0 until p).forall(i => (0 until i).forall(j => covariance(i, j) == covariance(j, i))) then Right(())
        else Left(SubjectGroupError.Numerical("residual covariance must be finite and symmetric without repair"))
      diagonalMatches = (0 until p).forall: i =>
        val expected = result.residualScales(i) * result.residualScales(i)
        expected.isFinite && expected > 0.0 && math.abs(covariance(i, i) - expected) <= 1e-10 * expected
      _ <- if diagonalMatches then Right(()) else Left(SubjectGroupError.Binding("residual covariance diagonal differs from actual loading residual variances"))
      eigen <- Eigen.eigSymmetric(covariance, EigenSelection.All, EigenVectors.ValuesOnly).flatMap(_.requireConverged)
        .left.map(e => SubjectGroupError.Numerical(e.toString))
      _ <- if eigen.eigenvalues(0) >= -1e-12 * (0 until p).map(i => covariance(i, i)).max then Right(())
        else Left(SubjectGroupError.Numerical("residual feature covariance is not positive semidefinite"))
    yield
      val owned = DMat.tabulate(p, p)(covariance.apply)
      val loading = SubjectGroupBridge.loadingIdentity(result)
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.loading-residual-covariance.v1")
        writer.string(loading); writer.string(source.toString); writer.string(receipt)
        writer.string(jointLaw.identity)
        SubjectGroupBridge.writeMatrix(writer, owned)
      new LoadingResidualCovariance(loading, owned, source, receipt, jointLaw, identity)

  private def finite(matrix: DMat): Boolean =
    (0 until matrix.rows).forall(i => (0 until matrix.cols).forall(j => matrix(i, j).isFinite))

/** Abstract measurement coordinates remain abstract. Their labels are exact
  * axis keys; this creates no voxel grid, affine or registration assertion. */
final class SubjectGroupDomain private (
    val axis: AxisDescriptor, val keys: Vector[String], val space: GroupSpace,
    val geometry: GroupGeometryEvidence
)
object SubjectGroupDomain:
  def samples(axis: AxisRef[?], geometry: GroupGeometryEvidence): Either[SubjectGroupError, SubjectGroupDomain] =
    if axis.size <= 0 then Left(SubjectGroupError.Invalid("empty common measurement axis"))
    else
      val keys = axis.toRecord.stableKeys
      keys.foldLeft[Either[SubjectGroupError, Vector[SampleLabel]]](Right(Vector.empty)): (previous, key) =>
        previous.flatMap(labels => SampleLabel(key).left.map(SubjectGroupError.Native.apply).map(labels :+ _))
      .map(labels => new SubjectGroupDomain(axis.descriptor, keys, GroupSpace.SampleAxis(axis.size, labels), geometry))

final class SubjectGroupSubject private (
    val coordinates: SubjectCoordinates, val uncertainty: SubjectGroupUncertainty, val identity: String
)

object SubjectGroupSubject:
  def declared(coordinates: SubjectCoordinates, declaration: SubjectGroupUncertainty): Either[SubjectGroupError, SubjectGroupSubject] =
    for
      _ <- if declaration.coordinateIdentity == coordinates.identity && declaration.covarianceSource == coordinates.source.covarianceSource &&
          declaration.uncertaintyReceipt == coordinates.source.uncertaintyReceipt then Right(())
        else Left(SubjectGroupError.Binding("uncertainty must name the actual coordinate/covariance source and receipt"))
      _ <- (coordinates.covarianceOrigin, declaration.origin) match
        case (SubjectCovarianceOrigin.Known(actualMethod), GroupVarianceOrigin.Known(method)) if method == actualMethod => Right(())
        case (SubjectCovarianceOrigin.Estimated(_), GroupVarianceOrigin.Estimated(df)) => compatibleDf(coordinates.degreesOfFreedom, coordinates.degreesOfFreedomRole, df)
        case (SubjectCovarianceOrigin.Approximate(_), GroupVarianceOrigin.Estimated(df)) => compatibleDf(coordinates.degreesOfFreedom, coordinates.degreesOfFreedomRole, df)
        case _ => Left(SubjectGroupError.Unavailable("uncertainty declaration cannot change the immutable covariance origin or promote estimated/approximate/unknown to known"))
    yield
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.subject-group-subject.v1"); writer.string(coordinates.identity)
        writer.string(declaration.covarianceSource.toString); writer.string(declaration.uncertaintyReceipt)
        writer.string(declaration.origin.toString); writer.string(declaration.fit.toString); writer.string(declaration.pooling.toString)
      new SubjectGroupSubject(coordinates, declaration, identity)

  /** Adopt M4.05's already-computed conditional coefficient covariance. The
    * supplied feature residual covariance has estimated marginal scales, so
    * the resulting uncertainty is Estimated even though residual df is known.
    * No fitter, distribution approximation or implicit independent-feature
    * covariance is introduced here. */
  def fromLoading[N <: SemanticSpace, K, L <: SemanticSpace, FK](
      subjects: Column[?, SubjectCoordinateKey], features: AxisRef[FK], design: ConfirmationDesign[?, ?],
      result: VoxelLoadingResult, residual: LoadingResidualCovariance,
      shared: SharedTaskCoordinates, alignment: SubjectSpatialAlignment[N, K, L], kind: SubjectComparisonKind,
      stability: SubjectAxisStability, units: SubjectCoordinateValueUnits,
      fit: GroupFitProvenance, pooling: PoolingScope,
      policy: SubjectCoordinatePolicy = SubjectCoordinatePolicy()
  ): Either[SubjectGroupError, SubjectGroupSubject] =
    val p = BigInt(features.size); val r = BigInt(result.componentDegreesOfFreedom)
    val cells = 3 * (p * r).pow(2) + p * r + p * p
    for
      _ <- if result.designIdentity == design.identity && result.neuralAxis == features.descriptor &&
          result.componentAxis == design.discovery.targetProjection.output.descriptor &&
          result.confirmationRows == design.confirmation.samples.rows.descriptor &&
          result.independentUnits == design.confirmation.samples.units.descriptor &&
          result.rowUnitOrdinals == design.confirmation.samples.rowUnitOrdinals && result.errorLaw == design.errorLaw then Right(())
        else Left(SubjectGroupError.Binding("actual loading design, feature/task axes, rows, units or error law"))
      _ <- if subjects.rowAxis == result.confirmationRows then Right(()) else Left(SubjectGroupError.Binding("subject column confirmation rows"))
      _ <- SubjectGroupBridge.budget(cells, policy.maximumOwnedCells)
      _ <- if residual.loadingIdentity == SubjectGroupBridge.loadingIdentity(result) && residual.jointLaw.loadingIdentity == residual.loadingIdentity then Right(())
        else Left(SubjectGroupError.Binding("residual covariance belongs to another loading result or source"))
      g = result.normalizedComponentCovariance
      _ <- if g.rows == r && g.cols == r && residual.values.rows == p && result.estimates.cols == r then Right(())
        else Left(SubjectGroupError.Binding("retained conditional component covariance dimensions"))
      joint = DMat.tabulate((p * r).toInt, (p * r).toInt)((i, j) => residual.values(i / r.toInt, j / r.toInt) * g(i % r.toInt, j % r.toInt))
      dfMethod = "VoxelLoadingConfirmation residual df n - nuisance/task rank"
      df = SubjectCoordinateDf.Known(result.residualDegreesOfFreedom.toDouble, dfMethod)
      receipt = s"estimated loading covariance; ${residual.receipt}; homogeneous separable joint Gaussian model ${residual.jointLaw.identity}: ${residual.jointLaw.receipt}; normalized component covariance from ${result.designIdentity}"
      coefficientSource = ValueIdentity.source(ValueId.unsafe(s"loading-effects-${residual.loadingIdentity}"))
      covarianceSource = ValueIdentity.compose(residual.valueSource, ValueIdentity.source(ValueId.unsafe(s"loading-covariance-${residual.identity}")))
      input <- SubjectCoefficientEstimate.bind(subjects, features, design, result.estimates, joint,
        result.brainEvidenceIdentity, result.targetEvidenceIdentity, coefficientSource, covarianceSource,
        df, receipt, stability, units,
        SubjectCovarianceOrigin.Estimated("VoxelLoadingConfirmation estimated residual feature covariance"), SubjectCoordinateDfRole.Residual, policy)
        .left.map(SubjectGroupError.Coordinates.apply)
      coordinates <- SubjectCoordinates.transport(input, shared, alignment, kind, policy).left.map(SubjectGroupError.Coordinates.apply)
      origin = GroupVarianceOrigin.Estimated(GroupDegreesOfFreedom(DfRole.Residual,
        GroupDfValues.Scalar(result.residualDegreesOfFreedom.toDouble), dfMethod, false))
      subject <- declared(coordinates, SubjectGroupUncertainty(coordinates.identity, covarianceSource, receipt, origin, fit, pooling))
    yield subject

  private def compatibleDf(source: SubjectCoordinateDf, role: SubjectCoordinateDfRole, df: GroupDegreesOfFreedom): Either[SubjectGroupError, Unit] =
    val expected = source match
      case SubjectCoordinateDf.Known(value, method) => Some((value, method, false))
      case SubjectCoordinateDf.Estimated(value, method) => Some((value, method, false))
      case SubjectCoordinateDf.Approximate(value, method) => Some((value, method, true))
      case _ => None
    val valid = (expected, df.values) match
      case (Some((value, method, approximate)), GroupDfValues.Scalar(actual)) =>
        actual == value && method == df.method && approximate == df.approximate
      case _ => false
    val roleMatches = role match
      case SubjectCoordinateDfRole.Residual => df.role == DfRole.Residual
      case SubjectCoordinateDfRole.Effective => df.role == DfRole.Effective
      case SubjectCoordinateDfRole.Reference | SubjectCoordinateDfRole.Unspecified => false
    if valid && roleMatches then Right(()) else Left(SubjectGroupError.Binding("estimated covariance requires the actual scalar residual/effective df provenance and role"))

/** Owned numeric snapshot; metadata/source witnesses are immutable values.
  * This is an in-memory conversion, not a durable estimate artifact. */
final case class MaterializedSubjectGroupRow(source: SubjectGroupSubject, estimates: DMat, covariance: DMat)
final case class MaterializedSubjectGroupInput(
    shared: SharedTaskCoordinates, domain: SubjectGroupDomain,
    subjects: Vector[SubjectCoordinateKey], rows: Vector[MaterializedSubjectGroupRow]
)

enum SubjectGroupCalculation:
  /** Explicit caller declaration: independent Gaussian effects with known
    * sampling variances. It does not follow from a known first-level df. */
  case KnownVarianceGaussianFixedEffects
  /** Native numerical policy only. No calibration claim follows from PM/mKH
    * parity; estimated-SE adverse results remain applicable. */
  case ApproximateMixedEffects(tau: TauEstimator, inference: MetaInference, justification: String)

final class SubjectGroupInput private[group] (
    val shared: SharedTaskCoordinates, val domain: SubjectGroupDomain,
    val subjectKeys: Vector[SubjectCoordinateKey], val rows: Vector[MaterializedSubjectGroupRow],
    val data: GroupData[VarianceCapability.WithVariances], val kind: SubjectComparisonKind,
    val taskAxis: AxisDescriptor, val coefficientUnits: String, val covarianceUnits: String,
    val plannedOwnedCells: Long, val identity: String
):
  def fullCovariances: Vector[DMat] = rows.map(_.covariance)
  def originalDegreesOfFreedom: Vector[SubjectCoordinateDf] = rows.map(_.source.coordinates.degreesOfFreedom)
  def originalDfRoles: Vector[SubjectCoordinateDfRole] = rows.map(_.source.coordinates.degreesOfFreedomRole)

  def materialize(maximumOwnedCells: Long = 1_000_000L): Either[SubjectGroupError, MaterializedSubjectGroupInput] =
    val cells = rows.foldLeft(BigInt(0))((n, row) => n + BigInt(row.estimates.rows) * row.estimates.cols + BigInt(row.covariance.rows) * row.covariance.cols)
    SubjectGroupBridge.budget(cells, maximumOwnedCells).map: _ =>
      val owned = rows.map(row => row.copy(estimates = DMat.tabulate(row.estimates.rows, row.estimates.cols)(row.estimates.apply),
        covariance = DMat.tabulate(row.covariance.rows, row.covariance.cols)(row.covariance.apply)))
      MaterializedSubjectGroupInput(shared, domain, subjectKeys, owned)

  /** Native group models operate on one component/contrast at a time. Full
    * covariance remains attached to this model's input, never silently reduced
    * to a claim about a joint component or spatial hypothesis. */
  def marginalModel(design: GroupDesign, declaredSubjects: Vector[SubjectId], calculation: SubjectGroupCalculation): Either[SubjectGroupError, SubjectGroupModel] =
    if declaredSubjects != data.subjects then Left(SubjectGroupError.Binding("group design subject order"))
    else calculation match
      case SubjectGroupCalculation.KnownVarianceGaussianFixedEffects =>
        if rows.exists(row => !row.source.uncertainty.origin.isInstanceOf[GroupVarianceOrigin.Known]) then
          Left(SubjectGroupError.Unavailable("estimated uncertainty cannot enter a known-variance Gaussian group reference"))
        else GroupModel.inverseVariance(data, design).left.map(SubjectGroupError.Native.apply)
          .map(model => new SubjectGroupModel(this, model, calculation))
      case SubjectGroupCalculation.ApproximateMixedEffects(tau, inference, justification) =>
        if justification.trim.isEmpty then Left(SubjectGroupError.Invalid("explicit mixed-effects approximation justification"))
        else GroupModel.mixedEffects(data, design, tau, inference).left.map(SubjectGroupError.Native.apply)
          .map(model => new SubjectGroupModel(this, model, calculation))

  def jointInference: Either[SubjectGroupError, Nothing] =
    Left(SubjectGroupError.Unavailable("native group engine has no full cross-component/cross-feature joint inference capability"))
  def durableEstimateExport: Either[SubjectGroupError, Nothing] =
    Left(SubjectGroupError.Unavailable("EstimateDomain V0 requires real voxel geometry and cannot store generic full cross-feature covariance"))

final class SubjectGroupModel private[group] (
    val input: SubjectGroupInput, val native: GroupModel[VarianceCapability.WithVariances],
    val calculation: SubjectGroupCalculation
):
  def fit(): Either[SubjectGroupError, SubjectGroupFit] =
    GroupEngine.fit(native).left.map(SubjectGroupError.Native.apply).map(result => new SubjectGroupFit(this, result))

final class SubjectGroupFit private[group] (val model: SubjectGroupModel, val native: GroupResult):
  val scope: String = "marginal pointwise native calculation; no joint or automatic scientific-release admission"

object SubjectGroupBridge:
  def eager(shared: SharedTaskCoordinates, domain: SubjectGroupDomain,
      subjects: Vector[SubjectCoordinateKey], inputs: Vector[SubjectGroupSubject],
      maximumOwnedCells: Long = 1_000_000L): Either[SubjectGroupError, SubjectGroupInput] =
    val rows = inputs.map(input => MaterializedSubjectGroupRow(input, input.coordinates.estimates, input.coordinates.covariance))
    fromMaterialized(MaterializedSubjectGroupInput(shared, domain, subjects, rows), maximumOwnedCells)

  def fromMaterialized(value: MaterializedSubjectGroupInput, maximumOwnedCells: Long = 1_000_000L): Either[SubjectGroupError, SubjectGroupInput] =
    val rows = value.rows; val p = BigInt(value.domain.axis.size)
    val r = rows.headOption.map(row => BigInt(row.source.coordinates.taskAxis.size)).getOrElse(BigInt(0))
    val s = BigInt(rows.size)
    val cells = 2 * s * (p * r).pow(2) + 4 * s * p * r
    for
      _ <- if value.subjects.nonEmpty && value.subjects.distinct.size == value.subjects.size &&
          rows.map(_.source.coordinates.subject) == value.subjects then Right(()) else Left(SubjectGroupError.Binding("ordered complete unique subject membership"))
      _ <- if rows.map(_.source.coordinates.brainEvidence).distinct.size == rows.size then Right(())
        else Left(SubjectGroupError.Binding("the same actual brain evidence cannot be relabelled as independent subjects"))
      matchingEndpoints = rows.forall: row =>
        val coordinates = row.source.coordinates
        coordinates.shared.identity == value.shared.identity && coordinates.featureAxis == value.domain.axis &&
          coordinates.taskAxis == rows.head.source.coordinates.taskAxis && coordinates.kind == rows.head.source.coordinates.kind &&
          coordinates.coefficientUnits == rows.head.source.coordinates.coefficientUnits && coordinates.covarianceUnits == rows.head.source.coordinates.covarianceUnits
      _ <- if value.domain.axis == value.shared.featureAxis && rows.nonEmpty && matchingEndpoints then Right(())
        else Left(SubjectGroupError.Binding("shared task/space/kind/value units"))
      _ <- budget(cells, maximumOwnedCells)
      _ <- if rows.forall(row => row.estimates.rows == p && row.estimates.cols == r && row.covariance.rows == p * r && row.covariance.cols == p * r) then Right(())
        else Left(SubjectGroupError.Binding("materialized coefficient/covariance dimensions"))
      _ <- if rows.forall(row => sameMatrix(row.estimates, row.source.coordinates.estimates) && sameMatrix(row.covariance, row.source.coordinates.covariance)) then Right(())
        else Left(SubjectGroupError.Binding("materialized values differ from their actual bound covariance/effect source"))
      subjectIds <- value.subjects.foldLeft[Either[SubjectGroupError, Vector[SubjectId]]](Right(Vector.empty)):
        (previous, key) => previous.flatMap(ids => SubjectId.make(key.value).left.map(error => SubjectGroupError.Invalid(error.toString)).map(ids :+ _))
      taskKeys = rows.head.source.coordinates.kind match
        case SubjectComparisonKind.SharedComponentCoefficients => value.shared.discovery.targetProjection.output.toRecord.stableKeys
        case SubjectComparisonKind.TaskLinkedForwardOperator => value.shared.discovery.targetProjection.input.toRecord.stableKeys
      contrasts = taskKeys.map(key => s"${rows.head.source.coordinates.taskAxis.stableKey}:$key")
      responses <- contrasts.indices.toVector.foldLeft[Either[SubjectGroupError, Vector[(String, GroupResponse[VarianceCapability.WithVariances])]]](Right(Vector.empty)):
        (previous, k) => previous.flatMap: accumulated =>
          val effects = DMat.tabulate(rows.size, p.toInt)((i, j) => rows(i).estimates(j, k))
          val variances = DMat.tabulate(rows.size, p.toInt)((i, j) => rows(i).covariance(j * r.toInt + k, j * r.toInt + k))
          GroupResponse.weighted(effects, variances).left.map(SubjectGroupError.Native.apply).map(response => accumulated :+ (contrasts(k) -> response))
      samples = Vector.range(0, p.toInt)
      sources = rows.zip(subjectIds).flatMap: (row, subject) =>
        contrasts.map(contrast => GroupUncertaintySource(subject, contrast, samples, None, row.source.uncertainty.origin,
          row.source.uncertainty.fit, Some(row.source.uncertainty.pooling)))
      uncertainty <- GroupUncertaintyReceipt.make(subjectIds, contrasts, samples, sources, value.domain.geometry).left.map(SubjectGroupError.Native.apply)
      data <- GroupData.build(subjectIds, value.domain.space, responses, Some(uncertainty)).left.map(SubjectGroupError.Native.apply)
    yield
      val owned = rows.map(row => row.copy(estimates = DMat.tabulate(row.estimates.rows, row.estimates.cols)(row.estimates.apply),
        covariance = DMat.tabulate(row.covariance.rows, row.covariance.cols)(row.covariance.apply)))
      val identity = AxisDigest.sha256Hex: writer =>
        writer.string("scalafim.subject-group-input.v1"); writer.string(value.shared.identity); writer.string(value.domain.axis.stableKey)
        writer.string(value.domain.geometry.toString)
        value.domain.keys.foreach(writer.string)
        value.subjects.foreach(key => writer.string(key.value))
        rows.foreach(row => writer.string(row.source.identity))
        owned.foreach: row =>
          writeMatrix(writer, row.estimates); writeMatrix(writer, row.covariance)
      new SubjectGroupInput(value.shared, value.domain, value.subjects, owned, data, rows.head.source.coordinates.kind,
        rows.head.source.coordinates.taskAxis, rows.head.source.coordinates.coefficientUnits, rows.head.source.coordinates.covarianceUnits, cells.toLong, identity)

  private[group] def budget(cells: BigInt, maximum: Long): Either[SubjectGroupError, Unit] =
    if maximum < 0L || cells > maximum || cells > Int.MaxValue then Left(SubjectGroupError.Budget(cells, maximum)) else Right(())

  private[group] def loadingIdentity(result: VoxelLoadingResult): String = AxisDigest.sha256Hex: writer =>
    writer.string("scalafim.voxel-loading-covariance-source.v1")
    writer.string(result.designIdentity); result.brainEvidenceIdentity.writeFramed(writer); result.targetEvidenceIdentity.writeFramed(writer)
    writer.string(result.confirmationRows.stableKey); writer.string(result.independentUnits.stableKey)
    result.rowUnitOrdinals.foreach(writer.intLE)
    writer.intLE(result.residualDegreesOfFreedom); writer.intLE(result.componentDegreesOfFreedom)
    writeMatrix(writer, result.estimates); writeMatrix(writer, result.normalizedComponentCovariance)
    result.residualScales.foreach(scale => writer.string(java.lang.Double.toHexString(scale)))

  private[group] def writeMatrix(writer: AxisDigest.Writer, matrix: DMat): Unit =
    writer.intLE(matrix.rows); writer.intLE(matrix.cols)
    var i = 0
    while i < matrix.rows do
      var j = 0
      while j < matrix.cols do
        writer.string(java.lang.Double.toHexString(matrix(i, j)))
        j += 1
      i += 1

  private def sameMatrix(left: DMat, right: DMat): Boolean =
    left.rows == right.rows && left.cols == right.cols && (0 until left.rows).forall(i => (0 until left.cols).forall(j =>
      java.lang.Double.doubleToLongBits(left(i, j)) == java.lang.Double.doubleToLongBits(right(i, j))))
