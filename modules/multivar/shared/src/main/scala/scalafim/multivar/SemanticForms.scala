package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.DoubleLinearOperator
import gale.linalg.LinAlgError
import gale.linalg.MutableDVec

enum CertificateNorm:
  case Frobenius
  case Spectral
  case Euclidean

enum NumericalPrecision:
  case Float32
  case Float64
  case Extended(label: String)

final case class CertificateTolerance private (
    absolute: Double,
    relative: Double
):
  def absoluteValue: Double = absolute
  def relativeValue: Double = relative

  def threshold(scale: Double): Double =
    absolute + relative * Math.max(1.0, scale)

object CertificateTolerance:
  def from(absolute: Double, relative: Double): Either[SemanticError, CertificateTolerance] =
    if !absolute.isFinite || absolute < 0.0 then
      Left(SemanticError.InvalidCertificateMetadata(s"absolute tolerance must be finite and non-negative, got $absolute"))
    else if !relative.isFinite || relative < 0.0 then
      Left(SemanticError.InvalidCertificateMetadata(s"relative tolerance must be finite and non-negative, got $relative"))
    else Right(CertificateTolerance(absolute, relative))

  val strict: CertificateTolerance =
    CertificateTolerance(1e-10, 1e-8)

final case class CertificateContext private (
    tolerance: CertificateTolerance,
    norm: CertificateNorm,
    method: String,
    backend: String,
    precision: NumericalPrecision,
    regularization: Option[String]
)

object CertificateContext:
  def from(
      tolerance: CertificateTolerance,
      norm: CertificateNorm,
      method: String,
      backend: String,
      precision: NumericalPrecision,
      regularization: Option[String] = None
  ): Either[SemanticError, CertificateContext] =
    val cleanMethod = method.trim
    val cleanBackend = backend.trim
    val cleanRegularization = regularization.map(_.trim)
    if cleanMethod.isEmpty then Left(SemanticError.InvalidCertificateMetadata("certificate method must be non-empty"))
    else if cleanBackend.isEmpty then Left(SemanticError.InvalidCertificateMetadata("certificate backend must be non-empty"))
    else if cleanRegularization.exists(_.isEmpty) then
      Left(SemanticError.InvalidCertificateMetadata("certificate regularization must be non-empty when present"))
    else
      Right(
        CertificateContext(
          tolerance,
          norm,
          cleanMethod,
          cleanBackend,
          precision,
          cleanRegularization
        )
      )

  val portableFloat64: CertificateContext =
    CertificateContext(
      CertificateTolerance.strict,
      CertificateNorm.Frobenius,
      "portable-spectral-check",
      "gale",
      NumericalPrecision.Float64,
      None
    )

enum CertificateClaim:
  case Symmetric(residual: Double, scale: Double)
  case PositiveSemidefinite(minimumEigenvalue: Double, symmetryResidual: Double, scale: Double)
  case PositiveDefinite(minimumEigenvalue: Double, symmetryResidual: Double, scale: Double)
  case Indefinite(minimumEigenvalue: Double, maximumEigenvalue: Double, symmetryResidual: Double, scale: Double)
  case Rank(rank: Int, dimension: Int, residual: Double, scale: Double)
  case Orthogonal(residual: Double, scale: Double)
  case Converged(iterations: Int, residual: Double, scale: Double)

  def property: String =
    this match
      case Symmetric(_, _)                     => "symmetric"
      case PositiveSemidefinite(_, _, _)        => "psd"
      case PositiveDefinite(_, _, _)            => "spd"
      case Indefinite(_, _, _, _)               => "indefinite"
      case Rank(_, _, _, _)                     => "rank"
      case Orthogonal(_, _)                     => "orthogonal"
      case Converged(_, _, _)                   => "converged"

final case class NumericalCertificate(
    valueIdentity: ValueIdentity,
    claim: CertificateClaim,
    context: CertificateContext
)

sealed trait CertificateProperty
sealed trait SymmetryProperty extends CertificateProperty
sealed trait PsdProperty extends CertificateProperty
sealed trait SpdProperty extends CertificateProperty
sealed trait IndefiniteProperty extends CertificateProperty
sealed trait RankProperty extends CertificateProperty
sealed trait OrthogonalProperty extends CertificateProperty
sealed trait ConvergenceProperty extends CertificateProperty

final class Certificate[P <: CertificateProperty] private (
    val runtime: NumericalCertificate
)

object Certificate:
  private[multivar] def unsafe[P <: CertificateProperty](
      valueIdentity: ValueIdentity,
      claim: CertificateClaim,
      context: CertificateContext
  ): Certificate[P] =
    new Certificate(NumericalCertificate(valueIdentity, claim, context))

  def rank(
      valueIdentity: ValueIdentity,
      rank: Int,
      dimension: Int,
      residual: Double,
      scale: Double,
      context: CertificateContext
  ): Either[SemanticError, Certificate[RankProperty]] =
    if dimension < 0 || rank < 0 || rank > dimension then
      Left(SemanticError.CertificateRejected("rank", s"rank $rank is outside 0..$dimension"))
    else
      residualCertificate[RankProperty]("rank", valueIdentity, residual, scale, context) {
        CertificateClaim.Rank(rank, dimension, residual, scale)
      }

  def orthogonal(
      valueIdentity: ValueIdentity,
      residual: Double,
      scale: Double,
      context: CertificateContext
  ): Either[SemanticError, Certificate[OrthogonalProperty]] =
    residualCertificate[OrthogonalProperty]("orthogonal", valueIdentity, residual, scale, context) {
      CertificateClaim.Orthogonal(residual, scale)
    }

  def converged(
      valueIdentity: ValueIdentity,
      iterations: Int,
      residual: Double,
      scale: Double,
      context: CertificateContext
  ): Either[SemanticError, Certificate[ConvergenceProperty]] =
    if iterations < 0 then
      Left(SemanticError.CertificateRejected("converged", s"iterations must be non-negative, got $iterations"))
    else
      residualCertificate[ConvergenceProperty]("converged", valueIdentity, residual, scale, context) {
        CertificateClaim.Converged(iterations, residual, scale)
      }

  private def residualCertificate[P <: CertificateProperty](
      property: String,
      valueIdentity: ValueIdentity,
      residual: Double,
      scale: Double,
      context: CertificateContext
  )(claim: => CertificateClaim): Either[SemanticError, Certificate[P]] =
    if !residual.isFinite || residual < 0.0 then
      Left(SemanticError.CertificateRejected(property, s"residual must be finite and non-negative, got $residual"))
    else if !scale.isFinite || scale < 0.0 then
      Left(SemanticError.CertificateRejected(property, s"scale must be finite and non-negative, got $scale"))
    else if residual > context.tolerance.threshold(scale) then
      Left(
        SemanticError.CertificateRejected(
          property,
          s"residual $residual exceeds threshold ${context.tolerance.threshold(scale)}"
        )
      )
    else Right(unsafe(valueIdentity, claim, context))

object FormCertificates:
  def symmetric[From <: Coordinate, To <: Coordinate](
      operator: Lin[From, To],
      context: CertificateContext = CertificateContext.portableFloat64
  ): Either[SemanticError, Certificate[SymmetryProperty]] =
    inspectSymmetry(operator, context).map { case (_, residual, scale) =>
      Certificate.unsafe(operator.valueIdentity, CertificateClaim.Symmetric(residual, scale), context)
    }

  def psd[From <: Coordinate, To <: Coordinate](
      operator: Lin[From, To],
      context: CertificateContext = CertificateContext.portableFloat64,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen
  ): Either[SemanticError, Certificate[PsdProperty]] =
    certifySpectrum(operator, context, eigenSolver, requirePositive = false, requireIndefinite = false).map {
      case (minimum, _, residual, scale) =>
        Certificate.unsafe(
          operator.valueIdentity,
          CertificateClaim.PositiveSemidefinite(minimum, residual, scale),
          context
        )
    }

  def spd[From <: Coordinate, To <: Coordinate](
      operator: Lin[From, To],
      context: CertificateContext = CertificateContext.portableFloat64,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen
  ): Either[SemanticError, Certificate[SpdProperty]] =
    certifySpectrum(operator, context, eigenSolver, requirePositive = true, requireIndefinite = false).map {
      case (minimum, _, residual, scale) =>
        Certificate.unsafe(
          operator.valueIdentity,
          CertificateClaim.PositiveDefinite(minimum, residual, scale),
          context
        )
    }

  def indefinite[From <: Coordinate, To <: Coordinate](
      operator: Lin[From, To],
      context: CertificateContext = CertificateContext.portableFloat64,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen
  ): Either[SemanticError, Certificate[IndefiniteProperty]] =
    certifySpectrum(operator, context, eigenSolver, requirePositive = false, requireIndefinite = true).map {
      case (minimum, maximum, residual, scale) =>
        Certificate.unsafe(
          operator.valueIdentity,
          CertificateClaim.Indefinite(minimum, maximum, residual, scale),
          context
        )
    }

  private def certifySpectrum[From <: Coordinate, To <: Coordinate](
      operator: Lin[From, To],
      context: CertificateContext,
      eigenSolver: SymmetricEigenSolver,
      requirePositive: Boolean,
      requireIndefinite: Boolean
  ): Either[SemanticError, (Double, Double, Double, Double)] =
    for
      checked <- inspectSymmetry(operator, context)
      (matrix, residual, scale) = checked
      eigen <- LinalgErrorAdapter
        .adapt(eigenSolver.decompose(MatrixOps.symmetrize(matrix)))
        .left
        .map(SemanticError.MultivarFailure.apply)
      maximum = eigen.values(0)
      minimum = eigen.values(eigen.values.length - 1)
      cutoff = context.tolerance.threshold(Math.max(scale, Math.max(Math.abs(maximum), Math.abs(minimum))))
      _ <-
        if requireIndefinite && !(minimum < -cutoff && maximum > cutoff) then
          Left(
            SemanticError.CertificateRejected(
              "indefinite",
              s"spectrum [$minimum, $maximum] does not contain both signs beyond threshold $cutoff"
            )
          )
        else if requirePositive && minimum <= cutoff then
          Left(SemanticError.CertificateRejected("spd", s"minimum eigenvalue $minimum is not above threshold $cutoff"))
        else if !requirePositive && !requireIndefinite && minimum < -cutoff then
          Left(SemanticError.CertificateRejected("psd", s"minimum eigenvalue $minimum is below -$cutoff"))
        else Right(())
    yield (minimum, maximum, residual, scale)

  private def inspectSymmetry[From <: Coordinate, To <: Coordinate](
      operator: Lin[From, To],
      context: CertificateContext
  ): Either[SemanticError, (DMat, Double, Double)] =
    if context.norm != CertificateNorm.Frobenius then
      Left(
        SemanticError.CertificateRejected(
          "symmetric",
          s"form certification currently implements the Frobenius norm, not ${context.norm}"
        )
      )
    else if operator.domain.descriptor.space != operator.codomain.descriptor.space then
      Left(
        SemanticError.CertificateRejected(
          "symmetric",
          "domain and codomain must refer to the same stable space"
        )
      )
    else if operator.rows != operator.cols then
      Left(SemanticError.CertificateRejected("symmetric", s"operator is ${operator.rows}x${operator.cols}"))
    else
      operator(DMat.eye(operator.cols)).flatMap { matrix =>
        val residual = symmetryResidual(matrix)
        val scale = frobeniusNorm(matrix)
        if !residual.isFinite || !scale.isFinite then
          Left(SemanticError.CertificateRejected("symmetric", "operator contains non-finite values"))
        else if residual > context.tolerance.threshold(scale) then
          Left(
            SemanticError.CertificateRejected(
              "symmetric",
              s"residual $residual exceeds threshold ${context.tolerance.threshold(scale)}"
            )
          )
        else Right((matrix, residual, scale))
      }

  private def symmetryResidual(matrix: DMat): Double =
    var sum = 0.0
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        val difference = matrix(row, col) - matrix(col, row)
        sum += difference * difference
        col += 1
      row += 1
    Math.sqrt(sum)

  private def frobeniusNorm(matrix: DMat): Double =
    var sum = 0.0
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        val value = matrix(row, col)
        sum += value * value
        col += 1
      row += 1
    Math.sqrt(sum)

sealed trait OperatorEvidence
sealed trait FormEvidence extends OperatorEvidence
sealed trait SymmetricEvidence extends FormEvidence
sealed trait PsdEvidence extends SymmetricEvidence
sealed trait SpdEvidence extends PsdEvidence
sealed trait IndefiniteEvidence extends SymmetricEvidence
sealed trait UncheckedEvidence extends FormEvidence
sealed trait CertifiedSymmetric extends SymmetricEvidence
sealed trait CertifiedPsd extends CertifiedSymmetric with PsdEvidence
sealed trait CertifiedSpd extends CertifiedPsd with SpdEvidence
sealed trait CertifiedIndefinite extends CertifiedSymmetric with IndefiniteEvidence
sealed trait AssumedSymmetric extends SymmetricEvidence
sealed trait AssumedPsd extends AssumedSymmetric with PsdEvidence
sealed trait AssumedSpd extends AssumedPsd with SpdEvidence

trait OperatorRoleTag
sealed trait FormRoleTag extends OperatorRoleTag
sealed trait BilinearFormRole extends FormRoleTag
sealed trait SymmetricFormRole extends FormRoleTag
sealed trait SemiMetricRole extends FormRoleTag
sealed trait MetricRole extends FormRoleTag
sealed trait IndefiniteFormRole extends FormRoleTag
sealed trait CometricRole extends FormRoleTag
sealed trait PenaltyRole extends FormRoleTag
sealed trait KernelRole extends FormRoleTag
sealed trait CovarianceRole extends FormRoleTag
sealed trait PrecisionRole extends FormRoleTag

enum FormRole:
  case BilinearForm
  case SymmetricForm
  case SemiMetric
  case Metric
  case IndefiniteForm
  case Cometric
  case Penalty
  case Kernel
  case Covariance
  case Precision

enum FormStructure:
  case General
  case Symmetric

enum PositivityStatus:
  case Unchecked
  case Psd
  case Spd
  case Indefinite

enum EvidenceStatus:
  case Unchecked
  case Certified
  case Assumed

final case class FormDescriptor(
    role: FormRole,
    space: MvSpace,
    direction: LinDescriptor,
    structure: FormStructure,
    positivity: PositivityStatus,
    evidence: EvidenceStatus,
    certificates: Vector[NumericalCertificate]
)

final class GeometricOperator[
    S <: SemanticSpace,
    From <: Coordinate,
    To <: Coordinate,
    Role <: FormRoleTag,
    Evidence <: FormEvidence
] private[multivar] (
    val operator: Lin[From, To],
    val space: SpaceEvidence[S],
    val descriptor: FormDescriptor
):
  def provenance: SemanticProvenance =
    operator.provenance

type BilinearForm[S <: SemanticSpace] =
  GeometricOperator[S, Primal[S], Dual[S], BilinearFormRole, UncheckedEvidence]
type SymmetricForm[S <: SemanticSpace, E <: SymmetricEvidence] =
  GeometricOperator[S, Primal[S], Dual[S], SymmetricFormRole, E]
type SemiMetric[S <: SemanticSpace, E <: PsdEvidence] =
  GeometricOperator[S, Primal[S], Dual[S], SemiMetricRole, E]
type MetricForm[S <: SemanticSpace, E <: SpdEvidence] =
  GeometricOperator[S, Primal[S], Dual[S], MetricRole, E]
type IndefiniteForm[S <: SemanticSpace] =
  GeometricOperator[S, Primal[S], Dual[S], IndefiniteFormRole, CertifiedIndefinite]
type CometricForm[S <: SemanticSpace, E <: SpdEvidence] =
  GeometricOperator[S, Dual[S], Primal[S], CometricRole, E]
type PenaltyForm[S <: SemanticSpace, E <: PsdEvidence] =
  GeometricOperator[S, Primal[S], Dual[S], PenaltyRole, E]
type KernelForm[S <: SemanticSpace, E <: PsdEvidence] =
  GeometricOperator[S, Dual[S], Primal[S], KernelRole, E]
type CovarianceForm[S <: SemanticSpace, E <: PsdEvidence] =
  GeometricOperator[S, Dual[S], Primal[S], CovarianceRole, E]
type PrecisionForm[S <: SemanticSpace, E <: SpdEvidence] =
  GeometricOperator[S, Primal[S], Dual[S], PrecisionRole, E]

object Form:
  def bilinear[S <: SemanticSpace](operator: Lin[Primal[S], Dual[S]], space: SpaceEvidence[S]): BilinearForm[S] =
    buildUnchecked(operator, space, FormRole.BilinearForm)

  def symmetric[S <: SemanticSpace](
      operator: Lin[Primal[S], Dual[S]],
      space: SpaceEvidence[S],
      certificate: Certificate[SymmetryProperty]
  ): Either[SemanticError, SymmetricForm[S, CertifiedSymmetric]] =
    buildCertified(
      operator,
      space,
      certificate,
      FormRole.SymmetricForm,
      FormStructure.Symmetric,
      PositivityStatus.Unchecked
    )

  def semiMetric[S <: SemanticSpace](
      operator: Lin[Primal[S], Dual[S]],
      space: SpaceEvidence[S],
      certificate: Certificate[PsdProperty]
  ): Either[SemanticError, SemiMetric[S, CertifiedPsd]] =
    buildCertified(operator, space, certificate, FormRole.SemiMetric, FormStructure.Symmetric, PositivityStatus.Psd)

  def metric[S <: SemanticSpace](
      operator: Lin[Primal[S], Dual[S]],
      space: SpaceEvidence[S],
      certificate: Certificate[SpdProperty]
  ): Either[SemanticError, MetricForm[S, CertifiedSpd]] =
    buildCertified(operator, space, certificate, FormRole.Metric, FormStructure.Symmetric, PositivityStatus.Spd)

  def indefinite[S <: SemanticSpace](
      operator: Lin[Primal[S], Dual[S]],
      space: SpaceEvidence[S],
      certificate: Certificate[IndefiniteProperty]
  ): Either[SemanticError, IndefiniteForm[S]] =
    buildCertified(
      operator,
      space,
      certificate,
      FormRole.IndefiniteForm,
      FormStructure.Symmetric,
      PositivityStatus.Indefinite
    )

  def cometric[S <: SemanticSpace](
      operator: Lin[Dual[S], Primal[S]],
      space: SpaceEvidence[S],
      certificate: Certificate[SpdProperty]
  ): Either[SemanticError, CometricForm[S, CertifiedSpd]] =
    buildCertified(operator, space, certificate, FormRole.Cometric, FormStructure.Symmetric, PositivityStatus.Spd)

  def penalty[S <: SemanticSpace](
      operator: Lin[Primal[S], Dual[S]],
      space: SpaceEvidence[S],
      certificate: Certificate[PsdProperty]
  ): Either[SemanticError, PenaltyForm[S, CertifiedPsd]] =
    buildCertified(operator, space, certificate, FormRole.Penalty, FormStructure.Symmetric, PositivityStatus.Psd)

  def kernel[S <: SemanticSpace](
      operator: Lin[Dual[S], Primal[S]],
      space: SpaceEvidence[S],
      certificate: Certificate[PsdProperty]
  ): Either[SemanticError, KernelForm[S, CertifiedPsd]] =
    buildCertified(operator, space, certificate, FormRole.Kernel, FormStructure.Symmetric, PositivityStatus.Psd)

  def covariance[S <: SemanticSpace](
      operator: Lin[Dual[S], Primal[S]],
      space: SpaceEvidence[S],
      certificate: Certificate[PsdProperty]
  ): Either[SemanticError, CovarianceForm[S, CertifiedPsd]] =
    buildCertified(operator, space, certificate, FormRole.Covariance, FormStructure.Symmetric, PositivityStatus.Psd)

  def precision[S <: SemanticSpace](
      operator: Lin[Primal[S], Dual[S]],
      space: SpaceEvidence[S],
      certificate: Certificate[SpdProperty]
  ): Either[SemanticError, PrecisionForm[S, CertifiedSpd]] =
    buildCertified(operator, space, certificate, FormRole.Precision, FormStructure.Symmetric, PositivityStatus.Spd)

  private[multivar] def buildAssumed[
      S <: SemanticSpace,
      From <: Coordinate,
      To <: Coordinate,
      Role <: FormRoleTag,
      E <: FormEvidence
  ](
      operator: Lin[From, To],
      space: SpaceEvidence[S],
      role: FormRole,
      positivity: PositivityStatus
  ): GeometricOperator[S, From, To, Role, E] =
    new GeometricOperator(
      operator,
      space,
      FormDescriptor(
        role,
        space.descriptor,
        operator.descriptor,
        FormStructure.Symmetric,
        positivity,
        EvidenceStatus.Assumed,
        Vector.empty
      )
    )

  private def buildUnchecked[
      S <: SemanticSpace,
      From <: Coordinate,
      To <: Coordinate,
      Role <: FormRoleTag
  ](
      operator: Lin[From, To],
      space: SpaceEvidence[S],
      role: FormRole
  ): GeometricOperator[S, From, To, Role, UncheckedEvidence] =
    new GeometricOperator(
      operator,
      space,
      FormDescriptor(
        role,
        space.descriptor,
        operator.descriptor,
        FormStructure.General,
        PositivityStatus.Unchecked,
        EvidenceStatus.Unchecked,
        Vector.empty
      )
    )

  private def buildCertified[
      S <: SemanticSpace,
      From <: Coordinate,
      To <: Coordinate,
      Role <: FormRoleTag,
      E <: FormEvidence,
      P <: CertificateProperty
  ](
      operator: Lin[From, To],
      space: SpaceEvidence[S],
      certificate: Certificate[P],
      role: FormRole,
      structure: FormStructure,
      positivity: PositivityStatus
  ): Either[SemanticError, GeometricOperator[S, From, To, Role, E]] =
    if certificate.runtime.valueIdentity != operator.valueIdentity then
      Left(
        SemanticError.CertificateValueMismatch(
          operator.valueIdentity,
          certificate.runtime.valueIdentity
        )
      )
    else
      Right(
        new GeometricOperator(
          operator,
          space,
          FormDescriptor(
            role,
            space.descriptor,
            operator.descriptor,
            structure,
            positivity,
            EvidenceStatus.Certified,
            Vector(certificate.runtime)
          )
        )
      )

object FormOperator:
  def primal[S <: SemanticSpace](
      metric: MetricSpec,
      space: SpaceEvidence[S],
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance = SemanticProvenance.source("legacy-mvmetric-primal")
  ): Either[SemanticError, Lin[Primal[S], Dual[S]]] =
    fromMetric(
      metric,
      space,
      CoordinateEvidence.primal(space),
      CoordinateEvidence.dual(space),
      valueIdentity,
      provenance
    )

  def dual[S <: SemanticSpace](
      metric: MetricSpec,
      space: SpaceEvidence[S],
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance = SemanticProvenance.source("legacy-mvmetric-dual")
  ): Either[SemanticError, Lin[Dual[S], Primal[S]]] =
    fromMetric(
      metric,
      space,
      CoordinateEvidence.dual(space),
      CoordinateEvidence.primal(space),
      valueIdentity,
      provenance
    )

  private def fromMetric[S <: SemanticSpace, From <: Coordinate, To <: Coordinate](
      metric: MetricSpec,
      space: SpaceEvidence[S],
      domain: CoordinateEvidence[From],
      codomain: CoordinateEvidence[To],
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance
  ): Either[SemanticError, Lin[From, To]] =
    metric.space match
      case Some(tag) if tag != space.descriptor =>
        Left(
          SemanticError.CoordinateMismatch(
            "metric space",
            CoordinateDescriptor(space.descriptor, domain.descriptor.variance),
            CoordinateDescriptor(tag, domain.descriptor.variance)
          )
        )
      case _ =>
        Lin.fromKernel(
          MetricKernel(metric),
          domain,
          codomain,
          valueIdentity,
          provenance.append(SemanticProvenanceEvent.Adapted("MetricSpec"))
        )

private[multivar] final case class MetricKernel(metric: MetricSpec) extends SemanticKernel:
  private val adapted = MetricLinearMap(metric)

  override def rows: Int = metric.dim
  override def cols: Int = metric.dim
  override def representation: OperatorRepresentation =
    metric match
      case _: MetricSpec.Identity         => OperatorRepresentation.Diagonal
      case _: MetricSpec.Diagonal         => OperatorRepresentation.Diagonal
      case _: MetricSpec.DenseSymmetric   => OperatorRepresentation.Dense
      case _: MetricSpec.SparseSymmetric  => OperatorRepresentation.Sparse
  override def linearMap: DoubleLinearOperator = adapted
  override def forward(input: DMat): Either[SemanticError, DMat] =
    metric.matvec(input).left.map(SemanticError.MultivarFailure.apply)
  override def adjoint: SemanticKernel = this

private final case class MetricLinearMap(metric: MetricSpec) extends DoubleLinearOperator:
  override def rows: Int = metric.dim
  override def cols: Int = metric.dim
  override def applyTo(input: DVec, output: MutableDVec): Unit =
    if input.length != cols then throw LinAlgError.VectorLengthMismatch(cols, input.length)
    if output.length != rows then throw LinAlgError.VectorLengthMismatch(rows, output.length)
    metric.applyVector(input) match
      case Left(error) => throw LinAlgError.InvalidArgument(error.message)
      case Right(result) =>
        var index = 0
        while index < result.length do
          output(index) = result(index)
          index += 1
  override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
    applyTo(input, output)

/** The only public path from an unproved numerical claim to a proved-looking role.
  * The type and runtime descriptor both retain that the property was assumed.
  */
object Unsafe:
  def assumeSymmetric[S <: SemanticSpace](
      operator: Lin[Primal[S], Dual[S]],
      space: SpaceEvidence[S],
      reason: String
  ): Either[SemanticError, SymmetricForm[S, AssumedSymmetric]] =
    withAssumption(operator, "symmetric", reason).map { unsafeOperator =>
      Form.buildAssumed[S, Primal[S], Dual[S], SymmetricFormRole, AssumedSymmetric](
        unsafeOperator,
        space,
        FormRole.SymmetricForm,
        PositivityStatus.Unchecked
      )
    }

  def assumePsd[S <: SemanticSpace](
      operator: Lin[Primal[S], Dual[S]],
      space: SpaceEvidence[S],
      reason: String
  ): Either[SemanticError, SemiMetric[S, AssumedPsd]] =
    withAssumption(operator, "psd", reason).map { unsafeOperator =>
      Form.buildAssumed[S, Primal[S], Dual[S], SemiMetricRole, AssumedPsd](
        unsafeOperator,
        space,
        FormRole.SemiMetric,
        PositivityStatus.Psd
      )
    }

  def assumeSpd[S <: SemanticSpace](
      operator: Lin[Primal[S], Dual[S]],
      space: SpaceEvidence[S],
      reason: String
  ): Either[SemanticError, MetricForm[S, AssumedSpd]] =
    withAssumption(operator, "spd", reason).map { unsafeOperator =>
      Form.buildAssumed[S, Primal[S], Dual[S], MetricRole, AssumedSpd](
        unsafeOperator,
        space,
        FormRole.Metric,
        PositivityStatus.Spd
      )
    }

  def assumeSpdCometric[S <: SemanticSpace](
      operator: Lin[Dual[S], Primal[S]],
      space: SpaceEvidence[S],
      reason: String
  ): Either[SemanticError, CometricForm[S, AssumedSpd]] =
    withAssumption(operator, "spd-cometric", reason).map { unsafeOperator =>
      Form.buildAssumed[S, Dual[S], Primal[S], CometricRole, AssumedSpd](
        unsafeOperator,
        space,
        FormRole.Cometric,
        PositivityStatus.Spd
      )
    }

  def assumeSymmetric[From <: Coordinate, To <: Coordinate, R <: OperatorRoleTag](
      operator: Op[From, To, R, UncheckedEvidence],
      reason: String
  )(using SelfDualPorts[From, To]): Either[SemanticError, Op[From, To, R, AssumedSymmetric]] =
    Op.assume(operator, "symmetric", reason)

  def assumePsd[From <: Coordinate, To <: Coordinate, R <: OperatorRoleTag](
      operator: Op[From, To, R, UncheckedEvidence],
      reason: String
  )(using SelfDualPorts[From, To]): Either[SemanticError, Op[From, To, R, AssumedPsd]] =
    Op.assume(operator, "psd", reason)

  def assumeSpd[From <: Coordinate, To <: Coordinate, R <: OperatorRoleTag](
      operator: Op[From, To, R, UncheckedEvidence],
      reason: String
  )(using SelfDualPorts[From, To]): Either[SemanticError, Op[From, To, R, AssumedSpd]] =
    Op.assume(operator, "spd", reason)

  def assumeSameRows[Left <: SemanticSpace, Right <: SemanticSpace](
      left: SpaceEvidence[Left],
      right: SpaceEvidence[Right],
      entitySpace: MvSpace,
      reason: String
  ): Either[AlignmentError, SameEntityEvidence[Left, Right]] =
    SameEntityEvidence.unsafeIdentity(left, right, entitySpace, reason)

  private def withAssumption[From <: Coordinate, To <: Coordinate](
      operator: Lin[From, To],
      property: String,
      reason: String
  ): Either[SemanticError, Lin[From, To]] =
    val cleanReason = reason.trim
    if cleanReason.isEmpty then
      Left(SemanticError.InvalidCertificateMetadata("unsafe assumptions require a non-empty reason"))
    else
      Right(
        new Lin(
          operator.kernel,
          operator.domain,
          operator.codomain,
          operator.valueIdentity,
          operator.provenance.append(
            SemanticProvenanceEvent.UnsafeAssumption(property, cleanReason)
          )
        )
      )
