package scalafim.fmri.mvpa.relation

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{CoordinateEvidence, Lin, SemanticProvenance, SemanticSpace, SpaceEvidence, ValueId, ValueIdentity}
import scalafim.fmri.mvpa.{AxisDescriptor, AxisDigest, EvidenceError}
import scalafim.fmri.mvpa.pattern.ResidualCovariance

final case class ResidualMetricProvenance(
    source: RelationSource,
    neuralAxis: AxisDescriptor,
    trainingAxis: AxisDescriptor,
    residualReceipt: String,
    estimator: String,
    degreesOfFreedom: Double
):
  require(residualReceipt.trim.nonEmpty, "residual receipt must be non-empty")
  require(estimator.trim.nonEmpty, "residual estimator must be non-empty")
  require(degreesOfFreedom.isFinite && degreesOfFreedom > 0.0, "residual degrees of freedom must be positive and finite")

enum ResidualMetricAdmission:
  /** Conditional claims are assessed against all endpoint origins at use time. */
  case DeclaredConditional(metric: MetricAdmission, provenance: ResidualMetricProvenance)
  case Descriptive(metric: MetricAdmission, provenance: ResidualMetricProvenance, reason: String)
  case Rejected(reason: String)

/** A separately admitted residual precision metric.  It retains its fitting
  * receipt and degrees of freedom, and refuses any request that would need a
  * hidden covariance substitution or materialization beyond the stated budget.
  */
final class ResidualPrecisionMetric[N] private[relation] (
    val covariance: ResidualCovariance[N],
    val admission: ResidualMetricAdmission,
    val maximumColumns: Int
):
  def closure: Either[EvidenceError, CrossClosure[covariance.neuralAxis.Id, covariance.neuralAxis.Id]] =
    closureFor(covariance.neuralAxis.evidence, covariance.neuralAxis.descriptor)

  def closureFor[S <: SemanticSpace](neural: SpaceEvidence[S], descriptor: AxisDescriptor): Either[EvidenceError, CrossClosure[S, S]] =
    if descriptor != covariance.neuralAxis.descriptor || neural.descriptor != covariance.neuralAxis.evidence.descriptor then
      Left(EvidenceError.AxisMismatch("residual precision rebind", covariance.neuralAxis.descriptor.stableKey, descriptor.stableKey))
    else build(neural)

  private def build[S <: SemanticSpace](neural: SpaceEvidence[S]): Either[EvidenceError, CrossClosure[S, S]] = admission match
    case ResidualMetricAdmission.Rejected(reason) => Left(EvidenceError.InvalidSource(reason))
    case _ =>
      val operator = new DoubleLinearOperator:
        val rows = covariance.neuralAxis.size
        val cols = covariance.neuralAxis.size
        // Precision is symmetric; retain the block-budget gate in its adjoint.
        override def adjoint: DoubleLinearOperator = this
        def applyTo(input: DVec, output: MutableDVec): Unit =
          covariance.applyPrecision(DMat.tabulate(cols, 1)((row, _) => input(row))) match
            case Left(error) => throw gale.linalg.LinAlgError.UnsupportedOperation(error.message)
            case Right(value) =>
              var row = 0
              while row < rows do
                output.update(row, value(row, 0))
                row += 1
        override def applyTo(input: DMat) =
          if input.cols > maximumColumns then Left(gale.linalg.LinAlgError.UnsupportedOperation(s"residual precision requires ${input.cols} columns; budget is $maximumColumns"))
          else covariance.applyPrecision(input).left.map(error => gale.linalg.LinAlgError.UnsupportedOperation(error.message))
        override def transposeApplyTo(input: DVec, output: MutableDVec): Unit = applyTo(input, output)
        override def transposeApplyTo(input: DMat) =
          if input.cols > maximumColumns then Left(gale.linalg.LinAlgError.UnsupportedOperation(s"residual precision transpose requires ${input.cols} columns; budget is $maximumColumns"))
          else covariance.applyPrecision(input).left.map(error => gale.linalg.LinAlgError.UnsupportedOperation(error.message))
      val provenance = admission match
        case ResidualMetricAdmission.DeclaredConditional(_, value) => value
        case ResidualMetricAdmission.Descriptive(_, value, _) => value
        case ResidualMetricAdmission.Rejected(_) => throw new IllegalStateException("unreachable")
      Lin.fromLinearMap(operator, CoordinateEvidence.primal(neural), CoordinateEvidence.dual(neural), ValueIdentity.source(ValueId.unsafe(identityDigest(provenance))), SemanticProvenance.source(s"residual-precision:${provenance.estimator}"))
        .left.map(EvidenceError.SemanticFailure.apply)


  private def identityDigest(provenance: ResidualMetricProvenance): String =
    AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.residual-precision.v1")
      writer.string(provenance.neuralAxis.stableKey)
      writer.string(provenance.trainingAxis.stableKey)
      writer.string(provenance.source.acquisitionRevision)
      writer.string(provenance.source.responseRevision)
      writer.string(provenance.source.readoutRevision)
      writer.string(provenance.source.preparationRevision)
      writer.string(provenance.source.noiseRevision)
      writer.string(provenance.residualReceipt)
      writer.string(provenance.estimator)
      writer.string(java.lang.Double.toHexString(provenance.degreesOfFreedom))
      covariance.diagonalValues.foreach(v => writer.string(java.lang.Double.toHexString(v)))
      val loadings = covariance.loadingsMatrix
      writer.intLE(loadings.cols)
      var row = 0
      while row < loadings.rows do
        var column = 0
        while column < loadings.cols do
          writer.string(java.lang.Double.toHexString(loadings(row, column)))
          column += 1
        row += 1

  def metricAdmission: Either[EvidenceError, MetricAdmission] = admission match
    case ResidualMetricAdmission.DeclaredConditional(metric, _) => Right(metric)
    case ResidualMetricAdmission.Descriptive(metric, _, _) => Right(metric)
    case ResidualMetricAdmission.Rejected(reason) => Left(EvidenceError.InvalidSource(reason))

  def provenance: Option[ResidualMetricProvenance] = admission match
    case ResidualMetricAdmission.DeclaredConditional(_, value) => Some(value)
    case ResidualMetricAdmission.Descriptive(_, value, _) => Some(value)
    case ResidualMetricAdmission.Rejected(_) => None

object ResidualPrecisionMetric:
  def apply[N](covariance: ResidualCovariance[N], admission: ResidualMetricAdmission, maximumColumns: Int): Either[EvidenceError, ResidualPrecisionMetric[N]] =
    admit(covariance, admission, maximumColumns, bound = false)

  private def admit[N](covariance: ResidualCovariance[N], admission: ResidualMetricAdmission, maximumColumns: Int, bound: Boolean): Either[EvidenceError, ResidualPrecisionMetric[N]] =
    if maximumColumns < 1 then Left(EvidenceError.InvalidAxis("residual precision budget", "maximum columns must be positive"))
    else admission match
      case ResidualMetricAdmission.DeclaredConditional(_, _) if !bound =>
        Left(EvidenceError.InvalidSource("conditional residual metric claims require bound residual capabilities"))
      case ResidualMetricAdmission.DeclaredConditional(MetricAdmission.Fixed(_), _) =>
        Left(EvidenceError.InvalidSource("estimated residual precision is not a fixed identity metric"))
      case ResidualMetricAdmission.Descriptive(_, _, reason) if reason.trim.isEmpty =>
        Left(EvidenceError.InvalidSource("descriptive residual metric requires a reason"))
      case ResidualMetricAdmission.Rejected(_) => Right(new ResidualPrecisionMetric(covariance, admission, maximumColumns))
      case ResidualMetricAdmission.DeclaredConditional(_, provenance) if provenance.neuralAxis != covariance.neuralAxis.descriptor =>
        Left(EvidenceError.AxisMismatch("residual precision neural axis", covariance.neuralAxis.descriptor.stableKey, provenance.neuralAxis.stableKey))
      case ResidualMetricAdmission.Descriptive(_, provenance, _) if provenance.neuralAxis != covariance.neuralAxis.descriptor =>
        Left(EvidenceError.AxisMismatch("residual precision neural axis", covariance.neuralAxis.descriptor.stableKey, provenance.neuralAxis.stableKey))
      case _ => Right(new ResidualPrecisionMetric(covariance, admission, maximumColumns))

  /** Bound residual capabilities supply the source and df. The caller still
    * declares training and residualization provenance; no independence proof
    * is minted by having a covariance object.
    */
  def fromBinding[E <: SemanticSpace, N <: SemanticSpace, A, K](
      binding: ResidualRelationBinding[E, N, A], covariance: ResidualCovariance[K],
      trainingAxis: AxisDescriptor, residualReceipt: String, estimator: String,
      metric: MetricAdmission, maximumColumns: Int
  ): Either[EvidenceError, ResidualPrecisionMetric[K]] =
    val supplied = binding.precision.noisePrecision(binding.point.value)
    val df = binding.degreesOfFreedom.residualDegreesOfFreedom(binding.point.value)
    if binding.point.relation.neuralAxis != covariance.neuralAxis.descriptor then
      Left(EvidenceError.AxisMismatch("bound noise precision", binding.point.relation.neuralAxis.stableKey, covariance.neuralAxis.descriptor.stableKey))
    else if !df.isFinite || df <= 0.0 || df > trainingAxis.size || residualReceipt.trim.isEmpty || estimator.trim.isEmpty then
      Left(EvidenceError.InvalidSource("bound residual estimator/receipt/df is invalid"))
    else supplied match
      case actual: ResidualCovariance[?] if actual eq covariance =>
        val provenance = ResidualMetricProvenance(binding.point.source, covariance.neuralAxis.descriptor,
          trainingAxis, residualReceipt, estimator, df)
        metric match
          case MetricAdmission.Fixed(_) =>
            apply(covariance, ResidualMetricAdmission.Descriptive(metric, provenance, "no conditional metric-independence argument"), maximumColumns)
          case MetricAdmission.IndependentlySourced(origins, _) if origins != binding.point.relation.origins.support =>
            Left(EvidenceError.InvalidSource("independent metric evidence differs from the bound residual source"))
          case MetricAdmission.EndpointLearned(origins, _) if origins != binding.point.relation.origins.support =>
            Left(EvidenceError.InvalidSource("endpoint metric evidence differs from the bound residual source"))
          case _ => admit(covariance, ResidualMetricAdmission.DeclaredConditional(metric, provenance), maximumColumns, bound = true)
      case _ => Left(EvidenceError.InvalidSource("bound precision capability does not supply this residual covariance"))
