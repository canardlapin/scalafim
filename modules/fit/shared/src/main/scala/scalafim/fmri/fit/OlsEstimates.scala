package scalafim.fmri.fit

import gale.backend.Backend
import gale.linalg.{DMat, DVec, Matrix, QR, TriangularSolve, Vec}

/** Positional numerical readout L, with requested estimates in rows and design
  * coefficients in columns. Structural callers compile their identified outputs
  * to this boundary; changing the column order requires changing L as well.
  */
final class CoefficientReadout private (val weights: DMat):
  def outputs: Int = weights.rows
  def predictors: Int = weights.cols

object CoefficientReadout:
  def fromMatrix(weights: DMat): Either[FitError, CoefficientReadout] =
    if weights.rows == 0 || weights.cols == 0 then
      Left(FitError.InvalidFitAxis("estimate readout", "requires at least one output and coefficient"))
    else if (0 until weights.rows).exists(row => (0 until weights.cols).exists(col => !weights(row, col).isFinite)) then
      Left(FitError.NonFiniteInput("estimate readout"))
    else Right(new CoefficientReadout(weights))

  /** Select coefficients in the caller's requested order. */
  def coefficients(predictors: Int, positions: Vector[Int]): Either[FitError, CoefficientReadout] =
    if predictors <= 0 || positions.isEmpty then
      Left(FitError.InvalidFitAxis("estimate coefficients", "requires positive coefficient count and a nonempty selection"))
    else if positions.distinct.size != positions.size || positions.exists(index => index < 0 || index >= predictors) then
      Left(FitError.InvalidFitAxis("estimate coefficients", "positions must be unique and within the design axis"))
    else fromMatrix(Matrix.tabulate(positions.size, predictors)((row, col) => if positions(row) == col then 1.0 else 0.0))

enum EstimateUncertaintyRequest:
  case None, Marginal, Joint

final case class OlsEstimateRequest(
    readout: CoefficientReadout,
    uncertainty: EstimateUncertaintyRequest = EstimateUncertaintyRequest.None
)

/** Unrequested inferential products have no allocated placeholder arrays. */
enum OlsEstimateUncertainty:
  case NotRequested
  case Marginal(
      standardErrors: StandardErrorBlock,
      residualVariance: DVec,
      residualDegreesOfFreedom: ResidualDegreesOfFreedom
  )
  case Joint(
      normalizedCovariance: DMat,
      standardErrors: StandardErrorBlock,
      residualVariance: DVec,
      residualDegreesOfFreedom: ResidualDegreesOfFreedom
  )

final case class OlsEstimateResult private[fit] (
    estimates: DMat,
    uncertainty: OlsEstimateUncertainty,
    diagnostics: OlsDiagnostics
)

/** A compiled, reusable estimator. Preparation depends only on the design and
  * requested outputs. Estimates-only execution performs one output-by-time times
  * time-by-voxel product; it never computes nuisance coefficients or residuals.
  */
final class OlsEstimatePlan private (
    val request: OlsEstimateRequest,
    val diagnostics: OlsDiagnostics,
    val operator: DMat,
    private val uncertaintyPlan: OlsEstimatePlan.UncertaintyPlan
):
  def timepoints: Int = operator.cols
  def outputs: Int = operator.rows

  def estimate(response: ResponseBlock)(using Backend): Either[FitError, OlsEstimateResult] =
    if response.timepoints != timepoints then
      Left(FitError.RowMismatch(timepoints, response.timepoints))
    else
      uncertaintyPlan.estimate(response, operator).map { (estimates, uncertainty) =>
        OlsEstimateResult(estimates, uncertainty, diagnostics)
      }

object OlsEstimatePlan:
  private enum UncertaintyPlan:
    case Omitted
    case Required(qr: QR, readout: DMat, scales: DVec, joint: Option[DMat], df: ResidualDegreesOfFreedom)

    def estimate(response: ResponseBlock, operator: DMat)(using Backend): Either[FitError, (DMat, OlsEstimateUncertainty)] = this match
      case Omitted => Right((operator * response.value, OlsEstimateUncertainty.NotRequested))
      case Required(qr, readout, scales, joint, df) =>
        residualCoordinates(qr, response, df).map { (leading, sigma2) =>
          val se = StandardErrorBlock(Matrix.tabulate(scales.length, response.voxels) { (output, voxel) =>
            math.sqrt(scales(output) * sigma2(voxel))
          })
          // Q^T Y is already needed for residual variance. Reuse its leading
          // coordinates for the estimates instead of multiplying the original
          // response by the compiled time-domain operator a second time.
          val estimates = readout * leading
          val uncertainty = joint match
            case None => OlsEstimateUncertainty.Marginal(se, sigma2, df)
            case Some(covariance) => OlsEstimateUncertainty.Joint(covariance, se, sigma2, df)
          (estimates, uncertainty)
        }

  private[fit] def prepare(
      design: DesignMatrix,
      request: OlsEstimateRequest,
      policy: OlsSolvePolicy
  ): Either[FitError, OlsEstimatePlan] =
    if request.readout.predictors != design.predictors then
      Left(FitError.InvalidFitAxis("estimate readout", s"expected ${design.predictors} coefficient columns, got ${request.readout.predictors}"))
    else
      for
        prepared <- Ols.prepareQr(design, policy)
        (qr, diagnostics) = prepared
        // There is no residual-df requirement for estimates alone, including
        // an exactly determined full-rank design.
        df <- request.uncertainty match
          case EstimateUncertaintyRequest.None => Right(None)
          case _ => ResidualDegreesOfFreedom(design.timepoints - diagnostics.rank).map(Some(_))
        dual <- solveDual(qr, request.readout.weights)
        padded = Matrix.tabulate(design.timepoints, dual.cols) { (row, col) =>
          if row < dual.rows then dual(row, col) else 0.0
        }
        columns <- qr.applyQ(padded).left.map(FitError.SingularDesign.apply)
      yield
        val uncertainty = df match
          case None => UncertaintyPlan.Omitted
          case Some(degrees) =>
            val scales = Vec.tabulate(dual.cols) { col =>
              var sum = 0.0
              var row = 0
              while row < dual.rows do
                val value = dual(row, col)
                sum += value * value
                row += 1
              sum
            }
            val joint = request.uncertainty match
              case EstimateUncertaintyRequest.Joint => Some(dual.t * dual)
              case _ => None
            UncertaintyPlan.Required(qr, packedReadout(dual.t), scales, joint, degrees)
        new OlsEstimatePlan(request, diagnostics, packedReadout(columns.t), uncertainty)

  /** Pack reusable readouts once. Gale's optional vector kernel requires unit
    * column stride, so keeping a transpose view would silently bypass SIMD.
    */
  private[fit] def packedReadout(matrix: DMat): DMat =
    if matrix.isContiguousRowMajor then matrix
    else Matrix.tabulate(matrix.rows, matrix.cols)(matrix.apply)

  /** Shared QR response work for uncertainty and precision-pooling consumers.
    * The residual coordinates avoid subtracting two nearly equal squared norms.
    */
  private[fit] def residualCoordinates(
      qr: QR,
      response: ResponseBlock,
      df: ResidualDegreesOfFreedom
  ): Either[FitError, (DMat, DVec)] =
    qr.applyQT(response.value).left.map(FitError.SingularDesign.apply).map { transformed =>
      val variance = Vec.newBuilder(response.voxels)
      var voxel = 0
      while voxel < response.voxels do
        var sum = 0.0
        var row = qr.coefficientCount
        while row < response.timepoints do
          val value = transformed(row, voxel)
          sum += value * value
          row += 1
        variance(voxel) = sum / df.value
        voxel += 1
      (transformed.slice(0, qr.coefficientCount, 0, response.voxels), variance.result())
    }

  private[fit] def solveDual(qr: QR, weights: DMat): Either[FitError, DMat] =
    val count = qr.coefficientCount
    val rt = qr.r.slice(0, count, 0, count).t
    val permutation = qr.columnPermutation.toIndexSeq
    val dual = Matrix.newBuilder(count, weights.rows)
    var output = 0
    while output < weights.rows do
      val rhs = Vec.tabulate(count)(index => weights(output, permutation(index)))
      TriangularSolve.lower(rt, rhs) match
        case Left(error) => return Left(FitError.SingularDesign(error))
        case Right(column) =>
          var row = 0
          while row < count do
            dual(row, output) = column(row)
            row += 1
      output += 1
    Right(dual.result())

/** Compatibility names for the initial selected-OLS surface. */
type OlsReadout = CoefficientReadout
object OlsReadout:
  export CoefficientReadout.{fromMatrix, coefficients}

type OlsUncertaintyRequest = EstimateUncertaintyRequest
object OlsUncertaintyRequest:
  export EstimateUncertaintyRequest.{None, Marginal, Joint}
