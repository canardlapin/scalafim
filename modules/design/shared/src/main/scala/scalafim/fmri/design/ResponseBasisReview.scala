package scalafim.fmri.design

import scalafim.fmri.hrf.*
import scala.util.control.NonFatal

enum BasisReviewError:
  case InvalidSampling(reason: String)
  case InvalidBasis(cause: BasisIdentityError)
  case InvalidFunctional(cause: BasisError)
  case NonFiniteValue(basis: BasisElementId, time: Seconds)
  case KernelEvaluation(reason: String)

  def message: String = this match
    case InvalidSampling(reason) => reason
    case InvalidBasis(cause) => cause.message
    case InvalidFunctional(cause) => cause.message
    case NonFiniteValue(basis,time) => s"Basis ${basis.value} is non-finite at ${time.value} seconds."
    case KernelEvaluation(reason) => s"The basis could not be evaluated: $reason"

final case class BasisPreviewPoint(time: Seconds, value: Double)

/** A FIR interval is retained separately from sampled values, so a renderer need
  * not invent ramps between discontinuous indicator functions.
  */
enum BasisPreviewGeometry:
  case Sampled(points: Vector[BasisPreviewPoint])
  case FirStep(from: Seconds, until: Seconds, height: Double)

final case class BasisPreviewCurve private[design] (
    element: BasisElement, values: Vector[Double], geometry: BasisPreviewGeometry):
  def label: String =
    val role = element.role match
      case BasisRole.Canonical => "Canonical"
      case BasisRole.TemporalDerivative => "Time derivative"
      case BasisRole.DispersionDerivative => "Dispersion derivative"
      case BasisRole.FirBin(_,from,until) => s"${from.value}–${until.value} s"
      case _ => element.label
    s"${element.index} · $role"

final case class BasisReadoutPreview private[design] (
    functional: ResponseFunctional, units: ResponseUnits,
    weights: Vector[Double], discretization: FunctionalDiscretizationReceipt)

/** Native basis values before any realized-design column normalization. This is
  * a model preview, not a fitted response or a run's convolved design matrix.
  */
final class ResponseBasisReview private (
    val descriptor: HrfDescriptor, val times: Vector[Seconds],
    val curves: Vector[BasisPreviewCurve], val readout: Option[BasisReadoutPreview]):
  def basisIds: Vector[BasisElementId] = curves.map(_.element.id)

object ResponseBasisReview:
  private def evaluate(hrf: Hrf, times: Vector[Seconds]): Either[BasisReviewError,scalafim.fmri.hrf.linalg.Mat] =
    try Right(hrf.evalDoubles(times.map(_.value)))
    catch case NonFatal(error) => Left(BasisReviewError.KernelEvaluation(Option(error.getMessage).getOrElse(error.toString)))

  def make(hrf: Hrf, functional: Option[ResponseFunctional] = None,
      samples: Int = 257, maximumValues: Int = 200000): Either[BasisReviewError,ResponseBasisReview] =
    try build(hrf,functional,samples,maximumValues)
    catch case NonFatal(error) => Left(BasisReviewError.KernelEvaluation(Option(error.getMessage).getOrElse(error.toString)))

  private def build(hrf: Hrf, functional: Option[ResponseFunctional], samples: Int,
      maximumValues: Int): Either[BasisReviewError,ResponseBasisReview] =
    for
      _ <- Either.cond(samples >= 2 && maximumValues > 0 && hrf.nbasis > 0 &&
        samples.toLong*hrf.nbasis <= maximumValues.toLong && hrf.span.value.isFinite && hrf.span.value > 0,
        (),BasisReviewError.InvalidSampling("Use at least two preview samples, a positive finite basis span and an explicit value budget."))
      elements <- hrf.basisElementsValidated.left.map(BasisReviewError.InvalidBasis.apply)
      _ <- Either.cond(elements.size == hrf.nbasis && elements.map(_.id).distinct.size == elements.size,
        (),BasisReviewError.InvalidSampling("Basis coordinates must have unique identities and match the kernel width."))
      readout <- functional match
        case None => Right(None)
        case Some(value) => ResponseBasis.of(hrf).responseFunctional(value)
          .left.map(BasisReviewError.InvalidFunctional.apply)
          .map(weights => Some(BasisReadoutPreview(value,weights.units,weights.values,weights.receipt)))
      times = Vector.tabulate(samples)(i => (hrf.span.value*(i.toDouble/(samples-1))).s)
      matrix <- evaluate(hrf,times)
      curves <- elements.zipWithIndex.foldLeft[Either[BasisReviewError,Vector[BasisPreviewCurve]]](Right(Vector.empty)) {
        case (acc,(element,column)) =>
          val values = times.indices.map(row => matrix(row,column)).toVector
          val invalid = values.indexWhere(value => !value.isFinite)
          val geometry: Either[BasisReviewError,BasisPreviewGeometry] = element.role match
            case BasisRole.FirBin(_,from,until) =>
              if !from.value.isFinite || !until.value.isFinite || from.value < 0 || until.value <= from.value then
                Left(BasisReviewError.InvalidSampling(s"Invalid physical FIR interval for ${element.id.value}."))
              else
                val middle = (from.value+(until.value-from.value)/2).s
                evaluate(hrf,Vector(middle)).flatMap { m =>
                  val height = m(0,column)
                  Either.cond(height.isFinite,BasisPreviewGeometry.FirStep(from,until,height),
                    BasisReviewError.NonFiniteValue(element.id,middle))
                }
            case _ => Right(BasisPreviewGeometry.Sampled(times.zip(values).map((time,value) => BasisPreviewPoint(time,value))))
          for
            previous <- acc
            _ <- Either.cond(invalid < 0,(),BasisReviewError.NonFiniteValue(element.id,times(math.max(0,invalid))))
            shape <- geometry
          yield previous :+ BasisPreviewCurve(element,values,shape)
      }
    yield new ResponseBasisReview(hrf.descriptor,times,curves,readout)
