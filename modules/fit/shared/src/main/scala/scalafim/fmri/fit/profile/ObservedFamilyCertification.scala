package scalafim.fmri.fit.profile

import gale.linalg.{DMat, QROptions, QRPivoting}
import scalafim.fmri.ar.{WhiteningPlan, WhiteningTransform}
import scalafim.fmri.design.event.EventTerm
import scalafim.fmri.design.hrf.ExpandedConditionDesign
import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{JetLayout, ShapePoint}
import scalafim.fmri.hrf.linalg.Mat

enum ObservedFamilyError:
  case Spectral(detail: String)
  case RankLoss(coordinates: Vector[Double], smallestSingularValue: Double, required: Double)
  case Whitening(detail: String)

  def message: String =
    this match
      case Spectral(detail) => s"certification factorisation failed: $detail"
      case RankLoss(coords, s, required) =>
        s"the direct condition design at ${coords.mkString("(", ", ", ")")} has smallest singular value $s below the admitted margin $required; the cell changes rank"
      case Whitening(detail) => s"whitening failed: $detail"

/** One held-out shape under the fitted geometry. `projectorError` is the sine
  * of the largest principal angle between the column spaces of the direct and
  * basis-approximated condition designs after whitening and nuisance
  * projection: the quantity that bounds the profile-energy error, not the
  * design error alone.
  */
final case class ObservedFamilyPoint(
    coordinates: Vector[Double],
    projectorError: Double,
    designError: Double,
    smallestSingularValue: Double,
    conditionNumber: Double)

final case class ObservedFamilyCertificate(
    points: Vector[ObservedFamilyPoint],
    maxProjectorError: Double,
    maxDesignError: Double,
    minSmallestSingularValue: Double,
    requiredSingularValue: Double,
    nuisanceRank: Int):
  def worst: Option[ObservedFamilyPoint] = points.maxByOption(_.projectorError)

/** Experiment-level certification of a compiled kernel basis: the kernel
  * certificate says the family is well represented on its own grid; this
  * says the *observed* family is, after convolution with the actual events,
  * the fitted whitening and the nuisance projection, where a small design
  * error can still become an order-one projector error near aliasing.
  */
object ObservedFamilyCertification:

  def certify(
      expanded: ExpandedConditionDesign,
      term: EventTerm,
      frame: SamplingFrame,
      precision: Seconds,
      whitening: Option[WhiteningPlan],
      nuisance: Option[DMat],
      points: Vector[ShapePoint],
      requiredSingularValue: Double
  ): Either[ObservedFamilyError, ObservedFamilyCertificate] =
    val family = expanded.basis.family
    val rows = expanded.rows
    def whiten(matrix: DMat): Either[ObservedFamilyError, DMat] =
      whitening match
        case None => Right(matrix)
        case Some(plan) => WhiteningTransform.matrix(plan, matrix).left.map(err => ObservedFamilyError.Whitening(err.toString))
    def project(qF: Option[DMat], matrix: DMat): DMat =
      qF match
        case None => matrix
        case Some(q) => matrix - q * (q.t * matrix)
    val nuisanceBasis: Either[ObservedFamilyError, (Option[DMat], Int)] =
      nuisance match
        case None => Right((None, 0))
        case Some(f) =>
          whiten(f).map { wf =>
            val qr = wf.qr(QROptions(pivoting = QRPivoting.Column, rankTolerance = Some(1e-10)))
            val rank = qr.diagnostics.rank.getOrElse(wf.cols)
            (Some(qr.q.slice(0, rows, 0, rank)), rank)
          }
    nuisanceBasis.flatMap { case (qF, nuisanceRank) =>
      val scale = new Array[Double](family.jetComponents)
      val results = Vector.newBuilder[ObservedFamilyPoint]
      var failure: Option[ObservedFamilyError] = None
      var index = 0
      while index < points.length && failure.isEmpty do
        val point = points(index)
        family.scaleJetInto(family.libraryNormalization, point, scale)
        val direct = term.convolve(family.toHrf(point), frame, precision = precision).data
        val directUnnormalised = Mat.unsafe(direct.rows, direct.cols, direct.data.map(_ / scale(JetLayout.Value)))
        val approximate = expanded.designAt(point)
        (for
          wd <- whiten(toDMat(directUnnormalised))
          wa <- whiten(toDMat(approximate))
        yield (project(qF, wd), project(qF, wa))) match
          case Left(err) => failure = Some(err)
          case Right((pd, pa)) =>
            pd.svd match
              case Left(err) => failure = Some(ObservedFamilyError.Spectral(err.getMessage))
              case Right(svdDirect) =>
                val sMin = svdDirect.singularValues(svdDirect.singularValues.length - 1)
                val sMax = svdDirect.singularValues(0)
                if !(sMin >= requiredSingularValue) then
                  failure = Some(ObservedFamilyError.RankLoss(point.coordinates, sMin, requiredSingularValue))
                else
                  pa.svd match
                    case Left(err) => failure = Some(ObservedFamilyError.Spectral(err.getMessage))
                    case Right(svdApprox) =>
                      // largest principal angle between the column spaces via the singular values of Ud' Ua
                      val k = pd.cols
                      val ud = svdDirect.u.slice(0, rows, 0, k)
                      val ua = svdApprox.u.slice(0, rows, 0, k)
                      (ud.t * ua).svd match
                        case Left(err) => failure = Some(ObservedFamilyError.Spectral(err.getMessage))
                        case Right(overlap) =>
                          val cosMin = math.min(1.0, overlap.singularValues(overlap.singularValues.length - 1))
                          val projectorError = math.sqrt(math.max(0.0, 1.0 - cosMin * cosMin))
                          val designError = frobenius(pd - pa) / frobenius(pd)
                          results += ObservedFamilyPoint(point.coordinates, projectorError, designError, sMin, sMax / sMin)
        index += 1
      failure match
        case Some(err) => Left(err)
        case None =>
          val pts = results.result()
          Right(
            ObservedFamilyCertificate(
              points = pts,
              maxProjectorError = pts.map(_.projectorError).maxOption.getOrElse(0.0),
              maxDesignError = pts.map(_.designError).maxOption.getOrElse(0.0),
              minSmallestSingularValue = pts.map(_.smallestSingularValue).minOption.getOrElse(0.0),
              requiredSingularValue = requiredSingularValue,
              nuisanceRank = nuisanceRank
            )
          )
    }

  private def toDMat(mat: Mat): DMat =
    val b = DMat.newBuilder(mat.rows, mat.cols)
    var i = 0
    while i < mat.data.length do
      b.writeLinear(i, mat.data(i))
      i += 1
    b.result()

  private def frobenius(m: DMat): Double =
    var acc = 0.0
    m.foreachRowMajor(v => acc += v * v)
    math.sqrt(acc)
