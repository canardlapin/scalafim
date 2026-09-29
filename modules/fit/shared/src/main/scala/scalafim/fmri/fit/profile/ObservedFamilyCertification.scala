package scalafim.fmri.fit.profile

import gale.linalg.{DMat, QROptions, QRPivoting}
import scalafim.fmri.ar.{WhiteningPlan, WhiteningTransform}
import scalafim.fmri.design.event.EventTerm
import scalafim.fmri.design.hrf.ExpandedConditionDesign
import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{JetLayout, ShapePoint}
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.FitPlan
import scalafim.fmri.fit.TaskBasisStructure

enum ObservedFamilyError:
  case Spectral(detail: String)
  case RankLoss(coordinates: Vector[Double], smallestSingularValue: Double, required: Double)
  case Whitening(detail: String)
  case Admission(detail: String)

  def message: String =
    this match
      case Spectral(detail) => s"certification factorisation failed: $detail"
      case RankLoss(coords, s, required) =>
        s"the direct condition design at ${coords.mkString("(", ", ", ")")} has smallest singular value $s below the admitted margin $required; the cell changes rank"
      case Whitening(detail) => s"whitening failed: $detail"
      case Admission(detail) => s"observed-family admission refused: $detail"

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
    conditionNumber: Double,
    approximateSmallestSingularValue: Double,
    approximateConditionNumber: Double)

final case class ObservedFamilyCertificate(
    points: Vector[ObservedFamilyPoint],
    maxProjectorError: Double,
    maxDesignError: Double,
    minSmallestSingularValue: Double,
    requiredSingularValue: Double,
    nuisanceRank: Int):
  def worst: Option[ObservedFamilyPoint] = points.maxByOption(_.projectorError)

/** Declared margins for empirical, held-out observed-family evidence.  These
  * are deliberately not a claim of a uniform cell certificate.
  */
final case class ObservedFamilyRequirements(
    maximumProjectorError: Double,
    maximumConditionNumber: Double,
    requiredSingularValue: Double)

/** A certificate admitted against one concrete design geometry.  Its
  * constructor is private so a certificate obtained for a same-sized design
  * cannot be paired with a different geometry by a caller.
  */
final class ObservedFamilyAdmission private[profile] (
    val certificate: ObservedFamilyCertificate,
    val requirements: ObservedFamilyRequirements,
    private val geometry: String,
    private val planGeometry: Option[String],
    private val basisProvenance: String):

  def fingerprint: String = geometry

  private[profile] def admits(plan: FitPlan, structure: TaskBasisStructure, basis: scalafim.fmri.design.hrf.HrfKernelBasis): Either[ObservedFamilyError, Unit] =
    ObservedFamilyCertification.planGeometry(plan, structure).flatMap { actual =>
      if basisProvenance != basis.provenance.canonical then Left(ObservedFamilyError.Admission("the certificate belongs to a different basis/family provenance"))
      else if planGeometry.contains(actual) then Right(())
      else Left(ObservedFamilyError.Admission("the certificate belongs to a different retained task-column geometry"))
    }

  private[profile] def admits(expanded: ExpandedConditionDesign, term: EventTerm, frame: SamplingFrame, precision: Seconds, whitening: Option[WhiteningPlan], nuisance: Option[DMat]): Either[ObservedFamilyError, Unit] =
    val actual = ObservedFamilyAdmission.geometry(expanded, term, frame, precision, whitening, nuisance)
    if geometry == actual then Right(())
    else Left(ObservedFamilyError.Admission("the certificate belongs to a different observed design, frame, whitening, nuisance, or precision geometry"))

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
    if points.isEmpty then return Left(ObservedFamilyError.Admission("held-out evidence must contain at least one shape"))
    if !requiredSingularValue.isFinite || requiredSingularValue <= 0.0 then return Left(ObservedFamilyError.Admission("required singular-value margin must be finite and positive"))
    if frame.blockLens.sum != expanded.rows || term.onsets.length != term.durations0.length || term.onsets.length != term.blockIds0.length ||
        nuisance.exists(m => m.rows != expanded.rows) then
      return Left(ObservedFamilyError.Admission("frame, term, and nuisance rows must match the observed design"))
    if points.exists(p => p.coordinates.length != expanded.basis.family.dimension || p.coordinates.exists(!_.isFinite) || expanded.basis.family.chart.point(p.coordinates*).isLeft) then
      return Left(ObservedFamilyError.Admission("held-out shapes must be finite and inside the family chart"))
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
            if pd.rows < pd.cols || pa.rows < pa.cols then
              failure = Some(ObservedFamilyError.Admission("held-out projected designs need at least as many rows as condition columns"))
            else pd.svd match
              case Left(err) => failure = Some(ObservedFamilyError.Spectral(err.getMessage))
              case Right(svdDirect) =>
                if svdDirect.singularValues.length != pd.cols then
                  failure = Some(ObservedFamilyError.Admission("direct held-out design has no singular values"))
                else
                  val sMin = svdDirect.singularValues(svdDirect.singularValues.length - 1)
                  val sMax = svdDirect.singularValues(0)
                  if !(sMin >= requiredSingularValue) then
                    failure = Some(ObservedFamilyError.RankLoss(point.coordinates, sMin, requiredSingularValue))
                  else pa.svd match
                    case Left(err) => failure = Some(ObservedFamilyError.Spectral(err.getMessage))
                    case Right(svdApprox) =>
                      if svdApprox.singularValues.length != pa.cols then
                        failure = Some(ObservedFamilyError.Admission("approximated held-out design has no singular values"))
                      else
                        val aMin = svdApprox.singularValues(svdApprox.singularValues.length - 1)
                        val aMax = svdApprox.singularValues(0)
                        if !(aMin >= requiredSingularValue) then
                          failure = Some(ObservedFamilyError.RankLoss(point.coordinates, aMin, requiredSingularValue))
                        else
                          // largest principal angle between the column spaces via the singular values of Ud' Ua
                          val k = pd.cols
                          val ud = svdDirect.u.slice(0, rows, 0, k)
                          val ua = svdApprox.u.slice(0, rows, 0, k)
                          (ud.t * ua).svd match
                            case Left(err) => failure = Some(ObservedFamilyError.Spectral(err.getMessage))
                            case Right(overlap) =>
                              if overlap.singularValues.length != pd.cols then failure = Some(ObservedFamilyError.Admission("held-out subspace overlap is rank deficient"))
                              else
                                val cosMin = math.min(1.0, overlap.singularValues(overlap.singularValues.length - 1))
                                val projectorError = math.sqrt(math.max(0.0, 1.0 - cosMin * cosMin))
                                val designError = frobenius(pd - pa) / frobenius(pd)
                                results += ObservedFamilyPoint(point.coordinates, projectorError, designError, sMin, sMax / sMin, aMin, aMax / aMin)
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

  /** Certify and bind empirical held-out evidence to the actual condition
    * preparation geometry.  This does not infer uniform-cell coverage.
    */
  def admitForCondition(
      plan: FitPlan,
      structure: TaskBasisStructure,
      expanded: ExpandedConditionDesign,
      term: EventTerm,
      frame: SamplingFrame,
      precision: Seconds,
      whitening: Option[WhiteningPlan],
      nuisance: Option[DMat],
      points: Vector[ShapePoint],
      requirements: ObservedFamilyRequirements
  ): Either[ObservedFamilyError, ObservedFamilyAdmission] =
    if whitening.nonEmpty then return Left(ObservedFamilyError.Admission("ordinary condition retention does not support whitening; use the compact condition entry point"))
    validateRequirements(requirements).flatMap { _ =>
      validatePlanNuisance(plan, structure, nuisance).flatMap { _ => certify(expanded, term, frame, precision, whitening, nuisance, points, requirements.requiredSingularValue).flatMap { certificate =>
        validateCertificate(certificate, requirements).flatMap { _ =>
          planGeometry(plan, structure).flatMap { actualPlanGeometry =>
            if !sameExpandedTaskGeometry(actualPlanGeometry, expanded) then
              Left(ObservedFamilyError.Admission("compiled plan task columns differ from the certified expanded condition design"))
            else Right(new ObservedFamilyAdmission(
            certificate,
            requirements,
            ObservedFamilyAdmission.geometry(expanded, term, frame, precision, whitening, nuisance),
            Some(actualPlanGeometry),
            expanded.basis.provenance.canonical
          ))
          }
        }
      }}
    }

  def admitForCompact(
      expanded: ExpandedConditionDesign,
      term: EventTerm,
      frame: SamplingFrame,
      precision: Seconds,
      whitening: Option[WhiteningPlan],
      nuisance: Option[DMat],
      points: Vector[ShapePoint],
      requirements: ObservedFamilyRequirements
  ): Either[ObservedFamilyError, ObservedFamilyAdmission] =
    validateRequirements(requirements).flatMap { _ =>
      certify(expanded, term, frame, precision, whitening, nuisance, points, requirements.requiredSingularValue).flatMap { certificate =>
        validateCertificate(certificate, requirements).map { _ =>
          new ObservedFamilyAdmission(certificate, requirements, ObservedFamilyAdmission.geometry(expanded, term, frame, precision, whitening, nuisance), None, expanded.basis.provenance.canonical)
        }
      }
    }

  private def validateRequirements(requirements: ObservedFamilyRequirements): Either[ObservedFamilyError, Unit] =
    if !requirements.maximumProjectorError.isFinite || requirements.maximumProjectorError < 0.0 ||
        !requirements.maximumConditionNumber.isFinite || requirements.maximumConditionNumber < 1.0 ||
        !requirements.requiredSingularValue.isFinite || requirements.requiredSingularValue <= 0.0 then
      Left(ObservedFamilyError.Admission("admission margins must be finite; projector error >= 0, condition number >= 1, and singular value > 0"))
    else Right(())

  private[profile] def planGeometry(plan: FitPlan, structure: TaskBasisStructure): Either[ObservedFamilyError, String] =
    plan.model.designSchema.toRight(ObservedFamilyError.Admission("compiled plan has no structural design schema")).flatMap { schema =>
      val matrix = plan.model.designMatrix
      val ids = schema.coefficientAxis.columnIds
      val columns = structure.conditions.flatten.map(id => id -> ids.indexOf(id))
      columns.find(_._2 < 0) match
        case Some((id, _)) => Left(ObservedFamilyError.Admission(s"retained task column '${id.value}' is absent from the compiled plan"))
        case None =>
          val values = Vector.newBuilder[String]
          val identities = Vector.newBuilder[String]
          columns.foreach { case (id, column) =>
            identities += framed(id.value)
            var row = 0
            while row < matrix.rows do
              values += java.lang.Double.toHexString(matrix(row, column))
              row += 1
          }
          Right(s"plan=${plan.designFingerprint.map(_.value).getOrElse("")}|structure=${structure.conditionCount}:${structure.basisSize}|ids=${identities.result().mkString}|values=${values.result().mkString}")
    }

  private def validatePlanNuisance(plan: FitPlan, structure: TaskBasisStructure, nuisance: Option[DMat]): Either[ObservedFamilyError, Unit] =
    plan.model.designSchema.toRight(ObservedFamilyError.Admission("compiled plan has no structural design schema")).flatMap { schema =>
      val matrix = plan.model.designMatrix
      val taskIndices = structure.columnIds.map(schema.coefficientAxis.columnIds.indexOf)
      if taskIndices.exists(_ < 0) then Left(ObservedFamilyError.Admission("retained task columns are absent from the compiled plan"))
      else
        val remaining = matrix.cols - taskIndices.length
        nuisance match
          case None if remaining == 0 => Right(())
          case None => Left(ObservedFamilyError.Admission("compiled-plan nuisance columns must be included in the certified geometry"))
          case Some(actual) if actual.rows != matrix.rows || actual.cols != remaining => Left(ObservedFamilyError.Admission("certified nuisance geometry does not match the compiled plan"))
          case Some(actual) =>
            val nonTask = (0 until matrix.cols).filterNot(taskIndices.toSet)
            var mismatch = false
            var row = 0
            while row < matrix.rows && !mismatch do
              var column = 0
              while column < remaining && !mismatch do
                mismatch = java.lang.Double.doubleToLongBits(actual(row, column)) != java.lang.Double.doubleToLongBits(matrix(row, nonTask(column)))
                column += 1
              row += 1
            if mismatch then Left(ObservedFamilyError.Admission("certified nuisance values differ from the compiled plan")) else Right(())
    }

  private def sameExpandedTaskGeometry(planGeometry: String, expanded: ExpandedConditionDesign): Boolean =
    val values = Vector.newBuilder[String]
    var condition = 0
    while condition < expanded.conditionCount do
      var basisIndex = 0
      while basisIndex < expanded.rank do
        var row = 0
        while row < expanded.rows do
          values += java.lang.Double.toHexString(expanded.term.data(row, expanded.column(condition, basisIndex)))
          row += 1
        basisIndex += 1
      condition += 1
    val marker = "|values="
    planGeometry.substring(planGeometry.indexOf(marker) + marker.length) == values.result().mkString

  private def framed(value: String): String = s"${value.length}:$value;"

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

  private def validateCertificate(certificate: ObservedFamilyCertificate, requirements: ObservedFamilyRequirements): Either[ObservedFamilyError, Unit] =
    if certificate.points.isEmpty || certificate.points.exists(p => !p.projectorError.isFinite || !p.designError.isFinite || !p.smallestSingularValue.isFinite || !p.conditionNumber.isFinite || !p.approximateSmallestSingularValue.isFinite || !p.approximateConditionNumber.isFinite) then
      Left(ObservedFamilyError.Admission("certificate must contain finite nonempty held-out evidence"))
    else if certificate.requiredSingularValue != requirements.requiredSingularValue || certificate.minSmallestSingularValue < requirements.requiredSingularValue then
      Left(ObservedFamilyError.Admission("certificate does not meet the declared direct-rank margin"))
    else if certificate.maxProjectorError > requirements.maximumProjectorError then
      Left(ObservedFamilyError.Admission("certificate exceeds the declared projector-error margin"))
    else if certificate.points.exists(p => p.conditionNumber > requirements.maximumConditionNumber || p.approximateConditionNumber > requirements.maximumConditionNumber) then
      Left(ObservedFamilyError.Admission("certificate exceeds the declared conditioning margin"))
    else Right(())

object ObservedFamilyAdmission:
  private[profile] def geometry(expanded: ExpandedConditionDesign, term: EventTerm, frame: SamplingFrame, precision: Seconds, whitening: Option[WhiteningPlan], nuisance: Option[DMat]): String =
    val drives = term.onsets.zip(term.durations0).zip(term.blockIds0).map { case ((onset, duration), run) => s"${onset.value},${duration.value},$run" }.mkString(";")
    val rows = s"${frame.blockLens.mkString(",")}/${frame.tr.map(_.value).mkString(",")}/${frame.startTime.map(_.value).mkString(",")}"
    val expandedValues = expanded.term.data.data.mkString(",")
    val eventWeights = term.designMatrix(dropEmpty = false).data.data.mkString(",")
    val w = whitening.map(p => s"${p.segments.mkString(",")}|${p.coefficients.mkString(",")}|${p.exactFirstAr1}|${p.method}").getOrElse("none")
    val n = nuisance.map { m =>
      val values = new Array[Double](m.rows * m.cols)
      m.copyRowMajorTo(values)
      s"${m.rows}x${m.cols}:${values.mkString(",")}"
    }.getOrElse("none")
    s"basis=${expanded.basis.provenance.canonical}|drives=$drives|event-weights=$eventWeights|conditions=${expanded.conditions.mkString(",")}|expanded=$expandedValues|frame=$rows|precision=${precision.value}|whitening=$w|nuisance=$n"
