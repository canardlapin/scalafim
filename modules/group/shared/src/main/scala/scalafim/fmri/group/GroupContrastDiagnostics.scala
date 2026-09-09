package scalafim.fmri.group

import gale.linalg.DMat
import scalafim.dataset.SubjectId

/** Design-only contribution of one independent subject to an equal-subject OLS
  * contrast. Shares use an identity working covariance, not observed precision.
  */
final case class GroupSubjectContrastDiagnostic private[group] (
    subject: SubjectId,
    leverage: Double,
    standardizedInfluence: Double,
    workingVarianceShare: Double
)

/** A working-model information diagnostic, not permission to use a t reference.
  * A finite df does not certify calibration under heteroskedasticity or skewness.
  */
enum GroupCr2WorkingInformation:
  case Available(satterthwaiteDf: Double)
  case Unavailable(detail: String)

/** Immutable, subject-bound review of a named scalar OLS contrast. Construction
  * binds the supplied row order to subject IDs; no positional join is performed
  * later. This review neither reads responses nor supplies p-values.
  */
final class GroupContrastDiagnostics private (
    val subjectAxis: SubjectAxis,
    val contrast: GroupContrast,
    val termNames: Vector[String],
    val rows: Vector[GroupSubjectContrastDiagnostic],
    val residualDf: Int,
    val workingEffectiveContributors: Double,
    val cr2WorkingInformation: GroupCr2WorkingInformation
):
  val methodId: String = "group-contrast-diagnostics/ols-identity/v1"
  def nSubjects: Int = subjectAxis.length
  def maxLeverage: Double = rows.map(_.leverage).max
  def maxWorkingVarianceShare: Double = rows.map(_.workingVarianceShare).max

object GroupContrastDiagnostics:
  /** Review a full-rank design with more independent subjects than terms.
    * `subjects(i)` must identify row i of `design`. The caller establishes that
    * binding; duplicate IDs and count mismatches are rejected.
    *
    * Signed influences have squared norm one. Their squares are the subject
    * variance shares under an identity working model, so they are unchanged by
    * an invertible design recoding that preserves the contrast. They are not
    * inverse-SE weights, residuals, Cook distances or independent-subject counts.
    */
  def review(subjects: Vector[SubjectId], design: GroupDesign,
      contrast: GroupContrast): Either[GroupError, GroupContrastDiagnostics] =
    for
      axis <- SubjectAxis.from(subjects)
      _ <- if axis.length == design.subjects then Right(())
        else Left(GroupError.subjectMismatch(axis.length, design.subjects))
      weights <- contrast.weightVector(design.termNames)
      prepared <- GroupGlm.prepare(design.matrix)
      result <- compile(axis, design, contrast, weights, prepared)
    yield result

  private def compile(axis: SubjectAxis, design: GroupDesign, contrast: GroupContrast,
      weights: Array[Double], prepared: GroupGlm.Prepared): Either[GroupError, GroupContrastDiagnostics] =
    val q = prepared.basis
    val transform = prepared.toOriginal
    val n = q.rows
    val p = q.cols
    // Normalize before the change of coordinates and again before squaring.
    // Extreme contrast units must not overflow a diagnostic that ignores scale.
    val scale = weights.iterator.map(math.abs).max
    val basisContrast = Array.tabulate(p) { k =>
      var value = 0.0
      var j = 0
      while j < p do
        value += (weights(j) / scale) * transform(j, k)
        j += 1
      value
    }
    val basisScale = basisContrast.iterator.map(math.abs).max
    if !basisScale.isFinite || basisScale <= 0.0 then
      Left(GroupError.NumericalFailure("contrast influence is not representable in the prepared design"))
    else
      var norm2 = 0.0
      var k = 0
      while k < p do
        basisContrast(k) /= basisScale
        norm2 += basisContrast(k) * basisContrast(k)
        k += 1
      val norm = math.sqrt(norm2)
      val influence = new Array[Double](n)
      val leverage = new Array[Double](n)
      val tolerance = 64.0 * n * math.ulp(1.0)
      var valid = true
      var i = 0
      while i < n do
        k = 0
        while k < p do
          influence(i) += q(i, k) * (basisContrast(k) / norm)
          leverage(i) += q(i, k) * q(i, k)
          k += 1
        valid &&= influence(i).isFinite && leverage(i).isFinite && leverage(i) <= 1.0 + tolerance
        leverage(i) = math.min(1.0, leverage(i))
        i += 1
      if !valid then Left(GroupError.NumericalFailure("non-finite or invalid group leverage/influence"))
      else
        // Remove only the floating-point drift from Q's unit-length projection.
        val influenceNorm = math.sqrt(influence.iterator.map(x => x*x).sum)
        val rows = Vector.tabulate(n) { row =>
          val a = influence(row) / influenceNorm
          GroupSubjectContrastDiagnostic(axis.subjects(row), leverage(row), a, a*a)
        }
        val concentration = rows.iterator.map(r => r.workingVarianceShare*r.workingVarianceShare).sum
        val information = if leverage.exists(h => 1.0-h <= tolerance) then
          GroupCr2WorkingInformation.Unavailable("a subject has unit or numerically unit leverage; its residual cannot identify CR2 uncertainty")
        else workingInformation(q, rows)
        Right(new GroupContrastDiagnostics(axis, contrast, design.termNames, rows,
          n-p, 1.0/concentration, information))

  private def workingInformation(q: DMat,
      rows: Vector[GroupSubjectContrastDiagnostic]): GroupCr2WorkingInformation =
    // For singleton subjects and identity target, CR2 is HC2. With M=I-QQ',
    // B=M diag(a_i^2/(1-h_i)) M, df=tr(B)^2/tr(B^2).
    // Accumulate nonnegative squared terms, avoiding cancellation in the
    // low-rank trace identity and avoiding a stored subjects-by-subjects matrix.
    val n = q.rows
    val d = rows.map(r => r.workingVarianceShare / (1.0-r.leverage))
    var trace = 0.0
    var traceSquare = 0.0
    var i = 0
    while i < n do
      trace += d(i) * (1.0-rows(i).leverage)
      var j = 0
      while j <= i do
        var m = if i == j then 1.0 else 0.0
        var k = 0
        while k < q.cols do
          m -= q(i,k)*q(j,k)
          k += 1
        val value = (d(i)*m)*(d(j)*m)
        traceSquare += (if i == j then value else 2.0*value)
        j += 1
      i += 1
    val df = (trace*trace)/traceSquare
    if df.isFinite && df > 0.0 then GroupCr2WorkingInformation.Available(df)
    else GroupCr2WorkingInformation.Unavailable("CR2 working information exceeds finite precision")
