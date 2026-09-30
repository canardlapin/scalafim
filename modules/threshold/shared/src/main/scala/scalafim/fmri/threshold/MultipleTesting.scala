package scalafim.fmri.threshold

import gale.linalg.DMat

enum CorrectionPolicy:
  case WestfallYoungStepDown
  case MaxTSingleStep

/** One adjusted test. `score` is the observed statistic as supplied, before
  * orientation; ordering and p-values refer to the oriented statistic.
  */
final case class AdjustedTest(testIndex: Int, score: Double, adjustedP: AdjustedP, rejected: Boolean):
  require(score.isFinite, "score must be finite")

  def adjustedPValue: Double =
    adjustedP.value

object MultipleTesting:
  /** Adjust `observed` against a draws-by-tests null matrix. Both are supplied
    * raw; `alternative` orients them identically before any comparison, and
    * `reference` fixes the permutation p-value convention.
    *
    * These matrix procedures are orientation-agnostic primitives: they do not
    * know whether the statistic is signed or unsigned evidence. Admissibility of
    * `alternative` for the evidence is checked where the evidence kind is known
    * (`StatisticField`, [[MaxNull.reduce]]).
    */
  def adjust(
    observed: Array[Double],
    nullMatrix: DMat,
    alpha: Alpha,
    policy: CorrectionPolicy,
    alternative: ThresholdAlternative,
    reference: NullReference
  ): Either[ThresholdError, Vector[AdjustedTest]] =
    policy match
      case CorrectionPolicy.WestfallYoungStepDown =>
        WestfallYoung.stepDown(observed, nullMatrix, alpha, alternative, reference)
      case CorrectionPolicy.MaxTSingleStep =>
        MaxT.singleStep(observed, nullMatrix, alpha, alternative, reference)

object WestfallYoung:

  /** Westfall-Young step-down over raw `observed` statistics and a raw
    * draws-by-tests null matrix, both oriented by `alternative`. Adjusted
    * p-values follow `reference`; under exact enumeration the identity action
    * must be among the rows.
    */
  def stepDown(
    observed: Array[Double],
    nullMatrix: DMat,
    alpha: Alpha,
    alternative: ThresholdAlternative,
    reference: NullReference
  ): Either[ThresholdError, Vector[AdjustedTest]] =
    validateObserved(observed) match
      case Left(err) => Left(err)
      case Right(()) =>
        val m = observed.length
        if nullMatrix.cols != m then return Left(ThresholdError.NullMatrixShape(nullMatrix.rows, nullMatrix.cols, m))
        if m == 0 then return Right(Vector.empty)
        if nullMatrix.rows <= 0 then return Left(ThresholdError.NullMatrixShape(nullMatrix.rows, nullMatrix.cols, m))
        validateNull(nullMatrix) match
          case Left(err) => Left(err)
          case Right(()) =>
            val oriented = orient(observed, alternative)
            requireIdentityRow(oriented, nullMatrix, alternative, reference) match
              case Left(err) => return Left(err)
              case Right(()) => ()
            val order = oriented.indices.toArray.sortWith((a, b) => oriented(a) > oriented(b))
            val counts = new Array[Int](m)

            var b = 0
            while b < nullMatrix.rows do
              var running = Double.NegativeInfinity
              var rank = m - 1
              while rank >= 0 do
                val testIndex = order(rank)
                val value = alternative.applyTo(nullMatrix(b, testIndex))
                if value > running then running = value
                if running >= oriented(testIndex) then counts(rank) += 1
                rank -= 1
              b += 1

            val sortedP = new Array[Double](m)
            var previous = 0.0
            var rank = 0
            while rank < m do
              val raw = reference.pValue(counts(rank), nullMatrix.rows)
              val adj = math.max(raw, previous)
              sortedP(rank) = adj
              previous = adj
              rank += 1

            val pByOriginal = new Array[Double](m)
            rank = 0
            while rank < m do
              pByOriginal(order(rank)) = sortedP(rank)
              rank += 1

            val out = Vector.newBuilder[AdjustedTest]
            out.sizeHint(m)
            var i = 0
            while i < m do
              val adjusted = AdjustedP.unsafe(pByOriginal(i))
              out += AdjustedTest(i, observed(i), adjusted, adjusted.value <= alpha.value)
              i += 1
            Right(out.result())

object MaxT:

  /** Single-step maxT over raw `observed` statistics and a raw draws-by-tests
    * null matrix, both oriented by `alternative`. Callers whose draws arrive one
    * at a time should use [[MaxNull.reduce]] instead of materializing the matrix.
    */
  def singleStep(
    observed: Array[Double],
    nullMatrix: DMat,
    alpha: Alpha,
    alternative: ThresholdAlternative,
    reference: NullReference
  ): Either[ThresholdError, Vector[AdjustedTest]] =
    validateObserved(observed) match
      case Left(err) => Left(err)
      case Right(()) =>
        val m = observed.length
        if nullMatrix.cols != m then return Left(ThresholdError.NullMatrixShape(nullMatrix.rows, nullMatrix.cols, m))
        if m == 0 then return Right(Vector.empty)
        if nullMatrix.rows <= 0 then return Left(ThresholdError.NullMatrixShape(nullMatrix.rows, nullMatrix.cols, m))
        validateNull(nullMatrix) match
          case Left(err) => Left(err)
          case Right(()) =>
            requireIdentityRow(orient(observed, alternative), nullMatrix, alternative, reference) match
              case Left(err) => return Left(err)
              case Right(()) => ()
            val maxNull = new Array[Double](nullMatrix.rows)
            var row = 0
            while row < nullMatrix.rows do
              var mx = Double.NegativeInfinity
              var col = 0
              while col < nullMatrix.cols do
                val value = alternative.applyTo(nullMatrix(row, col))
                if value > mx then mx = value
                col += 1
              maxNull(row) = mx
              row += 1

            val distribution = MaxNullDistribution.unsafe(maxNull, alternative, reference)
            MaxNull.pValues(observed, distribution).map { p =>
              val out = Vector.newBuilder[AdjustedTest]
              out.sizeHint(m)
              var i = 0
              while i < m do
                out += AdjustedTest(i, observed(i), p(i), p(i).value <= alpha.value)
                i += 1
              out.result()
            }

/** Per-draw maxima of an oriented null field.
  *
  * The distribution carries the alternative that oriented its draws and the
  * reference convention of its draw set, so observed statistics are always
  * oriented the same way before they are compared with it.
  */
final class MaxNullDistribution private (
    maxima: Array[Double],
    val alternative: ThresholdAlternative,
    val reference: NullReference
):
  def draws: Int =
    maxima.length

  def maximum(index: Int): Double =
    maxima(index)

  def toArray: Array[Double] =
    maxima.clone

object MaxNullDistribution:

  /** Admit per-draw maxima that the caller has already oriented by
    * `alternative` and whose draw set follows `reference`. This is a trust
    * boundary: neither claim can be checked from the maxima. Prefer
    * [[MaxNull.reduce]], which orients raw draws itself.
    */
  def fromOrientedMaxima(
    maxima: Array[Double],
    alternative: ThresholdAlternative,
    reference: NullReference
  ): Either[ThresholdError, MaxNullDistribution] =
    if maxima.isEmpty then return Left(ThresholdError.InvalidArgument("maxNull", "must be non-empty"))
    var i = 0
    while i < maxima.length do
      if !maxima(i).isFinite then return Left(ThresholdError.NonFiniteData("max-null distribution"))
      i += 1
    Right(new MaxNullDistribution(maxima.clone, alternative, reference))

  private[threshold] def unsafe(
    maxima: Array[Double],
    alternative: ThresholdAlternative,
    reference: NullReference
  ): MaxNullDistribution =
    new MaxNullDistribution(maxima, alternative, reference)

object MaxNull:

  /** Stream raw mask-space draws into per-draw oriented maxima.
    *
    * Each draw is requested exactly once and reduced before the next is
    * requested, so memory is one draw plus one maximum per draw, never a
    * draws-by-tests matrix. Draw values are oriented with `alternative`, which
    * must be admissible for `orientation`; unsigned evidence must be
    * non-negative. The reference convention comes from the draw set.
    */
  def reduce(
    nullDraw: NullDraw,
    fieldSize: Int,
    alternative: ThresholdAlternative,
    orientation: EvidenceOrientation
  ): Either[ThresholdError, MaxNullDistribution] =
    if fieldSize <= 0 then return Left(ThresholdError.InvalidArgument("fieldSize", "must be positive"))
    alternative.validate(orientation) match
      case Left(err) => return Left(err)
      case Right(()) => ()
    val draws = nullDraw.nPermutations.value
    val maxima = new Array[Double](draws)
    var b = 0
    while b < draws do
      nullDraw.draw(b) match
        case Left(err) => return Left(err)
        case Right(raw) =>
          if raw.length != fieldSize then
            return Left(ThresholdError.ShapeMismatch("null draw", fieldSize.toString, raw.length.toString))
          var mx = Double.NegativeInfinity
          var i = 0
          while i < raw.length do
            val rawValue = raw(i)
            if !rawValue.isFinite then return Left(ThresholdError.NonFiniteData("null draw"))
            if orientation == EvidenceOrientation.Unsigned && rawValue < 0.0 then
              return Left(ThresholdError.NegativeUnsignedEvidence(i, rawValue))
            val value = alternative.applyTo(rawValue)
            if value > mx then mx = value
            i += 1
          maxima(b) = mx
      b += 1
    Right(MaxNullDistribution.unsafe(maxima, alternative, nullDraw.reference))

  /** Family-wise adjusted p-values for raw `observed` statistics, oriented by
    * the distribution's alternative and computed with its reference convention.
    */
  def pValues(observed: Array[Double], nulls: MaxNullDistribution): Either[ThresholdError, Array[AdjustedP]] =
    validateObserved(observed) match
      case Left(err) => Left(err)
      case Right(()) =>
        val reference = nulls.reference
        val draws = nulls.draws
        val out = new Array[AdjustedP](observed.length)
        var i = 0
        while i < observed.length do
          val score = nulls.alternative.applyTo(observed(i))
          var count = 0
          var b = 0
          while b < draws do
            if nulls.maximum(b) >= score then count += 1
            b += 1
          if count < reference.minimumCount then return Left(ThresholdError.MissingIdentityAction(i))
          out(i) = AdjustedP.unsafe(reference.pValue(count, draws))
          i += 1
        Right(out)

  def pValueDoubles(observed: Array[Double], nulls: MaxNullDistribution): Either[ThresholdError, Array[Double]] =
    pValues(observed, nulls).map { values =>
      val out = new Array[Double](values.length)
      var i = 0
      while i < values.length do
        out(i) = values(i).value
        i += 1
      out
    }

  /** The family-wise cutoff at `alpha`, on the oriented scale: a raw score
    * `s` is rejected when the cutoff rejects `nulls.alternative.applyTo(s)`.
    * Decisions agree with `pValues(...) <= alpha` for every admissible score.
    */
  def cutoff(nulls: MaxNullDistribution, alpha: Alpha): Either[ThresholdError, ThresholdCutoff] =
    val reference = nulls.reference
    val draws = nulls.draws

    // Walk attainable exceedance counts with the same division used by
    // pValues. This avoids changing the decision at floating-point alpha
    // boundaries through a multiply-and-floor rearrangement.
    var count = reference.minimumCount
    while count < draws && reference.pValue(count, draws) <= alpha.value do count += 1

    if count == reference.minimumCount then Right(ThresholdCutoff.NoRejections)
    else
      val sorted = nulls.toArray.sortWith(_ > _)
      // A score is rejected iff at most count - 1 null maxima reach it, so
      // equality with the count-th descending maximum is never rejected. The
      // cutoff must be strict.
      ThresholdCutoff.exclusive(sorted(count - 1))

/** Under exact enumeration the identity action is one of the rows, so some
  * row oriented by `alternative` equals the oriented observed statistics
  * exactly. The per-count condition alone would accept a mislabelled Monte
  * Carlo sample whose draws happen to exceed every observed statistic.
  */
private def requireIdentityRow(
    oriented: Array[Double],
    nullMatrix: DMat,
    alternative: ThresholdAlternative,
    reference: NullReference
): Either[ThresholdError, Unit] =
  reference match
    case NullReference.MonteCarlo => Right(())
    case NullReference.ExactEnumeration =>
      var row = 0
      while row < nullMatrix.rows do
        var col = 0
        while col < oriented.length && alternative.applyTo(nullMatrix(row, col)) == oriented(col) do col += 1
        if col == oriented.length then return Right(())
        row += 1
      Left(ThresholdError.MissingIdentityRow)

private def orient(values: Array[Double], alternative: ThresholdAlternative): Array[Double] =
  val out = new Array[Double](values.length)
  var i = 0
  while i < values.length do
    out(i) = alternative.applyTo(values(i))
    i += 1
  out

private def validateObserved(observed: Array[Double]): Either[ThresholdError, Unit] =
  var i = 0
  while i < observed.length do
    if !observed(i).isFinite then return Left(ThresholdError.NonFiniteData("observed statistics"))
    i += 1
  Right(())

private def validateNull(nullMatrix: DMat): Either[ThresholdError, Unit] =
  var row = 0
  while row < nullMatrix.rows do
    var col = 0
    while col < nullMatrix.cols do
      if !nullMatrix(row, col).isFinite then return Left(ThresholdError.NonFiniteData("null matrix"))
      col += 1
    row += 1
  Right(())
