package scalafim.fmri.threshold

import scalafim.linalg.DoubleMatrix

enum CorrectionPolicy:
  case WestfallYoungStepDown
  case MaxTSingleStep

final case class AdjustedTest(testIndex: Int, score: Double, adjustedP: AdjustedP, rejected: Boolean):
  require(score.isFinite, "score must be finite")

  def adjustedPValue: Double =
    adjustedP.value

object MultipleTesting:
  def adjust(
    observed: Array[Double],
    nullMatrix: DoubleMatrix,
    alpha: Alpha,
    policy: CorrectionPolicy
  ): Either[ThresholdError, Vector[AdjustedTest]] =
    policy match
      case CorrectionPolicy.WestfallYoungStepDown =>
        WestfallYoung.stepDown(observed, nullMatrix, alpha)
      case CorrectionPolicy.MaxTSingleStep =>
        MaxT.singleStep(observed, nullMatrix, alpha)

object WestfallYoung:

  def stepDown(
    observed: Array[Double],
    nullMatrix: DoubleMatrix,
    alpha: Alpha
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
            val order = observed.indices.toArray.sortWith((a, b) => observed(a) > observed(b))
            val counts = new Array[Int](m)

            var b = 0
            while b < nullMatrix.rows do
              var running = Double.NegativeInfinity
              var rank = m - 1
              while rank >= 0 do
                val testIndex = order(rank)
                val value = nullMatrix(b, testIndex)
                if value > running then running = value
                if running >= observed(testIndex) then counts(rank) += 1
                rank -= 1
              b += 1

            val sortedP = new Array[Double](m)
            var previous = 0.0
            var rank = 0
            while rank < m do
              val raw = (counts(rank).toDouble + 1.0) / (nullMatrix.rows.toDouble + 1.0)
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

  def singleStep(
    observed: Array[Double],
    nullMatrix: DoubleMatrix,
    alpha: Alpha
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
            val maxNull = new Array[Double](nullMatrix.rows)
            var row = 0
            while row < nullMatrix.rows do
              var mx = Double.NegativeInfinity
              var col = 0
              while col < nullMatrix.cols do
                val value = nullMatrix(row, col)
                if value > mx then mx = value
                col += 1
              maxNull(row) = mx
              row += 1

            MaxNull.pValues(observed, maxNull).map { p =>
              val out = Vector.newBuilder[AdjustedTest]
              out.sizeHint(m)
              var i = 0
              while i < m do
                out += AdjustedTest(i, observed(i), p(i), p(i).value <= alpha.value)
                i += 1
              out.result()
            }

object MaxNull:

  def pValues(observed: Array[Double], maxNull: Array[Double]): Either[ThresholdError, Array[AdjustedP]] =
    validateObserved(observed) match
      case Left(err) => Left(err)
      case Right(()) =>
        if maxNull.isEmpty then return Left(ThresholdError.InvalidArgument("maxNull", "must be non-empty"))
        var b = 0
        while b < maxNull.length do
          if !maxNull(b).isFinite then return Left(ThresholdError.NonFiniteData("max-null distribution"))
          b += 1

        val out = new Array[AdjustedP](observed.length)
        var i = 0
        while i < observed.length do
          var count = 0
          b = 0
          while b < maxNull.length do
            if maxNull(b) >= observed(i) then count += 1
            b += 1
          out(i) = AdjustedP.unsafe((count.toDouble + 1.0) / (maxNull.length.toDouble + 1.0))
          i += 1
        Right(out)

  def pValueDoubles(observed: Array[Double], maxNull: Array[Double]): Either[ThresholdError, Array[Double]] =
    pValues(observed, maxNull).map { values =>
      val out = new Array[Double](values.length)
      var i = 0
      while i < values.length do
        out(i) = values(i).value
        i += 1
      out
    }

  def cutoff(maxNull: Array[Double], alpha: Alpha): Either[ThresholdError, ThresholdCutoff] =
    if maxNull.isEmpty then return Left(ThresholdError.InvalidArgument("maxNull", "must be non-empty"))
    var i = 0
    while i < maxNull.length do
      if !maxNull(i).isFinite then return Left(ThresholdError.NonFiniteData("max-null distribution"))
      i += 1

    val k = math.floor(alpha.value * (maxNull.length.toDouble + 1.0)).toInt
    if k < 1 then Right(ThresholdCutoff.NoRejections)
    else
      val sorted = maxNull.clone.sortWith(_ > _)
      ThresholdCutoff.inclusive(sorted(math.min(k, sorted.length) - 1))

  def threshold(maxNull: Array[Double], alpha: Alpha): Either[ThresholdError, Double] =
    cutoff(maxNull, alpha).map(_.toLegacyDouble)

private def validateObserved(observed: Array[Double]): Either[ThresholdError, Unit] =
  var i = 0
  while i < observed.length do
    if !observed(i).isFinite then return Left(ThresholdError.NonFiniteData("observed statistics"))
    i += 1
  Right(())

private def validateNull(nullMatrix: DoubleMatrix): Either[ThresholdError, Unit] =
  var row = 0
  while row < nullMatrix.rows do
    var col = 0
    while col < nullMatrix.cols do
      if !nullMatrix(row, col).isFinite then return Left(ThresholdError.NonFiniteData("null matrix"))
      col += 1
    row += 1
  Right(())
