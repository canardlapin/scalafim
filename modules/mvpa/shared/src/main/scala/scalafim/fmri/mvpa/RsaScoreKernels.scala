package scalafim.fmri.mvpa

/** Scientific undefinedness is separate from cache admission and execution. */
private[mvpa] enum RsaScoreFailure:
  case InsufficientData(actual: Int)
  case NonFiniteInput(index: Int)
  case ZeroVariance
  case ShapeMismatch(left: Int, right: Int)

/** Shared domain scoring used by the retained-relation and existing RSA
  * consumers. Partial regression remains a Gale factorization operation.
  */
private[mvpa] object RsaScoreKernels:
  def pearson(left: IndexedSeq[Double], right: IndexedSeq[Double]): Either[RsaScoreFailure, Double] =
    if left.size != right.size then Left(RsaScoreFailure.ShapeMismatch(left.size, right.size))
    else if left.size < 2 then Left(RsaScoreFailure.InsufficientData(left.size))
    else
      var leftScale = 0.0
      var rightScale = 0.0
      var index = 0
      while index < left.size do
        if !left(index).isFinite || !right(index).isFinite then return Left(RsaScoreFailure.NonFiniteInput(index))
        leftScale = math.max(leftScale, math.abs(left(index)))
        rightScale = math.max(rightScale, math.abs(right(index)))
        index += 1
      if leftScale == 0.0 || rightScale == 0.0 then Left(RsaScoreFailure.ZeroVariance)
      else
        // Subtract the finite anchor before scaling when representable, to
        // retain small differences beside a large offset. For opposite-sign
        // extremes, scale first so subtraction cannot overflow.
        def shifted(value: Double, anchor: Double, scale: Double): Double =
          val difference = value - anchor
          if difference.isFinite then difference / scale else value / scale - anchor / scale
        var leftMean = 0.0
        var rightMean = 0.0
        index = 0
        while index < left.size do
          leftMean += shifted(left(index), left.head, leftScale)
          rightMean += shifted(right(index), right.head, rightScale)
          index += 1
        leftMean /= left.size
        rightMean /= right.size
        var cross = 0.0
        var leftSquares = 0.0
        var rightSquares = 0.0
        index = 0
        while index < left.size do
          val x = shifted(left(index), left.head, leftScale) - leftMean
          val y = shifted(right(index), right.head, rightScale) - rightMean
          cross += x * y
          leftSquares += x * x
          rightSquares += y * y
          index += 1
        if leftSquares == 0.0 || rightSquares == 0.0 then Left(RsaScoreFailure.ZeroVariance)
        else Right(math.max(-1.0, math.min(1.0, (cross / math.sqrt(leftSquares)) / math.sqrt(rightSquares))))

  def averageRanks(values: IndexedSeq[Double]): Either[RsaScoreFailure, Vector[Double]] =
    var index = 0
    while index < values.size do
      if !values(index).isFinite then return Left(RsaScoreFailure.NonFiniteInput(index))
      index += 1
    val sorted = values.zipWithIndex.sortBy(_._1)
    val output = Array.ofDim[Double](values.size)
    var start = 0
    while start < values.size do
      var end = start + 1
      while end < values.size && sorted(end)._1 == sorted(start)._1 do
        end += 1
      val rank = (start + 1.0 + end) / 2.0
      index = start
      while index < end do
        output(sorted(index)._2) = rank
        index += 1
      start = end
    Right(output.toVector)

  def spearman(left: IndexedSeq[Double], right: IndexedSeq[Double]): Either[RsaScoreFailure, Double] =
    if left.size != right.size then Left(RsaScoreFailure.ShapeMismatch(left.size, right.size))
    else if left.size < 2 then Left(RsaScoreFailure.InsufficientData(left.size))
    else
      for
        a <- averageRanks(left)
        b <- averageRanks(right)
        score <- pearson(a, b)
      yield score
