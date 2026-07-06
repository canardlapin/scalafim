package scalafim.fmri.threshold

object ScoreSet:

  final case class DiffuseScore(score: Double, effectiveN: Double)

  final case class OmnibusScore(
      score: Double,
      diffuse: DiffuseScore,
      softMax: Double,
      kappa: Option[Kappa]
  )

  def softMax(
    indices: Array[Int],
    z: Array[Double],
    priors: PriorWeights,
    kappa: Kappa
  ): Either[ThresholdError, Double] =
    validate(indices, z, priors) match
      case Left(err) => Left(err)
      case Right(()) =>
        if indices.isEmpty then Right(Double.NegativeInfinity)
        else
          val k = kappa.value
          var aMax = Double.NegativeInfinity
          var i = 0
          while i < indices.length do
            val idx = indices(i)
            val w = priors(idx)
            if w > 0.0 then
              val a = k * z(idx)
              if a > aMax then aMax = a
            i += 1

          if aMax.isNegInfinity then Right(Double.NegativeInfinity)
          else
            var sum = 0.0
            i = 0
            while i < indices.length do
              val idx = indices(i)
              val w = priors(idx)
              if w > 0.0 then sum += w * math.exp(k * z(idx) - aMax)
              i += 1
            if sum <= 0.0 then Right(Double.NegativeInfinity)
            else Right(aMax + math.log(sum))

  def diffuse(
    indices: Array[Int],
    z: Array[Double],
    priors: PriorWeights
  ): Either[ThresholdError, DiffuseScore] =
    validate(indices, z, priors) match
      case Left(err) => Left(err)
      case Right(()) =>
        if indices.isEmpty then Right(DiffuseScore(Double.NegativeInfinity, 0.0))
        else
          var sumWZ = 0.0
          var sumW = 0.0
          var sumW2 = 0.0
          var i = 0
          while i < indices.length do
            val idx = indices(i)
            val w = priors(idx)
            sumWZ += w * z(idx)
            sumW += w
            sumW2 += w * w
            i += 1
          if sumW2 <= 0.0 then Right(DiffuseScore(0.0, 0.0))
          else Right(DiffuseScore(sumWZ / math.sqrt(sumW2), (sumW * sumW) / sumW2))

  def omnibus(
    indices: Array[Int],
    z: Array[Double],
    priors: PriorWeights,
    kappas: Vector[Kappa]
  ): Either[ThresholdError, OmnibusScore] =
    if kappas.isEmpty then return Left(ThresholdError.InvalidArgument("kappas", "must be non-empty"))
    diffuse(indices, z, priors) match
      case Left(err) => Left(err)
      case Right(d) =>
        if indices.isEmpty then Right(OmnibusScore(Double.NegativeInfinity, d, Double.NegativeInfinity, None))
        else
          var den1 = 0.0
          var i = 0
          while i < indices.length do
            den1 += priors(indices(i))
            i += 1
          if den1 <= 0.0 then Right(OmnibusScore(d.score, d, Double.NegativeInfinity, None))
          else
            val logDen1 = math.log(den1)
            var best = Double.NegativeInfinity
            var bestK: Option[Kappa] = None
            var kIndex = 0
            while kIndex < kappas.length do
              val kappa = kappas(kIndex)
              softMax(indices, z, priors, kappa) match
                case Left(err) => return Left(err)
                case Right(logSum) =>
                  val s = (logSum - logDen1) / kappa.value
                  if s > best then
                    best = s
                    bestK = Some(kappa)
              kIndex += 1
            Right(OmnibusScore(math.max(d.score, best), d, best, bestK))

  private def validate(
    indices: Array[Int],
    z: Array[Double],
    priors: PriorWeights
  ): Either[ThresholdError, Unit] =
    if z.length != priors.length then
      return Left(ThresholdError.ShapeMismatch("z/priors", z.length.toString, priors.length.toString))
    var i = 0
    while i < indices.length do
      val idx = indices(i)
      if idx < 0 || idx >= z.length then return Left(ThresholdError.IndexOutOfBounds(idx, z.length))
      if !z(idx).isFinite then return Left(ThresholdError.NonFiniteData("score values"))
      i += 1
    Right(())
