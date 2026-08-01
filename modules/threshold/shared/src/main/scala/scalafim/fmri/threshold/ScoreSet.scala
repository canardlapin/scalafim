package scalafim.fmri.threshold

enum ScoreAbsence:
  case EmptySet
  case ZeroPriorMass

  def message: String =
    this match
      case EmptySet =>
        "empty scoring set"
      case ZeroPriorMass =>
        "scoring set has zero prior mass"

enum ScoreValue:
  case Finite(value: Double)
  case NotScored(reason: ScoreAbsence)

  def toLegacyDouble: Double =
    this match
      case Finite(value) =>
        value
      case NotScored(_) =>
        Double.NegativeInfinity

  def finiteOrError(name: String): Either[ThresholdError, Double] =
    this match
      case Finite(value) =>
        Right(value)
      case NotScored(reason) =>
        Left(ThresholdError.InvalidArgument(name, reason.message))

object ScoreValue:
  def finite(value: Double): Either[ThresholdError, ScoreValue] =
    if value.isFinite then Right(Finite(value))
    else Left(ThresholdError.NonFiniteData("score"))

  def unsafeFinite(value: Double): ScoreValue =
    require(value.isFinite, "score must be finite")
    Finite(value)

  def max(left: ScoreValue, right: ScoreValue): ScoreValue =
    (left, right) match
      case (ScoreValue.Finite(a), ScoreValue.Finite(b)) =>
        ScoreValue.Finite(math.max(a, b))
      case (ScoreValue.Finite(_), ScoreValue.NotScored(_)) =>
        left
      case (ScoreValue.NotScored(_), ScoreValue.Finite(_)) =>
        right
      case (ScoreValue.NotScored(_), ScoreValue.NotScored(_)) =>
        left

final class ScoringInput private (
    val field: MaskedField,
    val priors: PriorWeights,
    val region: ThresholdRegion
):
  private[threshold] def indices: Array[Int] =
    region.indexArray

  private[threshold] def values: Array[Double] =
    field.data

object ScoringInput:
  def apply(field: MaskedField, priors: PriorWeights, region: ThresholdRegion): Either[ThresholdError, ScoringInput] =
    if field.size != priors.length then
      return Left(ThresholdError.ShapeMismatch("field/priors", field.size.toString, priors.length.toString))
    if !field.activeSpace.sameRuntimeOwnerAs(region.membership.space) then
      return Left(
        ThresholdError.ShapeMismatch(
          "field/region support",
          field.activeSpace.descriptor.toString,
          region.membership.space.descriptor.toString
        )
      )

    val indices = region.indexArray
    var i = 0
    while i < indices.length do
      val idx = indices(i)
      if idx < 0 || idx >= field.size then return Left(ThresholdError.IndexOutOfBounds(idx, field.size))
      i += 1

    Right(new ScoringInput(field, priors, region))

object ScoreSet:

  final case class DiffuseScore(scoreValue: ScoreValue, effectiveN: Double):
    require(effectiveN.isFinite && effectiveN >= 0.0, "effectiveN must be finite and non-negative")

    def score: Double =
      scoreValue.toLegacyDouble

  final case class OmnibusScore(
      scoreValue: ScoreValue,
      diffuse: DiffuseScore,
      softMaxValue: ScoreValue,
      kappa: Option[Kappa]
  ):
    def score: Double =
      scoreValue.toLegacyDouble

    def softMax: Double =
      softMaxValue.toLegacyDouble

  def softMax(input: ScoringInput, kappa: Kappa): Either[ThresholdError, ScoreValue] =
    softMaxValue(input.indices, input.values, input.priors, kappa)

  def softMax(
    indices: Array[Int],
    z: Array[Double],
    priors: PriorWeights,
    kappa: Kappa
  ): Either[ThresholdError, Double] =
    softMaxValue(indices, z, priors, kappa).map(_.toLegacyDouble)

  def diffuse(input: ScoringInput): Either[ThresholdError, DiffuseScore] =
    diffuseValue(input.indices, input.values, input.priors)

  def diffuse(
    indices: Array[Int],
    z: Array[Double],
    priors: PriorWeights
  ): Either[ThresholdError, DiffuseScore] =
    diffuseValue(indices, z, priors)

  def omnibus(
    input: ScoringInput,
    kappas: Vector[Kappa]
  ): Either[ThresholdError, OmnibusScore] =
    omnibusValue(input.indices, input.values, input.priors, kappas)

  def omnibus(
    indices: Array[Int],
    z: Array[Double],
    priors: PriorWeights,
    kappas: Vector[Kappa]
  ): Either[ThresholdError, OmnibusScore] =
    omnibusValue(indices, z, priors, kappas)

  private def softMaxValue(
    indices: Array[Int],
    z: Array[Double],
    priors: PriorWeights,
    kappa: Kappa
  ): Either[ThresholdError, ScoreValue] =
    validate(indices, z, priors) match
      case Left(err) => Left(err)
      case Right(()) =>
        if indices.isEmpty then Right(ScoreValue.NotScored(ScoreAbsence.EmptySet))
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

          if aMax.isNegInfinity then Right(ScoreValue.NotScored(ScoreAbsence.ZeroPriorMass))
          else
            var sum = 0.0
            i = 0
            while i < indices.length do
              val idx = indices(i)
              val w = priors(idx)
              if w > 0.0 then sum += w * math.exp(k * z(idx) - aMax)
              i += 1
            if sum <= 0.0 then Right(ScoreValue.NotScored(ScoreAbsence.ZeroPriorMass))
            else ScoreValue.finite(aMax + math.log(sum))

  private def diffuseValue(
    indices: Array[Int],
    z: Array[Double],
    priors: PriorWeights
  ): Either[ThresholdError, DiffuseScore] =
    validate(indices, z, priors) match
      case Left(err) => Left(err)
      case Right(()) =>
        if indices.isEmpty then Right(DiffuseScore(ScoreValue.NotScored(ScoreAbsence.EmptySet), 0.0))
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
          if sumW2 <= 0.0 then Right(DiffuseScore(ScoreValue.NotScored(ScoreAbsence.ZeroPriorMass), 0.0))
          else Right(DiffuseScore(ScoreValue.unsafeFinite(sumWZ / math.sqrt(sumW2)), (sumW * sumW) / sumW2))

  private def omnibusValue(
    indices: Array[Int],
    z: Array[Double],
    priors: PriorWeights,
    kappas: Vector[Kappa]
  ): Either[ThresholdError, OmnibusScore] =
    if kappas.isEmpty then return Left(ThresholdError.InvalidArgument("kappas", "must be non-empty"))
    diffuseValue(indices, z, priors) match
      case Left(err) => Left(err)
      case Right(d) =>
        if indices.isEmpty then
          Right(OmnibusScore(ScoreValue.NotScored(ScoreAbsence.EmptySet), d, ScoreValue.NotScored(ScoreAbsence.EmptySet), None))
        else
          var den1 = 0.0
          var i = 0
          while i < indices.length do
            den1 += priors(indices(i))
            i += 1
          if den1 <= 0.0 then
            val noSoftMax = ScoreValue.NotScored(ScoreAbsence.ZeroPriorMass)
            Right(OmnibusScore(d.scoreValue, d, noSoftMax, None))
          else
            val logDen1 = math.log(den1)
            var best = Double.NegativeInfinity
            var hasBest = false
            var bestK: Option[Kappa] = None
            var kIndex = 0
            while kIndex < kappas.length do
              val kappa = kappas(kIndex)
              softMaxValue(indices, z, priors, kappa) match
                case Left(err) => return Left(err)
                case Right(ScoreValue.Finite(logSum)) =>
                  val s = (logSum - logDen1) / kappa.value
                  if !hasBest || s > best then
                    best = s
                    hasBest = true
                    bestK = Some(kappa)
                case Right(ScoreValue.NotScored(_)) =>
                  ()
              kIndex += 1

            val softMaxScore =
              if hasBest then ScoreValue.unsafeFinite(best)
              else ScoreValue.NotScored(ScoreAbsence.ZeroPriorMass)
            Right(OmnibusScore(ScoreValue.max(d.scoreValue, softMaxScore), d, softMaxScore, bestK))

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
