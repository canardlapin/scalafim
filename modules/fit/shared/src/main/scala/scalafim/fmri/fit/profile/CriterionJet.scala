package scalafim.fmri.fit.profile

import gale.linalg.{CholeskyOptions, DMat}

/** Small identity tokens only: neither factor nor response storage belongs here.
  * The coherent core mints them from its accepted reference bundle. Consumer
  * identity checks alone do not qualify a native implementation.
  */
final class CriterionOwner private (val intrinsicLambda: Double)

object CriterionOwner:
  private[profile] def checked(lambda: Double): Either[CriterionJet.Error, CriterionOwner] =
    if !lambda.isFinite || lambda <= 0.0 then Left(CriterionJet.Error.InvalidLambda(lambda))
    else Right(new CriterionOwner(lambda))

enum CriterionDerivativeOrder:
  case Value, Full

final class CriterionReference private (
    val owner: CriterionOwner, val coordinates: Vector[Double], val order: CriterionDerivativeOrder):
  def dimension: Int = coordinates.length

object CriterionReference:
  private[profile] def checked(owner: CriterionOwner, coordinates: Vector[Double], order: CriterionDerivativeOrder)
      : Either[CriterionJet.Error, CriterionReference] =
    CriterionJet.validateCoordinates(coordinates).map(_ => new CriterionReference(owner, coordinates, order))

final class CriterionEpoch private (val owner: CriterionOwner, val sequence: Long)

object CriterionEpoch:
  private[profile] def checked(owner: CriterionOwner, sequence: Long): Either[CriterionJet.Error, CriterionEpoch] =
    if sequence <= 0L then Left(CriterionJet.Error.InvalidEpoch(sequence))
    else Right(new CriterionEpoch(owner, sequence))

/** Response-independent scalar log-determinant jet, without amplitudes. */
final class DeterminantJet private (
    val reference: CriterionReference, val value: Double, val gradient: Vector[Double], val hessian: Vector[Double])

object DeterminantJet:
  def checked(reference: CriterionReference, value: Double, gradient: Vector[Double], hessian: Vector[Double])
      : Either[CriterionJet.Error, DeterminantJet] =
    CriterionJet.validateFull(reference, value, gradient, hessian)
      .map(_ => new DeterminantJet(reference, value, gradient, hessian))

final class RawEnergyJet private (val reference: CriterionReference, val epoch: CriterionEpoch, val jet: ProfileJet)

object RawEnergyJet:
  def checked(reference: CriterionReference, epoch: CriterionEpoch, jet: ProfileJet, amplitudeCount: Int)
      : Either[CriterionJet.Error, RawEnergyJet] =
    for
      _ <- CriterionJet.validateOwner(reference, epoch)
      _ <- CriterionJet.validateFull(reference, jet.energy, jet.gradient, jet.hessian)
      _ <- CriterionJet.validateAmplitudes(jet.amplitudes, amplitudeCount)
      _ <- if jet.curvature == CurvatureStatus.GramNotPositiveDefinite then Left(CriterionJet.Error.ProfilingRefused) else Right(())
    yield new RawEnergyJet(reference, epoch, jet)

final class CoherentCriterionJet private (val raw: RawEnergyJet, val determinant: DeterminantJet)

object CoherentCriterionJet:
  def checked(raw: RawEnergyJet, determinant: DeterminantJet): Either[CriterionJet.Error, CoherentCriterionJet] =
    CriterionJet.validatePair(raw.reference, determinant.reference).map(_ => new CoherentCriterionJet(raw, determinant))

/** Distinct value-only results prevent hiding determinant derivatives behind
  * an energy-only call. Cached full node references may also supply values.
  */
final class RawEnergyValue private (
    val reference: CriterionReference, val epoch: CriterionEpoch, val energy: Double, val amplitudes: Vector[Double])

object RawEnergyValue:
  def checked(reference: CriterionReference, epoch: CriterionEpoch, energy: Double, amplitudes: Vector[Double], amplitudeCount: Int)
      : Either[CriterionJet.Error, RawEnergyValue] =
    for
      _ <- CriterionJet.validateOwner(reference, epoch)
      _ <- CriterionJet.validateFinite("raw energy", Vector(energy))
      _ <- CriterionJet.validateAmplitudes(amplitudes, amplitudeCount)
    yield new RawEnergyValue(reference, epoch, energy, amplitudes)

final class DeterminantValue private (val reference: CriterionReference, val value: Double)

object DeterminantValue:
  def checked(reference: CriterionReference, value: Double): Either[CriterionJet.Error, DeterminantValue] =
    CriterionJet.validateFinite("determinant", Vector(value)).map(_ => new DeterminantValue(reference, value))

final class CoherentCriterionValue private (val raw: RawEnergyValue, val determinant: DeterminantValue)

object CoherentCriterionValue:
  def checked(raw: RawEnergyValue, determinant: DeterminantValue): Either[CriterionJet.Error, CoherentCriterionValue] =
    CriterionJet.validatePair(raw.reference, determinant.reference).map(_ => new CoherentCriterionValue(raw, determinant))

/** J is assembled directly in energy units; score is its maximization form.
  * The prior belongs exclusively to ShapeDecoder. Raw E/HE remain unchanged.
  */
final class CriterionJet private (
    val raw: RawEnergyJet, val determinant: Option[DeterminantJet], val minimizationJet: ProfileJet, val score: Double)

object CriterionJet:
  enum Input:
    case Penalized(raw: RawEnergyJet)
    case TrialMl(coherent: CoherentCriterionJet)

  enum Error:
    case InvalidSigma2(value: Double)
    case InvalidLambda(value: Double)
    case InvalidEpoch(value: Long)
    case Dimension(actual: Int)
    case Length(field: String, expected: Int, actual: Int)
    case NonFinite(field: String, index: Int)
    case Asymmetric(row: Int, column: Int)
    case WrongOwner, WrongReference, WrongCoordinates, WrongEpoch, FullJetRequired, ProfilingRefused
    case AmplitudeCount(actual: Int)
    case AssembledNonFinite(field: String)

    def message: String = s"criterion validation failed: $this"

  private[profile] def validateSigma2(value: Double): Either[Error, Unit] =
    if value.isFinite && value > 0.0 then Right(()) else Left(Error.InvalidSigma2(value))

  private[profile] def validateFinite(field: String, values: Vector[Double]): Either[Error, Unit] =
    val index = values.indexWhere(!_.isFinite)
    if index < 0 then Right(()) else Left(Error.NonFinite(field, index))

  private[profile] def validateCoordinates(coordinates: Vector[Double]): Either[Error, Unit] =
    if coordinates.length < 1 || coordinates.length > 3 then Left(Error.Dimension(coordinates.length))
    else validateFinite("coordinates", coordinates)

  private[profile] def validateOwner(reference: CriterionReference, epoch: CriterionEpoch): Either[Error, Unit] =
    if reference.owner eq epoch.owner then Right(()) else Left(Error.WrongOwner)

  private[profile] def validatePair(raw: CriterionReference, determinant: CriterionReference): Either[Error, Unit] =
    if !(raw.owner eq determinant.owner) then Left(Error.WrongOwner)
    else if raw.coordinates != determinant.coordinates then Left(Error.WrongCoordinates)
    else if !(raw eq determinant) then Left(Error.WrongReference)
    else Right(())

  private[profile] def validateAmplitudes(amplitudes: Vector[Double], count: Int): Either[Error, Unit] =
    if count < 1 then Left(Error.AmplitudeCount(count))
    else if amplitudes.length != count then Left(Error.Length("amplitudes", count, amplitudes.length))
    else validateFinite("amplitudes", amplitudes)

  private[profile] def validateFull(reference: CriterionReference, value: Double, gradient: Vector[Double], hessian: Vector[Double])
      : Either[Error, Unit] =
    val d = reference.dimension
    if reference.order != CriterionDerivativeOrder.Full then Left(Error.FullJetRequired)
    else if gradient.length != d then Left(Error.Length("gradient", d, gradient.length))
    else if hessian.length != d * d then Left(Error.Length("hessian", d * d, hessian.length))
    else
      for
        _ <- validateFinite("value", Vector(value))
        _ <- validateFinite("gradient", gradient)
        _ <- validateFinite("hessian", hessian)
        _ <- validateSymmetry(d, hessian)
      yield ()

  private def validateSymmetry(d: Int, hessian: Vector[Double]): Either[Error, Unit] =
    var i = 0
    while i < d do
      var j = i + 1
      while j < d do
        if hessian(i * d + j) != hessian(j * d + i) then return Left(Error.Asymmetric(i, j))
        j += 1
      i += 1
    Right(())

  private[profile] def assembleValue(sigma2: Double, energy: Double, determinant: Double): Either[Error, (Double, Double)] =
    validateSigma2(sigma2).flatMap { _ =>
      val j = energy + sigma2 * determinant
      val score = if math.abs(j) <= sigma2 then -0.5 * (j / sigma2) else (-0.5 * j) / sigma2
      if !j.isFinite then Left(Error.AssembledNonFinite("J"))
      else if !score.isFinite then Left(Error.AssembledNonFinite("score"))
      else Right((j, score))
    }

  def assemble(sigma2: Double, input: Input): Either[Error, CriterionJet] =
    val (raw, determinant) = input match
      case Input.Penalized(raw) => (raw, None)
      case Input.TrialMl(pair) => (pair.raw, Some(pair.determinant))
    for
      values <- assembleValue(sigma2, raw.jet.energy, determinant.fold(0.0)(_.value))
      g = raw.jet.gradient.indices.map(i => raw.jet.gradient(i) + sigma2 * determinant.fold(0.0)(_.gradient(i))).toVector
      h = raw.jet.hessian.indices.map(i => raw.jet.hessian(i) + sigma2 * determinant.fold(0.0)(_.hessian(i))).toVector
      _ <- if g.forall(_.isFinite) then Right(()) else Left(Error.AssembledNonFinite("gJ"))
      _ <- if h.forall(_.isFinite) then Right(()) else Left(Error.AssembledNonFinite("HJ"))
    yield
      val d = raw.reference.dimension
      val positive = DMat.tabulate(d, d)((i, j) => h(i * d + j)).cholesky(CholeskyOptions(0.0)).isRight
      val jet = ProfileJet(values._1, g, h, raw.jet.amplitudes,
        if positive then CurvatureStatus.PositiveDefinite else CurvatureStatus.Indefinite)
      new CriterionJet(raw, determinant, jet, values._2)
