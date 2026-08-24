package scalafim.fmri.ar

import gale.linalg.Matrix
import gale.spectral.{Eigen, EigenSelection, EigenVectors}

object Pacf:

  private val DefaultRootMargin = 1e-6
  private val MaxStationarityShrinks = 60

  def pacfToAr(kappa: Vector[Double]): Vector[Double] =
    pacfToAr(PartialAutocorrelations.unsafe(kappa))

  def pacfToAr(kappa: PartialAutocorrelations): Vector[Double] =
    val values = kappa.values
    if values.isEmpty then Vector.empty
    else
      var previous = Array(values.head)
      var m = 2
      while m <= values.length do
        val km = values(m - 1)
        val current = new Array[Double](m)
        var j = 0
        while j < m - 1 do
          current(j) = previous(j) - km * previous((m - 2) - j)
          j += 1
        current(m - 1) = km
        previous = current
        m += 1
      previous.toVector

  def arToPacf(phi: Vector[Double], eps: Double = 1e-12): Vector[Double] =
    arToPartialAutocorrelations(phi, eps).toVector

  def arToPartialAutocorrelations(phi: Vector[Double], eps: Double = 1e-12): PartialAutocorrelations =
    arToPartialAutocorrelationsChecked(phi, eps)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def arToPartialAutocorrelationsChecked(
      phi: Vector[Double],
      eps: Double = 1e-12
  ): Either[ArError, PartialAutocorrelations] =
    require(eps > 0.0 && eps.isFinite, "PACF recursion tolerance must be positive and finite")
    validateFiniteAr(phi).flatMap { _ =>
      if phi.isEmpty then Right(PartialAutocorrelations.Empty)
      else
        var coefficients = phi.toArray
        val kappa = new Array[Double](phi.length)
        var m = phi.length
        while m >= 1 do
          val km = coefficients(m - 1)
          kappa(m - 1) = km
          if m > 1 then
            val den = math.max(1.0 - km * km, eps)
            val next = new Array[Double](m - 1)
            var j = 0
            while j < m - 1 do
              next(j) = (coefficients(j) + km * coefficients((m - 2) - j)) / den
              j += 1
            coefficients = next
          m -= 1
        PartialAutocorrelations(kappa.toVector)
    }

  def enforceStationary(phi: Vector[Double], bound: Double = 0.99): Vector[Double] =
    enforceStationaryChecked(phi, StationarityBound.unsafe(bound))
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def enforceStationary(phi: Vector[Double], bound: StationarityBound): Vector[Double] =
    enforceStationaryChecked(phi, bound)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def enforceStationaryChecked(
      phi: Vector[Double],
      bound: StationarityBound,
      rootMargin: Double = DefaultRootMargin
  ): Either[ArError, Vector[Double]] =
    require(rootMargin > 0.0 && rootMargin.isFinite, "root margin must be positive and finite")
    arToPartialAutocorrelationsChecked(phi).flatMap { partial =>
      val clipped = partial.clipped(bound)
      val initial = pacfToAr(clipped)
      validateFiniteAr(initial).flatMap(_ => shrinkToStationarityMargin(initial, rootMargin))
    }

  def validateStationary(
      phi: Vector[Double],
      rootMargin: Double = DefaultRootMargin
  ): Either[ArError, Unit] =
    validateFiniteAr(phi).flatMap(_ =>
      validateRecurrence(phi, rootMargin, ArError.NonStationaryArCoefficients.apply)
    )

  def validateInvertible(
      theta: Vector[Double],
      rootMargin: Double = DefaultRootMargin
  ): Either[ArError, Unit] =
    validateFiniteMa(theta).flatMap(_ =>
      validateRecurrence(theta.map(-_), rootMargin, ArError.NonInvertibleMaCoefficients.apply)
    )

  private def shrinkToStationarityMargin(
      phi: Vector[Double],
      rootMargin: Double
  ): Either[ArError, Vector[Double]] =
    var current = phi
    var attempt = 0
    while attempt <= MaxStationarityShrinks do
      recurrenceRootMagnitude(current) match
        case Left(error) => return Left(error)
        case Right(magnitude) if magnitude < recurrenceLimit(rootMargin) =>
          return Right(current)
        case Right(_) =>
          current = current.map(_ * 0.9)
      attempt += 1
    recurrenceRootMagnitude(current).flatMap { magnitude =>
      if magnitude < recurrenceLimit(rootMargin) then Right(current)
      else Left(ArError.NonStationaryArCoefficients(magnitude))
    }

  private def validateRecurrence(
      coefficients: Vector[Double],
      rootMargin: Double,
      error: Double => ArError
  ): Either[ArError, Unit] =
    require(rootMargin > 0.0 && rootMargin.isFinite, "root margin must be positive and finite")
    recurrenceRootMagnitude(coefficients).flatMap { magnitude =>
      if magnitude < recurrenceLimit(rootMargin) then Right(())
      else Left(error(magnitude))
    }

  private def recurrenceLimit(rootMargin: Double): Double =
    1.0 / (1.0 + rootMargin)

  private[ar] def recurrenceRootMagnitude(
      coefficients: Vector[Double]
  ): Either[ArError, Double] =
    validateFiniteAr(coefficients).flatMap(_ => recurrenceRootMagnitudeUnchecked(coefficients))

  private def recurrenceRootMagnitudeUnchecked(
      coefficients: Vector[Double]
  ): Either[ArError, Double] =
    if coefficients.isEmpty then Right(0.0)
    else
      val order = coefficients.length
      val companion = Matrix.newBuilder(order, order)
      var col = 0
      while col < order do
        companion(0, col) = coefficients(col)
        col += 1
      var row = 1
      while row < order do
        companion(row, row - 1) = 1.0
        row += 1
      Eigen
        .eigNonsymmetric(companion.result(), EigenSelection.All, EigenVectors.ValuesOnly)
        .left
        .map(error => ArError.StationarityCheckFailed(error.getMessage))
        .flatMap { decomposition =>
          var maximum = 0.0
          var index = 0
          while index < decomposition.size do
            maximum = math.max(maximum, decomposition.eigenvalue(index).magnitude)
            index += 1
          if maximum.isFinite then Right(maximum)
          else Left(ArError.StationarityCheckFailed("eigendecomposition returned a non-finite root"))
        }

  private def validateFiniteAr(coefficients: Vector[Double]): Either[ArError, Unit] =
    validateFinite(coefficients, ArError.NonFiniteArCoefficient.apply)

  private def validateFiniteMa(coefficients: Vector[Double]): Either[ArError, Unit] =
    validateFinite(coefficients, ArError.NonFiniteMaCoefficient.apply)

  private def validateFinite(
      coefficients: Vector[Double],
      error: (Int, Double) => ArError
  ): Either[ArError, Unit] =
    var index = 0
    while index < coefficients.length do
      val value = coefficients(index)
      if !value.isFinite then return Left(error(index, value))
      index += 1
    Right(())
