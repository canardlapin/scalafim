package scalafim.fmri.laws

import org.scalacheck.{Gen, Shrink}
import scalafim.fmri.ar.{NoiseEstimationLayout, NoisePooling, Pacf, TimeSegments}

/** A generated, estimable temporal-noise problem.
  *
  * Partial autocorrelations construct a stationary reference filter. Censor locations are constrained so every run
  * retains enough contiguous lag pairs for the requested fixed order.
  */
final case class ArLawCase private (
    order: Int,
    runLengths: Vector[Int],
    partialAutocorrelations: Vector[Double],
    columns: Int,
    censoredRows: Vector[Int],
    scale: Double,
    seed: Int,
    pooling: NoisePooling
):
  val rows: Int = runLengths.sum
  val coefficients: Vector[Double] = Pacf.pacfToAr(partialAutocorrelations)
  val baseSegments = TimeSegments.fromRunLengths(runLengths)
  val whiteningSegments = TimeSegments.withCensorResets(baseSegments, censoredRows.toSet)

  def survivingRowsByRun: Vector[Int] =
    val starts = runLengths.scanLeft(0)(_ + _)
    runLengths.indices.toVector.map { run =>
      val start = starts(run)
      val end = starts(run + 1)
      (start until end).count(row => !censoredRows.contains(row))
    }

object ArLawCase:
  def from(
      order: Int,
      runLengths: Vector[Int],
      partialAutocorrelations: Vector[Double],
      columns: Int,
      censoredRows: Vector[Int],
      scale: Double,
      seed: Int,
      pooling: NoisePooling
  ): Option[ArLawCase] =
    val rows = runLengths.sum
    val sortedCensors = censoredRows.distinct.sorted
    val basic =
      order >= 1 && order <= 4 &&
        runLengths.nonEmpty && runLengths.length <= 3 &&
        runLengths.forall(_ >= 32) &&
        partialAutocorrelations.length == order &&
        partialAutocorrelations.forall(value => value.isFinite && math.abs(value) <= 0.65) &&
        columns >= 1 && columns <= 4 &&
        sortedCensors == censoredRows &&
        sortedCensors.forall(row => row >= 0 && row < rows) &&
        scale > 0.0 && scale.isFinite
    if !basic then None
    else
      val baseSegments = TimeSegments.fromRunLengths(runLengths)
      val whiteningSegments = TimeSegments.withCensorResets(baseSegments, sortedCensors.toSet)
      val estimable =
        NoiseEstimationLayout
          .excludingRows(whiteningSegments, rows, sortedCensors.toSet)
          .toOption
          .exists(layout =>
            (0 until layout.runCount).forall(run => layout.segmentsForRun(run).exists(_.length > order))
          )
      if estimable then
        Some(
          new ArLawCase(
            order,
            runLengths,
            partialAutocorrelations,
            columns,
            sortedCensors,
            scale,
            seed,
            pooling
          )
        )
      else None

  given Shrink[ArLawCase] = Shrink.withLazyList { value =>
    val reducedOrder = math.max(1, value.order - 1)
    val oneRunLength = value.runLengths.head
    LazyList(
      from(
        reducedOrder,
        value.runLengths,
        value.partialAutocorrelations.take(reducedOrder),
        value.columns,
        value.censoredRows,
        value.scale,
        value.seed,
        value.pooling
      ),
      from(
        value.order,
        Vector(oneRunLength),
        value.partialAutocorrelations,
        value.columns,
        value.censoredRows.filter(_ < oneRunLength),
        value.scale,
        value.seed,
        value.pooling
      ),
      from(
        value.order,
        value.runLengths,
        value.partialAutocorrelations,
        1,
        Vector.empty,
        1.0,
        0,
        NoisePooling.Global
      ),
      from(
        value.order,
        value.runLengths,
        value.partialAutocorrelations,
        value.columns,
        Vector.empty,
        value.scale,
        value.seed,
        value.pooling
      )
    ).flatten.filterNot(_ == value).distinct
  }

object ArLawGenerators:
  private val partialAutocorrelation =
    Gen.oneOf(-0.55, -0.35, -0.15, 0.05, 0.2, 0.4, 0.6)

  private def valid(value: Option[ArLawCase]): Gen[ArLawCase] =
    value.fold(Gen.fail)(Gen.const)

  val estimationCase: Gen[ArLawCase] =
    for
      order <- Gen.choose(1, 4)
      runs <- Gen.choose(1, 3)
      lengths <- Gen.listOfN(runs, Gen.choose(72, 180)).map(_.toVector)
      partials <- Gen.listOfN(order, partialAutocorrelation).map(_.toVector)
      columns <- Gen.choose(1, 4)
      exponent <- Gen.choose(-5, 5)
      seed <- Gen.choose(0, 1000000)
      pooling <- Gen.oneOf(NoisePooling.Global, NoisePooling.Run)
      candidates = lengths.zipWithIndex.flatMap { case (length, run) =>
        val start = lengths.take(run).sum
        Vector(start + length / 3, start + (2 * length) / 3)
      }
      censorCount <- Gen.choose(0, math.min(4, candidates.length))
      censored <- Gen.pick(censorCount, candidates).map(_.toVector.sorted)
      generated <- valid(
        ArLawCase.from(
          order,
          lengths,
          partials,
          columns,
          censored,
          math.pow(10.0, exponent.toDouble),
          seed,
          pooling
        )
      )
    yield generated

  val recoveryCase: Gen[ArLawCase] =
    for
      order <- Gen.choose(1, 4)
      rows <- Gen.choose(320, 520)
      partials <- Gen.listOfN(order, partialAutocorrelation).map(_.toVector)
      columns <- Gen.choose(2, 4)
      exponent <- Gen.choose(-4, 4)
      seed <- Gen.choose(0, 1000000)
      generated <- valid(
        ArLawCase.from(
          order,
          Vector(rows),
          partials,
          columns,
          Vector.empty,
          math.pow(10.0, exponent.toDouble),
          seed,
          NoisePooling.Global
        )
      )
    yield generated

  val unstableCoefficients: Gen[Vector[Double]] =
    for
      order <- Gen.choose(1, 8)
      coefficients <- Gen
        .listOfN(order, Gen.choose(-250, 250).map(_.toDouble / 100.0))
        .map(_.toVector)
    yield coefficients

  val hostileAutocovariances: Gen[Vector[Double]] =
    for
      order <- Gen.choose(2, 6)
      tail <- Gen
        .listOfN(order, Gen.oneOf(-1.8, -1.2, -0.8, 0.9, 1.3, 1.9))
        .map(_.toVector)
    yield 1.0 +: tail
