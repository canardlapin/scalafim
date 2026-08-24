package scalafim.fmri.laws

import gale.linalg.{DMat, Matrix}
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import scalafim.fmri.ar.*

class ArGeneratedLawsSuite extends GeneratedLawSuite:

  property("fixed-order AR estimation is invariant to scale, signs, and response-column permutation"):
    forAll(ArLawGenerators.estimationCase) { generated =>
      val base = residualMatrix(generated)
      val transformed = transformColumns(base, generated.scale, generated.seed)
      val compared =
        for
          original <- fit(generated, base, ArOrder.Fixed(generated.order), generated.pooling)
          moved <- fit(generated, transformed, ArOrder.Fixed(generated.order), generated.pooling)
        yield (original, moved)
      compared match
        case Left(error)              => Prop.falsified :| error.message
        case Right((original, moved)) =>
          val gap = coefficientGap(original, moved)
          val lawful = original.coefficients.forall(coefficients =>
            coefficients.phi.forall(_.isFinite) &&
              coefficients.arOrder == generated.order &&
              Pacf.validateStationary(coefficients.phi).isRight
          )
          Prop(gap <= 2e-10 && lawful) :|
            describe(generated, s"coefficientGap=$gap lawful=$lawful")
    }

  property("censored rows cannot alter generated run or global AR estimates"):
    forAll(ArLawGenerators.estimationCase.suchThat(_.censoredRows.nonEmpty)) { generated =>
      val clean = residualMatrix(generated)
      val contaminated = contaminateExcluded(clean, generated.censoredRows)
      val compared =
        for
          expected <- fit(generated, clean, ArOrder.Fixed(generated.order), generated.pooling)
          actual <- fit(generated, contaminated, ArOrder.Fixed(generated.order), generated.pooling)
        yield (expected, actual)
      compared match
        case Left(error)               => Prop.falsified :| error.message
        case Right((expected, actual)) =>
          val gap = coefficientGap(expected, actual)
          Prop(gap <= 1e-12) :| describe(generated, s"censorLeakageGap=$gap")
    }

  property("distinct run-local offsets cannot enter AR estimation"):
    forAll(ArLawGenerators.estimationCase.suchThat(_.runLengths.length > 1)) { generated =>
      val centered = residualMatrix(generated)
      val shifted = addRunOffsets(centered, generated.runLengths, generated.scale, generated.seed)
      val compared =
        for
          expected <- fit(generated, centered, ArOrder.Fixed(generated.order), generated.pooling)
          actual <- fit(generated, shifted, ArOrder.Fixed(generated.order), generated.pooling)
        yield (expected, actual)
      compared match
        case Left(error)               => Prop.falsified :| error.message
        case Right((expected, actual)) =>
          val gap = coefficientGap(expected, actual)
          Prop(gap <= 2e-10) :| describe(generated, s"runOffsetGap=$gap")
    }

  property("global pooling is the surviving-row-weighted run estimate across generated orders"):
    forAll(ArLawGenerators.estimationCase) { generated =>
      val residuals = residualMatrix(generated)
      val compared =
        for
          byRun <- fit(generated, residuals, ArOrder.Fixed(generated.order), NoisePooling.Run)
          global <- fit(generated, residuals, ArOrder.Fixed(generated.order), NoisePooling.Global)
          expected <- expectedGlobal(byRun, generated.survivingRowsByRun)
        yield (global, expected)
      compared match
        case Left(error)               => Prop.falsified :| error.message
        case Right((global, expected)) =>
          val actual = global.coefficients.head.phi
          val gap = maxAbsDiff(actual, expected)
          Prop(gap <= 2e-10) :| describe(generated, s"poolingGap=$gap actual=$actual expected=$expected")
    }

  property("automatic AR order is bounded and invariant to equivalent response representations"):
    forAll(ArLawGenerators.estimationCase) { generated =>
      val base = residualMatrix(generated)
      val transformed = transformColumns(base, generated.scale, generated.seed)
      val maxOrder = 6
      val compared =
        for
          original <- fit(generated, base, ArOrder.Auto(maxOrder), generated.pooling)
          moved <- fit(generated, transformed, ArOrder.Auto(maxOrder), generated.pooling)
        yield (original, moved)
      compared match
        case Left(error)              => Prop.falsified :| error.message
        case Right((original, moved)) =>
          val bounded =
            original.pooling match
              case NoisePooling.Global =>
                original.arOrder <= math.min(maxOrder, generated.survivingRowsByRun.max / 5)
              case NoisePooling.Run =>
                original.coefficients.zip(generated.survivingRowsByRun).forall { case (coefficients, rows) =>
                  coefficients.arOrder <= math.min(maxOrder, rows / 5)
                }
          val gap = coefficientGap(original, moved)
          Prop(bounded && gap <= 2e-10) :|
            describe(generated, s"bounded=$bounded coefficientGap=$gap orders=${original.coefficients.map(_.arOrder)}")
    }

  property("fixed-order estimates recover generated stationary filters and whiten within a finite-sample envelope"):
    forAll(ArLawGenerators.recoveryCase) { generated =>
      val residuals = residualMatrix(generated)
      fit(generated, residuals, ArOrder.Fixed(generated.order), NoisePooling.Global) match
        case Left(error) => Prop.falsified :| error.message
        case Right(plan) =>
          val actual = plan.coefficients.head.phi
          val coefficientError = maxAbsDiff(actual, generated.coefficients)
          val recoveryBound = 5.0 / math.sqrt((generated.rows * generated.columns).toDouble) + 0.04
          val whitened = WhiteningTransform.matrix(plan, residuals)
          whitened match
            case Left(error)  => Prop.falsified :| error.message
            case Right(value) =>
              val before = meanAbsoluteAcf(residuals, generated.order)
              val after = meanAbsoluteAcf(value, generated.order)
              val whitenessBound = 2.5 / math.sqrt(generated.rows.toDouble) + 0.02
              val improvesWhenDetectable = before <= whitenessBound || after < before
              Prop(
                coefficientError <= recoveryBound &&
                  after <= whitenessBound &&
                  improvesWhenDetectable
              ) :|
                describe(
                  generated,
                  s"coefficientError=$coefficientError recoveryBound=$recoveryBound " +
                    s"acfBefore=$before acfAfter=$after whitenessBound=$whitenessBound " +
                    s"improvesWhenDetectable=$improvesWhenDetectable"
                )
    }

  property("stationarity enforcement safely validates or rejects and hostile autocovariances repair to finite filters"):
    val stationary = forAll(ArLawGenerators.unstableCoefficients) { coefficients =>
      Pacf.enforceStationaryChecked(coefficients, StationarityBound.Default) match
        case Left(ArError.NonFinitePartialAutocorrelation(_, value)) =>
          Prop(!value.isFinite) :| s"input=$coefficients typedOverflow=$value"
        case Left(error) =>
          Prop.falsified :| error.message
        case Right(stable) =>
          val validation = Pacf.validateStationary(stable)
          val second = Pacf.enforceStationaryChecked(stable, StationarityBound.Default)
          val repeatedValidation = second.flatMap(value => Pacf.validateStationary(value).map(_ => value))
          Prop(
            stable.forall(_.isFinite) &&
              validation.isRight &&
              repeatedValidation.exists(_.forall(_.isFinite))
          ) :| s"input=$coefficients stable=$stable validation=$validation repeated=$repeatedValidation"
    }
    val repaired = forAll(ArLawGenerators.hostileAutocovariances) { gamma =>
      val order = gamma.length - 1
      ArEstimation.yuleWalker(gamma, order) match
        case Left(error)     => Prop.falsified :| error.message
        case Right(estimate) =>
          Prop(
            estimate.coefficients.phi.forall(_.isFinite) &&
              estimate.innovationVariance.isFinite &&
              estimate.innovationVariance >= 0.0 &&
              Pacf.validateStationary(estimate.coefficients.phi).isRight
          ) :| s"gamma=$gamma estimate=$estimate"
    }
    stationary && repaired

  private def fit(
      generated: ArLawCase,
      residuals: DMat,
      order: ArOrder,
      pooling: NoisePooling
  ): Either[ArError, WhiteningPlan] =
    for
      layout <- NoiseEstimationLayout.excludingRows(
        generated.whiteningSegments,
        generated.rows,
        generated.censoredRows.toSet
      )
      plan <- ArEstimation.fitNoise(
        residuals,
        layout,
        ArFitOptions(order = order, pooling = pooling, exactFirstAr1 = false)
      )
    yield plan

  private def expectedGlobal(
      byRun: WhiteningPlan,
      survivingRows: Vector[Int]
  ): Either[ArError, Vector[Double]] =
    val total = survivingRows.sum.toDouble
    val order = byRun.coefficients.map(_.arOrder).max
    val weighted = Vector.tabulate(order) { lag =>
      byRun.coefficients
        .zip(survivingRows)
        .map { case (coefficients, rows) =>
          val value = if lag < coefficients.arOrder then coefficients.phi(lag) else 0.0
          value * rows.toDouble / total
        }
        .sum
    }
    Pacf.enforceStationaryChecked(weighted, StationarityBound.Default)

  private def residualMatrix(generated: ArLawCase): DMat =
    val out = Matrix.newBuilder(generated.rows, generated.columns)
    val history = Array.fill(generated.columns, generated.order)(0.0)
    val runStarts = generated.runLengths.scanLeft(0)(_ + _).dropRight(1).toSet
    var row = 0
    while row < generated.rows do
      if runStarts.contains(row) then
        var column = 0
        while column < generated.columns do
          java.util.Arrays.fill(history(column), 0.0)
          column += 1

      var column = 0
      while column < generated.columns do
        var value = deterministicInnovation(row, column, generated.seed)
        var lag = 0
        while lag < generated.order do
          value += generated.coefficients(lag) * history(column)(lag)
          lag += 1
        out(row, column) = value
        lag = generated.order - 1
        while lag > 0 do
          history(column)(lag) = history(column)(lag - 1)
          lag -= 1
        history(column)(0) = value
        column += 1
      row += 1
    out.result()

  private def transformColumns(input: DMat, scale: Double, seed: Int): DMat =
    val out = Matrix.newBuilder(input.rows, input.cols)
    var row = 0
    while row < input.rows do
      var column = 0
      while column < input.cols do
        val source = input.cols - column - 1
        val sign = if (column + seed) % 2 == 0 then 1.0 else -1.0
        out(row, column) = input(row, source) * scale * sign
        column += 1
      row += 1
    out.result()

  private def addRunOffsets(
      residuals: DMat,
      runLengths: Vector[Int],
      scale: Double,
      seed: Int
  ): DMat =
    val ends = runLengths.scanLeft(0)(_ + _)
    val offsetScale = math.max(1.0, math.min(scale, 100.0))
    val out = Matrix.newBuilder(residuals.rows, residuals.cols)
    var row = 0
    while row < residuals.rows do
      val run = ends.indexWhere(end => row < end) - 1
      var column = 0
      while column < residuals.cols do
        val sign = if (run + column + seed) % 2 == 0 then 1.0 else -1.0
        out(row, column) = residuals(row, column) + sign * offsetScale * (run + 1).toDouble * 17.0
        column += 1
      row += 1
    out.result()

  private def contaminateExcluded(input: DMat, excludedRows: Vector[Int]): DMat =
    val excluded = excludedRows.toSet
    Matrix.tabulate(input.rows, input.cols) { (row, column) =>
      if excluded.contains(row) then 1.0e12 * (column + 1).toDouble
      else input(row, column)
    }

  private def deterministicInnovation(row: Int, column: Int, seed: Int): Double =
    val raw = math.sin(
      (row + 1).toDouble * 12.9898 +
        (column + 1).toDouble * 78.233 +
        (seed + 1).toDouble * 0.017
    ) * 43758.5453
    (raw - math.floor(raw)) * 2.0 - 1.0

  private def coefficientGap(left: WhiteningPlan, right: WhiteningPlan): Double =
    if left.coefficients.length != right.coefficients.length then Double.PositiveInfinity
    else
      left.coefficients
        .zip(right.coefficients)
        .map { case (a, b) =>
          maxAbsDiff(a.phi, b.phi)
        }
        .maxOption
        .getOrElse(0.0)

  private def maxAbsDiff(left: Vector[Double], right: Vector[Double]): Double =
    if left.length != right.length then Double.PositiveInfinity
    else left.zip(right).map((a, b) => math.abs(a - b)).maxOption.getOrElse(0.0)

  private def meanAbsoluteAcf(residuals: DMat, maxLag: Int): Double =
    val diagnostics = AcorrDiagnostics.compute(residuals, maxLag, AcfAggregation.None)
    val values = diagnostics.acf.valuesRowMajor
    if values.isEmpty then 0.0 else values.iterator.map(math.abs).sum / values.length.toDouble

  private def describe(generated: ArLawCase, detail: String): String =
    s"order=${generated.order} runs=${generated.runLengths} columns=${generated.columns} " +
      s"censored=${generated.censoredRows} scale=${generated.scale} seed=${generated.seed} " +
      s"pooling=${generated.pooling} $detail"
