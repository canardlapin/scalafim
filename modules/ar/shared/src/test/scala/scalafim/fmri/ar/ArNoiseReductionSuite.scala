package scalafim.fmri.ar

import gale.linalg.{DMat, Matrix}

class ArNoiseReductionSuite extends munit.FunSuite:

  test("raw summaries merge spatial blocks before fixed and automatic finalization") {
    val layout = value(NoiseEstimationLayout.excludingRows(
      TimeSegments.fromRunLengths(Vector(7, 6)), 13, Set(2, 9)
    ))
    val residuals = residualMatrix(13, 4)
    val full = value(ArEstimation.summarizeNoise(residuals, layout, ArOrderValue.unsafe(3)))
    val merged = value(
      value(ArEstimation.summarizeNoise(columns(residuals, Vector(3, 0)), layout, ArOrderValue.unsafe(3)))
        .merge(value(ArEstimation.summarizeNoise(columns(residuals, Vector(1, 2)), layout, ArOrderValue.unsafe(3))))
    )

    assertEquals(merged.pairCountsByRun, full.pairCountsByRun)
    assertNestedClose(merged.lagSumsByRun, full.lagSumsByRun)
    Vector(
      ArFitOptions(order = ArOrder.Fixed(2), pooling = NoisePooling.Global, exactFirstAr1 = false),
      ArFitOptions(order = ArOrder.Fixed(2), pooling = NoisePooling.Run, exactFirstAr1 = false),
      ArFitOptions(order = ArOrder.Auto(3), pooling = NoisePooling.Global, exactFirstAr1 = false)
    ).foreach { options =>
      assertPlanClose(value(ArEstimation.fitNoise(full, options)), value(ArEstimation.fitNoise(merged, options)))
      assertPlanClose(value(ArEstimation.fitNoise(residuals, layout, options)), value(ArEstimation.fitNoise(merged, options)))
    }
  }

  test("block summaries are bit-identical to the whole-volume estimator for every chunking and merge order") {
    val layout = value(NoiseEstimationLayout.excludingRows(
      TimeSegments.fromRunLengths(Vector(14, 15)), 29, Set(1, 15)
    ))
    val voxels = 7
    val residuals = Matrix.tabulate(29, voxels) { (row, column) =>
      val raw = math.sin((row + 1).toDouble * 12.9898 + (column + 1).toDouble * 78.233) * 43758.5453
      (raw - math.floor(raw)) * 2.0 - 1.0 + 0.37 * math.sin(row.toDouble * 0.23 + column)
    }
    val order = ArOrderValue.unsafe(2)
    val full = value(ArEstimation.summarizeNoise(residuals, layout, order))
    val whole = Vector.tabulate(layout.runCount) { run =>
      value(ArEstimation.pooledAutocovariance(residuals, layout.segmentsForRun(run), order)).sums.toVector
    }
    // Bit identity is the contract here, so these comparisons are exact by design.
    assertEquals(full.lagSumsByRun, whole)
    val options = ArFitOptions(order = ArOrder.Fixed(1), pooling = NoisePooling.Global, exactFirstAr1 = false)
    val wholePlan = value(ArEstimation.fitNoise(residuals, layout, options))
    (1 to voxels).foreach { width =>
      val blocks = (0 until voxels).grouped(width).map(columnsIn => value(
        ArEstimation.summarizeNoise(columns(residuals, columnsIn.toVector), layout, order)
      )).toVector
      Vector(blocks, blocks.reverse).foreach { ordered =>
        val merged = ordered.reduceLeft((left, right) => value(left.merge(right)))
        assertEquals(merged.lagSumsByRun, whole, s"width=$width")
        assertEquals(merged.pairCountsByRun, full.pairCountsByRun, s"width=$width")
        assertEquals(
          value(ArEstimation.fitNoise(merged, options)).coefficients.map(_.phi),
          wholePlan.coefficients.map(_.phi),
          s"width=$width"
        )
      }
    }
  }

  test("summary counts and lag products preserve reset gaps and run means") {
    val layout = value(NoiseEstimationLayout.excludingRows(
      TimeSegments.fromRunLengths(Vector(4, 3)), 7, Set(1, 5)
    ))
    val residuals = Matrix.tabulate(7, 1)((row, _) => Vector(1.0, 100.0, 3.0, 5.0, -2.0, 40.0, 2.0)(row))
    val summary = value(ArEstimation.summarizeNoise(residuals, layout, ArOrderValue.unsafe(1)))

    assertEquals(summary.pairCountsByRun, Vector(Vector(3L, 1L), Vector(2L, 0L)))
    assertNestedClose(summary.lagSumsByRun, Vector(Vector(8.0, 0.0), Vector(8.0, 0.0)))
  }

  test("summary merges reject incompatible layouts and requested orders") {
    val residuals = residualMatrix(8, 1)
    val first = value(NoiseEstimationLayout.allRows(TimeSegments.continuous(8), 8))
    val censored = value(NoiseEstimationLayout.excludingRows(TimeSegments.continuous(8), 8, Set(3)))
    val left = value(ArEstimation.summarizeNoise(residuals, first, ArOrderValue.unsafe(1)))
    val otherLayout = value(ArEstimation.summarizeNoise(residuals, censored, ArOrderValue.unsafe(1)))
    val otherOrder = value(ArEstimation.summarizeNoise(residuals, first, ArOrderValue.unsafe(2)))

    assert(left.merge(otherLayout).left.toOption.exists(_.isInstanceOf[ArError.IncompatibleNoiseSummaries]))
    assert(left.merge(otherOrder).left.toOption.exists(_.isInstanceOf[ArError.IncompatibleNoiseSummaries]))
  }

  test("nonfinite data, empty spatial blocks, and merged overflow have typed failures") {
    val layout = value(NoiseEstimationLayout.allRows(TimeSegments.continuous(2), 2))
    val nonfinite = Matrix.tabulate(2, 1)((row, _) => if row == 0 then Double.NaN else 1.0)
    val empty = Matrix.tabulate(2, 0)((_, _) => 0.0)
    val large = Matrix.tabulate(2, 1)((row, _) => if row == 0 then -9e153 else 9e153)

    assert(ArEstimation.summarizeNoise(nonfinite, layout, ArOrderValue.Zero).left.toOption.exists {
      case ArError.NonFiniteResidual(0, 0, _) => true
      case _ => false
    })
    assertEquals(
      ArEstimation.summarizeNoise(empty, layout, ArOrderValue.Zero).left.toOption,
      Some(ArError.EmptySpatialNoiseBlock)
    )
    val summary = value(ArEstimation.summarizeNoise(large, layout, ArOrderValue.Zero))
    assert(summary.merge(summary).left.toOption.exists {
      case ArError.NonFiniteNoiseSummary(0, lag, value) => lag.value == 0 && !value.isFinite
      case _ => false
    })
  }

  private def residualMatrix(rows: Int, columns: Int): DMat =
    Matrix.tabulate(rows, columns) { (row, column) =>
      val x = (row + 1).toDouble
      val innovation = math.sin(x * (column + 2).toDouble * 1.731) + 0.2 * math.cos(x * 0.73 + column)
      innovation + (if row == 0 || row == 7 then 0.0 else 0.45 * math.sin(row.toDouble * (column + 1)))
    }

  private def columns(matrix: DMat, indices: Vector[Int]): DMat =
    Matrix.tabulate(matrix.rows, indices.length)((row, column) => matrix(row, indices(column)))

  private def assertPlanClose(actual: WhiteningPlan, expected: WhiteningPlan): Unit =
    assertEquals(actual.pooling, expected.pooling)
    assertEquals(actual.coefficients.length, expected.coefficients.length)
    actual.coefficients.zip(expected.coefficients).foreach { (left, right) =>
      assertClose(left.phi, right.phi)
    }

  private def assertNestedClose(actual: Vector[Vector[Double]], expected: Vector[Vector[Double]]): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).foreach { (left, right) => assertClose(left, right) }

  private def assertClose(actual: Vector[Double], expected: Vector[Double]): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).foreach { (left, right) => assertEqualsDouble(left, right, 1e-10) }

  private def value[A](result: Either[ArError, A]): A =
    result.fold(error => fail(error.message), identity)
