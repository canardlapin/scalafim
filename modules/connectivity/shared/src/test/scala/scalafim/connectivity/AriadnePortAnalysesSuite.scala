package scalafim.connectivity

import scalafim.linalg.DoubleMatrix

class AriadnePortAnalysesSuite extends munit.FunSuite:

  test("ETS energy uses the pairwise edge-product identity") {
    val z = DoubleMatrix.fromRows(Vector(
      Vector(1.0, 2.0, 3.0),
      Vector(0.0, -1.0, 4.0)
    ))

    val energy = EventTimeSeries.energy(z).toOption.get

    assertEqualsDouble(energy(0), 49.0, 1e-12)
    assertEqualsDouble(energy(1), 16.0, 1e-12)
    assert(EventTimeSeries.eventIndices(energy, 0.5).toOption.get.exists(_.value == 0))
  }

  test("event-weighted correlation produces a typed static estimator with event diagnostics") {
    val series = fixtureSeries()
    val weighting = EventWeighting.from(0.6, power = 1.0).toOption.get

    val connectivity =
      ConnectivityEstimators.eventWeightedCorrelation(series, weighting).toOption.get

    assertEquals(connectivity.estimator.map(_.name), Some("event-weighted-correlation"))
    assert(connectivity.diagnostics.metrics("event.count") > 0.0)
    assertEqualsDouble(connectivity.matrix.values(0, 0), 1.0, 1e-12)
    assertEqualsDouble(connectivity.matrix.values(0, 1), connectivity.matrix.values(1, 0), 1e-12)
  }

  test("ridge partial correlation is exact on orthogonal columns") {
    val rows = Vector(
      Vector(-1.0, -1.0, 1.0),
      Vector(-1.0, 1.0, -1.0),
      Vector(1.0, -1.0, -1.0),
      Vector(1.0, 1.0, 1.0)
    )
    val series = fixtureSeries(rows)

    val connectivity =
      ConnectivityEstimators.ridgePartialCorrelation(series, lambda = 0.25).toOption.get

    assertEquals(connectivity.estimator.map(_.name), Some("ridge-partial-correlation:0.25"))
    assertEqualsDouble(connectivity.matrix.values(0, 1), 0.0, 1e-12)
    assertEqualsDouble(connectivity.matrix.values(0, 2), 0.0, 1e-12)
    assertEqualsDouble(connectivity.matrix.values(1, 2), 0.0, 1e-12)
  }

  test("PC partial correlation is equivariant to node permutation") {
    val rows = Vector(
      Vector(-2.0, 4.0, 2.0),
      Vector(-1.0, 1.0, 0.0),
      Vector(0.0, 0.0, 1.0),
      Vector(1.0, 1.0, 2.0),
      Vector(2.0, 4.0, 6.0),
      Vector(3.0, 9.0, 11.0)
    )
    val original = ConnectivityEstimators.pcPartialCorrelation(fixtureSeries(rows), components = 1).toOption.get.matrix.values
    val permutedRows = rows.map(row => Vector(row(2), row(0), row(1)))
    val permuted = ConnectivityEstimators.pcPartialCorrelation(fixtureSeries(permutedRows), components = 1).toOption.get.matrix.values
    val inverse = Vector(1, 2, 0)

    var row = 0
    while row < 3 do
      var col = 0
      while col < 3 do
        assertEqualsDouble(original(row, col), permuted(inverse(row), inverse(col)), 1e-8)
        col += 1
      row += 1
  }

  test("edgewise and global connectivity inference detect a planted linear edge effect") {
    val set = plantedConnectivitySet()
    val x = Vector(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5)
    val effect = DoubleMatrix.fromRows(x.map(value => Vector(value)))
    val nuisance = DoubleMatrix.fromRows(x.map(_ => Vector(1.0)))
    val design = ConnectivityDesign.from("dose", effect, Some(nuisance)).toOption.get

    val edgewise = ConnectivityInference.edgewiseF(set, design).toOption.get
    val kmt = ConnectivityInference.kernelMachine(set, design).toOption.get
    val pc = ConnectivityInference.pcManova(set, design).toOption.get

    assert(edgewise.statistics(0) > edgewise.statistics(1))
    assert(edgewise.pValues(0) < 1e-6)
    assert(edgewise.fdr(0) <= edgewise.pValues(0) * set.edgeCount.toDouble)
    assert(kmt.statistic > 0.0)
    assert(pc.statistic > 0.0)
    assertEquals(pc.rank.exists(_ > 0), true)
  }

  test("sliding-window correlation reuses static weighted-correlation semantics") {
    val series = fixtureSeries()
    val window = WindowSpec.unsafe(length = 3, step = 2)
    val dynamic =
      ConnectivityEstimators.slidingWindowCorrelation(series, window).toOption.get
    val firstSeries = fixtureSeries(series.values.toRows.take(3))
    val expected =
      ConnectivityEstimators.weightedCorrelation(firstSeries).toOption.get.matrix.values

    assertEquals(dynamic.windowAxis.windows.map(_.description), Vector("0:3", "2:5"))
    assertMatrixClose(dynamic.slices.head.connectivity.matrix.values, expected, 1e-12)
  }

  test("instantaneous and EWMA stacks are dynamic connectivity slices with unit diagonals") {
    val series = fixtureSeries()

    val outer = ConnectivityEstimators.instantaneousOuterProductStack(series).toOption.get
    val ewma = ConnectivityEstimators.ewmaCorrelationStack(series, halfLifeFrames = 2.0, warmupFrames = 1).toOption.get

    assertEquals(outer.size, series.samples)
    assertEquals(outer.windowAxis.windows.head.start.value, 0)
    assertEquals(ewma.size, series.samples - 1)
    assertEquals(ewma.windowAxis.windows.head.start.value, 1)
    assertEqualsDouble(outer.slices.head.connectivity.matrix.values(0, 0), 1.0, 1e-12)
    assertEqualsDouble(ewma.slices.head.connectivity.matrix.values(1, 1), 1.0, 1e-12)
  }

  private def fixtureSeries(
      rows: Vector[Vector[Double]] = Vector(
        Vector(1.0, 2.0, 3.0),
        Vector(2.0, 1.0, 4.0),
        Vector(3.0, 4.0, 2.0),
        Vector(4.0, 3.0, 1.0),
        Vector(5.0, 5.0, 5.0),
        Vector(6.0, 4.0, 7.0)
      )
  ): ParcelTimeSeries =
    val axis = NodeAxis.generated(rows.head.length).toOption.get
    val time = TimeAxis.unsafeSeconds(rows.length, 1.0)
    ParcelTimeSeries.from(DoubleMatrix.fromRows(rows), axis, time).toOption.get

  private def plantedConnectivitySet(): ConnectivitySet =
    val axis = NodeAxis.generated(3).toOption.get
    val space = EdgeSpace.undirected(axis).toOption.get
    val xs = Vector(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5)
    val subjects = xs.zipWithIndex.map { case (x, index) =>
      val edgeVector = EdgeVector.from(space, Vector(0.15 + 0.12 * x, 0.05, -0.02)).toOption.get
      val matrix = ConnectivityMatrix.fromEdgeVector(edgeVector, measure = ConnectivityMeasure.correlation, diagonalPolicy = DiagonalPolicy.Unit).toOption.get
      SubjectConnectivity(SubjectId.unsafe(s"sub-${index + 1}"), StaticConnectivity(matrix))
    }
    ConnectivitySet.from(subjects).toOption.get

  private def assertMatrixClose(actual: DoubleMatrix, expected: DoubleMatrix, tol: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tol)
        col += 1
      row += 1
