package scalafim.connectivity

import gale.linalg.{DMat, Matrix}

class AriadnePortSyntheticSuite extends munit.FunSuite:

  test("event-weighted correlation concentrates a planted coactivation burst") {
    val series = syntheticEventBurstSeries()
    val ordinary = ConnectivityEstimators.weightedCorrelation(series).toOption.get.matrix.values
    val eventWeighted =
      ConnectivityEstimators
        .eventWeightedCorrelation(series, EventWeighting.unsafe(0.85, power = 1.0))
        .toOption
        .get

    val matrix = eventWeighted.matrix.values

    assert(matrix(0, 1) > ordinary(0, 1) + 0.25)
    assert(matrix(0, 1) > 0.85)
    assert(matrix(0, 1) > matrix(0, 2) + 0.50)
    assert(eventWeighted.diagnostics.metrics("event.count") >= 2.0)
    assertSymmetricUnitDiagonal(matrix, 1e-12)
  }

  test("directed ETS recovers a planted one-frame lag") {
    val series = laggedSeries()
    val dets =
      EventTimeSeries.directedConnectivity(series, lag = 1).toOption.get.matrix.values

    assert(dets(1, 0) > dets(0, 1) + 0.30)
    assert(dets(1, 0) > 0.60)
  }

  test("ridge partial correlation suppresses a common-cause marginal edge") {
    val series = commonCauseSeries()
    val marginal =
      ConnectivityEstimators.weightedCorrelation(series).toOption.get.matrix.values
    val partial =
      ConnectivityEstimators.ridgePartialCorrelation(series, lambda = 0.05).toOption.get.matrix.values

    assert(Math.abs(marginal(0, 1)) > 0.85)
    assert(Math.abs(partial(0, 1)) < Math.abs(marginal(0, 1)) * 0.45)
    assert(Math.abs(partial(0, 2)) > Math.abs(partial(0, 1)))
    assertSymmetricUnitDiagonal(partial, 1e-12)
  }

  test("PC partial correlation stays finite and symmetric in a high-dimensional collinear case") {
    val rows =
      Vector.tabulate(6): t =>
        val a = (t - 2.5) / 2.0
        val b = if t % 2 == 0 then 1.0 else -1.0
        Vector(
          a,
          2.0 * a + 0.1 * b,
          -a + 0.2 * b,
          b,
          a - b,
          0.5 * a + 0.3 * b,
          1.5 * a - 0.2 * b,
          -0.7 * a + b
        )
    val pc =
      ConnectivityEstimators.pcPartialCorrelation(series(rows), components = 20).toOption.get

    assertEquals(pc.diagnostics.metrics("partial.components"), 5.0)
    assertFinite(pc.matrix.values)
    assertSymmetricUnitDiagonal(pc.matrix.values, 1e-9)
  }

  test("connectivity-set inference prefers a distributed planted effect over a shuffled design") {
    val (set, x, plantedEdges) = distributedEffectSet()
    val design =
      ConnectivityDesign.from(
        "trait",
        GaleTestMatrix.fromRows(x.map(value => Vector(value))),
        Some(GaleTestMatrix.fromRows(x.map(_ => Vector(1.0))))
      ).toOption.get
    val nullTrait = Vector(-1.0, 1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0, 1.0)
    val shuffled =
      ConnectivityDesign.from(
        "trait.null",
        GaleTestMatrix.fromRows(nullTrait.map(value => Vector(value))),
        Some(GaleTestMatrix.fromRows(x.map(_ => Vector(1.0))))
      ).toOption.get

    val edgewise = ConnectivityInference.edgewiseF(set, design).toOption.get
    val kmt = ConnectivityInference.kernelMachine(set, design).toOption.get
    val kmtNull = ConnectivityInference.kernelMachine(set, shuffled).toOption.get
    val pc = ConnectivityInference.pcManova(set, design).toOption.get
    val pcNull = ConnectivityInference.pcManova(set, shuffled).toOption.get
    val topThree = edgewise.statistics.toVector.zipWithIndex.sortBy(-_._1).take(3).map(_._2).toSet

    assertEquals(topThree, plantedEdges.toSet)
    assert(kmt.statistic > kmtNull.statistic * 4.0)
    assert(pc.statistic > pcNull.statistic * 4.0)
    plantedEdges.foreach: edge =>
      assert(edgewise.pValues(edge) < 1e-6)
  }

  test("dynamic estimators track a synthetic sign reversal") {
    val rows =
      Vector.tabulate(30): t =>
        val x = if t % 2 == 0 then 1.0 else -1.0
        val y = if t < 15 then x else -x
        val z = if t % 3 == 0 then 0.5 else -0.5
        Vector(x, y, z)
    val signal = series(rows)

    val sliding =
      ConnectivityEstimators
        .slidingWindowCorrelation(signal, WindowSpec.unsafe(length = 10, step = 10))
        .toOption
        .get
    val ewma =
      ConnectivityEstimators
        .ewmaCorrelationStack(signal, halfLifeFrames = 3.0, warmupFrames = 2)
        .toOption
        .get

    assert(sliding.slices.head.connectivity.matrix.values(0, 1) > 0.95)
    assert(sliding.slices.last.connectivity.matrix.values(0, 1) < -0.95)
    assert(ewma.slices(7).connectivity.matrix.values(0, 1) > 0.85)
    assert(ewma.slices.last.connectivity.matrix.values(0, 1) < -0.70)
  }

  test("instantaneous outer-product snapshots preserve sample-level edge signs") {
    val signal = series(Vector(
      Vector(2.0, 2.0, 1.0),
      Vector(2.0, -2.0, 1.0),
      Vector(-3.0, -1.0, 0.5)
    ))

    val stack =
      ConnectivityEstimators.instantaneousOuterProductStack(signal, centerScale = false).toOption.get

    assert(stack.slices(0).connectivity.matrix.values(0, 1) > 0.0)
    assert(stack.slices(1).connectivity.matrix.values(0, 1) < 0.0)
    assert(stack.slices(2).connectivity.matrix.values(0, 1) > 0.0)
  }

  private def syntheticEventBurstSeries(): ParcelTimeSeries =
    val rows =
      Vector.tabulate(36): t =>
        if t >= 14 && t <= 18 then
          val a = 4.0 + (t - 14).toDouble
          Vector(a, a + 0.2 * (t - 14).toDouble, if t % 2 == 0 then 1.0 else -1.0)
        else
          val x = if t % 2 == 0 then 1.0 else -1.0
          val z = if t % 3 == 0 then 0.6 else -0.6
          Vector(x, -x, z)
    series(rows)

  private def laggedSeries(): ParcelTimeSeries =
    val x = Vector(-1.7, 0.2, 1.1, -0.6, 2.0, -1.2, 0.8, -0.1, 1.6, -1.9, 0.5, 1.3, -0.8, 0.9, -1.4, 0.3)
    val rows = x.indices.toVector.map: t =>
      val lagged = if t == 0 then 0.0 else x(t - 1)
      val distractor = if t % 2 == 0 then 0.75 else -0.25
      Vector(x(t), lagged, distractor)
    series(rows)

  private def commonCauseSeries(): ParcelTimeSeries =
    val rows =
      Vector.tabulate(16): t =>
        val z = (t.toDouble - 7.5) / 3.0
        val u = if t % 4 < 2 then 0.4 else -0.4
        val v = if t % 3 == 0 then -0.3 else 0.3
        Vector(z + u, 1.1 * z + v, z)
    series(rows)

  private def distributedEffectSet(): (ConnectivitySet, Vector[Double], Vector[Int]) =
    val axis = NodeAxis.generated(5).toOption.get
    val space = EdgeSpace.undirected(axis).toOption.get
    val x = Vector(-4.5, -3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5, 4.5)
    val planted = Vector(0, 3, 7)
    val subjects = x.zipWithIndex.map { case (value, subjectIndex) =>
      val edges = Vector.tabulate(space.size): edge =>
        val background = 0.02 * ((edge % 3) - 1).toDouble
        val slope =
          if edge == planted(0) then 0.08
          else if edge == planted(1) then -0.07
          else if edge == planted(2) then 0.06
          else 0.0
        background + slope * value
      val vector = EdgeVector.from(space, edges).toOption.get
      val matrix = ConnectivityMatrix.fromEdgeVector(
        vector,
        measure = ConnectivityMeasure.correlation,
        diagonalPolicy = DiagonalPolicy.Unit
      ).toOption.get
      SubjectConnectivity(SubjectId.unsafe(s"sub-${subjectIndex + 1}"), StaticConnectivity(matrix))
    }
    (ConnectivitySet.from(subjects).toOption.get, x, planted)

  private def series(rows: Vector[Vector[Double]]): ParcelTimeSeries =
    val axis = NodeAxis.generated(rows.head.length).toOption.get
    val time = TimeAxis.unsafeSeconds(rows.length, 1.0)
    ParcelTimeSeries.from(GaleTestMatrix.fromRows(rows), axis, time).toOption.get

  private def assertFinite(matrix: DMat): Unit =
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        assert(matrix(row, col).isFinite)
        col += 1
      row += 1

  private def assertSymmetricUnitDiagonal(matrix: DMat, tol: Double): Unit =
    var row = 0
    while row < matrix.rows do
      assertEqualsDouble(matrix(row, row), 1.0, tol)
      var col = row + 1
      while col < matrix.cols do
        assertEqualsDouble(matrix(row, col), matrix(col, row), tol)
        col += 1
      row += 1
