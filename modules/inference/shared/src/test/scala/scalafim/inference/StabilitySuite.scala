package scalafim.inference

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector
import scalafim.multivar.SpaceId

class StabilitySuite extends munit.FunSuite:

  import InferenceRReferenceFixtures as R

  private def accepted[A](value: Either[InferenceError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def matrix(value: R.MatrixData): DoubleMatrix =
    DoubleMatrix.fromRows(Vector.tabulate(value.rows) { row =>
      Vector.tabulate(value.cols)(col => value(row, col))
    })

  private def assertMatrixClose(actual: DoubleMatrix, expected: R.MatrixData, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1

  test("Hungarian matching and sign alignment reproduce external fixtures") {
    R.alignment.foreach { fixture =>
      val result = accepted(ComponentAlignment.align(
        matrix(fixture.reference),
        matrix(fixture.replicate)
      ))

      assertEquals(result.matching.permutation.map(_ + 1), fixture.permutationOneBased)
      assertEqualsDouble(result.matching.score, fixture.matchScore, 1e-12)
      assertEqualsDouble(result.matching.margin, fixture.matchMargin, 1e-12)
      assertEquals(result.matching.ambiguous, fixture.ambiguous)
      assertMatrixClose(result.aligned, fixture.aligned, 1e-12)
    }
  }

  test("principal angles reproduce analytic axis and plane fixtures") {
    R.principalAngles.foreach { fixture =>
      val angles = accepted(PrincipalAngles.between(matrix(fixture.a), matrix(fixture.b)))
      assertEquals(angles.values.length, fixture.anglesRadians.length)
      angles.values.zip(fixture.anglesRadians).foreach { case (actual, expected) =>
        assertEqualsDouble(actual, expected, 1e-12)
      }
    }
  }

  test("principal angles report rank loss rather than inventing a subspace") {
    val rankDeficient = DoubleMatrix.fromRows(Vector(
      Vector(1.0, 1.0),
      Vector(0.0, 0.0),
      Vector(0.0, 0.0)
    ))
    val fullRank = DoubleMatrix.fromRows(Vector(
      Vector(1.0, 0.0),
      Vector(0.0, 1.0),
      Vector(0.0, 0.0)
    ))

    PrincipalAngles.between(rankDeficient, fullRank) match
      case Left(InferenceError.RankLoss(expected, actual)) =>
        assertEquals(expected, 2)
        assertEquals(actual, 1)
      case other => fail(s"expected rank loss, got $other")

    ComponentAlignment.matchComponents(fullRank, DoubleMatrix.fromRows(Vector(
      Vector(1.0),
      Vector(0.0),
      Vector(0.0)
    ))) match
      case Left(InferenceError.RankLoss(expected, actual)) =>
        assertEquals(expected, 2)
        assertEquals(actual, 1)
      case other => fail(s"expected component rank loss, got $other")
  }

  test("row and cluster bootstrap actions are deterministic replicate programs") {
    val rows = accepted(RowCount(6))
    val replicate = accepted(ReplicateId(3))
    val rowAction = BootstrapAction.rows(rows)
    val first = accepted(rowAction.draw(RootSeed(19L), replicate))
    val second = accepted(rowAction.draw(RootSeed(19L), replicate))
    assertEquals(first, second)
    assertEquals(first.rows.length, 6)
    assert(first.rows.forall(row => row.value >= 0 && row.value < 6))

    val partition = accepted(ClusterPartition.from(
      accepted(RowCount(6)),
      Vector(Vector(0, 1), Vector(2, 3), Vector(4, 5))
    ))
    val clustered = accepted(BootstrapAction.clusters(partition).draw(RootSeed(23L), replicate))
    assertEquals(clustered.sampledClusters.length, 3)
    assertEquals(clustered.rows.length, 6)
    assert(clustered.sampledClusters.forall(index => index >= 0 && index < 3))
  }

  test("vector stability uses bounded-memory online moments keyed by unit and domain") {
    val key = StabilityKey(
      accepted(UnitId("u1")),
      SpaceId.unsafe("x-domain"),
      StabilityChannel.Loadings
    )
    var reducer = accepted(VectorStabilityReducer.empty(key, dimension = 3))
    var replicate = 0
    while replicate < 1000 do
      reducer = accepted(reducer.add(
        DoubleVector.fromSeq(Vector(
          replicate.toDouble,
          2.0 * replicate,
          if replicate % 2 == 0 then 1.0 else -1.0
        )),
        wasSelected = replicate < 750,
        ambiguousMatch = replicate % 100 == 0
      ))
      replicate += 1

    val storedArrays = reducer.productIterator.collect { case values: Array[Double] => values.length }.toVector
    assertEquals(storedArrays, Vector(3, 3))
    reducer.result match
      case Evidence.Computed(summary) =>
        assertEquals(summary.key, key)
        assertEquals(summary.replicates, 1000)
        assertEqualsDouble(summary.selectionFrequency, 0.75, 1e-15)
        assertEquals(summary.ambiguousMatches, 10)
        assertEqualsDouble(summary.mean(0), 499.5, 1e-12)
      case other => fail(s"expected computed vector stability, got $other")
  }

  test("subspace stability records failures without retaining replicate bases") {
    val unit = accepted(UnitId("plane-1"))
    val domain = SpaceId.unsafe("loading-domain")
    val angle = accepted(PrincipalAngles.between(
      DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(0.0))),
      DoubleMatrix.fromRows(Vector(Vector(Math.cos(0.2)), Vector(Math.sin(0.2))))
    ))
    val reducer = SubspaceStabilityReducer
      .empty(unit, domain)
      .add(Right(angle))
      .add(Left(UnavailableReason.InsufficientRank(expected = 1, actual = 0)))

    reducer.result match
      case Evidence.Computed(summary) =>
        assertEquals(summary.unit, unit)
        assertEquals(summary.domain, domain)
        assertEquals(summary.replicates, 1)
        assertEquals(summary.failedReplicates, 1)
        assertEqualsDouble(summary.meanAngle, 0.2, 1e-12)
      case other => fail(s"expected computed subspace stability, got $other")
  }
