package scalafim.fmri.threshold

import scalafim.image.{PrimitiveBuffers, NeuroSpace, NeuroVol}

class HierScanSuite extends munit.FunSuite:

  private def value[A](e: Either[ThresholdError, A]): A =
    e.fold(err => fail(err.message), identity)

  test("HierScan marks a rejected singleton child in the whole-mask split") {
    val stat = volume(Vector(2, 2, 2), Array(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 5.0))
    val nulls = FixedNullDraw(Vector.fill(9)(Array.fill(8)(1.0)))
    val result = value(HierScan.run(stat, nulls, config = simpleConfig(alpha = 0.2)))

    assertEquals(result.method, ThresholdMethod.HierScan)
    assertEquals(result.significantRegions.size, 1)
    assertEquals(result.significantRegions.head.path, Vector(7))
    assertEquals(result.significantRegions.head.maskSpaceIndices, Vector(7))
    assertEquals(result.nodeTests.size, 8)
    assertEquals(result.nodeTests.filter(_.rejected).map(_.path), Vector(Vector(7)))
    assert(result.reject.linear(7))
    assertEquals((0 until 8).count(result.reject.linear), 1)
    assertEqualsDouble(result.threshold, result.significantRegions.head.score, 1e-12)
    assertEqualsDouble(result.cutoff.toLegacyDouble, result.significantRegions.head.score, 1e-12)
  }

  test("HierScan descends into rejected octree children") {
    val data = Array.fill(16)(0.1)
    data(15) = 6.0
    val stat = volume(Vector(4, 2, 2), data)
    val nulls = FixedNullDraw(Vector.fill(9)(Array.fill(16)(0.2)))
    val result = value(HierScan.run(stat, nulls, config = simpleConfig(alpha = 0.8)))

    assertEquals(result.significantRegions.size, 1)
    assertEquals(result.significantRegions.head.path, Vector(7, 1))
    assertEquals(result.significantRegions.head.maskSpaceIndices, Vector(15))
    assert(result.nodeTests.exists(t => t.path == Vector(7) && t.rejected))
    assert(result.nodeTests.exists(t => t.path == Vector(7, 1) && t.rejected))
    assert(result.reject.linear(15))
    assertEquals((0 until 16).count(result.reject.linear), 1)
  }

  test("HierScan keeps the reject mask empty when the null dominates") {
    val stat = volume(Vector(2, 2, 2), Array(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 3.0))
    val nulls = FixedNullDraw(Vector.fill(9)(Array.fill(8)(10.0)))
    val result = value(HierScan.run(stat, nulls, config = simpleConfig(alpha = 0.5)))

    assertEquals(result.significantRegions, Vector.empty)
    assertEquals(result.nodeTests.count(_.rejected), 0)
    assert(result.threshold.isPosInfinity)
    assertEquals(result.cutoff, ThresholdCutoff.NoRejections)
    assertEquals((0 until 8).count(result.reject.linear), 0)
  }

  test("HierScan accepts an unsigned statistic map only with a greater alternative") {
    val stat = StatisticMap.negLog10P(volume(Vector(2, 2, 2), Array(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 5.0)), PSide.OneSided)
    val nulls = FixedNullDraw(Vector.fill(9)(Array.fill(8)(1.0)))
    val ok = value(HierScan.runMap(stat, nulls, config = simpleConfig(alpha = 0.2)))

    assertEquals(ok.significantRegions.size, 1)
    assertEquals(
      HierScan.runMap(
        stat,
        nulls,
        config = simpleConfig(alpha = 0.2).copy(alternative = ThresholdAlternative.TwoSided)
      ).left.toOption,
      Some(ThresholdError.IncompatibleAlternative(ThresholdAlternative.TwoSided, EvidenceOrientation.Unsigned))
    )
  }

  test("HierScan rejects null draws with the wrong mask-space length") {
    val stat = volume(Vector(2, 2, 2), Array.fill(8)(1.0))
    val nulls = FixedNullDraw(Vector(Array.fill(7)(0.0)))

    assertEquals(
      HierScan.run(stat, nulls, config = simpleConfig(alpha = 0.5)).left.toOption,
      Some(ThresholdError.ShapeMismatch("null draw", "8", "7"))
    )
  }

  private def simpleConfig(alpha: Double): HierScanConfig =
    HierScanConfig(
      alpha = Alpha.unsafe(alpha),
      kappas = Vector(Kappa.unsafe(1.0)),
      minVoxels = 1,
      minAlpha = 1e-9,
      maxDepth = 8,
      priorEta = 1.0,
      minPriorMass = 0.0
    )

  private def volume(dims: Vector[Int], data: Array[Double]): NeuroVol[Double] =
    NeuroVol.fromLinear(PrimitiveBuffers.fromArray(data), NeuroSpace(dims))

  private final class FixedNullDraw(rows: Vector[Array[Double]]) extends NullDraw:
    override val nPermutations: PermutationCount =
      PermutationCount.unsafe(rows.length)

    override def draw(index: Int): Either[ThresholdError, Array[Double]] =
      if index < 0 || index >= rows.length then Left(ThresholdError.IndexOutOfBounds(index, rows.length))
      else Right(rows(index).clone)
