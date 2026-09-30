package scalafim.fmri.threshold

import gale.linalg.Matrix
import scalafim.image.{Mask, PrimitiveBuffers, SampleSpaces, SomeScalarVolume, valueAtCanonicalOrdinal}

class DecisionScaleSuite extends munit.FunSuite:

  private val dims = Vector(4, 4, 4)
  private val n = 64

  private def value[A](e: Either[ThresholdError, A]): A =
    e.fold(err => fail(err.message), identity)

  private def oracleOrient(alternative: ThresholdAlternative, x: Double): Double =
    alternative match
      case ThresholdAlternative.Greater  => x
      case ThresholdAlternative.Less     => 0.0 - x
      case ThresholdAlternative.TwoSided => if x < 0.0 then 0.0 - x else x

  test("voxelwise maxT map decisions, p-values and cutoff agree with a raw-draw oracle"):
    val rng = scala.util.Random(21L)
    val stat = Array.tabulate(n)(i => if i % 9 == 0 then 8.0 * (if i % 2 == 0 then 1.0 else -1.0) else rng.nextGaussian())
    val inMask = (0 until n).filter(_ % 7 != 3).toArray
    val mask = Mask.fromIndices(SampleSpaces(dims), inMask, "analysis")
    val draws = Vector.fill(49)(Array.fill(inMask.length)(rng.nextGaussian() * 1.5))
    val alpha = Alpha.unsafe(0.1)

    for alternative <- ThresholdAlternative.values do
      val result = value(MaxT.runMap(StatisticMap.z(volume(stat)), FixedNullDraw(draws), Some(mask), alpha, alternative))
      assertEquals(result.method, ThresholdMethod.MaxT)
      val pMap = result.pValueSemantics match
        case ThresholdPValues.Adjusted(values, CorrectionPolicy.MaxTSingleStep) => values
        case other                                                             => fail(s"expected maxT-adjusted p-values, found $other")

      val maxima = draws.map(_.map(oracleOrient(alternative, _)).max)
      var rejections = 0
      for v <- 0 until n do
        if inMask.contains(v) then
          val t = oracleOrient(alternative, stat(v))
          val expectedP = (maxima.count(_ >= t) + 1).toDouble / (draws.length + 1).toDouble
          val p = pMap.valueAtCanonicalOrdinal(v)
          val rejected = result.reject.valueAtCanonicalOrdinal(v)
          assertEqualsDouble(p, expectedP, 0.0)
          assertEquals(rejected, expectedP <= alpha.value, s"voxel $v under $alternative")
          assertEquals(value(result.cutoff.rejects(t)), rejected, s"cutoff at voxel $v under $alternative")
          if rejected then rejections += 1
        else
          assert(pMap.valueAtCanonicalOrdinal(v).isNaN)
          assert(!result.reject.valueAtCanonicalOrdinal(v))
      assert(rejections > 0, s"$alternative rejected nothing")
      assertEquals(result.params.get("nullReference"), Some("MonteCarlo"))

  test("voxelwise maxT map equals the matrix maxT procedure on the same draws"):
    val rng = scala.util.Random(5L)
    val stat = Array.tabulate(n)(i => if i == 17 then -5.0 else rng.nextGaussian())
    val draws = Vector.fill(29)(Array.fill(n)(rng.nextGaussian()))
    val alpha = Alpha.unsafe(0.2)

    for alternative <- ThresholdAlternative.values do
      val map = value(MaxT.runMap(StatisticMap.z(volume(stat)), FixedNullDraw(draws), None, alpha, alternative))
      val matrixResult = value(
        MaxT.singleStep(stat, Matrix.tabulate(draws.length, n)((r, c) => draws(r)(c)), alpha, alternative, NullReference.MonteCarlo)
      )
      val p = map.pValues.get
      for test <- matrixResult do
        assertEqualsDouble(p.valueAtCanonicalOrdinal(test.testIndex), test.adjustedP.value, 0.0)
        assertEquals(map.reject.valueAtCanonicalOrdinal(test.testIndex), test.rejected)

  test("HierScan rejects exactly the union of its significant regions and exposes no voxel cutoff"):
    val rng = scala.util.Random(31L)
    val stat = Array.tabulate(n): i =>
      val (x, y, z) = (i % 4, (i / 4) % 4, i / 16)
      if x < 2 && y < 2 && z < 2 then 3.0 + rng.nextDouble() else 0.3 * rng.nextGaussian()
    val draws = Vector.fill(39)(Array.fill(n)(rng.nextGaussian()))
    val result = value(HierScan.run(volume(stat), FixedNullDraw(draws), config = scanConfig(0.8)))

    val union = result.significantRegions.flatMap(_.maskSpaceIndices).toSet
    assert(union.nonEmpty)
    for v <- 0 until n do assertEquals(result.reject.valueAtCanonicalOrdinal(v), union.contains(v), s"voxel $v")
    assert(compileErrors("(??? : scalafim.fmri.threshold.HierScanResult).cutoff").nonEmpty)
    assert(compileErrors("(??? : scalafim.fmri.threshold.HierScanResult).threshold").nonEmpty)

  test("a failed null draw is reported with its index and never dropped"):
    val rng = scala.util.Random(2L)
    val draws = Vector.fill(9)(Array.fill(n)(rng.nextGaussian()))
    val failing = FixedNullDraw(draws, failAt = Some(4))
    val stat = StatisticMap.z(volume(Array.tabulate(n)(i => if i == 0 then 6.0 else 0.0)))

    assertEquals(
      MaxT.runMap(stat, failing, None, Alpha.unsafe(0.2), ThresholdAlternative.Greater).left.toOption,
      Some(ThresholdError.NullDrawFailed(4, ThresholdError.InvalidArgument("draw", "simulated failure")))
    )
    assertEquals(
      HierScan.runMap(stat, FixedNullDraw(draws, failAt = Some(4)), config = scanConfig(0.5)).left.toOption,
      Some(ThresholdError.NullDrawFailed(4, ThresholdError.InvalidArgument("draw", "simulated failure")))
    )

  test("HierScan refuses a null draw that changes when revisited at a deeper node"):
    val rng = scala.util.Random(8L)
    val stat = Array.tabulate(n): i =>
      val (x, y, z) = (i % 4, (i / 4) % 4, i / 16)
      if x < 2 && y < 2 && z < 2 then 5.0 else 0.1 * rng.nextGaussian()
    val draws = Vector.fill(39)(Array.fill(n)(rng.nextGaussian()))

    // The deterministic control descends, so the scan revisits every draw.
    val stable = value(HierScan.run(volume(stat), FixedNullDraw(draws), config = scanConfig(0.8)))
    assert(stable.nodeTests.map(_.depth).max > 1)

    val drifting = FixedNullDraw(draws, driftAfterFirstFetch = true)
    assertEquals(
      HierScan.run(volume(stat), drifting, config = scanConfig(0.8)).left.toOption,
      Some(ThresholdError.NondeterministicNullDraw(0))
    )

  private def scanConfig(alpha: Double): HierScanConfig =
    HierScanConfig(
      alpha = Alpha.unsafe(alpha),
      kappas = Vector(Kappa.unsafe(1.0)),
      minVoxels = 1,
      minAlpha = 1e-9,
      maxDepth = 8,
      priorEta = 1.0,
      minPriorMass = 0.0
    )

  private def volume(data: Array[Double]): SomeScalarVolume[Double] =
    SomeScalarVolume.unsafeCopyFromCanonicalArray(PrimitiveBuffers.fromArray(data), SampleSpaces(dims))

  private final class FixedNullDraw(
      rows: Vector[Array[Double]],
      failAt: Option[Int] = None,
      driftAfterFirstFetch: Boolean = false
  ) extends NullDraw:
    private val fetched = scala.collection.mutable.Set.empty[Int]

    override val nPermutations: PermutationCount =
      PermutationCount.unsafe(rows.length)

    override val reference: NullReference =
      NullReference.MonteCarlo

    override def draw(index: Int): Either[ThresholdError, Array[Double]] =
      if failAt.contains(index) then Left(ThresholdError.InvalidArgument("draw", "simulated failure"))
      else
        val out = rows(index).clone
        if driftAfterFirstFetch && !fetched.add(index) then out(0) = out(0) + 1e-9
        Right(out)
