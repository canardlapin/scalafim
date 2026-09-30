package scalafim.fmri.threshold

import scalafim.fmri.threshold.fixtures.HierScanRParityFixtures
import scalafim.fmri.threshold.fixtures.HierScanRParityFixtures.RHit
import scalafim.image.{PrimitiveBuffers, SampleSpaces, SomeScalarVolume, valueAtCanonicalOrdinal}

/** HierScan against `neurothresh::hier_descend` (step-down, whole brain) on
  * fixed statistic maps and null matrices; see
  * tools/r-parity/generate_threshold_hierscan_fixtures.R.
  */
class HierScanRParitySuite extends munit.FunSuite:

  private def value[A](e: Either[ThresholdError, A]): A =
    e.fold(err => fail(err.message), identity)

  private def run(
      dims: Vector[Int],
      stat: Vector[Double],
      nulls: Vector[Vector[Double]],
      alpha: Double,
      kappas: Vector[Double],
      gamma: Double,
      minVoxels: Int,
      priorEta: Double
  ): HierScanResult =
    val volume = SomeScalarVolume.unsafeCopyFromCanonicalArray(PrimitiveBuffers.fromArray(stat.toArray), SampleSpaces(dims))
    val draws = new NullDraw:
      override val nPermutations: PermutationCount = PermutationCount.unsafe(nulls.length)
      override val reference: NullReference = NullReference.MonteCarlo
      override def draw(index: Int): Either[ThresholdError, Array[Double]] = Right(nulls(index).toArray)
    val config = HierScanConfig(
      alpha = Alpha.unsafe(alpha),
      kappas = kappas.map(Kappa.unsafe),
      minVoxels = minVoxels,
      minAlpha = 1e-6,
      priorEta = priorEta,
      gamma = gamma
    )
    value(HierScan.run(volume, draws, config = config))

  private def assertParity(result: HierScanResult, expected: Vector[RHit], n: Int): Unit =
    val actual = result.significantRegions.map(hit => (hit.maskSpaceIndices.sorted, hit)).sortBy(_._1.mkString(","))
    val reference = expected.sortBy(_.indices.mkString(","))
    assertEquals(actual.map(_._1), reference.map(_.indices))
    actual.map(_._2).zip(reference).foreach: (hit, r) =>
      assertEqualsDouble(hit.score, r.score, 1e-9 * math.max(1.0, math.abs(r.score)))
      assertEqualsDouble(hit.adjustedP.value, r.adjustedP, 1e-12)
      assertEqualsDouble(hit.alphaTest, r.alphaTest, 1e-15)
    val union = reference.flatMap(_.indices).toSet
    for v <- 0 until n do assertEquals(result.reject.valueAtCanonicalOrdinal(v), union.contains(v), s"voxel $v")

  test("block signal: every rejected node at every level matches neurothresh"):
    import HierScanRParityFixtures.BlockSignal as F
    val result = run(F.dims, F.stat, F.nulls, F.alpha, F.kappas, F.gamma, F.minVoxels, F.priorEta)
    assert(F.hits.map(_.alphaTest).distinct.length > 1, "fixture must span more than one level")
    assertParity(result, F.hits, F.dims.product)

  test("single strong voxel: the coarse region and its localized voxel are both reported"):
    import HierScanRParityFixtures.SingleVoxel as F
    val result = run(F.dims, F.stat, F.nulls, F.alpha, F.kappas, F.gamma, F.minVoxels, F.priorEta)
    assertParity(result, F.hits, F.dims.product)
