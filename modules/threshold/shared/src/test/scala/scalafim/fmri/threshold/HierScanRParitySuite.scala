package scalafim.fmri.threshold

import scalafim.fmri.threshold.fixtures.HierScanRParityFixtures
import scalafim.fmri.threshold.fixtures.HierScanRParityFixtures.RHit
import scalafim.image.{PrimitiveBuffers, SampleSpaces, SomeScalarVolume, valueAtCanonicalOrdinal}

/** HierScan against `neurothresh::hier_descend` (step-down, whole brain) on
  * fixed statistic maps and null matrices; see
  * tools/r-parity/generate_threshold_hierscan_fixtures.R.
  */
class HierScanRParitySuite extends munit.FunSuite:

  // R vectors are column-major (x fastest); scalafim's canonical ordinal is
  // z fastest. Convert values in and region indices back out.
  private def rIndex(dims: Vector[Int], canonical: Int): Int =
    val (nx, ny, nz) = (dims(0), dims(1), dims(2))
    val z = canonical % nz
    val y = (canonical / nz) % ny
    val x = canonical / (nz * ny)
    x + nx * (y + ny * z)

  private def toCanonical(dims: Vector[Int], rValues: Vector[Double]): Array[Double] =
    Array.tabulate(dims.product)(c => rValues(rIndex(dims, c)))

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
      priorEta: Double,
      prior: Vector[Double]
  ): HierScanResult =
    def vol(values: Vector[Double]) = SomeScalarVolume.unsafeCopyFromCanonicalArray(PrimitiveBuffers.fromArray(toCanonical(dims, values)), SampleSpaces(dims))
    val volume = vol(stat)
    val draws = new NullDraw:
      override val nPermutations: PermutationCount = PermutationCount.unsafe(nulls.length)
      override val reference: NullReference = NullReference.MonteCarlo
      override def draw(index: Int): Either[ThresholdError, Array[Double]] = Right(toCanonical(dims, nulls(index)))
    val config = HierScanConfig(
      alpha = Alpha.unsafe(alpha),
      kappas = kappas.map(Kappa.unsafe),
      minVoxels = minVoxels,
      minAlpha = 1e-6,
      priorEta = priorEta,
      gamma = gamma
    )
    value(HierScan.run(volume, draws, prior = Some(vol(prior)), config = config))

  private def assertParity(result: HierScanResult, expected: Vector[RHit], dims: Vector[Int]): Unit =
    // A full mask makes mask-space indices equal canonical ordinals.
    val actual = result.significantRegions.map(hit => (hit.maskSpaceIndices.map(rIndex(dims, _)).sorted, hit)).sortBy(_._1.mkString(","))
    val reference = expected.sortBy(_.indices.mkString(","))
    assertEquals(actual.map(_._1), reference.map(_.indices))
    actual.map(_._2).zip(reference).foreach: (hit, r) =>
      assertEqualsDouble(hit.score, r.score, 1e-9 * math.max(1.0, math.abs(r.score)))
      assertEqualsDouble(hit.adjustedP.value, r.adjustedP, 1e-12)
      assertEqualsDouble(hit.alphaTest, r.alphaTest, 1e-15)
    val union = reference.flatMap(_.indices).toSet
    for c <- 0 until dims.product do assertEquals(result.reject.valueAtCanonicalOrdinal(c), union.contains(rIndex(dims, c)), s"canonical voxel $c")

  test("block signal: every rejected node at every level matches neurothresh"):
    import HierScanRParityFixtures.BlockSignal as F
    val result = run(F.dims, F.stat, F.nulls, F.alpha, F.kappas, F.gamma, F.minVoxels, F.priorEta, F.prior)
    assert(F.hits.map(_.alphaTest).distinct.length > 1, "fixture must span more than one level")
    assertParity(result, F.hits, F.dims)

  test("single strong voxel: the coarse region and its localized voxel are both reported"):
    import HierScanRParityFixtures.SingleVoxel as F
    val result = run(F.dims, F.stat, F.nulls, F.alpha, F.kappas, F.gamma, F.minVoxels, F.priorEta, F.prior)
    assertParity(result, F.hits, F.dims)

  test("unequal extents and a non-uniform prior: mass-weighted descendant budgets and non-floor p-values match"):
    import HierScanRParityFixtures.UnequalPrior as F
    val result = run(F.dims, F.stat, F.nulls, F.alpha, F.kappas, F.gamma, F.minVoxels, F.priorEta, F.prior)
    val floor = 1.0 / (F.nulls.length + 1).toDouble
    assert(F.hits.exists(_.adjustedP > floor), "fixture must include a p-value above the floor")
    assert(F.hits.map(_.alphaTest).exists(a => a != F.alpha * F.gamma && a != F.alpha * F.gamma * (1 - F.gamma)), "fixture must split a budget by mass")
    assertParity(result, F.hits, F.dims)
