package scalafim.fmri.threshold

import gale.linalg.{DMat, Matrix}
import scalafim.image.{PrimitiveBuffers, SampleSpaces, SomeScalarVolume, valueAtCanonicalOrdinal}

/** `StatKind` carries no degrees of freedom and no p-value sidedness.
  *
  * These tests pin why that is sound for the admitted permutation procedures.
  * Adjusted p-values depend on the observed and null statistics only through
  * the order of their oriented values. A strictly increasing, odd transform
  * applied to both the map and its null draws preserves that order under
  * every alternative. In exact arithmetic, a t-to-z conversion with one df
  * shared by all voxels is such a transform, so a df could not change maxT or
  * Westfall-Young p-values or rejections (the cutoff, which is on the value
  * scale, is transformed with it). Likewise, one- versus two-sided evidence is carried
  * by the alternative and the null draws together, not by a label.
  */
class StatKindSuite extends munit.FunSuite:

  private val dims = Vector(4, 4, 4)
  private val n = 64

  private def value[A](e: Either[ThresholdError, A]): A =
    e.fold(err => fail(err.message), identity)

  test("a stat kind fixes the evidence orientation and hence the admissible alternatives"):
    val expected = Map(
      StatKind.Z -> EvidenceOrientation.Signed,
      StatKind.T -> EvidenceOrientation.Signed,
      StatKind.NegLog10P -> EvidenceOrientation.Unsigned
    )
    assertEquals(expected.keySet, StatKind.values.toSet)
    val stat = volume(Array.tabulate(n)(i => 0.25 * (i % 5)))
    for kind <- StatKind.values; alternative <- ThresholdAlternative.values do
      val map = value(StatisticMap(stat, kind))
      assertEquals(map.orientation, expected(kind))
      val admissible = kind != StatKind.NegLog10P || alternative == ThresholdAlternative.Greater
      assertEquals(alternative.compatibleWith(map.orientation), admissible, s"$kind under $alternative")
      val field = StatisticField.fromMap(map, alternative)
      if admissible then assert(field.isRight, s"$kind under $alternative")
      else
        assertEquals(field.left.toOption, Some(ThresholdError.IncompatibleAlternative(alternative, EvidenceOrientation.Unsigned)))

  test("a signed t map accepts negative evidence that an unsigned map refuses"):
    val stat = volume(Array.tabulate(n)(i => if i == 3 then -2.5 else 1.0))
    assert(StatisticField.fromMap(StatisticMap.t(stat), ThresholdAlternative.Less).isRight)
    assertEquals(
      StatisticField.fromMap(StatisticMap.negLog10P(stat), ThresholdAlternative.Greater).left.toOption,
      Some(ThresholdError.NegativeUnsignedEvidence(3, -2.5))
    )

  test("t and z labels on the same values give identical maxT and HierScan decisions"):
    val rng = scala.util.Random(3090L)
    val stat = Array.tabulate(n)(i => if i < 8 then 5.0 else if i >= 56 then -5.0 else rng.nextGaussian())
    val draws = Vector.fill(39)(Array.fill(n)(rng.nextGaussian()))
    val alpha = Alpha.unsafe(0.1)
    var maxTRejections = 0
    for alternative <- ThresholdAlternative.values do
      val asT = value(MaxT.runMap(StatisticMap.t(volume(stat)), FixedNullDraw(draws), None, alpha, alternative))
      val asZ = value(MaxT.runMap(StatisticMap.z(volume(stat)), FixedNullDraw(draws), None, alpha, alternative))
      assertEquals(asT.params.get("statKind"), Some("T"))
      assertEquals(asZ.params.get("statKind"), Some("Z"))
      assertEquals(asT.params - "statKind", asZ.params - "statKind")
      assertEquals(asT.cutoff, asZ.cutoff)
      for v <- 0 until n do
        assertEqualsDouble(asT.pValues.get.valueAtCanonicalOrdinal(v), asZ.pValues.get.valueAtCanonicalOrdinal(v), 0.0)
        assertEquals(asT.reject.valueAtCanonicalOrdinal(v), asZ.reject.valueAtCanonicalOrdinal(v))
        if asT.reject.valueAtCanonicalOrdinal(v) then maxTRejections += 1

      val config = HierScanConfig(alpha = Alpha.unsafe(0.2), alternative = alternative, kappas = Vector(Kappa.unsafe(1.0)))
      val scanT = value(HierScan.runMap(StatisticMap.t(volume(stat)), FixedNullDraw(draws), config = config))
      val scanZ = value(HierScan.runMap(StatisticMap.z(volume(stat)), FixedNullDraw(draws), config = config))
      assert(scanT.significantRegions.nonEmpty, s"HierScan rejected nothing under $alternative")
      assertEquals(scanT.significantRegions.map(_.path), scanZ.significantRegions.map(_.path))
      for (a, b) <- scanT.significantRegions.zip(scanZ.significantRegions) do
        assertEqualsDouble(a.adjustedPValue, b.adjustedPValue, 0.0)
        assertEqualsDouble(a.score, b.score, 0.0)
      assertEquals(scanT.nodeTests.map(t => (t.path, t.rejected)), scanZ.nodeTests.map(t => (t.path, t.rejected)))
      // HierScan scores are not invariant under t-to-z, so the label must
      // survive into the result even though it changes no number here.
      assertEquals(scanT.params.get("statKind"), Some("T"))
      assertEquals(scanZ.params.get("statKind"), Some("Z"))
      assertEquals(scanT.params - "statKind", scanZ.params - "statKind")
    assert(maxTRejections > 0, "maxT rejected nothing, so the comparison is vacuous")

  test("every procedure records the stat kind of the map it was run on"):
    val rng = scala.util.Random(4090L)
    val stat = Array.tabulate(n)(i => if i < 8 then 5.0 else rng.nextGaussian())
    val draws = Vector.fill(19)(Array.fill(n)(rng.nextGaussian()))
    val config = HierScanConfig(alpha = Alpha.unsafe(0.2), kappas = Vector(Kappa.unsafe(1.0)))
    for kind <- StatKind.values do
      val map = value(StatisticMap(volume(stat.map(math.abs)), kind))
      val scan = value(HierScan.runMap(map, FixedNullDraw(draws.map(_.map(math.abs))), config = config))
      assertEquals(scan.params.get("statKind"), Some(kind.toString), s"HierScan on $kind")
      val maxT = value(MaxT.runMap(map, FixedNullDraw(draws.map(_.map(math.abs))), None, Alpha.unsafe(0.1), ThresholdAlternative.Greater))
      assertEquals(maxT.params.get("statKind"), Some(kind.toString), s"maxT on $kind")

  test("maxT and Westfall-Young p-values are invariant under a common odd strictly increasing transform"):
    // g(x) = x + x^3 is odd with g'(x) = 1 + 3x^2 > 0, the shape of any
    // fixed-df t-to-z conversion. On quarter-integer inputs with |x| <= 4 it
    // is computed exactly, so order and ties are preserved bit for bit.
    def g(x: Double): Double = x + x * x * x
    val rng = scala.util.Random(930L)
    def quarter(): Double = (rng.nextInt(33) - 16).toDouble / 4.0
    val tests = 12
    val observed = Array.tabulate(tests)(i => if i == 0 then 4.0 else if i == 1 then -4.0 else quarter())
    val nullRows = Vector.fill(59)(Array.fill(tests)(quarter()))
    val alpha = Alpha.unsafe(0.2)
    val families = Vector(
      NullReference.MonteCarlo -> nullRows,
      NullReference.ExactEnumeration -> (observed.clone +: nullRows)
    )
    for
      (reference, rows) <- families
      alternative <- ThresholdAlternative.values
      policy <- CorrectionPolicy.values
    do
      val raw = value(MultipleTesting.adjust(observed, matrix(rows), alpha, policy, alternative, reference))
      val mapped = value(MultipleTesting.adjust(observed.map(g), matrix(rows.map(_.map(g))), alpha, policy, alternative, reference))
      val label = s"$policy, $alternative, $reference"
      assertEquals(mapped.map(_.testIndex), raw.map(_.testIndex), label)
      for (a, b) <- raw.zip(mapped) do
        assertEqualsDouble(b.adjustedPValue, a.adjustedPValue, 0.0, label)
        assertEquals(b.rejected, a.rejected, label)

  test("a non-monotone transform does change the adjusted p-values"):
    // Negative control for the invariance test: squaring signed evidence
    // reorders it, so it is not a canonicalization the procedures may ignore.
    val observed = Array(3.0, -3.0, 0.5, -0.5)
    val nullRows = Vector.tabulate(19)(r => Array.tabulate(4)(c => ((r * 7 + c * 3) % 9 - 4).toDouble / 2.0))
    val raw = value(MaxT.singleStep(observed, matrix(nullRows), Alpha.unsafe(0.2), ThresholdAlternative.Greater, NullReference.MonteCarlo))
    val squared = value(
      MaxT.singleStep(
        observed.map(x => x * x),
        matrix(nullRows.map(_.map(x => x * x))),
        Alpha.unsafe(0.2),
        ThresholdAlternative.Greater,
        NullReference.MonteCarlo
      )
    )
    assert(raw.zip(squared).exists((a, b) => math.abs(a.adjustedPValue - b.adjustedPValue) > 0.0))

  test("one- and two-sided p-value evidence is carried by the alternative and the null draws"):
    // An unsigned map whose values are an increasing function of the oriented
    // t statistic, thresholded with Greater against draws transformed the same
    // way, reproduces the t map's maxT result under that orientation. No
    // sidedness label is needed or consulted.
    def up(a: Double): Double = a + a * a * a // increasing and non-negative on [0, inf)
    val rng = scala.util.Random(1930L)
    def quarter(): Double = (rng.nextInt(17) - 8).toDouble / 4.0 // in [-2, 2]
    val stat = Array.tabulate(n)(i => if i == 5 then 4.0 else if i == 50 then -4.0 else quarter())
    val draws = Vector.fill(39)(Array.fill(n)(quarter()))
    val alpha = Alpha.unsafe(0.1)
    for alternative <- Vector(ThresholdAlternative.TwoSided, ThresholdAlternative.Greater) do
      // Two-sided evidence depends on |t|; one-sided evidence on t, shifted
      // to be non-negative by the smallest attainable value (-4).
      def evidence(x: Double): Double =
        if alternative == ThresholdAlternative.TwoSided then up(math.abs(x)) else up(x + 4.0)
      val tResult = value(MaxT.runMap(StatisticMap.t(volume(stat)), FixedNullDraw(draws), None, alpha, alternative))
      val pResult = value(
        MaxT.runMap(
          StatisticMap.negLog10P(volume(stat.map(evidence))),
          FixedNullDraw(draws.map(_.map(evidence))),
          None,
          alpha,
          ThresholdAlternative.Greater
        )
      )
      var rejections = 0
      for v <- 0 until n do
        assertEqualsDouble(pResult.pValues.get.valueAtCanonicalOrdinal(v), tResult.pValues.get.valueAtCanonicalOrdinal(v), 0.0)
        assertEquals(pResult.reject.valueAtCanonicalOrdinal(v), tResult.reject.valueAtCanonicalOrdinal(v))
        if tResult.reject.valueAtCanonicalOrdinal(v) then rejections += 1
      assert(rejections > 0, s"$alternative rejected nothing, so the comparison is vacuous")

  private def matrix(rows: Vector[Array[Double]]): DMat =
    Matrix.tabulate(rows.length, rows.head.length)((r, c) => rows(r)(c))

  private def volume(data: Array[Double]): SomeScalarVolume[Double] =
    SomeScalarVolume.unsafeCopyFromCanonicalArray(PrimitiveBuffers.fromArray(data), SampleSpaces(dims))

  private final class FixedNullDraw(rows: Vector[Array[Double]]) extends NullDraw:
    override val nPermutations: PermutationCount = PermutationCount.unsafe(rows.length)
    override val reference: NullReference = NullReference.MonteCarlo
    override def draw(index: Int): Either[ThresholdError, Array[Double]] = Right(rows(index).clone)
