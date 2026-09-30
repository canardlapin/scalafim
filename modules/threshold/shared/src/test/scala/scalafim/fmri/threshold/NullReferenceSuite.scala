package scalafim.fmri.threshold

import gale.linalg.{DMat, Matrix}
import scalafim.image.{PrimitiveBuffers, SampleSpaces, SomeScalarVolume, valueAtCanonicalOrdinal}

class NullReferenceSuite extends munit.FunSuite:

  private val alternatives = ThresholdAlternative.values.toVector
  private val references = NullReference.values.toVector

  private def value[A](e: Either[ThresholdError, A]): A =
    e.fold(err => fail(err.message), identity)

  // Written from the definitions, independently of ThresholdAlternative.applyTo.
  private def oracleOrient(alternative: ThresholdAlternative, x: Double): Double =
    alternative match
      case ThresholdAlternative.Greater  => x
      case ThresholdAlternative.Less     => 0.0 - x
      case ThresholdAlternative.TwoSided => if x < 0.0 then 0.0 - x else x

  private def oracleP(reference: NullReference, exceedances: Int, draws: Int): Double =
    reference match
      case NullReference.MonteCarlo       => (exceedances + 1).toDouble / (draws + 1).toDouble
      case NullReference.ExactEnumeration => exceedances.toDouble / draws.toDouble

  test("only implemented threshold methods are advertised"):
    assertEquals(ThresholdMethod.values.toVector, Vector(ThresholdMethod.HierScan, ThresholdMethod.MaxT))
    assert(compileErrors("scalafim.fmri.threshold.ThresholdMethod.Tfce").nonEmpty)
    assert(compileErrors("scalafim.fmri.threshold.ThresholdMethod.ClusterFdr").nonEmpty)
    assert(compileErrors("scalafim.fmri.threshold.ThresholdMethod.RftPeak").nonEmpty)
    assert(compileErrors("scalafim.fmri.threshold.ThresholdMethod.RftCluster").nonEmpty)

  test("streamed max-null p-values and cutoffs equal a raw-draw oracle for every alternative and reference"):
    val levels = Vector(-2.0, -0.5, 1.0, 2.0)
    val fields = for a <- levels; b <- levels yield Array(a, b)
    val drawSets = Vector.range(1, 4).flatMap(n => sequences(n, fields))
    val observedRaw = Vector(-2.5, -2.0, -1.0, -0.5, 0.0, 0.5, 1.0, 2.0, 2.5)
    var comparisons = 0
    var refusals = 0

    for
      draws <- drawSets
      alternative <- alternatives
      reference <- references
    do
      // The Monte Carlo reduction orients the raw draws; the exact-enumeration
      // arithmetic is then exercised on the same maxima through the trust
      // boundary, since these draw sets need not contain an identity draw.
      val streamed = value(MaxNull.reduce(FixedNullDraw(draws, NullReference.MonteCarlo), Array(0.0, 0.0), alternative, EvidenceOrientation.Signed))
      val nulls = value(MaxNullDistribution.fromOrientedMaxima(streamed.toArray, alternative, reference))
      val oracleMaxima = draws.map(field => field.map(oracleOrient(alternative, _)).max)
      assertEquals(nulls.toArray.toVector, oracleMaxima)
      val b = draws.length

      alphas(b).foreach: alpha =>
        val cutoff = value(MaxNull.cutoff(nulls, alpha))
        observedRaw.foreach: raw =>
          val t = oracleOrient(alternative, raw)
          val exceedances = oracleMaxima.count(_ >= t)
          val published = MaxNull.pValues(Array(raw), nulls)
          if reference == NullReference.ExactEnumeration && exceedances == 0 then
            assertEquals(published.left.toOption, Some(ThresholdError.MissingIdentityAction(0)))
            refusals += 1
          else
            val expected = oracleP(reference, exceedances, b)
            assertEqualsDouble(value(published).head.value, expected, 0.0)
            assertEquals(value(cutoff.rejects(t)), expected <= alpha.value)
            comparisons += 1

    assertEquals(drawSets.length, 16 + 256 + 4096)
    assert(comparisons > 1000000, s"only $comparisons comparisons")
    assert(refusals > 0)

  test("exact enumeration with the identity action equals Monte Carlo over the remaining actions"):
    val rng = scala.util.Random(20260930L)
    for
      trial <- 0 until 40
      alternative <- alternatives
    do
      val m = 1 + trial % 5
      val observed = Array.fill(m)(rng.nextGaussian() * 2.0)
      val others = Vector.fill(3 + trial % 11)(Array.fill(m)(rng.nextGaussian() * 2.0))
      val exactRows = observed.clone +: others
      val alpha = Alpha.unsafe(0.2)

      val wyExact = value(WestfallYoung.stepDown(observed, matrix(exactRows), alpha, alternative, NullReference.ExactEnumeration))
      val wyMonte = value(WestfallYoung.stepDown(observed, matrix(others), alpha, alternative, NullReference.MonteCarlo))
      assertEquals(wyExact.map(_.adjustedP.value), wyMonte.map(_.adjustedP.value))
      assertEquals(wyExact.map(_.rejected), wyMonte.map(_.rejected))

      val maxTExact = value(MaxT.singleStep(observed, matrix(exactRows), alpha, alternative, NullReference.ExactEnumeration))
      val maxTMonte = value(MaxT.singleStep(observed, matrix(others), alpha, alternative, NullReference.MonteCarlo))
      assertEquals(maxTExact.map(_.adjustedP.value), maxTMonte.map(_.adjustedP.value))

      val exact = value(MaxNull.reduce(FixedNullDraw(exactRows, NullReference.ExactEnumeration), observed, alternative, EvidenceOrientation.Signed))
      val monte = value(MaxNull.reduce(FixedNullDraw(others, NullReference.MonteCarlo), observed, alternative, EvidenceOrientation.Signed))
      assertEquals(value(MaxNull.pValueDoubles(observed, exact)).toVector, value(MaxNull.pValueDoubles(observed, monte)).toVector)

  test("less and two-sided alternatives equal the greater alternative on negated and absolute inputs"):
    val rng = scala.util.Random(7L)
    for trial <- 0 until 40 do
      val m = 1 + trial % 6
      val observed = Array.fill(m)(rng.nextGaussian() * 2.0)
      val rows = Vector.fill(4 + trial % 9)(Array.fill(m)(rng.nextGaussian() * 2.0))
      val alpha = Alpha.unsafe(0.25)
      val reference = NullReference.MonteCarlo

      def greater(transform: Double => Double): Vector[Double] =
        value(WestfallYoung.stepDown(observed.map(transform), matrix(rows.map(_.map(transform))), alpha, ThresholdAlternative.Greater, reference))
          .map(_.adjustedP.value)

      def via(alternative: ThresholdAlternative): Vector[Double] =
        value(WestfallYoung.stepDown(observed, matrix(rows), alpha, alternative, reference)).map(_.adjustedP.value)

      assertEquals(via(ThresholdAlternative.Less), greater(x => -x))
      assertEquals(via(ThresholdAlternative.TwoSided), greater(math.abs))

  test("a hand-computed less-alternative max-null orients observed and null draws together"):
    val draws = Vector(Array(1.0, -3.0), Array(2.0, -1.0), Array(0.0, -5.0))
    val nulls = value(MaxNull.reduce(FixedNullDraw(draws, NullReference.MonteCarlo), Array(0.0, 0.0), ThresholdAlternative.Less, EvidenceOrientation.Signed))
    assertEquals(nulls.toArray.toVector, Vector(3.0, 1.0, 5.0))
    assertEquals(nulls.alternative, ThresholdAlternative.Less)
    // Observed -4 orients to 4; only the draw with maximum 5 reaches it.
    assertEqualsDouble(value(MaxNull.pValueDoubles(Array(-4.0, 4.0), nulls))(0), 0.5, 0.0)
    // Observed +4 orients to -4, which every draw reaches.
    assertEqualsDouble(value(MaxNull.pValueDoubles(Array(-4.0, 4.0), nulls))(1), 1.0, 0.0)

  test("exact enumeration refuses a null matrix without the identity row"):
    val observed = Array(1.0, 9.0)
    val rows = Vector(Array(0.5, 2.0), Array(3.0, 1.0))
    def refusal(r: Vector[Array[Double]], alternative: ThresholdAlternative) =
      (
        MaxT.singleStep(observed, matrix(r), Alpha.unsafe(0.5), alternative, NullReference.ExactEnumeration).left.toOption,
        WestfallYoung.stepDown(observed, matrix(r), Alpha.unsafe(0.5), alternative, NullReference.ExactEnumeration).left.toOption
      )
    assertEquals(refusal(rows, ThresholdAlternative.Greater), (Some(ThresholdError.MissingIdentityRow), Some(ThresholdError.MissingIdentityRow)))
    assert(MaxT.singleStep(observed, matrix(rows), Alpha.unsafe(0.5), ThresholdAlternative.Greater, NullReference.MonteCarlo).isRight)

    // Every observed statistic is reached, yet no row is the identity action:
    // a mislabelled Monte Carlo sample must not be admitted as exact.
    val dominating = Vector(Array(2.0, 10.0), Array(0.5, 0.5))
    assertEquals(refusal(dominating, ThresholdAlternative.Greater), (Some(ThresholdError.MissingIdentityRow), Some(ThresholdError.MissingIdentityRow)))
    assertEquals(
      WestfallYoung.stepDown(Array(1.0), matrix(Vector(Array(2.0), Array(0.5))), Alpha.unsafe(0.5), ThresholdAlternative.Greater, NullReference.ExactEnumeration).left.toOption,
      Some(ThresholdError.MissingIdentityRow)
    )

    // The identity row is recognised after orientation: under a two-sided
    // alternative a sign-flipped copy of the observed statistics is equivalent.
    val flipped = Vector(Array(-1.0, -9.0), Array(0.5, 2.0))
    assertEquals(refusal(flipped, ThresholdAlternative.TwoSided), (None, None))
    assertEquals(refusal(flipped, ThresholdAlternative.Greater), (Some(ThresholdError.MissingIdentityRow), Some(ThresholdError.MissingIdentityRow)))

  test("max-null exact enumeration refuses observed statistics that no draw reaches"):
    val nulls = value(MaxNullDistribution.fromOrientedMaxima(Array(2.0, 3.0), ThresholdAlternative.Greater, NullReference.ExactEnumeration))
    assertEquals(MaxNull.pValues(Array(1.0, 9.0), nulls).left.toOption, Some(ThresholdError.MissingIdentityAction(1)))

  test("exact cutoffs admit only attainable p-values"):
    // B = 10 exact actions: attainable p-values are 1/10, 2/10, ...
    val maxima = Array(10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0)
    val exact = value(MaxNullDistribution.fromOrientedMaxima(maxima, ThresholdAlternative.Greater, NullReference.ExactEnumeration))
    assertEquals(value(MaxNull.cutoff(exact, Alpha.unsafe(0.05))), ThresholdCutoff.NoRejections)
    value(MaxNull.cutoff(exact, Alpha.unsafe(0.2))) match
      case ThresholdCutoff.Exclusive(boundary) => assertEqualsDouble(boundary, 8.0, 0.0)
      case other                               => fail(s"expected an exclusive cutoff, found $other")
    // Plus-one with the same ten maxima needs one more exceedance of room.
    val monte = value(MaxNullDistribution.fromOrientedMaxima(maxima, ThresholdAlternative.Greater, NullReference.MonteCarlo))
    value(MaxNull.cutoff(monte, Alpha.unsafe(0.2))) match
      case ThresholdCutoff.Exclusive(boundary) => assertEqualsDouble(boundary, 9.0, 0.0)
      case other                               => fail(s"expected an exclusive cutoff, found $other")

  test("streaming reduction requests each draw once and validates it"):
    val draws = Vector(Array(1.0, 2.0), Array(0.5, -1.0), Array(3.0, 0.0))
    val counting = FixedNullDraw(draws, NullReference.MonteCarlo)
    value(MaxNull.reduce(counting, Array(0.0, 0.0), ThresholdAlternative.TwoSided, EvidenceOrientation.Signed))
    assertEquals(counting.requests.toVector, Vector(0, 1, 2))

    def reduce(rows: Vector[Array[Double]], alternative: ThresholdAlternative, orientation: EvidenceOrientation) =
      MaxNull.reduce(FixedNullDraw(rows, NullReference.MonteCarlo), Array(0.0, 0.0), alternative, orientation).left.toOption

    assertEquals(reduce(Vector(Array(1.0)), ThresholdAlternative.Greater, EvidenceOrientation.Signed), Some(ThresholdError.NullDrawFailed(0, ThresholdError.ShapeMismatch("null draw", "2", "1"))))
    assertEquals(reduce(Vector(Array(1.0, Double.NaN)), ThresholdAlternative.Greater, EvidenceOrientation.Signed), Some(ThresholdError.NullDrawFailed(0, ThresholdError.NonFiniteData("null draw"))))
    assertEquals(reduce(Vector(Array(1.0, -0.5)), ThresholdAlternative.Greater, EvidenceOrientation.Unsigned), Some(ThresholdError.NullDrawFailed(0, ThresholdError.NegativeUnsignedEvidence(1, -0.5))))
    assertEquals(
      reduce(Vector(Array(1.0, 0.5)), ThresholdAlternative.Less, EvidenceOrientation.Unsigned),
      Some(ThresholdError.IncompatibleAlternative(ThresholdAlternative.Less, EvidenceOrientation.Unsigned))
    )
    assert(MaxNullDistribution.fromOrientedMaxima(Array.emptyDoubleArray, ThresholdAlternative.Greater, NullReference.MonteCarlo).isLeft)
    assert(MaxNull.reduce(FixedNullDraw(draws, NullReference.MonteCarlo), Array.emptyDoubleArray, ThresholdAlternative.Greater, EvidenceOrientation.Signed).isLeft)

  test("streamed exact enumeration requires the identity draw, not only dominating draws"):
    val observed = Array(1.0, -2.0)
    val dominating = Vector(Array(5.0, 5.0), Array(4.0, 3.0))
    assertEquals(
      MaxNull.reduce(FixedNullDraw(dominating, NullReference.ExactEnumeration), observed, ThresholdAlternative.TwoSided, EvidenceOrientation.Signed).left.toOption,
      Some(ThresholdError.MissingIdentityRow)
    )
    // A sign-flipped copy is the identity under the two-sided alternative only.
    val withIdentity = Array(-1.0, 2.0) +: dominating
    assert(MaxNull.reduce(FixedNullDraw(withIdentity, NullReference.ExactEnumeration), observed, ThresholdAlternative.TwoSided, EvidenceOrientation.Signed).isRight)
    assertEquals(
      MaxNull.reduce(FixedNullDraw(withIdentity, NullReference.ExactEnumeration), observed, ThresholdAlternative.Greater, EvidenceOrientation.Signed).left.toOption,
      Some(ThresholdError.MissingIdentityRow)
    )

  test("HierScan under the less alternative equals the greater alternative on negated statistic and draws"):
    val stat = hierField(sign = -1.0)
    val rng = scala.util.Random(11L)
    val draws = Vector.fill(39)(Array.fill(64)(rng.nextGaussian()))
    val config = scanConfig(0.8)

    val less = value(HierScan.run(cube(stat), FixedNullDraw(draws, NullReference.MonteCarlo), config = config.copy(alternative = ThresholdAlternative.Less)))
    val greater = value(HierScan.run(cube(stat.map(-_)), FixedNullDraw(draws.map(_.map(-_)), NullReference.MonteCarlo), config = config))

    assertEquals(less.nodeTests.map(t => (t.path, t.adjustedP.value, t.rejected)), greater.nodeTests.map(t => (t.path, t.adjustedP.value, t.rejected)))
    assertEquals((0 until 64).map(less.reject.valueAtCanonicalOrdinal), (0 until 64).map(greater.reject.valueAtCanonicalOrdinal))
    assert(less.significantRegions.nonEmpty)
    assert(less.nodeTests.filter(_.rejected).map(_.depth).max > 1)
    assertEquals(less.params.get("nullReference"), Some("MonteCarlo"))

  test("HierScan exact enumeration with the identity action equals Monte Carlo over the remaining actions"):
    val rng = scala.util.Random(13L)
    val others = Vector.fill(39)(Array.fill(64)(rng.nextGaussian()))
    val config = scanConfig(0.8)

    for alternative <- alternatives do
      val stat = hierField(sign = if alternative == ThresholdAlternative.Less then -1.0 else 1.0)
      val c = config.copy(alternative = alternative)
      val exact = value(HierScan.run(cube(stat), FixedNullDraw(stat.clone +: others, NullReference.ExactEnumeration), config = c))
      val monte = value(HierScan.run(cube(stat), FixedNullDraw(others, NullReference.MonteCarlo), config = c))
      assertEquals(exact.nodeTests.map(t => (t.path, t.adjustedP.value, t.rejected)), monte.nodeTests.map(t => (t.path, t.adjustedP.value, t.rejected)))
      assert(exact.significantRegions.nonEmpty, s"$alternative rejected nothing")
      assert(exact.nodeTests.filter(_.rejected).map(_.depth).max > 1, s"$alternative did not descend")
      assertEquals(exact.params.get("nullReference"), Some("ExactEnumeration"))

  /** A 4x4x4 field with weak asymmetric background and a strong signal in
    * one 2x2x2 octant, so the scan rejects and descends below the root split.
    */
  private def hierField(sign: Double): Array[Double] =
    val rng = scala.util.Random(3L)
    Array.tabulate(64): i =>
      val x = i % 4
      val y = (i / 4) % 4
      val z = i / 16
      if x >= 2 && y >= 2 && z >= 2 then sign * (6.0 + (i % 3).toDouble) else 0.3 * rng.nextGaussian()

  private def cube(data: Array[Double]): SomeScalarVolume[Double] =
    SomeScalarVolume.unsafeCopyFromCanonicalArray(PrimitiveBuffers.fromArray(data), SampleSpaces(Vector(4, 4, 4)))

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

  private def matrix(rows: Vector[Array[Double]]): DMat =
    Matrix.tabulate(rows.length, rows.head.length)((row, col) => rows(row)(col))

  private def sequences(length: Int, fields: Vector[Array[Double]]): Vector[Vector[Array[Double]]] =
    if length == 0 then Vector(Vector.empty)
    else sequences(length - 1, fields).flatMap(prefix => fields.map(prefix :+ _))

  private def alphas(draws: Int): Vector[Alpha] =
    val attainable =
      Vector.range(1, draws + 2).flatMap: k =>
        Vector(k.toDouble / draws.toDouble, k.toDouble / (draws + 1).toDouble)
    (Vector(0.01, 0.05, 0.3, 0.5, 0.9) ++ attainable.flatMap(v => Vector(Math.nextDown(v), v, Math.nextUp(v))))
      .distinct
      .flatMap(v => Alpha(v).toOption)

  private final class FixedNullDraw(rows: Vector[Array[Double]], override val reference: NullReference) extends NullDraw:
    val requests = scala.collection.mutable.ArrayBuffer.empty[Int]

    override val nPermutations: PermutationCount =
      PermutationCount.unsafe(rows.length)

    override def draw(index: Int): Either[ThresholdError, Array[Double]] =
      requests += index
      if index < 0 || index >= rows.length then Left(ThresholdError.IndexOutOfBounds(index, rows.length))
      else Right(rows(index).clone)
