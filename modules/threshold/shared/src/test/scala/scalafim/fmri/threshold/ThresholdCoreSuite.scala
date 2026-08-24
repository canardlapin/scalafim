package scalafim.fmri.threshold

import gale.linalg.{DMat, Matrix}
import scalafim.image.{Mask, SampleSpaces, SomeScalarVolume}

class ThresholdCoreSuite extends munit.FunSuite:

  private def matrix(rows: Vector[Vector[Double]]): DMat =
    require(rows.nonEmpty && rows.forall(_.length == rows.head.length))
    Matrix.tabulate(rows.length, rows.head.length)((row, col) => rows(row)(col))

  private def value[A](e: Either[ThresholdError, A]): A =
    e.fold(err => fail(err.message), identity)

  test("checked scalar constructors reject invalid inference settings") {
    assert(Alpha(0.05).isRight)
    assertEquals(Alpha(0.0).left.toOption, Some(ThresholdError.InvalidAlpha(0.0)))
    assertEquals(QValue(1.0).left.toOption, Some(ThresholdError.InvalidQValue(1.0)))
    assertEquals(Kappa(-1.0).left.toOption, Some(ThresholdError.InvalidKappa(-1.0)))
    assertEquals(DegreesOfFreedom(0.0).left.toOption, Some(ThresholdError.InvalidDegreesOfFreedom(0.0)))
    assertEquals(AdjustedP(1.1).left.toOption, Some(ThresholdError.InvalidAdjustedPValue(1.1)))
    assertEquals(PermutationCount(0).left.toOption, Some(ThresholdError.InvalidPermutationCount(0)))
  }

  test("masked field gathers finite voxels and applies tail transform") {
    val sp = SampleSpaces(Vector(2, 2, 1))
    val stat = SomeScalarVolume.unsafeCopyFromCanonicalArray[Double](Array(1.0, -2.0, Double.NaN, 4.0), sp)
    val field = value(MaskedField.fromVolume(stat, Tail.TwoSided))

    assertEquals(field.size, 3)
    assertEquals(field.volumeIndicesCopy.toVector, Vector(0, 1, 3))
    assertEquals(field.valuesCopy.toVector, Vector(1.0, 2.0, 4.0))
    assertEquals(field.coord(2), (1, 1, 0))
  }

  test("statistic maps separate evidence orientation from threshold alternative") {
    val sp = SampleSpaces(Vector(2, 2, 1))
    val evidence = SomeScalarVolume.unsafeCopyFromCanonicalArray[Double](Array(1.0, 2.0, 0.5, 3.0), sp)
    val statistic = StatisticMap.negLog10P(evidence, PSide.OneSided)
    val field = value(MaskedField.fromStatisticMap(statistic, ThresholdAlternative.Greater))

    assertEquals(statistic.kind, StatKind.NegLog10P(PSide.OneSided))
    assertEquals(statistic.orientation, EvidenceOrientation.Unsigned)
    assertEquals(field.valuesCopy.toVector, Vector(1.0, 2.0, 0.5, 3.0))
    assertEquals(
      MaskedField.fromStatisticMap(statistic, ThresholdAlternative.TwoSided).left.toOption,
      Some(ThresholdError.IncompatibleAlternative(ThresholdAlternative.TwoSided, EvidenceOrientation.Unsigned))
    )
  }

  test("unsigned statistic maps reject negative evidence inside the mask") {
    val sp = SampleSpaces(Vector(2, 1, 1))
    val evidence = SomeScalarVolume.unsafeCopyFromCanonicalArray[Double](Array(1.0, -0.1), sp)
    val statistic = StatisticMap.negLog10P(evidence, PSide.OneSided)

    assertEquals(
      MaskedField.fromStatisticMap(statistic, ThresholdAlternative.Greater).left.toOption,
      Some(ThresholdError.NegativeUnsignedEvidence(1, -0.1))
    )
  }

  test("masked field rejects non-finite values inside an explicit mask") {
    val sp = SampleSpaces(Vector(2, 2, 1))
    val stat = SomeScalarVolume.unsafeCopyFromCanonicalArray[Double](Array(1.0, -2.0, Double.NaN, 4.0), sp)
    val mask = Mask.fromIndices(sp, Array(2))

    assertEquals(
      MaskedField.fromVolume(stat, mask, Tail.Positive).left.toOption,
      Some(ThresholdError.NonFiniteData("stat volume inside mask"))
    )
  }

  test("prior weights normalize and shrink toward uniform mass") {
    val priors = value(PriorWeights.fromArray(Array(1.0, 1.0, 2.0)))
    assertEqualsDouble(priors.totalMass, 1.0, 1e-12)
    assertEqualsDouble(priors(0), 0.25, 1e-12)
    assertEqualsDouble(priors(2), 0.5, 1e-12)

    val mixed = value(priors.shrinkToUniform(0.5))
    assertEqualsDouble(mixed.totalMass, 1.0, 1e-12)
    assertEqualsDouble(mixed(0), (0.5 / 3.0) + 0.5 * 0.25, 1e-12)
    assertEqualsDouble(mixed(2), (0.5 / 3.0) + 0.5 * 0.5, 1e-12)
  }

  test("softmax and diffuse set scores match hand-computable values") {
    val z = Array(1.0, 2.0, 3.0)
    val priors = value(PriorWeights.fromArray(Array(1.0, 1.0, 2.0)))
    val idx = Array(0, 2)
    val k = Kappa.unsafe(1.0)

    val soft = value(ScoreSet.softMax(idx, z, priors, k))
    val expectedSoft = math.log(0.25 * math.exp(1.0) + 0.5 * math.exp(3.0))
    assertEqualsDouble(soft, expectedSoft, 1e-12)

    val diffuse = value(ScoreSet.diffuse(idx, z, priors))
    val sumWZ = 0.25 * 1.0 + 0.5 * 3.0
    val sumW2 = 0.25 * 0.25 + 0.5 * 0.5
    val sumW = 0.75
    assertEqualsDouble(diffuse.score, sumWZ / math.sqrt(sumW2), 1e-12)
    assertEqualsDouble(diffuse.effectiveN, (sumW * sumW) / sumW2, 1e-12)
  }

  test("scoring input ties a masked field, priors, and region for typed scores") {
    val sp = SampleSpaces(Vector(3, 1, 1))
    val stat = SomeScalarVolume.unsafeCopyFromCanonicalArray[Double](Array(1.0, 2.0, 3.0), sp)
    val field = value(MaskedField.fromVolume(stat))
    val priors = value(PriorWeights.uniform(field.size))
    val root = value(Octree.root(field, priors))
    val input = value(ScoringInput(field, priors, root))
    val typed = value(ScoreSet.softMax(input, Kappa.unsafe(1.0)))
    val legacy = value(ScoreSet.softMax(root.indexArray, field.valuesCopy, priors, Kappa.unsafe(1.0)))

    assertEqualsDouble(typed.toLegacyDouble, legacy, 1e-12)
  }

  test("omnibus score keeps the best softmax channel and diffuse score") {
    val z = Array(0.0, 3.0)
    val priors = value(PriorWeights.uniform(2))
    val out = value(ScoreSet.omnibus(Array(0, 1), z, priors, Vector(Kappa.unsafe(0.5), Kappa.unsafe(2.0))))

    assert(out.kappa.contains(Kappa.unsafe(2.0)))
    assert(out.score >= out.diffuse.score)
    assertEqualsDouble(out.softMax, value(ScoreSet.softMax(Array(0, 1), z, priors, Kappa.unsafe(2.0))) / 2.0, 1e-12)
  }

  test("Westfall-Young step-down matches a hand-computed null matrix") {
    val observed = Array(3.5, 2.1, 4.2)
    val nulls = matrix(
      Vector(
        Vector(2.0, 1.0, 3.0),
        Vector(4.0, 1.0, 2.0),
        Vector(3.0, 3.0, 3.0),
        Vector(5.0, 0.0, 1.0)
      )
    )

    val out = value(WestfallYoung.stepDown(observed, nulls, Alpha.unsafe(0.5)))
    assertEquals(out.map(_.testIndex), Vector(0, 1, 2))
    assertEqualsDouble(out(0).adjustedP.value, 0.6, 1e-12)
    assertEqualsDouble(out(1).adjustedP.value, 0.6, 1e-12)
    assertEqualsDouble(out(2).adjustedP.value, 0.4, 1e-12)
    assert(!out(0).rejected)
    assert(!out(1).rejected)
    assert(out(2).rejected)
  }

  test("single-step maxT and max-null threshold use plus-one permutation rules") {
    val observed = Array(3.5, 2.1, 4.2)
    val nulls = matrix(
      Vector(
        Vector(2.0, 1.0, 3.0),
        Vector(4.0, 1.0, 2.0),
        Vector(3.0, 3.0, 3.0),
        Vector(5.0, 0.0, 1.0)
      )
    )

    val out = value(MaxT.singleStep(observed, nulls, Alpha.unsafe(0.5)))
    assertEqualsDouble(out(0).adjustedP.value, 0.6, 1e-12)
    assertEqualsDouble(out(1).adjustedP.value, 1.0, 1e-12)
    assertEqualsDouble(out(2).adjustedP.value, 0.4, 1e-12)

    val adjusted = value(MultipleTesting.adjust(observed, nulls, Alpha.unsafe(0.5), CorrectionPolicy.MaxTSingleStep))
    assertEquals(adjusted.map(_.adjustedP.value), out.map(_.adjustedP.value))
    assertEqualsDouble(value(MaxNull.cutoff(Array(3.0, 4.0, 3.0, 5.0), Alpha.unsafe(0.4))).toLegacyDouble, 4.0, 1e-12)
    assertEquals(value(MaxNull.cutoff(Array(3.0, 4.0, 3.0, 5.0), Alpha.unsafe(0.05))), ThresholdCutoff.NoRejections)
    assertEqualsDouble(value(MaxNull.threshold(Array(3.0, 4.0, 3.0, 5.0), Alpha.unsafe(0.4))), 4.0, 1e-12)
    assert(value(MaxNull.threshold(Array(3.0, 4.0, 3.0, 5.0), Alpha.unsafe(0.05))).isPosInfinity)
  }

  test("octree root and split use mask-space coordinates and prior mass") {
    val sp = SampleSpaces(Vector(2, 2, 2))
    val stat =
      SomeScalarVolume.unsafeCopyFromCanonicalArray[Double](
        Array(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0),
        sp
      )
    val field = value(MaskedField.fromVolume(stat, Tail.Positive))
    val priors = value(PriorWeights.uniform(field.size))
    val root = value(Octree.root(field, priors))

    assertEquals(root.size, 8)
    assertEquals(root.bbox, BoundingBox(0, 1, 0, 1, 0, 1))

    val children = value(Octree.split(root, field, priors, minPriorMass = 0.0))
    assertEquals(children.map(_.id), Vector(0, 1, 2, 3, 4, 5, 6, 7))
    assert(children.forall(_.size == 1))
    assertEquals(children(7).indicesVector, Vector(7))
    assertEquals(children(7).bbox, BoundingBox(1, 1, 1, 1, 1, 1))
    children.foreach(child => assertEqualsDouble(child.priorMass, 1.0 / 8.0, 1e-12))
  }
