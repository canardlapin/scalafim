package scalafim.fmri.threshold

class MaxNullDecisionOracleSuite extends munit.FunSuite:

  test("cutoff decisions equal an independent plus-one rank oracle exhaustively"):
    val distributions =
      Vector.range(1, 6).flatMap(length => arraysOfLength(length, Vector(-1.0, 0.0, 1.0, 2.0)))
    var comparisons = 0

    distributions.foreach: nulls =>
      val observed = observedCandidates(nulls)
      alphaCandidates(nulls.length).foreach: alpha =>
        val cutoff = MaxNull.cutoff(nulls, alpha).toOption.get
        observed.foreach: score =>
          val exceedances = nulls.count(_ >= score)
          val referenceP = (exceedances.toDouble + 1.0) / (nulls.length.toDouble + 1.0)
          val referenceReject = referenceP <= alpha.value
          val publicP = MaxNull.pValues(Array(score), nulls).toOption.get.head.value
          val cutoffReject = cutoff.rejects(score).toOption.get
          val legacyReject = score >= cutoff.toLegacyDouble

          assertEqualsDouble(publicP, referenceP, 0.0)
          assertEquals(cutoffReject, referenceReject)
          assertEquals(legacyReject, referenceReject)
          comparisons += 1

    assertEquals(distributions.length, 1364)
    assert(comparisons > 200000)

  test("the reported defect rejects only above the maximum null draw"):
    val nulls = Array.tabulate(19)(index => index.toDouble + 1.0)
    val alpha = Alpha.unsafe(0.05)
    val cutoff = MaxNull.cutoff(nulls, alpha).toOption.get

    cutoff match
      case ThresholdCutoff.Exclusive(boundary) =>
        assertEqualsDouble(boundary, 19.0, 0.0)
      case other =>
        fail(s"expected an exclusive cutoff, found $other")

    assertEqualsDouble(MaxNull.pValues(Array(19.0), nulls).toOption.get.head.value, 0.1, 0.0)
    assertEqualsDouble(
      MaxNull.pValues(Array(Math.nextUp(19.0)), nulls).toOption.get.head.value,
      0.05,
      0.0
    )
    assertEquals(cutoff.rejects(19.0).toOption.get, false)
    assertEquals(cutoff.rejects(Math.nextUp(19.0)).toOption.get, true)
    assertEqualsDouble(cutoff.toLegacyDouble, Math.nextUp(19.0), 0.0)

  test("ties, finite extrema, and legacy conversion retain explicit comparison semantics"):
    val tied = MaxNull.cutoff(Array(5.0, 5.0, 4.0), Alpha.unsafe(0.5)).toOption.get
    assertEquals(tied.rejects(5.0).toOption.get, false)
    assertEquals(tied.rejects(Math.nextUp(5.0)).toOption.get, true)

    val maximum =
      MaxNull
        .cutoff(Array(Double.MaxValue), Alpha.unsafe(0.5))
        .toOption
        .get
    assert(maximum.toLegacyDouble.isPosInfinity)
    assertEquals(maximum.rejects(Double.MaxValue).toOption.get, false)

    val minimum = ThresholdCutoff.exclusive(-Double.MaxValue).toOption.get
    assertEqualsDouble(minimum.toLegacyDouble, Math.nextUp(-Double.MaxValue), 0.0)
    assertEquals(minimum.rejects(-Double.MaxValue).toOption.get, false)
    assertEquals(minimum.rejects(Math.nextUp(-Double.MaxValue)).toOption.get, true)

    val imported = ThresholdCutoff.fromLegacy(5.0).toOption.get
    imported match
      case ThresholdCutoff.Inclusive(boundary) =>
        assertEqualsDouble(boundary, 5.0, 0.0)
      case other =>
        fail(s"legacy finite thresholds must import as inclusive, found $other")
    assertEquals(imported.rejects(5.0).toOption.get, true)
    assertEquals(ThresholdCutoff.fromLegacy(Double.PositiveInfinity).toOption.get, ThresholdCutoff.NoRejections)
    assert(ThresholdCutoff.fromLegacy(Double.NegativeInfinity).isLeft)
    assert(ThresholdCutoff.exclusive(Double.NaN).isLeft)
    assert(tied.rejects(Double.NaN).isLeft)

  private def arraysOfLength(length: Int, values: Vector[Double]): Vector[Array[Double]] =
    if length == 0 then Vector(Array.emptyDoubleArray)
    else
      arraysOfLength(length - 1, values).flatMap: prefix =>
        values.map(value => prefix :+ value)

  private def observedCandidates(nulls: Array[Double]): Vector[Double] =
    (
      Vector(-Double.MaxValue, -2.5, 2.5, Double.MaxValue) ++
        nulls.toVector.flatMap: value =>
          Vector(Math.nextDown(value), value, Math.nextUp(value))
    ).distinct

  private def alphaCandidates(draws: Int): Vector[Alpha] =
    val denominator = draws.toDouble + 1.0
    val fixed = Vector(0.01, 0.05, 0.1, 0.2, 0.25, 0.4, 0.5, 0.75, 0.9, 0.99)
    val attainable =
      Vector.range(1, draws + 1).flatMap: rank =>
        val value = rank.toDouble / denominator
        Vector(Math.nextDown(value), value, Math.nextUp(value))
    (fixed ++ attainable).distinct.flatMap(value => Alpha(value).toOption)
