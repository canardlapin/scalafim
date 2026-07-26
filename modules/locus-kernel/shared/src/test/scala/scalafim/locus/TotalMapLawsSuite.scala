package scalafim.locus

class TotalMapLawsSuite extends munit.FunSuite:
  private sealed trait X
  private sealed trait Y
  private sealed trait Z

  private val x = FiniteSpace.make[X](SpaceKey.unsafe("laws:x"), 3).toOption.get
  private val y = FiniteSpace.make[Y](SpaceKey.unsafe("laws:y"), 3).toOption.get
  private val z = FiniteSpace.make[Z](SpaceKey.unsafe("laws:z"), 2).toOption.get

  test("identity and composition laws are exhaustive for bounded total maps"):
    val xx = allMaps(x, x)
    val xy = allMaps(x, y)
    val yz = allMaps(y, z)
    val identityX = TotalMap.identity(x)
    val identityY = TotalMap.identity(y)
    val identityZ = TotalMap.identity(z)

    xx.foreach: mapping =>
      assertEquals(identityX.andThen(mapping).toOption.get, mapping)
      assertEquals(mapping.andThen(identityX).toOption.get, mapping)

    xy.foreach: first =>
      assertEquals(identityX.andThen(first).toOption.get, first)
      assertEquals(first.andThen(identityY).toOption.get, first)
      yz.foreach: second =>
        val left = first.andThen(second).toOption.get.andThen(identityZ).toOption.get
        val right = first.andThen(second.andThen(identityZ).toOption.get).toOption.get
        assertEquals(left, right)

  test("pullback functoriality and Boolean preservation are exhaustive"):
    val xy = allMaps(x, y)
    val yz = allMaps(y, z)
    val xRegions = allRegions(x)
    val yRegions = allRegions(y)
    val zRegions = allRegions(z)

    xRegions.foreach: region =>
      assertEquals(TotalMap.identity(x).pullback(region).toOption.get, region)

    xy.foreach: mapping =>
      yRegions.foreach: a =>
        assertEquals(
          mapping.pullback(a.complement).toOption.get,
          mapping.pullback(a).toOption.get.complement
        )
        yRegions.foreach: b =>
          assertEquals(
            mapping.pullback(a.intersect(b).toOption.get).toOption.get,
            mapping.pullback(a).toOption.get
              .intersect(mapping.pullback(b).toOption.get)
              .toOption
              .get
          )

      yz.foreach: next =>
        val composed = mapping.andThen(next).toOption.get
        zRegions.foreach: region =>
          assertEquals(
            composed.pullback(region).toOption.get,
            mapping.pullback(next.pullback(region).toOption.get).toOption.get
          )

  test("existential and universal adjunctions are exhaustive"):
    val maps = allMaps(x, y)
    val xRegions = allRegions(x)
    val yRegions = allRegions(y)

    maps.foreach: mapping =>
      xRegions.foreach: sourceRegion =>
        yRegions.foreach: targetRegion =>
          val existsSubset =
            mapping.existsAlong(sourceRegion).toOption.get
              .subsetOf(targetRegion)
              .toOption
              .get
          val sourceSubsetPullback =
            sourceRegion
              .subsetOf(mapping.pullback(targetRegion).toOption.get)
              .toOption
              .get
          assertEquals(existsSubset, sourceSubsetPullback)

          val pullbackSubset =
            mapping.pullback(targetRegion).toOption.get
              .subsetOf(sourceRegion)
              .toOption
              .get
          val targetSubsetForall =
            targetRegion
              .subsetOf(mapping.forallAlong(sourceRegion).toOption.get)
              .toOption
              .get
          assertEquals(pullbackSubset, targetSubsetForall)

  test("Frobenius reciprocity is exhaustive"):
    val maps = allMaps(x, y)
    val xRegions = allRegions(x)
    val yRegions = allRegions(y)

    maps.foreach: mapping =>
      xRegions.foreach: sourceRegion =>
        yRegions.foreach: targetRegion =>
          val left =
            mapping.existsAlong(
              sourceRegion
                .intersect(mapping.pullback(targetRegion).toOption.get)
                .toOption
                .get
            ).toOption.get
          val right =
            mapping.existsAlong(sourceRegion).toOption.get
              .intersect(targetRegion)
              .toOption
              .get
          assertEquals(left, right)

  test("universal image includes empty fibers and supported universal image removes them"):
    val source = FiniteSpace.make[X](SpaceKey.unsafe("vacuous:source"), 2).toOption.get
    val target = FiniteSpace.make[Y](SpaceKey.unsafe("vacuous:target"), 3).toOption.get
    val mapping = TotalMap.fromTargetOrdinals(source, target, Array(0, 0)).toOption.get
    val wholeSource = Region.whole(source)

    assertEquals(
      mapping.forallAlong(wholeSource).toOption.get,
      Region.whole(target)
    )
    assertEquals(
      mapping.forallAlongOnImage(wholeSource).toOption.get.ordinalsInDomainOrder.toVector,
      Vector(0)
    )

  test("map construction and composition reject invalid runtime domains"):
    val targets = Array(0, 1, 1)
    val mapping = TotalMap.fromTargetOrdinals(x, y, targets).toOption.get
    targets(0) = 2
    assertEquals(mapping.targetOrdinals.toVector, Vector(0, 1, 1))

    assertEquals(
      TotalMap.fromTargetOrdinals(x, y, Array(0)),
      Left(TotalMapError.WrongTargetCount(3, 1))
    )
    assertEquals(
      TotalMap.fromTargetOrdinals(x, y, Array(0, 1, 3)),
      Left(TotalMapError.TargetOutOfBounds(2, 3, 3))
    )

    val wrongY = FiniteSpace.make[Y](SpaceKey.unsafe("laws:wrong-y"), 3).toOption.get
    val wrongNext = TotalMap.fromTargetOrdinals(wrongY, z, Array(0, 0, 1)).toOption.get
    assert(mapping.andThen(wrongNext).isLeft)

  private def allRegions[A](space: FiniteSpace[A]): Vector[Region[A]] =
    Vector.tabulate(1 << space.size): mask =>
      Region.tabulate(space): point =>
        (mask & (1 << point.ordinal)) != 0

  private def allMaps[A, B](
      from: FiniteSpace[A],
      to: FiniteSpace[B]
  ): Vector[TotalMap[A, B]] =
    if from.size == 0 then
      Vector(TotalMap.fromTargetOrdinals(from, to, Array.emptyIntArray).toOption.get)
    else if to.size == 0 then
      Vector.empty
    else
      val count = integerPower(to.size, from.size)
      Vector.tabulate(count): encoded =>
        val targets = Array.ofDim[Int](from.size)
        var value = encoded
        var source = 0
        while source < from.size do
          targets(source) = value % to.size
          value /= to.size
          source += 1
        TotalMap.fromTargetOrdinals(from, to, targets).toOption.get

  private def integerPower(base: Int, exponent: Int): Int =
    var result = 1
    var i = 0
    while i < exponent do
      result *= base
      i += 1
    result
