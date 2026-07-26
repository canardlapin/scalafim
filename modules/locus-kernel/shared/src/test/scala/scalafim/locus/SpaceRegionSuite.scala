package scalafim.locus

class SpaceRegionSuite extends munit.FunSuite:
  private sealed trait A

  private val key = SpaceKey.unsafe("test:space-a")
  private val space = FiniteSpace.make[A](key, 6).toOption.get

  test("space keys and sizes are validated"):
    assertEquals(SpaceKey.make("   "), Left(SpaceError.EmptyKey))
    assertEquals(FiniteSpace.make[A](key, -1), Left(SpaceError.NegativeSize(-1)))
    assertEquals(space.point(-1), None)
    assertEquals(space.point(6), None)
    assertEquals(space.point(3).map(_.ordinal), Some(3))

  test("region construction is extensional, canonical, and defensively owned"):
    val input = Array(4, 1, 4, 2)
    val region = Region.fromOrdinals(space, input).toOption.get
    input(0) = 0

    assertEquals(region.ordinalsInDomainOrder.toVector, Vector(1, 2, 4))
    assertEquals(region.cardinality, 3)

    val exported = region.ordinalsInDomainOrder
    exported(0) = 5
    assertEquals(region.ordinalsInDomainOrder.toVector, Vector(1, 2, 4))

  test("region construction rejects out-of-bounds ordinals"):
    assertEquals(
      Region.fromOrdinals(space, Vector(0, 6)),
      Left(RegionError.OutOfBounds(1, 6, 6))
    )

  test("selection preserves order, rejects duplicates, and owns its array"):
    val input = Array(4, 1, 2)
    val selection = Selection.fromOrdinals(space, input).toOption.get
    input(0) = 0

    assertEquals(selection.ordinals.toVector, Vector(4, 1, 2))
    assertEquals(selection.region.ordinalsInDomainOrder.toVector, Vector(1, 2, 4))

    val exported = selection.ordinals
    exported(0) = 5
    assertEquals(selection.ordinals.toVector, Vector(4, 1, 2))
    assertEquals(
      Selection.fromOrdinals(space, Vector(1, 2, 1)),
      Left(SelectionError.DuplicateOrdinal(1))
    )

  test("runtime space identity is checked even when a phantom type is reused"):
    val other = FiniteSpace.make[A](SpaceKey.unsafe("test:other-a"), 6).toOption.get
    val left = Region.fromOrdinals(space, Vector(1, 2)).toOption.get
    val right = Region.fromOrdinals(other, Vector(1, 2)).toOption.get

    assert(left.union(right).isLeft)
    assert(left.intersect(right).isLeft)
    assert(left.subsetOf(right).isLeft)
    assertNotEquals(left, right)

  test("an existential runtime space retains one fresh point type"):
    val loaded = SomeFiniteSpace.make(SpaceKey.unsafe("runtime:subject-01"), 4).toOption.get
    val runtimeSpace = loaded.value
    val region = Region.fromOrdinals(runtimeSpace, Vector(3, 1)).toOption.get

    def cardinalityIn[S](expected: FiniteSpace[S], value: Region[S]): Int =
      assertEquals(value.space, expected)
      value.cardinality

    assertEquals(cardinalityIn(runtimeSpace, region), 2)

  test("public point-based constructors validate point ordinals against their space"):
    val points = space.points.drop(2).take(2).toVector
    val region = Region.fromPoints(space, points).toOption.get
    val selection = Selection.fromPoints(space, points.reverse).toOption.get

    assertEquals(region.ordinalsInDomainOrder.toVector, Vector(2, 3))
    assertEquals(selection.ordinals.toVector, Vector(3, 2))
