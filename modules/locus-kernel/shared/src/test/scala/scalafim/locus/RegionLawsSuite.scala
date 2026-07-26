package scalafim.locus

class RegionLawsSuite extends munit.FunSuite:
  private sealed trait S

  private val space = FiniteSpace.make[S](SpaceKey.unsafe("laws:boolean"), 4).toOption.get
  private val regions = allRegions(space)
  private val empty = Region.empty(space)
  private val whole = Region.whole(space)

  test("Boolean identities, complements, idempotence, and subset law are exhaustive"):
    regions.foreach: a =>
      assertEquals(a.union(empty).toOption.get, a)
      assertEquals(a.intersect(whole).toOption.get, a)
      assertEquals(a.union(a).toOption.get, a)
      assertEquals(a.intersect(a).toOption.get, a)
      assertEquals(a.union(a.complement).toOption.get, whole)
      assertEquals(a.intersect(a.complement).toOption.get, empty)

      regions.foreach: b =>
        val meet = a.intersect(b).toOption.get
        assertEquals(a.subsetOf(b).toOption.get, meet == a)

  test("distributivity is exhaustive over every region triple"):
    regions.foreach: a =>
      regions.foreach: b =>
        regions.foreach: c =>
          val left = a.intersect(b.union(c).toOption.get).toOption.get
          val right =
            a.intersect(b).toOption.get
              .union(a.intersect(c).toOption.get)
              .toOption
              .get
          assertEquals(left, right)

  test("construction history does not affect extensional equality"):
    val direct = Region.fromOrdinals(space, Vector(0, 2)).toOption.get
    val byDifference = whole.diff(Region.fromOrdinals(space, Vector(1, 3)).toOption.get).toOption.get
    val duplicatedInput = Region.fromOrdinals(space, Vector(2, 0, 2, 0)).toOption.get

    assertEquals(direct, byDifference)
    assertEquals(direct, duplicatedInput)
    assertEquals(direct.hashCode(), duplicatedInput.hashCode())

  private def allRegions[A](finiteSpace: FiniteSpace[A]): Vector[Region[A]] =
    Vector.tabulate(1 << finiteSpace.size): mask =>
      Region.tabulate(finiteSpace): point =>
        (mask & (1 << point.ordinal)) != 0
