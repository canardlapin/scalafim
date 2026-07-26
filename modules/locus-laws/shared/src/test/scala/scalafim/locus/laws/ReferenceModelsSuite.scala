package scalafim.locus.laws

class ReferenceModelsSuite extends munit.FunSuite:
  test("bounded exhaustive enumerators cover every small finite value"):
    assertEquals(Exhaustive.regions(3).toOption.get.size, 8)
    assertEquals(Exhaustive.totalMaps(2, 3).toOption.get.size, 9)
    assertEquals(Exhaustive.relations(2, 2).toOption.get.size, 16)
    assertEquals(Exhaustive.parcellations(3, 2).toOption.get.size, 12)
    assertEquals(Exhaustive.fields(3, Vector(false, true)).toOption.get.size, 8)
    assertEquals(
      Exhaustive.sections(Vector(ReferenceField(Vector(1, 2, 3)))).toOption.get.size,
      8
    )

  test("enumeration rejects impossible maps and combinatorial explosions before allocation"):
    assertEquals(
      Exhaustive.totalMaps(1, 0),
      Left(EnumerationError.NoTotalMap(1, 0))
    )
    assert(Exhaustive.relations(5, 5).isLeft)
    assert(Exhaustive.parcellations(9, 5).isLeft)
    assert(Exhaustive.regions(13).isLeft)

  test("reference Boolean and relation law groups are independently green"):
    assertEquals(ReferenceLaws.booleanAlgebra(3).toOption.get, Vector.empty)
    assertEquals(ReferenceLaws.relationCategory(2).toOption.get, Vector.empty)

  test("reference parcellation coarsening preserves support and unions fibers"):
    val partition =
      ReferenceParcellation(5, 3, Vector(Some(0), Some(1), None, Some(2), Some(2)))
    val mapping = ReferenceTotalMap(3, 2, Vector(0, 0, 1))
    val coarsened = partition.coarsen(mapping)

    assertEquals(coarsened.support, partition.support)
    assertEquals(
      coarsened.fiber(0),
      partition.fiber(0).union(partition.fiber(1))
    )

  test("exact and tolerant numeric comparison policies remain distinct"):
    val left = (1.0e16 + -1.0e16) + 1.0
    val right = 1.0e16 + (-1.0e16 + 1.0)
    val exact = NumericComparison.exact[Double]
    val tolerant = NumericComparison.double(absoluteTolerance = 1.0, relativeTolerance = 0.0)

    assert(!exact.equivalent(left, right))
    assert(tolerant.equivalent(left, right))
