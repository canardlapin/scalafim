package scalafim.locus

class IndexedFieldSuite extends munit.FunSuite:
  private sealed trait S

  private val space = FiniteSpace.make[S](SpaceKey.unsafe("field:test"), 6).toOption.get
  private val field = IndexedField.fromValues(space, Vector(0, 10, 20, 30, 40, 50)).toOption.get

  test("dense field construction validates size and owns input values"):
    val values = scala.collection.mutable.ArrayBuffer(1, 2, 3, 4, 5, 6)
    val owned = IndexedField.fromValues(space, values).toOption.get
    values(0) = 99

    assertEquals(owned(space.point(0).get), 1)
    assertEquals(
      IndexedField.fromValues(space, Vector(1, 2)),
      Left(IndexedFieldError.WrongValueCount(6, 2))
    )

  test("restriction identity and nested-intersection laws hold"):
    val whole = Region.whole(space)
    val a = Region.fromOrdinals(space, Vector(0, 1, 3, 5)).toOption.get
    val b = Region.fromOrdinals(space, Vector(1, 2, 3)).toOption.get

    assertEquals(
      field.restrict(whole).toOption.get.valuesInDomainOrder.toVector,
      Vector(0, 10, 20, 30, 40, 50)
    )
    assertEquals(
      field.restrict(a).toOption.get
        .restrict(b)
        .toOption
        .get
        .support,
      a.intersect(b).toOption.get
    )

  test("field map commutes with restriction"):
    val region = Region.fromOrdinals(space, Vector(0, 2, 5)).toOption.get
    val mappedThenRestricted =
      field.map(_ + 1).restrict(region).toOption.get.valuesInDomainOrder.toVector
    val restrictedThenMapped =
      field.restrict(region).toOption.get.map(_ + 1).valuesInDomainOrder.toVector

    assertEquals(mappedThenRestricted, restrictedThenMapped)

  test("section access and selection preserve support and explicit order"):
    val support = Region.fromOrdinals(space, Vector(1, 3, 5)).toOption.get
    val section = field.restrict(support).toOption.get
    val selection = Selection.fromOrdinals(space, Vector(5, 1, 3)).toOption.get

    assertEquals(section(space.point(1).get), Some(10))
    assertEquals(section(space.point(2).get), None)
    assertEquals(section.valuesIn(selection).toOption.get.toVector, Vector(50, 10, 30))

    val outside = Selection.fromOrdinals(space, Vector(1, 2)).toOption.get
    assertEquals(
      section.valuesIn(outside).left.toOption.get,
      SectionSelectionError.OutsideSupport(2)
    )

  test("restriction rejects a reused phantom with a different runtime identity"):
    val other = FiniteSpace.make[S](SpaceKey.unsafe("field:other"), 6).toOption.get
    val wrongRegion = Region.whole(other)

    assert(field.restrict(wrongRegion).isLeft)
