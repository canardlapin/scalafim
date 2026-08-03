package scalafim.response

class PrimitiveStorageJsSuite extends munit.FunSuite:
  test("axis storage uses a whole canonical Ravel primitive array"):
    val indices =
      OrderedIndices
        .fromInts(
          DomainId.unsafe[TimeAxis]("js-storage"),
          3,
          Vector(2, 0)
        )
        .toOption
        .get

    assertEquals(indices.primitiveValues.dtype.name, "Int")
    assert(indices.primitiveValues.isCanonicalLayout)
    assert(indices.primitiveValues.isWholeBuffer)
