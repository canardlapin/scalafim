package scalafim.response


class PrimitiveStorageJvmSuite extends munit.FunSuite:
  test("axis storage uses a whole canonical Ravel primitive array"):
    val indices =
      OrderedIndices
        .fromInts(
          DomainId.unsafe[TimeAxis]("jvm-storage"),
          3,
          Vector(2, 0)
        )
        .toOption
        .get

    assertEquals(indices.primitiveValues.dtype.name, "Int")
    assert(indices.primitiveValues.isCanonicalLayout)
    assert(indices.primitiveValues.isWholeBuffer)

  test("JVM exact-bit comparison preserves distinct NaN payloads"):
    val first = java.lang.Double.longBitsToDouble(0x7ff8000000000001L)
    val second = java.lang.Double.longBitsToDouble(0x7ff8000000000002L)

    assert(DecodeConsistency.ExactBits.agrees(first, first))
    assert(!DecodeConsistency.ExactBits.agrees(first, second))
