package scalafim.response

import narr.NArray

class PrimitiveStorageJvmSuite extends munit.FunSuite:
  test("axis and response storage use JVM primitive arrays"):
    val indices =
      OrderedIndices
        .fromInts(
          DomainId.unsafe[TimeAxis]("jvm-storage"),
          3,
          Vector(2, 0)
        )
        .toOption
        .get

    assertEquals(indices.primitiveValues.getClass.getName, "[I")
    assertEquals(NArray[Double](1.0).getClass.getName, "[D")

  test("JVM exact-bit comparison preserves distinct NaN payloads"):
    val first = java.lang.Double.longBitsToDouble(0x7ff8000000000001L)
    val second = java.lang.Double.longBitsToDouble(0x7ff8000000000002L)

    assert(DecodeConsistency.ExactBits.agrees(first, first))
    assert(!DecodeConsistency.ExactBits.agrees(first, second))
