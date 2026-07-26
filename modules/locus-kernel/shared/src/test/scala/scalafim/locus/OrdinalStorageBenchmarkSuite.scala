package scalafim.locus

import scala.collection.immutable.BitSet

class OrdinalStorageBenchmarkSuite extends munit.FunSuite:
  private sealed trait S

  test("sorted primitive regions and immutable BitSet agree on sparse and dense workloads"):
    val space = FiniteSpace.make[S](SpaceKey.unsafe("benchmark:storage"), 20000).toOption.get
    val sparseLeftValues = Array.tabulate(400)(i => i * 37 % space.size)
    val sparseRightValues = Array.tabulate(500)(i => i * 43 % space.size)
    val denseLeftValues = Array.tabulate(14000)(identity)
    val denseRightValues = Array.tabulate(14000)(i => i + 6000)

    val cases = Vector(
      ("sparse", sparseLeftValues, sparseRightValues),
      ("dense", denseLeftValues, denseRightValues)
    )

    cases.foreach: (name, leftValues, rightValues) =>
      val left = Region.fromOrdinals(space, leftValues).toOption.get
      val right = Region.fromOrdinals(space, rightValues).toOption.get
      val leftBits = BitSet.fromSpecific(leftValues)
      val rightBits = BitSet.fromSpecific(rightValues)

      assertEquals(
        left.union(right).toOption.get.ordinalsInDomainOrder.toVector,
        (leftBits | rightBits).toVector
      )
      assertEquals(
        left.intersect(right).toOption.get.ordinalsInDomainOrder.toVector,
        (leftBits & rightBits).toVector
      )

      val sortedNanos = timeNanos(40):
        left.union(right).toOption.get.cardinality +
          left.intersect(right).toOption.get.cardinality
      val bitSetNanos = timeNanos(40):
        (leftBits | rightBits).size + (leftBits & rightBits).size

      println:
        s"locus storage benchmark [$name]: sorted-array=${sortedNanos}ns, " +
          s"immutable-bitset=${bitSetNanos}ns"

  private def timeNanos(iterations: Int)(body: => Int): Long =
    var checksum = 0
    var i = 0
    val start = System.nanoTime()
    while i < iterations do
      checksum ^= body
      i += 1
    val elapsed = System.nanoTime() - start
    assert(checksum >= 0)
    elapsed
