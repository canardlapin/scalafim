package scalafim.locus

import cats.kernel.CommutativeMonoid

class AggregationSuite extends munit.FunSuite:
  private sealed trait X
  private sealed trait P
  private sealed trait N

  private given CommutativeMonoid[Int] with
    def empty: Int = 0
    def combine(left: Int, right: Int): Int = left + right

  private given CommutativeMonoid[Set[Int]] with
    def empty: Set[Int] = Set.empty
    def combine(left: Set[Int], right: Set[Int]): Set[Int] = left union right

  private val ambient =
    FiniteSpace.make[X](SpaceKey.unsafe("aggregate:ambient"), 7).toOption.get
  private val parcels =
    FiniteSpace.make[P](SpaceKey.unsafe("aggregate:parcels"), 3).toOption.get
  private val partition =
    Parcellation.fromAssignments(
      ambient,
      parcels,
      Vector(Some(0), Some(0), None, Some(1), Some(2), Some(2), None)
    ).toOption.get

  test("foldMapBy scans each supported source point exactly once"):
    var reads = 0
    val field = IndexedField.tabulate(ambient): point =>
      reads += 1
      point.ordinal + 1

    val result = Aggregation.foldMapBy(partition, field)(identity).toOption.get
    assertEquals(reads, partition.support.cardinality)
    assertEquals(parcels.points.map(result.apply).toVector, Vector(3, 4, 11))

  test("aggregation fusion is exact for a lawful commutative monoid"):
    val networks =
      FiniteSpace.make[N](SpaceKey.unsafe("aggregate:networks"), 2).toOption.get
    val parcelToNetwork =
      Surjection.validate(
        TotalMap.fromTargetOrdinals(parcels, networks, Array(0, 0, 1)).toOption.get
      ).toOption.get
    val field = IndexedField.tabulate(ambient)(point => point.ordinal)

    val directPartition = partition.coarsen(parcelToNetwork).toOption.get
    val direct =
      Aggregation.foldMapBy(directPartition, field)(value => Set(value)).toOption.get

    val parcelValues =
      Aggregation.foldMapBy(partition, field)(value => Set(value)).toOption.get
    val networkPartition = Parcellation.fromSurjection(parcelToNetwork)
    val hierarchical =
      Aggregation.foldMapBy(networkPartition, parcelValues)(identity).toOption.get

    networks.points.foreach: network =>
      assertEquals(hierarchical(network), direct(network))

  test("aggregation rejects a field from a different runtime space"):
    val other =
      FiniteSpace.make[X](SpaceKey.unsafe("aggregate:other"), ambient.size).toOption.get
    val field = IndexedField.tabulate(other)(_.ordinal)

    assert(Aggregation.foldMapBy(partition, field)(identity).isLeft)
