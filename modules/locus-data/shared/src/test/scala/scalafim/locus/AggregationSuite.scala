package scalafim.locus

import cats.kernel.CommutativeMonoid

class AggregationSuite extends munit.FunSuite:
  private given CommutativeMonoid[Int] with
    def empty: Int = 0
    def combine(left: Int, right: Int): Int = left + right

  private given CommutativeMonoid[Set[Int]] with
    def empty: Set[Int] = Set.empty
    def combine(left: Set[Int], right: Set[Int]): Set[Int] = left union right

  private val ambientResolution =
    DomainFactory.unsafeRestore(SpaceKey.unsafe("aggregate:ambient"), 7)
  private type X = ambientResolution.S
  private val ambient: FiniteSpace[X] = ambientResolution.space

  private val parcelResolution =
    DomainFactory.unsafeRestore(SpaceKey.unsafe("aggregate:parcels"), 3)
  private type P = parcelResolution.S
  private val parcels: FiniteSpace[P] = parcelResolution.space

  private val partition =
    Parcellation.fromAssignments(
      ambient,
      parcels,
      Vector(Some(0), Some(0), None, Some(1), Some(2), Some(2), None)
    ).toOption.get

  test("foldMapBy scans each supported source point exactly once"):
    var reads = 0
    val field = IndexedField.tabulate(ambient)(point => point.ordinal + 1)

    val result =
      Aggregation
        .foldMapBy(partition, field): value =>
          reads += 1
          value
        .toOption
        .get
    assertEquals(reads, partition.support.cardinality)
    assertEquals(parcels.indices.map(result.apply).toVector, Vector(3, 4, 11))

  test("aggregation fusion is exact for a lawful commutative monoid"):
    val networkResolution =
      DomainFactory.unsafeRestore(SpaceKey.unsafe("aggregate:networks"), 2)
    type N = networkResolution.S
    val networks: FiniteSpace[N] = networkResolution.space
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

    networks.indices.foreach: network =>
      assertEquals(hierarchical(network), direct(network))

  test("aggregation rejects a field from a different runtime space"):
    val other =
      DomainFactory.unsafeRestore(SpaceKey.unsafe("aggregate:other"), ambient.size).space
    val field =
      IndexedField
        .tabulate(other)(_.ordinal)
        .asInstanceOf[IndexedField[X, Int]]

    assert(Aggregation.foldMapBy(partition, field)(identity).isLeft)
