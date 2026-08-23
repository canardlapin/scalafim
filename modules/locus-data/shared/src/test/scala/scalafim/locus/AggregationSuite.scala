package scalafim.locus

import cats.kernel.CommutativeMonoid
import locus4s.{DomainRegistry, PartialSurjection, Surjection}

class AggregationSuite extends munit.FunSuite:
  private given CommutativeMonoid[Int] with
    def empty: Int = 0
    def combine(left: Int, right: Int): Int = left + right

  private given CommutativeMonoid[Set[Int]] with
    def empty: Set[Int] = Set.empty
    def combine(left: Set[Int], right: Set[Int]): Set[Int] = left union right

  private val ambientResolution =
    DomainFactory.unsafeRestore(
      DomainRegistry.empty,
      SpaceKey.unsafe("aggregate:ambient"),
      7
    )
  private type X = ambientResolution.S
  private val ambient: FiniteSpace[X] = ambientResolution.space

  private val parcelResolution =
    DomainFactory.unsafeRestore(
      ambientResolution.registry,
      SpaceKey.unsafe("aggregate:parcels"),
      3
    )
  private type P = parcelResolution.S
  private val parcels: FiniteSpace[P] = parcelResolution.space

  private val assignment =
    PartialSurjection.fromOptionalTargetOrdinals(
      ambient,
      parcels,
      Vector(Some(0), Some(0), None, Some(1), Some(2), Some(2), None)
    ).toOption.get

  test("foldMapBy scans each supported source point exactly once"):
    var reads = 0
    val field = IndexedField.tabulate(ambient)(point => point.ordinal + 1)

    val result =
      Aggregation
        .foldMapBy(assignment, field): value =>
          reads += 1
          value
        .toOption
        .get
    assertEquals(reads, assignment.support.cardinality)
    assertEquals(parcels.indices.map(result.apply).toVector, Vector(3, 4, 11))

  test("aggregation fusion is exact for a lawful commutative monoid"):
    val networkResolution =
      DomainFactory.unsafeRestore(
        parcelResolution.registry,
        SpaceKey.unsafe("aggregate:networks"),
        2
      )
    type N = networkResolution.S
    val networks: FiniteSpace[N] = networkResolution.space
    val parcelToNetwork =
      Surjection
        .fromTargetOrdinals(parcels, networks, Array(0, 0, 1))
        .toOption
        .get
    val field = IndexedField.tabulate(ambient)(point => point.ordinal)

    val directAssignment = assignment.andThen(parcelToNetwork)
    val direct =
      Aggregation.foldMapBy(directAssignment, field)(value => Set(value)).toOption.get

    val parcelValues =
      Aggregation.foldMapBy(assignment, field)(value => Set(value)).toOption.get
    val networkAssignment =
      PartialSurjection
        .fromOptionalTargetOrdinals(
          parcels,
          networks,
          Vector(Some(0), Some(0), Some(1))
        )
        .toOption
        .get
    val hierarchical =
      Aggregation.foldMapBy(networkAssignment, parcelValues)(identity).toOption.get

    networks.indices.foreach: network =>
      assertEquals(hierarchical(network), direct(network))

  test("aggregation rejects a field from a different runtime space"):
    val other =
      DomainFactory
        .unsafeRestore(
          parcelResolution.registry,
          SpaceKey.unsafe("aggregate:other"),
          ambient.size
        )
        .space
    val field =
      IndexedField
        .tabulate(other)(_.ordinal)
        .asInstanceOf[IndexedField[X, Int]]

    assert(Aggregation.foldMapBy(assignment, field)(identity).isLeft)
