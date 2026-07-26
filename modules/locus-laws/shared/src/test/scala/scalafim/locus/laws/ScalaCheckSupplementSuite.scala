package scalafim.locus.laws

import org.scalacheck.{Gen, Prop, Test}
import org.scalacheck.rng.Seed
import scalafim.locus.*

class ScalaCheckSupplementSuite extends munit.FunSuite:
  private sealed trait S

  test("bounded sparse region constructor sequences match Set semantics"):
    val caseGenerator =
      for
        size <- Gen.choose(1, 64)
        left <- Gen.listOf(Gen.choose(-size, size * 2))
        right <- Gen.listOf(Gen.choose(-size, size * 2))
      yield (size, left.filter(i => i >= 0 && i < size), right.filter(i => i >= 0 && i < size))

    val property = Prop.forAllNoShrink(caseGenerator): (size, left, right) =>
      val space =
        FiniteSpace.make[S](SpaceKey.unsafe(s"scalacheck:region:$size"), size).toOption.get
      val leftRegion = Region.fromOrdinals(space, left).toOption.get
      val rightRegion = Region.fromOrdinals(space, right).toOption.get
      val union = leftRegion.union(rightRegion).toOption.get
      val intersection = leftRegion.intersect(rightRegion).toOption.get

      union.ordinalsInDomainOrder.toSet == (left.toSet union right.toSet) &&
      intersection.ordinalsInDomainOrder.toSet == (left.toSet intersect right.toSet)

    assertProperty(property)

  test("bounded sparse relation constructor sequences match dense pairs"):
    val caseGenerator =
      for
        size <- Gen.choose(1, 24)
        rawRows <- Gen.listOfN(
          size,
          Gen.listOf(Gen.choose(-size, size * 2))
        )
      yield (size, rawRows.map(_.filter(i => i >= 0 && i < size)))

    val property = Prop.forAllNoShrink(caseGenerator): (size, rows) =>
      val space =
        FiniteSpace.make[S](SpaceKey.unsafe(s"scalacheck:relation:$size"), size).toOption.get
      val production =
        Relation.fromOrdinalRows(space, space, rows.map(_.toArray).toArray).toOption.get
      val referencePairs =
        rows.zipWithIndex.flatMap: (row, source) =>
          row.map(target => (source, target))
        .toSet

      LocusDifferential.relation(production) ==
        ReferenceRelation(size, size, referencePairs)

    assertProperty(property)

  private def assertProperty(property: Prop): Unit =
    val parameters =
      Test.Parameters.default
        .withMinSuccessfulTests(200)
        .withInitialSeed(Seed(0x5ca1af1L))
    val result = Test.check(parameters, property)
    assert(result.passed, result.toString)
