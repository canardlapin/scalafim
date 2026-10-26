package providercontract.locus

import locus4s.PartialSurjection
import locus4s.data.{Aggregation, VectorField}

/** Qualify the native partial-parcel/domain composition, not upstream algebra laws. */
class ProviderAggregationSuite extends munit.FunSuite:
  test("parcellation assignments feed provider aggregation without losing background or order"):
    val source =
      DomainFixtures.restore("provider-aggregate:source", 6).space
    val target =
      DomainFixtures.restore("provider-aggregate:target", 2).space
    val partition =
      PartialSurjection
        .fromOptionalTargetOrdinals(source, target, Vector(Some(1), None, Some(0), Some(1), None, Some(0)))
        .toOption
        .get
    val mapping = partition.toPartialMap
    val field = VectorField.tabulate(source)(_.ordinal)
    var reads = Vector.empty[Int]
    val contribution: Int => Vector[Int] = value =>
      reads :+= value
      Vector(value)
    val result =
      Aggregation
        .foldMapByChecked(mapping, field)(Vector.empty[Int])(contribution)(_ ++ _)
        .toOption
        .get
    assertEquals(reads, Vector(0, 2, 3, 5))
    assertEquals(target.indices.map(result.apply).toVector, Vector(Vector(2, 5), Vector(0, 3)))
    val foreign =
      DomainFixtures.restore("provider-aggregate:foreign", 6).space
    val foreignField = VectorField.tabulate(foreign)(_.ordinal)
    assert(Aggregation.foldMapByChecked(mapping, foreignField)(0)(identity)(_ + _).isLeft)

  test("an empty parcel domain retains the provider empty-target contract"):
    val source =
      DomainFixtures.restore("provider-aggregate:background", 3).space
    val target =
      DomainFixtures.restore("provider-aggregate:empty", 0).space
    val partition =
      PartialSurjection.fromOptionalTargetOrdinals(source, target, Vector(None, None, None)).toOption.get
    val mapping = partition.toPartialMap
    val field = VectorField.tabulate(source)(_.ordinal)
    var reads = 0
    val contribution: Int => Int = value =>
      reads += 1
      value
    val result =
      Aggregation
        .foldMapByChecked(mapping, field)(0)(contribution)(_ + _)
        .toOption
        .get
    assertEquals(reads, 0)
    assertEquals(result.space.size, 0)
