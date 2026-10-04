package scalafim.locus

import locus4s.PartialMap
import locus4s.data.Aggregation

/** Qualify the remaining ScalaFIM parcellation/domain seam, not upstream algebra laws. */
class ProviderAggregationSuite extends munit.FunSuite:
  test("parcellation assignments feed provider aggregation without losing background or order"):
    val source = DomainFactory.unsafeRestore(SpaceKey.unsafe("provider-aggregate:source"), 6).space
    val target = DomainFactory.unsafeRestore(SpaceKey.unsafe("provider-aggregate:target"), 2).space
    val partition = Parcellation.fromAssignments(source, target, Vector(Some(1), None, Some(0), Some(1), None, Some(0))).toOption.get
    val mapping = PartialMap.fromOptionalTargetOrdinals(source, target, partition.assignmentOrdinals).toOption.get
    val field = IndexedField.tabulate(source)(_.ordinal)
    var reads = Vector.empty[Int]
    val result = Aggregation.foldMapByChecked(mapping, field)(Vector.empty[Int]) { value =>
      reads :+= value
      Vector(value)
    }(_ ++ _).toOption.get
    assertEquals(reads, Vector(0, 2, 3, 5))
    assertEquals(target.indices.map(result.apply).toVector, Vector(Vector(2, 5), Vector(0, 3)))
    val foreign = DomainFactory.unsafeRestore(SpaceKey.unsafe("provider-aggregate:foreign"), 6).space
    assert(Aggregation.foldMapByChecked(mapping, IndexedField.tabulate(foreign)(_.ordinal))(0)(identity)(_ + _).isLeft)

  test("an empty ScalaFIM parcel domain retains the provider empty-target contract"):
    val source = DomainFactory.unsafeRestore(SpaceKey.unsafe("provider-aggregate:background"), 3).space
    val target = DomainFactory.unsafeRestore(SpaceKey.unsafe("provider-aggregate:empty"), 0).space
    val partition = Parcellation.fromAssignments(source, target, Vector(None, None, None)).toOption.get
    val mapping = PartialMap.fromOptionalTargetOrdinals(source, target, partition.assignmentOrdinals).toOption.get
    var reads = 0
    val result = Aggregation.foldMapByChecked(mapping, IndexedField.tabulate(source)(_.ordinal))(0) { value =>
      reads += 1
      value
    }(_ + _).toOption.get
    assertEquals(reads, 0)
    assertEquals(result.space.size, 0)
