package scalafim.locus.laws

import scalafim.locus.*

class LocusDifferentialSuite extends munit.FunSuite:
  private sealed trait X
  private sealed trait Y
  private sealed trait P

  private val x = FiniteSpace.make[X](SpaceKey.unsafe("differential:x"), 3).toOption.get
  private val y = FiniteSpace.make[Y](SpaceKey.unsafe("differential:y"), 2).toOption.get

  test("every small optimized region operation matches the dense reference"):
    val production = Exhaustive.regions(3).toOption.get.map: reference =>
      Region.fromOrdinals(x, reference.members).toOption.get

    production.foreach: left =>
      production.foreach: right =>
        val leftReference = LocusDifferential.region(left)
        val rightReference = LocusDifferential.region(right)
        assertEquals(
          LocusDifferential.region(left.union(right).toOption.get),
          leftReference.union(rightReference)
        )
        assertEquals(
          LocusDifferential.region(left.intersect(right).toOption.get),
          leftReference.intersect(rightReference)
        )
        assertEquals(
          LocusDifferential.region(left.diff(right).toOption.get),
          leftReference.diff(rightReference)
        )

  test("every bounded optimized total map matches the dense reference"):
    Exhaustive.totalMaps(3, 2).toOption.get.foreach: reference =>
      val mapping =
        TotalMap.fromTargetOrdinals(x, y, reference.targets.toArray).toOption.get
      assertEquals(LocusDifferential.totalMap(mapping), reference)

      Exhaustive.regions(3).toOption.get.foreach: sourceRegion =>
        val productionSource =
          Region.fromOrdinals(x, sourceRegion.members).toOption.get
        assertEquals(
          LocusDifferential.region(mapping.existsAlong(productionSource).toOption.get),
          reference.existsAlong(sourceRegion)
        )
        assertEquals(
          LocusDifferential.region(mapping.forallAlong(productionSource).toOption.get),
          reference.forallAlong(sourceRegion)
        )

  test("every bounded optimized relation matches dense composition"):
    val fromReference = Exhaustive.relations(3, 2).toOption.get
    val toReference = Exhaustive.relations(2, 3).toOption.get
    val z = FiniteSpace.make[X](SpaceKey.unsafe("differential:z"), 3).toOption.get

    fromReference.foreach: firstReference =>
      val first = relationFromReference(x, y, firstReference)
      toReference.foreach: secondReference =>
        val second = relationFromReference(y, z, secondReference)
        assertEquals(
          LocusDifferential.relation(first.andThen(second).toOption.get),
          firstReference.andThen(secondReference)
        )

  test("supported parcellations, fields, and sections convert without storage assumptions"):
    val parcels =
      FiniteSpace.make[P](SpaceKey.unsafe("differential:parcels"), 2).toOption.get
    Exhaustive.parcellations(3, 2).toOption.get.foreach: reference =>
      val production =
        Parcellation.fromAssignments(x, parcels, reference.assignments).toOption.get
      assertEquals(LocusDifferential.parcellation(production), reference)

    val field = IndexedField.fromValues(x, Vector("a", "b", "c")).toOption.get
    val support = Region.fromOrdinals(x, Vector(2, 0)).toOption.get
    val section = field.restrict(support).toOption.get
    assertEquals(
      LocusDifferential.section(section).valuesInDomainOrder,
      Vector("a", "c")
    )

  private def relationFromReference[A, B](
      from: FiniteSpace[A],
      to: FiniteSpace[B],
      reference: ReferenceRelation
  ): Relation[A, B] =
    val rows = Array.tabulate(from.size): source =>
      reference.row(source).members.toArray.sorted
    Relation.fromOrdinalRows(from, to, rows).toOption.get
