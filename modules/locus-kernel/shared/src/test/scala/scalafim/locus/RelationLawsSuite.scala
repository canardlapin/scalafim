package scalafim.locus

class RelationLawsSuite extends munit.FunSuite:
  private sealed trait S
  private sealed trait X
  private sealed trait Y
  private sealed trait Z

  private val space = FiniteSpace.make[S](SpaceKey.unsafe("laws:relation"), 2).toOption.get
  private val relations = allRelations(space, space)
  private val regions = allRegions(space)

  test("relation category laws are exhaustive"):
    val identity = Relation.identity(space)
    relations.foreach: first =>
      assertEquals(identity.andThen(first).toOption.get, first)
      assertEquals(first.andThen(identity).toOption.get, first)
      relations.foreach: second =>
        relations.foreach: third =>
          assertEquals(
            first.andThen(second).toOption.get.andThen(third).toOption.get,
            first.andThen(second.andThen(third).toOption.get).toOption.get
          )

  test("converse is an involutive dagger"):
    relations.foreach: first =>
      assertEquals(first.converse.converse, first)
      relations.foreach: second =>
        assertEquals(
          first.andThen(second).toOption.get.converse,
          second.converse.andThen(first.converse).toOption.get
        )

  test("composition distributes over union on both sides"):
    relations.foreach: first =>
      relations.foreach: second =>
        relations.foreach: third =>
          assertEquals(
            first.union(second).toOption.get.andThen(third).toOption.get,
            first.andThen(third).toOption.get
              .union(second.andThen(third).toOption.get)
              .toOption
              .get
          )
          assertEquals(
            first.andThen(second.union(third).toOption.get).toOption.get,
            first.andThen(second).toOption.get
              .union(first.andThen(third).toOption.get)
              .toOption
              .get
          )

  test("relational image and all-related-inside form an adjunction"):
    relations.foreach: relation =>
      regions.foreach: sourceRegion =>
        regions.foreach: targetRegion =>
          val imageSubset =
            relation.image(sourceRegion).toOption.get
              .subsetOf(targetRegion)
              .toOption
              .get
          val sourceSubsetErosion =
            sourceRegion
              .subsetOf(relation.allRelatedInside(targetRegion).toOption.get)
              .toOption
              .get
          assertEquals(imageSubset, sourceSubsetErosion)

  test("sparse composition matches an independent dense Boolean reference"):
    val from = FiniteSpace.make[X](SpaceKey.unsafe("reference:x"), 3).toOption.get
    val middle = FiniteSpace.make[Y](SpaceKey.unsafe("reference:y"), 2).toOption.get
    val to = FiniteSpace.make[Z](SpaceKey.unsafe("reference:z"), 3).toOption.get
    val firstRelations = allRelations(from, middle)
    val secondRelations = allRelations(middle, to)

    firstRelations.foreach: first =>
      secondRelations.foreach: second =>
        val actual = first.andThen(second).toOption.get
        val expectedRows = Array.tabulate(from.size): source =>
          Array.tabulate(to.size)(i => i).filter: target =>
            var intermediate = 0
            var related = false
            while intermediate < middle.size && !related do
              val sourcePoint = from.point(source).get
              val middlePoint = middle.point(intermediate).get
              val targetPoint = to.point(target).get
              related =
                first.isRelated(sourcePoint, middlePoint) &&
                  second.isRelated(middlePoint, targetPoint)
              intermediate += 1
            related
        val expected = Relation.fromOrdinalRows(from, to, expectedRows).toOption.get
        assertEquals(actual, expected)

  test("relation constructors defensively own and validate sparse rows"):
    val rows = Array(Array(1, 1), Array(0))
    val relation = Relation.fromOrdinalRows(space, space, rows).toOption.get
    rows(0)(0) = 0

    assertEquals(relation.ordinalRows.map(_.toVector).toVector, Vector(Vector(1), Vector(0)))
    val exported = relation.ordinalRows
    exported(0)(0) = 0
    assertEquals(relation.ordinalRows(0).toVector, Vector(1))
    assertEquals(
      Relation.fromOrdinalRows(space, space, Array(Array(2), Array.emptyIntArray)),
      Left(RelationError.TargetOutOfBounds(0, 0, 2, 2))
    )

  private def allRegions[A](finiteSpace: FiniteSpace[A]): Vector[Region[A]] =
    Vector.tabulate(1 << finiteSpace.size): mask =>
      Region.tabulate(finiteSpace): point =>
        (mask & (1 << point.ordinal)) != 0

  private def allRelations[A, B](
      from: FiniteSpace[A],
      to: FiniteSpace[B]
  ): Vector[Relation[A, B]] =
    val pairCount = from.size * to.size
    Vector.tabulate(1 << pairCount): mask =>
      val rows = Array.tabulate(from.size): source =>
        Array.tabulate(to.size)(i => i).filter: target =>
          val bit = source * to.size + target
          (mask & (1 << bit)) != 0
      Relation.fromOrdinalRows(from, to, rows).toOption.get
