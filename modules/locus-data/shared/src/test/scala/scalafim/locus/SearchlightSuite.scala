package scalafim.locus

class SearchlightSuite extends munit.FunSuite:
  private sealed trait S

  private val space =
    FiniteSpace.make[S](SpaceKey.unsafe("searchlight:test"), 4).toOption.get
  private val centers =
    Region.fromOrdinals(space, Vector(0, 2)).toOption.get

  test("searchlight exposes relation rows only at allowed centers"):
    val relation =
      Relation.fromOrdinalRows(
        space,
        space,
        Array(Array(0, 1), Array.emptyIntArray, Array(1, 2, 3), Array.emptyIntArray)
      ).toOption.get
    val searchlight = Searchlight.make(centers, relation).toOption.get

    assertEquals(
      searchlight.regionAt(space.point(0).get).map(_.ordinalsInDomainOrder.toVector),
      Some(Vector(0, 1))
    )
    assertEquals(searchlight.regionAt(space.point(1).get), None)
    assert(CenteredSearchlight.validate(searchlight).isRight)

  test("rows outside the center domain must be empty"):
    val relation =
      Relation.fromOrdinalRows(
        space,
        space,
        Array(Array(0), Array(1), Array(2), Array.emptyIntArray)
      ).toOption.get

    assertEquals(
      Searchlight.make(centers, relation),
      Left(SearchlightError.NonEmptyOutsideCenters(1))
    )

  test("centered evidence is validated separately from searchlight validity"):
    val relation =
      Relation.fromOrdinalRows(
        space,
        space,
        Array(Array(1), Array.emptyIntArray, Array(1, 3), Array.emptyIntArray)
      ).toOption.get
    val searchlight = Searchlight.make(centers, relation).toOption.get

    assertEquals(
      CenteredSearchlight.validate(searchlight),
      Left(CenteredSearchlightError.MissingCenter(0))
    )

  test("searchlight construction checks exact runtime space identity"):
    val other =
      FiniteSpace.make[S](SpaceKey.unsafe("searchlight:other"), 4).toOption.get
    val wrong = Relation.identity(other)

    assert(Searchlight.make(centers, wrong).isLeft)
