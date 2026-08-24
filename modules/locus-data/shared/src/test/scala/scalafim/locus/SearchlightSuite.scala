package scalafim.locus

class SearchlightSuite extends munit.FunSuite:
  private val resolution =
    DomainFactory.unsafeRestore(SpaceKey.unsafe("searchlight:test"), 4)
  private type S = resolution.S
  private val space: FiniteSpace[S] = resolution.space
  private val centers =
    Region.fromOrdinals(space, Vector(0, 2)).toOption.get

  test("searchlight exposes relation rows only at allowed centers"):
    val relation =
      Relation.fromOrdinalRows(
        space,
        space,
        Iterator(
          Iterator(0, 1),
          Iterator.empty,
          Iterator(1, 2, 3),
          Iterator.empty
        )
      ).toOption.get
    val searchlight = Searchlight.make(centers, relation).toOption.get

    assertEquals(
      searchlight.regionAt(space.indexOption(0).get).map(_.ordinalsInDomainOrder.toVector),
      Some(Vector(0, 1))
    )
    assertEquals(searchlight.regionAt(space.indexOption(1).get), None)
    assert(CenteredSearchlight.validate(searchlight).isRight)

  test("rows outside the center domain must be empty"):
    val relation =
      Relation.fromOrdinalRows(
        space,
        space,
        Iterator(Iterator(0), Iterator(1), Iterator(2), Iterator.empty)
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
        Iterator(Iterator(1), Iterator.empty, Iterator(1, 3), Iterator.empty)
      ).toOption.get
    val searchlight = Searchlight.make(centers, relation).toOption.get

    assertEquals(
      CenteredSearchlight.validate(searchlight),
      Left(CenteredSearchlightError.MissingCenter(0))
    )

  test("searchlight construction checks exact runtime space identity"):
    val other =
      DomainFactory.unsafeRestore(SpaceKey.unsafe("searchlight:other"), 4).space
    val wrong =
      Relation.identity(other).asInstanceOf[Relation[S, S]]

    assert(Searchlight.make(centers, wrong).isLeft)
