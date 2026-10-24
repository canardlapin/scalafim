package providercontract.locus

import locus4s.{CenteredNeighborhoodSystem, FiniteSpace, NeighborhoodSystem, NeighborhoodSystemError, Region, Relation, RelationError, Selection}

/** Compact provider neighborhoods replace the retired ambient-row facade. */
class SearchlightSuite extends munit.FunSuite:
  private val resolution = DomainFixtures.restore("searchlight:test", 4)
  private type S = resolution.S
  private val space: FiniteSpace[S] = resolution.space
  private val centers = Region.fromOrdinals(space, Vector(0, 2)).toOption.get
  private val selected = Selection.fromRegion(centers).toOption.get

  test("searchlight exposes relation rows only at allowed ambient centers"):
    val relation = Relation.fromOrdinalRows(selected.positions, space, Vector(Vector(0, 1), Vector(1, 2, 3))).toOption.get
    val neighborhoods = NeighborhoodSystem.fromSelection(selected, relation)
    val atZero = neighborhoods.centers.indices.find(c => neighborhoods.center(c) == space.indexOption(0).get)
    val atOne = neighborhoods.centers.indices.find(c => neighborhoods.center(c) == space.indexOption(1).get)
    assertEquals(atZero.map(neighborhoods.neighborhood(_).ordinalsInDomainOrder.toVector), Some(Vector(0, 1)))
    assertEquals(atOne, None)
    assert(CenteredNeighborhoodSystem.from(neighborhoods).isRight)

  test("compact membership rejects ambient-sized rows instead of dropping noncenter entries"):
    // The old invalid ambient relation had a nonempty row at ambient ordinal1.
    // Compact storage has exactly two rows, for embedded centers0and2, so the
    // old four-row layout cannot be represented or silently truncated here.
    val result = Relation.fromOrdinalRows(selected.positions, space,
      Vector(Vector(0), Vector(1), Vector(2), Vector.empty[Int]))
    assertEquals(result.left.toOption, Some(RelationError.WrongRowCount(2, 4)))

  test("ordinary neighborhoods remain valid when centered evidence is refused"):
    val relation = Relation.fromOrdinalRows(selected.positions, space, Vector(Vector(1), Vector(1, 3))).toOption.get
    val ordinary = NeighborhoodSystem.from(selected.embedding, relation)
    assert(ordinary.isRight)
    assertEquals(CenteredNeighborhoodSystem.from(ordinary.toOption.get).left.toOption,
      Some(NeighborhoodSystemError.MissingEmbeddedCenter(0, 0)))

  test("dynamic neighborhood construction checks both exact runtime endpoint owners"):
    val other = DomainFixtures.restore("searchlight:other", 4).space
    val otherSelected = Selection.fromOrdinals(other, Vector(0, 2)).toOption.get
    val wrongCenters = Relation.fromOrdinalRows(otherSelected.positions, space, Vector(Vector(0), Vector(2))).toOption.get
    val wrongAmbient = Relation.fromOrdinalRows(selected.positions, other, Vector(Vector(0), Vector(2))).toOption.get
    assert(NeighborhoodSystem.from(selected.embedding, wrongCenters).left.toOption.exists:
      case NeighborhoodSystemError.MembershipCenterOwnerMismatch(_) => true
      case _ => false
    )
    assert(NeighborhoodSystem.from(selected.embedding, wrongAmbient).left.toOption.exists:
      case NeighborhoodSystemError.MembershipAmbientOwnerMismatch(_) => true
      case _ => false
    )
