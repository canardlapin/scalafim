package scalafim.locus

class RelationEvidenceSuite extends munit.FunSuite:
  private sealed trait S

  private val space = FiniteSpace.make[S](SpaceKey.unsafe("evidence:relation"), 3).toOption.get

  test("reflexive evidence validates every diagonal pair"):
    val identity = Relation.identity(space)
    val missing =
      Relation.fromOrdinalRows(space, space, Array(Array(0), Array(0), Array(2))).toOption.get

    assert(ReflexiveRelation.validate(identity).isRight)
    assertEquals(
      ReflexiveRelation.validate(missing),
      Left(RelationEvidenceError.MissingReflexivePoint(1))
    )

  test("symmetric evidence validates converse pairs without requiring loops"):
    val symmetric =
      Relation.fromOrdinalRows(space, space, Array(Array(1), Array(0, 2), Array(1))).toOption.get
    val asymmetric =
      Relation.fromOrdinalRows(space, space, Array(Array(1), Array(2), Array.emptyIntArray)).toOption.get

    assert(SymmetricRelation.validate(symmetric).isRight)
    assertEquals(
      SymmetricRelation.validate(asymmetric),
      Left(RelationEvidenceError.MissingConverse(0, 1))
    )

  test("endorelation evidence rejects a reused phantom with different runtime spaces"):
    val other = FiniteSpace.make[S](SpaceKey.unsafe("evidence:other"), 3).toOption.get
    val relation =
      Relation.fromOrdinalRows(space, other, Array(Array(0), Array(1), Array(2))).toOption.get

    assert(ReflexiveRelation.validate(relation).isLeft)
    assert(SymmetricRelation.validate(relation).isLeft)
