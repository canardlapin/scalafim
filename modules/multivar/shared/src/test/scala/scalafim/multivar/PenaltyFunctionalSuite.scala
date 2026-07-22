package scalafim.multivar

class PenaltyFunctionalSuite extends munit.FunSuite:

  test("canonical penalty identities have unique stable keys and total lookup"):
    val identities = PenaltyFunctionalIdentity.values.toVector
    assertEquals(identities.map(_.stableKey).distinct.size, identities.size)
    identities.foreach: identity =>
      assertEquals(PenaltyFunctionalIdentity.fromStableKey(identity.stableKey), Some(identity))
    assertEquals(PenaltyFunctionalIdentity.fromStableKey("squared_smoothness"), None)

  test("every operator-program functional maps to exactly one canonical identity"):
    val geometry = identity("penalty-geometry")
    val groups = identity("penalty-groups")
    val cases = Vector(
      FunctionalKind.SquaredNorm(geometry) -> PenaltyFunctionalIdentity.SquaredNorm,
      FunctionalKind.L1 -> PenaltyFunctionalIdentity.L1,
      FunctionalKind.GroupL21 -> PenaltyFunctionalIdentity.GroupL21,
      FunctionalKind.GroupL2(groups) -> PenaltyFunctionalIdentity.GroupL2,
      FunctionalKind.SparseGroup(UnitFraction.unsafe(0.25), groups) -> PenaltyFunctionalIdentity.SparseGroup,
      FunctionalKind.ElasticNet(UnitFraction.unsafe(0.5)) -> PenaltyFunctionalIdentity.ElasticNet,
      FunctionalKind.Huber(PenaltyWeight.unsafe(1.5)) -> PenaltyFunctionalIdentity.Huber,
      FunctionalKind.TotalVariation -> PenaltyFunctionalIdentity.TotalVariation,
      FunctionalKind.NuclearNorm -> PenaltyFunctionalIdentity.NuclearNorm,
      FunctionalKind.NegativeLogDet -> PenaltyFunctionalIdentity.NegativeLogDet
    )

    assertEquals(cases.map((witness, _) => witness.functionalIdentity), cases.map(_._2))
    assertEquals(cases.map(_._2).toSet, PenaltyFunctionalIdentity.values.toSet)

  test("GLRM, block structure, and quadratic provenance retain distinct witnesses"):
    assertEquals(
      GlrmFactorPenalty.values.toVector.map(_.functionalIdentity),
      Vector(PenaltyFunctionalIdentity.L1, PenaltyFunctionalIdentity.SquaredNorm)
    )
    assertEquals(
      BlockStructuredPenaltyKind.values.toVector.map(_.functionalIdentity),
      Vector(
        PenaltyFunctionalIdentity.TotalVariation,
        PenaltyFunctionalIdentity.SquaredNorm,
        PenaltyFunctionalIdentity.TotalVariation,
        PenaltyFunctionalIdentity.SquaredNorm
      )
    )
    assert(QuadraticFamily.values.forall(_.functionalIdentity == PenaltyFunctionalIdentity.SquaredNorm))
    assert(BlockStructuredPenaltyKind.GraphSmoothness != BlockStructuredPenaltyKind.LinearSmoothness)
    assert(BlockStructuredPenaltyKind.GraphTotalVariation != BlockStructuredPenaltyKind.LinearTotalVariation)

  private def identity(value: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(value))
