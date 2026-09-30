package scalafim.group.research.bootstrap

class DfAndDecisionSuite extends munit.FunSuite:
  test("df admission: Known -> inf, exact OLS/GLS >= 8 admitted, everything else refused with a typed reason"):
    assert(DfAdmission.admit(FirstLevelDfClaim.Known).exists(_.isKnown))
    assertEquals(DfAdmission.admit(FirstLevelDfClaim.ExactOls(8)).map(_.value), Right(8.0))
    assertEquals(DfAdmission.admit(FirstLevelDfClaim.ExactFixedGls(40)).map(_.value), Right(40.0))
    assertEquals(DfAdmission.admit(FirstLevelDfClaim.ExactOls(7)), Left(DfRefusal.BelowFloor(7, 8)))
    assertEquals(DfAdmission.admit(FirstLevelDfClaim.ExactOls(0)), Left(DfRefusal.NonPositive(0)))
    assertEquals(DfAdmission.admit(FirstLevelDfClaim.Unknown), Left(DfRefusal.UnknownDf))
    assertEquals(DfAdmission.admit(FirstLevelDfClaim.NominalResidual(120)), Left(DfRefusal.NominalTagIsNotALaw))
    assertEquals(DfAdmission.admit(FirstLevelDfClaim.EstimatedAutoregressive(120)), Left(DfRefusal.EstimatedAutoregressiveDf))
    assertEquals(DfAdmission.admit(FirstLevelDfClaim.Satterthwaite(55.2)), Left(DfRefusal.SatterthwaiteStressOnly))

  test("every core and H cell is df-admissible; the nu 3/4/5 and declared-4 stress cells are not"):
    assert(Cell.core.forall(c => DfAdmission.floorAdmits(c.declaredNu)))
    assert(Cell.hierarchy.forall(c => DfAdmission.floorAdmits(c.declaredNu)))
    val refused = Cell.stress.filterNot(c => DfAdmission.floorAdmits(c.declaredNu)).map(_.id.value)
    assertEquals(refused.length, 8)
    assert(refused.forall(id => id.startsWith("S-nu-") || id.startsWith("S-df4-")))

  test("Clopper-Pearson bounds: edge conventions and the frozen integer regions at R = 20000"):
    assertEquals(ClopperPearson.upper(20000, 20000, Decision.Delta), 1.0)
    assertEquals(ClopperPearson.lower(0, 20000, Decision.Delta), 0.0)
    val r = Decision.Studies
    assert(ClopperPearson.upper(Decision.NullPassMax, r, Decision.Delta) <= Decision.Margin)
    assert(ClopperPearson.upper(Decision.NullPassMax + 1, r, Decision.Delta) > Decision.Margin)
    assert(ClopperPearson.lower(Decision.NullFailMin, r, Decision.Delta) > Decision.Margin)
    assert(ClopperPearson.lower(Decision.NullFailMin - 1, r, Decision.Delta) <= Decision.Margin)
    assert(ClopperPearson.upper(Decision.FailurePassMax, r, Decision.Delta) <= Decision.FailureMargin)
    assert(ClopperPearson.upper(Decision.FailurePassMax + 1, r, Decision.Delta) > Decision.FailureMargin)
    assert(Decision.Tails * Decision.Delta <= 0.001)
    assertEquals(Decision.nullVerdict(1149, r), NullVerdict.Pass)
    assertEquals(Decision.nullVerdict(1150, r), NullVerdict.Unresolved)
    assertEquals(Decision.nullVerdict(1455, r), NullVerdict.Unresolved)
    assertEquals(Decision.nullVerdict(1456, r), NullVerdict.Fail)
    assertEquals(Decision.failureVerdict(59, r), FailureVerdict.Pass)
    assertEquals(Decision.failureVerdict(60, r), FailureVerdict.NotPass)

  test("Clopper-Pearson bounds agree with R qbeta (reference fixture)"):
    BootstrapReferenceFixtures.ClopperPearsonCases.foreach { c =>
      assertEqualsDouble(ClopperPearson.upper(c.k, c.r, c.delta), c.upper, 1e-10, s"upper k=${c.k}")
      assertEqualsDouble(ClopperPearson.lower(c.k, c.r, c.delta), c.lower, 1e-10, s"lower k=${c.k}")
    }
    assertEquals(BootstrapReferenceFixtures.NullPassMax, Decision.NullPassMax)
    assertEquals(BootstrapReferenceFixtures.NullFailMin, Decision.NullFailMin)
    assertEquals(BootstrapReferenceFixtures.FailurePassMax, Decision.FailurePassMax)
    assertEqualsDouble(BootstrapReferenceFixtures.ChiSquare49At999, ResearchTestSupport.ChiSquare49At999, 1e-8)

  test("candidate outcomes: Adopt, Decline on a Fail or a definite loss, Bound on a sub-family, else Unresolved"):
    val cells = CellManifest.FixedConfirmation.flatMap(Cell.byId)
    val r = Decision.Studies
    def evidence(k: Cell => Int): Vector[CellEvidence] = cells.map(c => CellEvidence(c, r, k(c), 0))
    def power(n10: Int, n01: Int): Vector[PowerEvidence] =
      CellManifest.PowerCells.flatMap(Cell.byId).map(c => PowerEvidence(c, r, n10, n01))
    assertEquals(Decision.outcome(evidence(_ => 1000), power(800, 400)), CandidateOutcome.Adopt)
    assertEquals(Decision.outcome(evidence(_ => 1000), power(400, 400)), CandidateOutcome.Unresolved)
    assertEquals(Decision.outcome(evidence(c => if c.n == 80 then 1500 else 1000), power(800, 400)), CandidateOutcome.Decline)
    assertEquals(Decision.outcome(evidence(_ => 1000), power(100, 1500)), CandidateOutcome.Decline)
    assertEquals(
      Decision.outcome(evidence(c => if c.n < 20 then 1300 else 1000), power(800, 400)),
      CandidateOutcome.Bound(SubFamily.LargeN)
    )
    val gain = Decision.powerVerdict(PowerEvidence(cells.head, r, 800, 400))
    assert(gain.gain && gain.nonLoss && !gain.definiteLoss)
