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

  import StudyVerdict.{Reject, Retain, Unresolved, Failed}

  private val r = Decision.Studies
  private val confirmation: Vector[Cell] =
    CellManifest.FixedConfirmation.flatMap(Cell.byId) ++
      Cell.core.filterNot(c => CellManifest.FixedConfirmation.contains(c.id)).take(6)

  private def verdicts(k: Int): Vector[StudyVerdict] = Vector.fill(k)(Reject) ++ Vector.fill(r - k)(Retain)
  private def evidence(k: Cell => Int, cells: Vector[Cell] = confirmation): Vector[CellEvidence] =
    cells.map(c => CellEvidence.aggregate(c, verdicts(k(c))))
  private def power(n10: Int, n01: Int, studies: Int = r): Vector[PowerEvidence] =
    CellManifest.PowerCells.flatMap(Cell.byId).map { c =>
      PowerEvidence.aggregate(
        c,
        Vector.fill(n10)(Reject -> ComparatorVerdict.Retain) ++ Vector.fill(n01)(Retain -> ComparatorVerdict.Reject) ++
          Vector.fill(studies - n10 - n01)(Retain -> ComparatorVerdict.Retain)
      )
    }

  test("§6 accounting: a failure or Unresolved bound is a study failure and a level rejection"):
    val c = confirmation.head
    val e = CellEvidence.aggregate(c, Vector(Reject, Retain, Unresolved, Failed, Retain))
    assertEquals((e.studies, e.nullRejections, e.studyFailures), (5, 3, 2))
    assertEquals(Vector(Reject, Retain, Unresolved, Failed).map(_.powerRejects), Vector(true, false, false, false))

  test("§6 power accounting: candidate failures are non-rejections, comparator failures are rejections"):
    val c = Cell.byId(CellManifest.PowerCells.head).get
    val p = PowerEvidence.aggregate(c, Vector(
      Reject -> ComparatorVerdict.Retain,     // n10
      Failed -> ComparatorVerdict.Retain,     // neither
      Unresolved -> ComparatorVerdict.Failed, // n01: comparator failure rejects, candidate Unresolved does not
      Reject -> ComparatorVerdict.Failed,     // both
      Retain -> ComparatorVerdict.Reject      // n01
    ))
    assertEquals((p.studies, p.candidateOnly, p.comparatorOnly), (5, 1, 2))

  test("verdicts from study results: outer failures are Failed, p bounds decide the rest"):
    val o = BootstrapOutcome(Scheme.Plug, 2.0, 99, 3, 0, 0, 0.0, 0.0, None)
    assertEquals(StudyVerdict.of(Right(o), 0.05), Reject)
    assertEquals(StudyVerdict.of(Right(o.copy(failed = 2)), 0.05), Unresolved)
    assertEquals(StudyVerdict.of(Right(o.copy(exceed = 10)), 0.05), Retain)
    assertEquals(StudyVerdict.of(Left(StudyFailure.ObservedFit(FitStatus.NoConvergence)), 0.05), Failed)

  test("candidate outcomes: Adopt, Decline on a Fail or a definite loss, Bound on a sub-family, else Unresolved"):
    assertEquals(Decision.outcome(evidence(_ => 1000), power(800, 400)), Right(CandidateOutcome.Adopt))
    assertEquals(Decision.outcome(evidence(_ => 1000), power(400, 400)), Right(CandidateOutcome.Unresolved))
    // A Fail inside n >= 20 blocks that Bound; nu >= 40 has an n = 20 power cell with a gain, so it still qualifies.
    assertEquals(Decision.outcome(evidence(c => if c.n == 80 then 1500 else 1000), power(800, 400)), Right(CandidateOutcome.Bound(Vector(SubFamily.LargeNu))))
    assertEquals(Decision.outcome(evidence(_ => 1500), power(800, 400)), Right(CandidateOutcome.Decline))
    assertEquals(Decision.outcome(evidence(_ => 1000), power(100, 1500)), Right(CandidateOutcome.Decline))
    val largeN = Decision.outcome(evidence(c => if c.n < 20 then 1300 else 1000), power(800, 400))
    assertEquals(largeN, Right(CandidateOutcome.Bound(Vector(SubFamily.LargeN))))
    val gain = Decision.powerVerdict(power(800, 400).head)
    assert(gain.gain && gain.nonLoss && !gain.definiteLoss)

  test("outcome refuses: R != 20000, not the 12 confirmation cells, not exactly the 4 power cells"):
    val small = confirmation.map(c => CellEvidence.aggregate(c, Vector.fill(100)(Retain)))
    assert(Decision.outcome(small, power(800, 400)).left.exists(_.isInstanceOf[DecisionRefusal.WrongStudyCount]))
    assert(Decision.outcome(evidence(_ => 1000).take(11), power(800, 400)).left.exists(_.isInstanceOf[DecisionRefusal.WrongConfirmationCells]))
    val noFixed = evidence(_ => 1000, Cell.core.filterNot(c => CellManifest.FixedConfirmation.contains(c.id)).take(12))
    assert(Decision.outcome(noFixed, power(800, 400)).left.exists(_.isInstanceOf[DecisionRefusal.WrongConfirmationCells]))
    val withStress = evidence(_ => 1000, confirmation.dropRight(1) :+ Cell.stress.head)
    assert(Decision.outcome(withStress, power(800, 400)).left.exists(_.isInstanceOf[DecisionRefusal.WrongConfirmationCells]))
    assert(Decision.outcome(evidence(_ => 1000), power(800, 400).take(3)).left.exists(_.isInstanceOf[DecisionRefusal.WrongPowerCells]))
    assert(Decision.outcome(evidence(_ => 1000), power(800, 400, 19999)).left.exists(_.isInstanceOf[DecisionRefusal.WrongStudyCount]))

  private val pool = SelectionPool.CoreMinusFixed.cells
  private def pilot(rate: Cell => Map[Scheme, Vector[StudyVerdict]]): Vector[PilotEvidence] = pool.map(c => PilotEvidence(c, rate(c)))
  private def flat(k: Int, n: Int = 2000): Vector[StudyVerdict] = Vector.fill(k)(Reject) ++ Vector.fill(n - k)(Retain)
  private val candidates = Vector(Scheme.Plug, Scheme.FixV, Scheme.EmpiricalBayes)

  test("the owner selection rule: CountAsRejection, core minus fixed, max over the 3 candidates, lowest ID string, 6 cells"):
    assertEquals(SelectionRule.Owner, SelectionRule(PilotFailureAccounting.CountAsRejection, SelectionPool.CoreMinusFixed, candidates, TieOrder.LowestIdString, 6))
    assertEquals(pool.length, 84)
    assert(pool.forall(c => c.family == Family.Core && !CellManifest.FixedConfirmation.contains(c.id)))

  test("selection: the six largest max-over-candidates rates, ties to the lowest ID string"):
    val worst = pool.map(_.id.value).sorted.reverse.take(4).toSet
    val tied = pool.map(_.id.value).sorted.take(5).toSet
    val chosen = ConfirmationSelection.select(pilot { c =>
      val k = if worst.contains(c.id.value) then 150 else if tied.contains(c.id.value) then 120 else 90
      // The excess can sit in any one candidate: the max over candidates must find it.
      Map(Scheme.Plug -> flat(90), Scheme.FixV -> flat(if c.n == 8 then k else 90), Scheme.EmpiricalBayes -> flat(if c.n != 8 then k else 90))
    })
    val expected = worst.toVector.sorted ++ tied.toVector.sorted.take(2)
    assertEquals(chosen.map(_.map(_.value)), Right(expected))

  test("selection accounting options change k as declared"):
    val v = Vector(Reject, Reject, Unresolved, Failed, Retain, Retain, Retain, Retain)
    assertEquals(ConfirmationSelection.rate(v, PilotFailureAccounting.CountAsRejection), Some(4.0 / 8))
    assertEquals(ConfirmationSelection.rate(v, PilotFailureAccounting.CountAsRetention), Some(2.0 / 8))
    assertEquals(ConfirmationSelection.rate(v, PilotFailureAccounting.ExcludeFromDenominator), Some(2.0 / 6))
    val rule = SelectionRule.Owner.copy(accounting = PilotFailureAccounting.CountAsRetention)
    val failing = pool.take(6).map(_.id).toSet
    val picked = ConfirmationSelection.select(pilot { c =>
      val vs = if failing.contains(c.id) then Vector.fill(300)(Failed) ++ Vector.fill(1700)(Retain) else flat(120)
      candidates.map(_ -> vs).toMap
    }, rule)
    assert(picked.exists(_.toSet.intersect(failing).isEmpty), "failures are retentions under CountAsRetention")
    val default = ConfirmationSelection.select(pilot { c =>
      val vs = if failing.contains(c.id) then Vector.fill(300)(Failed) ++ Vector.fill(1700)(Retain) else flat(120)
      candidates.map(_ -> vs).toMap
    })
    assertEquals(default.map(_.toSet), Right(failing))

  test("selection refuses missing, duplicated or incomplete pilot evidence"):
    val full = pilot(_ => candidates.map(_ -> flat(100)).toMap)
    assert(ConfirmationSelection.select(full.tail).left.exists(_.isInstanceOf[SelectionError.MissingCell]))
    assert(ConfirmationSelection.select(full :+ full.head).left.exists(_.isInstanceOf[SelectionError.DuplicateCell]))
    val noEb = full.updated(0, full.head.copy(verdicts = full.head.verdicts - Scheme.EmpiricalBayes))
    assert(ConfirmationSelection.select(noEb).left.exists(_.isInstanceOf[SelectionError.MissingCandidate]))
    val empty = full.updated(0, full.head.copy(verdicts = candidates.map(_ -> Vector.empty[StudyVerdict]).toMap))
    assert(ConfirmationSelection.select(empty).left.exists(_.isInstanceOf[SelectionError.WrongStudyCount]))
    val allFailed = full.updated(0, full.head.copy(verdicts = candidates.map(_ -> Vector.fill(2000)(Failed)).toMap))
    val excluding = SelectionRule.Owner.copy(accounting = PilotFailureAccounting.ExcludeFromDenominator)
    assert(ConfirmationSelection.select(allFailed, excluding).left.exists(_.isInstanceOf[SelectionError.NoStudies]))

  test("selection refuses R != 2000 for any candidate, and unequal R across candidates"):
    val full = pilot(_ => candidates.map(_ -> flat(100)).toMap)
    val short = full.updated(3, full(3).copy(verdicts = full(3).verdicts.updated(Scheme.FixV, flat(100, 1999))))
    assert(ConfirmationSelection.select(short).left.exists(_.isInstanceOf[SelectionError.UnequalStudyCounts]))
    val allShort = full.updated(3, full(3).copy(verdicts = candidates.map(_ -> flat(100, 1999)).toMap))
    assert(ConfirmationSelection.select(allShort).left.exists(_.isInstanceOf[SelectionError.WrongStudyCount]))
    val tiny = pilot(_ => candidates.map(_ -> flat(1, 3)).toMap)
    assert(ConfirmationSelection.checkStudyCounts(tiny, SelectionRule.Owner, 3).isRight)
    assert(ConfirmationSelection.select(tiny).left.exists(_.isInstanceOf[SelectionError.WrongStudyCount]))

  test("the selection score counts each study once, whether Reject, Unresolved or Failed"):
    val v = Vector(Reject, Unresolved, Failed) ++ Vector.fill(1997)(Retain)
    assertEquals(ConfirmationSelection.rate(v, SelectionRule.Owner.accounting), Some(3.0 / 2000))

  // N1 / N3: decision-rule gaps.
  test("outcome refuses four power cells that are not the declared ones"):
    val notPower = Cell.core.filterNot(c => CellManifest.PowerCells.contains(c.id)).take(4)
    val wrong = notPower.map(c => PowerEvidence.aggregate(c, Vector.fill(800)(Reject -> ComparatorVerdict.Retain) ++ Vector.fill(r - 800)(Retain -> ComparatorVerdict.Retain)))
    assertEquals(wrong.length, 4)
    assert(Decision.outcome(evidence(_ => 1000), wrong).left.exists(_.isInstanceOf[DecisionRefusal.WrongPowerCells]))

  private def mixedPower(gainAt: CellId): Vector[PowerEvidence] =
    CellManifest.PowerCells.flatMap(Cell.byId).map { c =>
      val (n10, n01) = if c.id == gainAt then (800, 400) else (400, 400)
      PowerEvidence.aggregate(c, Vector.fill(n10)(Reject -> ComparatorVerdict.Retain) ++ Vector.fill(n01)(Retain -> ComparatorVerdict.Reject) ++
        Vector.fill(r - n10 - n01)(Retain -> ComparatorVerdict.Retain))
    }

  test("Gain needs only one power cell (mixed gain): Adopt, and Bound(n >= 20) through its n80 power cell"):
    val n80 = CellManifest.PowerCells.find(_.value.startsWith("C-n80")).get
    val mixed = mixedPower(n80)
    assertEquals(mixed.count(e => Decision.powerVerdict(e).gain), 1)
    assert(mixed.forall(e => Decision.powerVerdict(e).nonLoss))
    assertEquals(Decision.outcome(evidence(_ => 1000), mixed), Right(CandidateOutcome.Adopt))
    val bounded = Decision.outcome(evidence(c => if outside(c) then 1300 else 1000), mixed)
    assertEquals(bounded, Right(CandidateOutcome.Bound(Vector(SubFamily.LargeN))))

  test("a failure rate above 59 with k < 1456 does not Pass: neither Adopt nor Bound, and Unresolved rather than Decline"):
    // 60 failed studies (counted as rejections) + 1000 resolved rejections: k = 1060 <= 1149, f = 60 >= 60.
    val failing = confirmation.map(c => CellEvidence.aggregate(c, Vector.fill(60)(Failed) ++ Vector.fill(1000)(Reject) ++ Vector.fill(r - 1060)(Retain)))
    assert(failing.forall(e => Decision.nullVerdict(e.nullRejections, e.studies) == NullVerdict.Pass))
    assert(failing.forall(e => Decision.failureVerdict(e.studyFailures, e.studies) == FailureVerdict.NotPass))
    assertEquals(Decision.outcome(failing, power(800, 400)), Right(CandidateOutcome.Unresolved))

  test("a Definite loss in a power cell outside S does not block Bound(S)"):
    val outsidePower = CellManifest.PowerCells.flatMap(Cell.byId).find(outside).get
    val lossOutside = power(800, 400).map(e => if e.cell == outsidePower then power(100, 1500).find(_.cell == outsidePower).get else e)
    assert(Decision.powerVerdict(lossOutside.find(_.cell == outsidePower).get).definiteLoss)
    assertEquals(Decision.outcome(evidence(_ => 1000), lossOutside), Right(CandidateOutcome.Bound(Vector(SubFamily.LargeN, SubFamily.LargeNu))))

  // n8 cells with finite nu < 40 lie in neither sub-family.
  private def outside(c: Cell): Boolean = !SubFamily.LargeN.contains(c) && !SubFamily.LargeNu.contains(c)

  test("Bound: both sub-families qualify, and the declared domain is their union"):
    val both = Decision.outcome(evidence(c => if outside(c) then 1300 else 1000), power(800, 400))
    assertEquals(both, Right(CandidateOutcome.Bound(Vector(SubFamily.LargeN, SubFamily.LargeNu))))
    val domain = both.toOption.get
    assert(confirmation.filterNot(outside).forall(domain.domain), "every cell of either sub-family is in the domain")
    assert(!confirmation.filter(outside).exists(domain.domain))

  test("Bound: a Fail outside S is allowed"):
    val failOutside = Decision.outcome(evidence(c => if outside(c) then 1500 else 1000), power(800, 400))
    assertEquals(failOutside, Right(CandidateOutcome.Bound(Vector(SubFamily.LargeN, SubFamily.LargeNu))))
    val failInNu = Decision.outcome(evidence(c => if c.n < 20 then 1500 else 1000), power(800, 400))
    assertEquals(failInNu, Right(CandidateOutcome.Bound(Vector(SubFamily.LargeN))))

  test("Bound: a Pass outside S does not block it"):
    val oneUnresolved = confirmation.find(outside).get
    val result = Decision.outcome(evidence(c => if c == oneUnresolved then 1300 else 1000), power(800, 400))
    assert(confirmation.filter(outside).exists(c => c != oneUnresolved), "other cells outside S pass")
    assertEquals(result, Right(CandidateOutcome.Bound(Vector(SubFamily.LargeN, SubFamily.LargeNu))))

  test("Bound: a sub-family without a power cell cannot qualify (Gain is unreachable)"):
    val cells = evidence(_ => 1000)
    val noPowerCell: Cell => Boolean = c => c.n == 8 && !CellManifest.PowerCells.contains(c.id)
    assert(cells.exists(e => noPowerCell(e.cell)) && !power(800, 400).exists(e => noPowerCell(e.cell)))
    assert(!Decision.qualifies(noPowerCell, cells, power(800, 400)))
    assert(Decision.qualifies(SubFamily.LargeN.contains, cells, power(800, 400)))
    // A failing power cell in S (non-loss broken) blocks S even when its cells pass.
    val lossy = power(800, 400).map(e => if SubFamily.LargeNu.contains(e.cell) then power(100, 1500).find(_.cell == e.cell).get else e)
    assert(!Decision.qualifies(SubFamily.LargeNu.contains, cells, lossy))
