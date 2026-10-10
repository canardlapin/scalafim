package scalafim.phrfcmp.exec

class SchedulePolicySuite extends munit.FunSuite:

  private def cell(s: String): CellId = CellId.parse(s).fold(sys.error, identity)
  private def arm(s: String): ArmId = ArmId.parse(s).fold(sys.error, identity)
  private def pilotCell(s: String): PilotCell = PilotCell(cell(s), Vector(arm("a1"), arm("a2")))

  test("dispatch is round-robin: index 0 of every cell, then index 1, in cell order") {
    val cells = Vector(pilotCell("C-1"), pilotCell("C-2"), pilotCell("T-1"))
    val order = Dispatch.order(cells, 3)
    assertEquals(order.length, 9)
    assertEquals(
      order.map(j => (j.cell.value, j.dataset)),
      Vector(
        ("C-1", 0), ("C-2", 0), ("T-1", 0),
        ("C-1", 1), ("C-2", 1), ("T-1", 1),
        ("C-1", 2), ("C-2", 2), ("T-1", 2)
      )
    )
  }

  test("probe wave is the first two datasets of every cell") {
    val cells = Vector(pilotCell("C-1"), pilotCell("T-1"))
    val (probe, rest) = Dispatch.waves(Dispatch.order(cells, 5), 2)
    assertEquals(probe.length, 4)
    assert(probe.forall(_.dataset < 2))
    assertEquals(rest.length, 6)
    assert(rest.forall(_.dataset >= 2))
    assertEquals(rest.head, Job(cell("C-1"), 2))
  }

  test("names that would escape the output tree are refused") {
    assert(CellId.parse("C-TX-.5").isRight)
    assert(CellId.parse("../x").isLeft)
    assert(CellId.parse("a/b").isLeft)
    assert(CellId.parse("").isLeft)
    assert(CellId.parse(".hidden").isLeft)
    assert(ArmId.parse("a b").isLeft)
  }

  private def completed(counts: (String, Seq[Int])*): Map[CellId, Set[Int]] =
    counts.map((c, ds) => cell(c) -> ds.toSet).toMap

  test("drop rule: D is the smallest per-cell count; higher indices dropped uniformly, highest first") {
    val in = completed("A" -> (0 until 20), "B" -> (0 until 17), "C" -> (0 until 18))
    val d = PartialPilot.decide(in).toOption.get
    assertEquals(d.D, 17)
    assert(d.kept.values.forall(_.length == 17))
    assertEquals(d.dropped(cell("A")), Vector(19, 18, 17))
    assertEquals(d.dropped(cell("B")), Vector.empty[Int])
    assertEquals(d.dropped(cell("C")), Vector(17))
    assertEquals(d.kept(cell("A")), (0 until 17).toVector)
    assertEquals(d.df, 16)
    assert(!d.isFull)
  }

  test("drop rule keeps the longest completed prefix: a hole is never kept, everything above it is dropped (D2)") {
    // Before D2 this test asserted D = 4 and kept(A) = 0, 1, 2, 4, i.e. it kept a hole. A kept set with a hole depends
    // on which job a crash or hard stop abandoned, it is not "dropped from the highest index down", and the scorer's
    // corpus requires indices 0 until D in order.
    val in = completed("A" -> Seq(0, 1, 2, 4, 5, 6), "B" -> Seq(0, 1, 2, 3))
    val d = PartialPilot.decide(in, minDatasets = 3).toOption.get
    assertEquals(d.D, 3)
    assertEquals(d.kept(cell("A")), Vector(0, 1, 2))
    assertEquals(d.kept(cell("B")), Vector(0, 1, 2))
    assertEquals(d.dropped(cell("A")), Vector(6, 5, 4))
    assertEquals(d.dropped(cell("B")), Vector(3))
    assertEquals(PartialPilot.prefixLength(Set(0, 1, 2, 4)), 3)
    assertEquals(PartialPilot.prefixLength(Set(1, 2)), 0)
    assertEquals(PartialPilot.decide(completed("A" -> Seq(1, 2, 3), "B" -> (0 until 4)), minDatasets = 2), Left(PilotRefusal.TooFewDatasets(0, 2)))
  }

  test("a resume must finish every incomplete job below the highest completed index, in any cell (D2)") {
    val cells = Vector(pilotCell("A"), pilotCell("B"))
    val order = Dispatch.order(cells, 6)
    def j(c: String, d: Int) = Job(cell(c), d)
    val done = Set(j("A", 0), j("A", 2), j("A", 3), j("B", 0), j("B", 1))
    assertEquals(Dispatch.mustFinish(order, done), Set(j("A", 1), j("B", 2)))
    assertEquals(Dispatch.mustFinish(order, Set.empty), Set.empty[Job])
    assertEquals(Dispatch.mustFinish(order, Set(j("A", 0), j("B", 0))), Set.empty[Job])
  }

  test("M2: a resume also finishes every dispatched incomplete job, at or above the highest completed index") {
    val cells = Vector(pilotCell("A"), pilotCell("B"))
    val order = Dispatch.order(cells, 6)
    def j(c: String, d: Int) = Job(cell(c), d)
    val done = Set(j("A", 0), j("A", 1), j("A", 2), j("B", 0), j("B", 1))
    // (B, 2) at the highest completed index and (A, 3) above it were in flight
    val dispatched = done ++ Set(j("B", 2), j("A", 3))
    assertEquals(Dispatch.mustFinish(order, done), Set.empty[Job], "the completed set alone cannot see them")
    assertEquals(Dispatch.mustFinish(order, done, dispatched), Set(j("B", 2), j("A", 3)))
    assertEquals(Dispatch.mustFinish(order, done, done), Set.empty[Job], "a completed job is never finished again")
  }

  test("D < 15 refuses a partial pilot; D = 15 is accepted with df 14") {
    val bad = completed("A" -> (0 until 20), "B" -> (0 until 14))
    assertEquals(PartialPilot.decide(bad), Left(PilotRefusal.TooFewDatasets(14, 15)))
    val ok = completed("A" -> (0 until 20), "B" -> (0 until 15))
    val d = PartialPilot.decide(ok).toOption.get
    assertEquals((d.D, d.df), (15, 14))
    assert(PartialPilot.decide(Map.empty).isLeft)
  }

  test("chi-square quantile and UCL factor are recomputed from df") {
    assertEqualsDouble(Ucl.chiSquareQuantile(0.20, 2), -2.0 * math.log(0.8), 1e-9)
    assertEqualsDouble(Ucl.chiSquareQuantile(0.20, 19), 13.7158, 1e-3)
    assertEqualsDouble(Ucl.factor(19), 1.177, 5e-3)
    List(1, 5, 14, 19, 40).foreach(df => assertEqualsDouble(Ucl.chiSquareCdf(Ucl.chiSquareQuantile(0.2, df), df), 0.2, 1e-9))
    assert(Ucl.factor(14) > Ucl.factor(19))
    val full = PartialPilot.decide(completed("A" -> (0 until 20), "B" -> (0 until 20))).toOption.get
    assert(full.isFull)
    assertEqualsDouble(full.uclFactor, Ucl.factor(19), 1e-12)
    val part = PartialPilot.decide(completed("A" -> (0 until 20), "B" -> (0 until 16))).toOption.get
    assertEqualsDouble(part.uclFactor, Ucl.factor(15), 1e-12)
  }

  test("retries are capped at 2, refusals are never retried, done commits at once") {
    val p = RetryPolicy()
    val fail = ArmResult.Failed("boom")
    assertEquals(p.decide(0, fail), RetryPolicy.Step.Retry("boom"))
    assertEquals(p.decide(1, fail), RetryPolicy.Step.Retry("boom"))
    assertEquals(p.decide(2, fail), RetryPolicy.Step.Commit(UnitStatus.Failed, "boom"))
    assertEquals(p.decide(0, ArmResult.Refused("rank")), RetryPolicy.Step.Commit(UnitStatus.Refused, "rank"))
    assertEquals(p.decide(2, ArmResult.Done()),RetryPolicy.Step.Commit(UnitStatus.Done, ""))
    assertEquals(RetryPolicy(0).decide(0, fail), RetryPolicy.Step.Commit(UnitStatus.Failed, "boom"))
  }

  test("CPU guard: soft stop at 45 and hard stop at 60 core-hours, cumulative seconds") {
    val g = CpuGuard()
    assertEquals(g.check(44.99 * 3600), CpuGuard.State.Ok)
    assertEquals(g.check(45.0 * 3600), CpuGuard.State.SoftStop)
    assertEquals(g.check(59.99 * 3600), CpuGuard.State.SoftStop)
    assertEquals(g.check(60.0 * 3600), CpuGuard.State.HardStop)
    assertEqualsDouble(CpuGuard.projectCoreHours(7200.0, 4, 40), 20.0, 1e-12)
    assertEqualsDouble(CpuGuard.projectCoreHours(0.0, 0, 40), 0.0, 0.0)
  }

  test("ledger records are status only, carry invocation and run id, round-trip, and reject extra keys") {
    val sha = "ab" * 32
    val u = WorkUnit(cell("C-1"), 3, arm("a1"))
    val r = LedgerRecord(u, 2, "0123abcd", CommitPhase.Scheduled, 3, UnitStatus.Failed, "timeout", "data/C-1/d0003/a1/0123abcd", sha, Vector("x.bin"))
    assertEquals(LedgerRecord.parse(r.json), Right(r))
    assert(LedgerRecord.parse(r.json.replace("}", ",\"rate\":0.05}")).isLeft)
    assert(LedgerRecord.parse(r.json.replace("\"failed\"", "\"retried\"")).isLeft, "a non-terminal status is never committed")
    assert(LedgerRecord.parse(r.json.replace("\"invocation\":2", "\"invocation\":2.5")).isLeft)
    intercept[IllegalArgumentException](LedgerRecord(u, 1, "r", CommitPhase.Rerun, 1, UnitStatus.Retried, "", "p", sha))
    intercept[IllegalArgumentException](LedgerRecord(u, 0, "r", CommitPhase.Rerun, 1, UnitStatus.Done, "", "p", sha))
    assert(LedgerRecord.validCode("rank_deficient:3"))
    assert(!LedgerRecord.validCode("0.0512 > 0.05 with spaces"))
  }

  test("sealed names (F2): only the root check and the stamp are deterministic; every unit and aggregate name carries the run id") {
    val u = WorkUnit(cell("C-1"), 3, arm("a1"))
    assertEquals(SealedNames.data(u, "r1"), "data/C-1/d0003/a1/r1")
    val unique = Vector(SealedNames.data(u, "r1"), SealedNames.ledger(u, "r1"), SealedNames.timing(u, "r1"),
      SealedNames.rerunData(u, "r1"), SealedNames.rerunLedger(u, "r1"), SealedNames.rerunTiming(u, "r1"),
      SealedNames.aggregateDiagnostics("r1"), SealedNames.aggregateRecord("r1"))
    assert(unique.forall(_.contains("r1")), unique.toString)
    val all = SealedNames.RootCheck +: SealedNames.Stamp +: unique
    assertEquals(all.distinct.length, all.length)
    assert(!all.exists(a => all.exists(b => a != b && b.startsWith(a + "/"))), "no name is a directory prefix of another")
  }

  test("stamp json is canonical and detects the first difference") {
    val a = PilotStamp(Vector("git_sha" -> "abc", "java_version" -> "21"))
    assertEquals(PilotStamp.parse(a.json), Right(a))
    assertEquals(a.firstDifference(PilotStamp(Vector("git_sha" -> "abc", "java_version" -> "17"))), Some("java_version"))
    assertEquals(a.firstDifference(a), None)
  }
