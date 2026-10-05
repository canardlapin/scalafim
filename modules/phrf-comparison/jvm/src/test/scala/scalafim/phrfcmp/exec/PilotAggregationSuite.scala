package scalafim.phrfcmp.exec

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.util.concurrent.atomic.AtomicInteger

import scalafim.phrfcmp.score.{PilotAggregator, ScoreSynth}

class PilotAggregationSuite extends munit.FunSuite:
  private val owner = TestOwner.random()
  private def store() = SealedStore.open(Files.createTempDirectory("phrf-s7-agg-").resolve("sealed"), owner.recipient, owner.fingerprint).toOption.get
  private def cell(s: String): CellId = CellId.parse(s).fold(sys.error, identity)
  private def arm(s: String): ArmId = ArmId.parse(s).fold(sys.error, identity)

  private def inputs(runId: String): Vector[ScorerInput] =
    Vector(
      ScorerInput(WorkUnit(cell("C1"), 0, arm("a0")), runId, CommitPhase.Scheduled, "aa" * 32),
      ScorerInput(WorkUnit(cell("C0"), 1, arm("a0")), runId, CommitPhase.Rerun, "bb" * 32)
    )

  test("the aggregator runs in-process; its sealed diagnostics and record are appended to the sealed store and nowhere else") {
    val corpus = ScoreSynth.corpus(d = 15)
    val s = store()
    val out = PilotAggregation.aggregateAndSeal(corpus, s, "agg1", inputs("agg1")).fold(e => fail(e.message), identity)
    assertEquals(out.whitelistSha256, PilotAggregator.aggregateGuarded(corpus).toOption.get.whitelistSha256)
    s.close().fold(e => fail(e.message), identity)
    val items = OwnerReader.readAll(s.dir, owner.priv).fold(e => fail(e), identity)
    assertEquals(items.keySet, Set(PilotAggregation.diagnosticsName("agg1"), PilotAggregation.recordName("agg1")))
    assert(items(PilotAggregation.diagnosticsName("agg1")).startsWith("PHRFCMP-S8-SEALED-1".getBytes("US-ASCII")))
    val record = ujson.read(new String(items(PilotAggregation.recordName("agg1")), UTF_8))
    assertEquals(record.obj.keySet.toSet, Set("run_id", "datasets", "whitelist_sha256", "corpus_outcomes_sha256", "corpus_timing_sha256", "units"))
    assertEquals(record("whitelist_sha256").str, out.whitelistSha256)
    assertEquals(record("datasets").num.toInt, 15)
    assertEquals(record("corpus_outcomes_sha256").str, CorpusDigest.outcomes(corpus))
    // F4: one entry per consumed unit, sorted by (cell, dataset, arm)
    assertEquals(
      record("units").arr.map(u => (u("cell").str, u("dataset").num.toInt, u("arm").str, u("run_id").str, u("phase").str, u("payload_sha256").str)).toVector,
      Vector(("C0", 1, "a0", "agg1", "rerun", "bb" * 32), ("C1", 0, "a0", "agg1", "scheduled", "aa" * 32))
    )
    // nothing but ciphertext in the store directory
    Fs.listFiles(s.dir).foreach(p => assert(!new String(Files.readAllBytes(p), "ISO-8859-1").contains("PHRFCMP-S8"), p.toString))
    assert(out.runComplete.startsWith("run complete; total CPU hours:"))
  }

  test("the aggregate takes the runner's run id: scorer inputs of another invocation are refused") {
    intercept[IllegalArgumentException](PilotAggregation.aggregateAndSeal(ScoreSynth.corpus(d = 15), store(), "agg1", inputs("other")))
  }

  test("a closed store refuses the append with a typed seal error") {
    val s = store()
    s.close()
    PilotAggregation.aggregateAndSeal(ScoreSynth.corpus(d = 15), s, "agg1", inputs("agg1")) match
      case Left(AggregationRefusal.Seal(SealError.StoreClosed)) => ()
      case other => fail(s"expected a seal refusal, got ${other.left.map(_.message)}")
  }

  test("once per run id: a second aggregation with the same run id is refused before anything is sealed, even with another corpus") {
    val s = store()
    PilotAggregation.aggregateAndSeal(ScoreSynth.corpus(d = 15), s, "same", inputs("same")).fold(e => fail(e.message), identity)
    val blobs = SealedStore.listBlobs(s.dir.resolve("blobs")).length
    assertEquals(PilotAggregation.aggregateAndSeal(ScoreSynth.corpus(d = 16), s, "same", inputs("same")).left.toOption, Some(AggregationRefusal.AlreadyAggregated("same")))
    assertEquals(SealedStore.listBlobs(s.dir.resolve("blobs")).length, blobs)
    s.close()
    assert(OwnerReader.readAll(s.dir, owner.priv).isRight)
    // another store may aggregate under the same run id string (the claim is per store)
    assert(PilotAggregation.aggregateAndSeal(ScoreSynth.corpus(d = 15), store(), "same", inputs("same")).isRight)
  }

  test("corpus digests: the outcome digest depends on the corpus content and not on the run; timing is digested separately") {
    val a = ScoreSynth.corpus(d = 15)
    assertEquals(CorpusDigest.outcomes(a), CorpusDigest.outcomes(ScoreSynth.corpus(d = 15)))
    assertNotEquals(CorpusDigest.outcomes(a), CorpusDigest.outcomes(ScoreSynth.corpus(d = 16)))
    assertNotEquals(CorpusDigest.outcomes(a), CorpusDigest.timing(a))
  }

  test("R5 (M1): aggregating a partial pilot and later the full one in the same store leaves a store the owner reads") {
    val st = store()
    PilotAggregation.aggregateAndSeal(ScoreSynth.corpus(d = 15), st, "aggA", inputs("aggA")).fold(e => fail(e.message), identity)
    PilotAggregation.aggregateAndSeal(ScoreSynth.corpus(d = 16), st, "aggB", inputs("aggB")).fold(e => fail(e.message), identity)
    val r = OwnerReader.readAll(st.dir, owner.priv, allowPartial = true)
    assert(r.isRight, s"owner reader refused the store: ${r.left.toOption}")
    assertEquals(r.toOption.get.keys.count(_.endsWith("/record")), 2)
  }

  /** The owner check of format spec section 10: units whose consumed commit differs from the first completed attempt. */
  private def deviations(items: Map[String, Array[Byte]], runId: String): Vector[String] =
    val ledgers = items.toVector.filter(_._1.startsWith("ledger/")).map((_, d) => LedgerRecord.parse(new String(d, UTF_8)).fold(e => fail(e), identity))
    val first = ledgers.groupBy(_.unit).view.mapValues(_.minBy(r => (r.invocation, r.runId))).toMap
    val record = ujson.read(new String(items(PilotAggregation.recordName(runId)), UTF_8))
    record("units").arr.toVector.flatMap { u =>
      val unit = WorkUnit(cell(u("cell").str), u("dataset").num.toInt, arm(u("arm").str))
      Option.when(!first.get(unit).exists(_.payloadSha256 == u("payload_sha256").str))(s"${unit.cell.value}/${unit.dataset}/${unit.arm.value}")
    }

  test("F4: end to end, the aggregate record localises a nondeterministic unit; a deterministic pilot has no deviation") {
    def pilot(nondeterministic: Boolean): (Map[String, Array[Byte]], String) =
      val out = Files.createTempDirectory("phrf-s7-f4-")
      val st = SealedStore.open(out.resolve("sealed"), owner.recipient, owner.fingerprint).fold(e => fail(e.message), identity)
      val plan = PilotPlan(Vector(PilotCell(cell("C0"), Vector(arm("a0"), arm("a1")))), 3)
      val flips = new AtomicInteger(0)
      val arms: ArmRunner = ctx =>
        val tag = if nondeterministic && ctx.unit.dataset == 1 && ctx.unit.arm.value == "a1" then flips.incrementAndGet() else 0
        ctx.emit("o", s"${ctx.unit}-$tag".getBytes(UTF_8))
        ArmResult.Done()
      val crash: CommitHook = (s, j) => if s == CommitStage.MarkerWritten && j.dataset == 2 then throw new SimulatedCrash
      val clk = new FakeClock
      intercept[SimulatedCrash](new PilotRunner(plan, out, PilotStamp(Vector("k" -> "v")), st, new PilotRoot(1L), arms, CpuGuard(1000.0, 2000.0), clk, hook = crash).run())
      val r2 = new PilotRunner(plan, out, PilotStamp(Vector("k" -> "v")), st, new PilotRoot(1L), arms, CpuGuard(1000.0, 2000.0), clk)
      val rep = r2.run().fold(x => fail(x.message), identity)
      assertEquals(rep.scorerInputs.length, 6)
      assert(rep.scorerInputs.exists(_.phase == CommitPhase.Rerun) && rep.scorerInputs.exists(_.phase == CommitPhase.Scheduled))
      PilotAggregation.aggregateAndSeal(ScoreSynth.corpus(d = 15), st, rep.runId, rep.scorerInputs).fold(e => fail(e.message), identity)
      (OwnerReader.readAll(st.dir, owner.priv, allowPartial = true).fold(e => fail(e), identity), rep.runId)
    val (clean, cleanId) = pilot(nondeterministic = false)
    assertEquals(deviations(clean, cleanId), Vector.empty[String])
    val (dirty, dirtyId) = pilot(nondeterministic = true)
    assertEquals(deviations(dirty, dirtyId), Vector("C0/1/a1"), "exactly the nondeterministic unit, recomputed in the aggregating invocation")
  }
