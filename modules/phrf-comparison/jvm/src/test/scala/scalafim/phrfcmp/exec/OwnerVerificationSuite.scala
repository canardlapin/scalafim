package scalafim.phrfcmp.exec

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger

import scalafim.phrfcmp.score.ScoreSynth
import scalafim.phrfcmp.score.PilotCell as ScoreCell

/** The synthetic S10 seam: scorer contributions sealed in the committed payloads, the corpus assembled from them in
  * process, and the owner-side verification over the decrypted store. Synthetic data only: nothing here admits a
  * pilot or a campaign.
  */
class OwnerVerificationSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(3, "min")

  private val owner = TestOwner.random()
  private def cell(s: String): CellId = CellId.parse(s).fold(sys.error, identity)
  private def arm(s: String): ArmId = ArmId.parse(s).fold(sys.error, identity)
  private val c0 = cell("C0")
  private val a0 = arm("a0")
  private val plan = PilotPlan(Vector(PilotCell(c0, Vector(a0))), datasets = 15)
  private val stamp = PilotStamp(Vector("k" -> "v"))
  private val root = new PilotRoot(7L)
  private val roomy = CpuGuard(1000.0, 2000.0)
  private def unit(d: Int): WorkUnit = WorkUnit(c0, d, a0)

  private def enc(x: Double): Array[Byte] = ByteBuffer.allocate(8).putDouble(x).array()
  private def dec(b: Array[Byte]): Double = ByteBuffer.wrap(b).getDouble

  /** A synthetic S10 mapping: unit (C0, d, a0) contributes one double, the planted log-ratio of dataset d in every
    * condition cell; trial cells and coverage are synthetic and contribution-free. `shift` models an owner-side
    * mapping that disagrees with the runner's.
    */
  private final class Synthetic(val schema: String = "synthetic-v1", shift: Double = 0.0, refuse: Boolean = false) extends CorpusAssembler:
    def outcomes(datasets: Int, cs: Vector[UnitContribution]): Either[String, CorpusOutcomes] =
      val byDataset = cs.collect { case u if u.unit.cell == c0 => u.unit.dataset -> u.payload }.toMap
      if refuse || (0 until datasets).exists(d => !byDataset.get(d).flatten.exists(_.length == 8)) then Left("missing_contribution")
      else
        val planted = (0 until datasets).map(d => dec(byDataset(d).get) + shift)
        Right(
          CorpusOutcomes(
            ScoreCell.conditionCells.map(c => c -> Vector.tabulate(datasets)(d => ScoreSynth.conditionDataset(c, d, planted(d)))).toMap,
            ScoreCell.trialCells.map(c => c -> Vector.tabulate(datasets)(ScoreSynth.trialDataset(c, _))).toMap,
            Vector.tabulate(datasets)(ScoreSynth.coverageDataset(_, 50))
          )
        )

  private def contributing(value: (WorkUnit, Int) => Double, result: (WorkUnit, Int) => ArmResult = (_, _) => ArmResult.Done()): ArmRunner =
    ctx =>
      ctx.emit("o", s"${ctx.unit.dataset}".getBytes(UTF_8))
      ctx.contribute(enc(value(ctx.unit, ctx.attempt)))
      result(ctx.unit, ctx.attempt)

  private val steady: (WorkUnit, Int) => Double = (u, _) => 0.1 + 0.01 * u.dataset

  private final case class Pilot(out: Path, store: SealedStore, report: PilotReport)

  /** One pilot, optionally crashed once after the marker data of dataset `crashAt`, then resumed to the end. */
  private def pilot(arms: ArmRunner, crashAt: Option[Int] = Some(7)): Pilot =
    val out = Files.createTempDirectory("phrf-s10-owner-")
    val st = SealedStore.open(out.resolve("sealed"), owner.recipient, owner.fingerprint).fold(e => fail(e.message), identity)
    val clk = new FakeClock
    crashAt.foreach { d =>
      val crash: CommitHook = (s, j) => if s == CommitStage.MarkerWritten && j.dataset == d then throw new SimulatedCrash
      intercept[SimulatedCrash](new PilotRunner(plan, out, stamp, st, root, arms, roomy, clk, hook = crash).run())
    }
    val rep = new PilotRunner(plan, out, stamp, st, root, arms, roomy, clk).run().fold(x => fail(x.message), identity)
    Pilot(out, st, rep)

  private def items(p: Pilot): Map[String, Array[Byte]] =
    OwnerReader.readAll(p.store.dir, owner.priv, allowPartial = true).fold(e => fail(e), identity)

  private def aggregate(p: Pilot, assembler: CorpusAssembler = new Synthetic()): String =
    PilotAggregation.aggregateContributions(p.report, assembler, ScoreSynth.timing, p.store).fold(e => fail(e.message), identity).whitelistSha256

  test("a deterministic resumed pilot aggregated from its contributions verifies Valid; the record names the schema") {
    val p = pilot(contributing(steady))
    assert(p.report.scorerInputs.exists(_.phase == CommitPhase.Rerun), "the resume recomputed earlier jobs")
    val wl = aggregate(p)
    val all = items(p)
    val record = ujson.read(new String(all(SealedNames.aggregateRecord(p.report.runId)), UTF_8))
    assertEquals(record("contribution_schema").str, "synthetic-v1")
    assertEquals(OwnerVerification.verify(all, plan, wl, Vector(new Synthetic())), OwnerVerdict.Valid(p.report.runId, 15, 15, Vector.empty))
  }

  test("the in-process corpus is assembled from the committed contributions only: its outcome digest is the owner's rebuild") {
    val p = pilot(contributing(steady), crashAt = None)
    aggregate(p)
    val record = ujson.read(new String(items(p)(SealedNames.aggregateRecord(p.report.runId)), UTF_8))
    val rebuilt = new Synthetic().outcomes(15, UnitContribution.canonical(p.report.contributions)).fold(e => fail(e), identity)
    assertEquals(record("corpus_outcomes_sha256").str, CorpusDigest.outcomes(15, rebuilt))
    assertEquals(p.report.contributions.map(_.unit), p.report.scorerInputs.map(_.unit))
  }

  test("a contribution that differs between invocations invalidates the whitelist and names exactly that unit") {
    val flips = new AtomicInteger(0)
    val p = pilot(contributing((u, a) => if u.dataset == 3 then 0.5 + flips.incrementAndGet() else steady(u, a)))
    val wl = aggregate(p)
    OwnerVerification.verify(items(p), plan, wl, Vector(new Synthetic())) match
      case OwnerVerdict.Invalidated(rid, mismatched, digest, _) =>
        assertEquals(rid, p.report.runId)
        assertEquals(mismatched, Vector(unit(3)))
        assertEquals(digest, DigestCheck.Mismatched)
      case other => fail(s"expected Invalidated, got $other")
  }

  test("an owner-side mapping that disagrees with the sealed digest invalidates even when every unit matches") {
    val p = pilot(contributing(steady))
    val wl = aggregate(p)
    assertEquals(
      OwnerVerification.verify(items(p), plan, wl, Vector(new Synthetic(shift = 0.25))),
      OwnerVerdict.Invalidated(p.report.runId, Vector.empty, DigestCheck.Mismatched, Vector.empty)
    )
  }

  test("a legacy aggregate (no contribution schema) is Unverifiable, never Valid; so is an unknown schema or a failed rebuild") {
    val p = pilot(contributing(steady))
    val legacy = PilotAggregation.aggregateAndSeal(ScoreSynth.corpus(d = 15), p.store, p.report.runId, p.report.scorerInputs).fold(e => fail(e.message), identity)
    OwnerVerification.verify(items(p), plan, legacy.whitelistSha256, Vector(new Synthetic())) match
      case OwnerVerdict.Unverifiable(_, UnverifiableReason.LegacyAggregate, _) => ()
      case other => fail(s"expected Unverifiable(LegacyAggregate), got $other")
    val q = pilot(contributing(steady))
    val wl = aggregate(q)
    assertEquals(
      OwnerVerification.verify(items(q), plan, wl, Vector.empty),
      OwnerVerdict.Unverifiable(q.report.runId, UnverifiableReason.UnknownSchema("synthetic-v1"), Vector.empty)
    )
    assertEquals(
      OwnerVerification.verify(items(q), plan, wl, Vector(new Synthetic(refuse = true))),
      OwnerVerdict.Unverifiable(q.report.runId, UnverifiableReason.RebuildFailed("missing_contribution"), Vector.empty)
    )
  }

  test("a legacy store whose units do not match is still Invalidated: the primary check needs no contributions") {
    val flips = new AtomicInteger(0)
    val p = pilot(contributing((u, a) => if u.dataset == 2 then flips.incrementAndGet().toDouble else steady(u, a)))
    val legacy = PilotAggregation.aggregateAndSeal(ScoreSynth.corpus(d = 15), p.store, p.report.runId, p.report.scorerInputs).fold(e => fail(e.message), identity)
    OwnerVerification.verify(items(p), plan, legacy.whitelistSha256, Vector(new Synthetic())) match
      case OwnerVerdict.Invalidated(_, Vector(u), DigestCheck.NotChecked(UnverifiableReason.LegacyAggregate), _) => assertEquals(u, unit(2))
      case other => fail(s"expected Invalidated, got $other")
  }

  test("retries are transactional: a retried attempt's contribution is neither sealed nor consumed; a committed failure is the first attempt") {
    val arms = contributing(
      (u, a) => if u.dataset == 2 && a == 1 then 99.0 else steady(u, a),
      (u, a) =>
        if u.dataset == 2 && a == 1 then ArmResult.Failed("transient")
        else if u.dataset == 5 then ArmResult.Failed("always")
        else ArmResult.Done()
    )
    val p = pilot(arms, crashAt = None)
    val c2 = p.report.contributions.find(_.unit == unit(2)).flatMap(_.payload).map(dec)
    assertEquals(c2, Some(steady(unit(2), 2)))
    val all = items(p)
    val payload = all(SealedNames.data(unit(2), p.report.runId))
    val entries = UnitPayload.decode(payload).fold(e => fail(e), identity)._2
    assertEquals(entries.map(_._1), Vector("o", ScorerContribution.EntryName))
    assertEqualsDouble(dec(entries(1)._2), steady(unit(2), 2), 0.0)
    val l5 = LedgerRecord.parse(new String(all(SealedNames.ledger(unit(5), p.report.runId)), UTF_8)).fold(e => fail(e), identity)
    assertEquals((l5.status, l5.attempts), (UnitStatus.Failed, 3))
    val wl = aggregate(p)
    assertEquals(OwnerVerification.verify(items(p), plan, wl, Vector(new Synthetic())), OwnerVerdict.Valid(p.report.runId, 15, 15, Vector.empty))
  }

  test("several aggregates carrying the manifest's whitelist refuse as ambiguous; a whitelist no aggregate carries refuses") {
    val p = pilot(contributing(steady), crashAt = None)
    val wl = aggregate(p)
    val clk = new FakeClock
    val again = new PilotRunner(plan, p.out, stamp, p.store, root, contributing(steady), roomy, clk).run().fold(x => fail(x.message), identity)
    assertEquals(again.rerunJobs, 15)
    val wl2 = aggregate(Pilot(p.out, p.store, again))
    assertEquals(wl2, wl)
    assertEquals(
      OwnerVerification.verify(items(p), plan, wl, Vector(new Synthetic())),
      OwnerVerdict.Refused(VerificationRefusal.AmbiguousAggregate(Vector(p.report.runId, again.runId).sorted))
    )
    assertEquals(OwnerVerification.verify(items(p), plan, "00" * 32, Vector(new Synthetic())), OwnerVerdict.Refused(VerificationRefusal.NoAggregate))
  }

  test("a payload that does not match its ledger hash, or a unit list that is not the kept set, refuses") {
    val p = pilot(contributing(steady))
    val wl = aggregate(p)
    val all = items(p)
    val first = all.keys.find(_.startsWith(s"data/C0/d0000/a0/")).get
    assertEquals(
      OwnerVerification.verify(all.updated(first, "x".getBytes(UTF_8)), plan, wl, Vector(new Synthetic())),
      OwnerVerdict.Refused(VerificationRefusal.PayloadMismatch(first))
    )
    val q = pilot(contributing(steady), crashAt = None)
    val short = q.report.copy(scorerInputs = q.report.scorerInputs.filterNot(_.unit == unit(4)))
    val wlShort = aggregate(Pilot(q.out, q.store, short))
    OwnerVerification.verify(items(q), plan, wlShort, Vector(new Synthetic())) match
      case OwnerVerdict.Refused(VerificationRefusal.UnitListMismatch(_)) => ()
      case other => fail(s"expected UnitListMismatch, got $other")
  }
