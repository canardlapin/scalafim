package scalafim.phrfcmp.exec

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

final class SimulatedCrash extends RuntimeException("simulated crash")

final class FakeClock extends CpuClock:
  private val ns = new AtomicLong(0L)
  def advance(seconds: Double): Unit = { val _ = ns.addAndGet((seconds * 1e9).toLong) }
  def processCpuSeconds(): Double = ns.get() / 1e9

class PilotRunnerSuite extends munit.FunSuite:
  // the F11 determinism test seals ~1300 blobs three times (20-36 s on a loaded host); munit's default is 30 s
  override val munitTimeout = scala.concurrent.duration.Duration(3, "min")

  private def cell(s: String): CellId = CellId.parse(s).fold(sys.error, identity)
  private def arm(s: String): ArmId = ArmId.parse(s).fold(sys.error, identity)

  private def mkPlan(cells: Int, datasets: Int, arms: Int, min: Int = 2): PilotPlan =
    PilotPlan(
      (0 until cells).toVector.map(i => PilotCell(cell(s"C$i"), (0 until arms).toVector.map(a => arm(s"a$a")))),
      datasets,
      minDatasets = min
    )

  private val stamp = PilotStamp(Vector("git_sha" -> "abc", "java_version" -> "21"))
  private val owner = TestOwner.random()
  private val root = new PilotRoot(0x0123456789abcdefL)
  private val Planted = "PLANTED-0.123456"
  private val PlantedDouble = java.nio.ByteBuffer.allocate(8).putDouble(0.123456).array()

  private def tmp(): Path = Files.createTempDirectory("phrf-s7-")
  private def blobBytes(u: WorkUnit): Array[Byte] = s"${u.cell.value}|${u.dataset}|${u.arm.value}".getBytes(UTF_8)
  private def clock(): FakeClock = new FakeClock

  private def openStore(out: Path, entropy: SealEntropy = SealEntropy.secure, hook: StoreHook = StoreHook.none, key: TestOwner = owner): SealedStore =
    SealedStore.open(out.resolve("sealed"), key.recipient, key.fingerprint, entropy, hook).fold(e => fail(e.message), identity)

  private class Counting(clock: FakeClock, cost: Double, behaviour: WorkUnit => Int => ArmResult, payload: WorkUnit => Array[Byte] = blobBytes) extends ArmRunner:
    val calls = new AtomicInteger(0)
    def run(ctx: ArmContext): ArmResult =
      calls.incrementAndGet()
      clock.advance(cost)
      val r = behaviour(ctx.unit)(ctx.attempt)
      ctx.emit("out.bin", payload(ctx.unit))
      r

  private val ok: WorkUnit => Int => ArmResult = _ => _ => ArmResult.Done()
  private val roomy: CpuGuard = CpuGuard()

  private def runner(plan: PilotPlan, out: Path, arms: ArmRunner, clk: CpuClock, guard: CpuGuard = roomy, threads: Int = 1,
      hook: CommitHook = CommitHook.none, st: PilotStamp = stamp, store: Option[SealedStore] = None, rt: PilotRoot = root,
      runId: String = PilotRunner.freshRunId(), wall: () => Double = () => System.nanoTime() / 1e9): PilotRunner =
    new PilotRunner(plan, out, st, store.getOrElse(openStore(out)), rt, arms, guard, clk, threads, hook, _ => (), () => runId, wall)

  private def read(out: Path, partial: Boolean = true): Map[String, Array[Byte]] =
    OwnerReader.readAll(out.resolve("sealed"), owner.priv, allowPartial = partial).fold(e => fail(e), identity)

  /** What the owner scores, applied as format spec section 10 says: per unit, the scheduled ledger record with the
    * smallest invocation (ties: smallest run id) and the SHA-256 of the payload it names; plus the root check.
    * Independent of run ids, invocations, timings, keys and the output path.
    */
  private def logicalDigest(out: Path): String =
    val items = read(out)
    val ledgers = items.toVector.filter(_._1.startsWith("ledger/")).map((_, d) => LedgerRecord.parse(new String(d, UTF_8)).fold(e => fail(e), identity))
    val scored = ledgers.groupBy(_.unit).toVector.map { (u, rs) =>
      val first = rs.minBy(r => (r.invocation, r.runId))
      assertEquals(Fs.sha256(items(first.payload)), first.payloadSha256)
      s"${unitPath(u)} ${first.status.code} ${first.payloadSha256}"
    }.sorted
    Fs.sha256((Fs.sha256(items(SealedNames.RootCheck)) +: scored).mkString("\n").getBytes(UTF_8))

  private def unitPath(u: WorkUnit): String = s"${u.cell.value}/${SealedNames.dataset(u.dataset)}/${u.arm.value}"

  /** Every scheduled ledger record of a unit, in invocation order. */
  private def ledgersOf(items: Map[String, Array[Byte]], u: WorkUnit): Vector[LedgerRecord] =
    items.toVector.filter(_._1.startsWith(s"ledger/${unitPath(u)}/"))
      .map((_, d) => LedgerRecord.parse(new String(d, UTF_8)).fold(e => fail(e), identity)).sortBy(_.invocation)

  private def ledgerOf(out: Path, u: WorkUnit): LedgerRecord =
    ledgersOf(read(out), u) match
      case Vector(one) => one
      case other => fail(s"expected one ledger record for $u, got ${other.length}")

  private def cost(out: Path): ujson.Value = ujson.read(Files.readString(out.resolve("cost.json")))

  /** Plaintext files the runner may leave next to the sealed store: stamp, CPU total, D, journal markers (dispatch
    * and completion).
    */
  private def assertOnlyAllowedPlaintext(out: Path): Unit =
    Fs.listFiles(out).foreach { p =>
      val rel = out.relativize(p).toString
      val allowed = rel == "stamp.json" || rel == "cost.json" || rel == "selection.json" ||
        rel == "output-id" ||
        (rel.startsWith("progress/") && Vector(".done", ".done.sha256", ".dispatched", ".dispatched.sha256").exists(rel.endsWith)) ||
        (rel.startsWith("sealed/blobs/") && (rel.endsWith(".enc") || rel.endsWith(".enc.tmp"))) || rel == "sealed/SEALED"
      assert(allowed, s"unexpected file $rel")
    }
    assert(!Files.exists(out.resolve("staging")), "no plaintext scratch area exists")

  test("full run: every unit sealed (payload, status-only ledger, timing), journal complete, no plaintext results on disk") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(3, 4, 2)
    val ex = new Counting(clk, 1.0, ok, u => (Planted + blobBytes(u).length).getBytes(UTF_8) ++ PlantedDouble)
    val rep = runner(plan, out, ex, clk).run().fold(r => fail(r.message), identity)
    assertEquals(ex.calls.get(), 3 * 4 * 2)
    assert(rep.outcome.isInstanceOf[PilotOutcome.Complete])
    assertEquals((rep.outcome.decision.D, rep.invocation, rep.rerunJobs), (4, 1, 0))
    val items = read(out)
    assertEquals(items.keys.count(_.startsWith("data/")), 24)
    assertEquals(items.keys.count(_.startsWith("ledger/")), 24)
    assertEquals(items.keys.count(_.startsWith("timing/")), 24)
    assert(items.contains(SealedNames.RootCheck) && items.contains(SealedNames.Stamp))
    items.filter(_._1.startsWith("ledger/")).values.foreach(d => assertEquals(LedgerRecord.parse(new String(d, UTF_8)).toOption.get.status, UnitStatus.Done))
    val u = WorkUnit(cell("C1"), 2, arm("a1"))
    val (status, entries) = UnitPayload.decode(items(SealedNames.data(u, rep.runId))).fold(e => fail(e), identity)
    assertEquals((status, entries.map(_._1)), (UnitStatus.Done, Vector("out.bin")))
    assertEquals(ledgerOf(out, u).payloadSha256, Fs.sha256(items(SealedNames.data(u, rep.runId))))
    assertEquals(items.keys.filterNot(n => n.contains(rep.runId)).toSet, Set(SealedNames.RootCheck, SealedNames.Stamp), "F2: only meta names are deterministic")
    assertEquals(rep.scorerInputs.length, 24)
    assert(rep.scorerInputs.forall(i => i.phase == CommitPhase.Scheduled && i.runId == rep.runId))
    assertEquals(rep.scorerInputs.find(_.unit == u).map(_.payloadSha256), Some(ledgerOf(out, u).payloadSha256))
    assertOnlyAllowedPlaintext(out)
    assertEquals(Fs.listFiles(out.resolve("progress")).count(_.toString.endsWith(".done")), 12)
    Fs.listFiles(out).foreach { p =>
      val b = new String(Files.readAllBytes(p), "ISO-8859-1")
      assert(!b.contains(Planted), s"planted text in ${out.relativize(p)}")
      assert(!b.contains(new String(PlantedDouble, "ISO-8859-1")), s"planted double in ${out.relativize(p)}")
    }
  }

  test("plaintext journal and cost file reveal only completed jobs, total CPU and the invocation count; no status, code, count or hash") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(2, 3, 2)
    val behaviour: WorkUnit => Int => ArmResult = u => _ => if u.arm.value == "a1" then ArmResult.Refused("rank_deficient_secretcode") else ArmResult.Done()
    runner(plan, out, new Counting(clk, 2.0, behaviour), clk).run().fold(r => fail(r.message), identity)
    val plain = Fs.listFiles(out).filterNot(_.toString.contains("/sealed/")).map(p => out.relativize(p).toString -> Files.readString(p))
    plain.foreach((n, t) => assert(!t.contains("rank_deficient") && !t.contains("refused"), n))
    assertEquals(plain.find(_._1 == "cost.json").map(p => ujson.read(p._2).obj.keySet.toSet), Some(Set("cpu_seconds_total", "invocations")))
    assertEquals(plain.find(_._1 == "selection.json").map(p => ujson.read(p._2).obj.keySet.toSet), Some(Set("D", "df", "ucl_factor")))
    val out2 = tmp(); val c2 = clock()
    runner(plan, out2, new Counting(c2, 2.0, ok), c2).run().fold(r => fail(r.message), identity)
    def journal(o: Path) = Fs.listFiles(o.resolve("progress")).map(p => o.resolve("progress").relativize(p).toString -> Files.readString(p))
    assertEquals(journal(out), journal(out2), "the journal is identical whatever the arms returned")
  }

  test("closeStore writes CLOSE and SEALED; the owner reader accepts the complete store") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(2, 3, 1)
    val r = runner(plan, out, new Counting(clk, 1.0, ok), clk)
    r.run().fold(x => fail(x.message), identity)
    val receipt = r.closeStore().fold(x => fail(x.message), identity)
    assertEquals(receipt.sealedTreeDigest, SealedStore.digest(out.resolve("sealed")))
    assert(OwnerReader.readAll(out.resolve("sealed"), owner.priv, allowPartial = false).isRight)
    assertOnlyAllowedPlaintext(out)
  }

  test("resume (D1): soft stop, then the final invocation runs the incomplete jobs and recomputes the earlier ones once; CPU is cumulative") {
    val full = tmp(); val c0 = clock()
    runner(mkPlan(2, 6, 1), full, new Counting(c0, 10.0, ok), c0).run().fold(r => fail(r.message), identity)
    val out = tmp(); val plan = mkPlan(2, 6, 1)
    val c1 = clock(); val e1 = new Counting(c1, 10.0, ok)
    val soft = CpuGuard(45.0 / 3600, 60.0)
    val strict = plan.copy(minDatasets = 3)
    val first = runner(strict, out, e1, c1, soft).run()
    assertEquals(first.left.toOption.map(_.isInstanceOf[PilotRefusal.TooFewDatasets]), Some(true))
    val done1 = e1.calls.get()
    assert(done1 < 12)
    assert(!read(out).keys.exists(_.startsWith("rerun/")), "a refused invocation recomputes nothing")
    val cost1 = cost(out)("cpu_seconds_total").num
    val c2 = clock(); val e2 = new Counting(c2, 10.0, ok)
    val r2 = runner(strict, out, e2, c2, roomy)
    val rep = r2.run().fold(r => fail(r.message), identity)
    assertEquals(e2.calls.get(), (12 - done1) + done1, "incomplete jobs once, earlier jobs recomputed once")
    assertEquals((rep.outcome.decision.D, rep.invocation, rep.rerunJobs), (6, 2, done1))
    assertEqualsDouble(cost(out)("cpu_seconds_total").num, cost1 + 10.0 * 12, 1e-6)
    assertEqualsDouble(rep.totalCpuSeconds, cost1 + 10.0 * 12, 1e-6)
    assertEquals(cost(out)("invocations").num.toInt, 2)
    val items = read(out)
    assertEquals(items.keys.count(_.startsWith(s"rerun/${r2.runId}/")), 3 * done1, "payload, ledger and timing per recomputed unit")
    items.filter(_._1.startsWith(s"rerun/${r2.runId}/ledger/")).values.foreach { d =>
      val rec = LedgerRecord.parse(new String(d, UTF_8)).toOption.get
      assertEquals((rec.phase, rec.invocation), (CommitPhase.Rerun, 2))
    }
    assertEquals(logicalDigest(out), logicalDigest(full))
  }

  test("D1: intermediate resumes recompute nothing; only the invocation that reaches a decision recomputes the earlier jobs") {
    val out = tmp(); val plan = mkPlan(1, 4, 1, min = 4)
    val clk = clock()
    val e1 = new Counting(clk, 10.0, ok)
    val r1 = runner(plan, out, e1, clk, CpuGuard(15.0 / 3600, 60.0))
    assert(r1.run().left.toOption.exists(_.isInstanceOf[PilotRefusal.TooFewDatasets]))
    val e2 = new Counting(clk, 10.0, ok)
    val r2 = runner(plan, out, e2, clk, CpuGuard(25.0 / 3600, 60.0))
    assert(r2.run().left.toOption.exists(_.isInstanceOf[PilotRefusal.TooFewDatasets]))
    assertEquals((e1.calls.get(), e2.calls.get()), (2, 1), "soft-stopped runs: new jobs only, no recomputation")
    val e3 = new Counting(clk, 10.0, ok)
    val r3 = runner(plan, out, e3, clk, roomy)
    val rep = r3.run().fold(r => fail(r.message), identity)
    assertEquals((rep.rerunJobs, e3.calls.get(), rep.invocation), (3, 4, 3))
    val items = read(out)
    assert(!items.keys.exists(n => n.startsWith(s"rerun/${r1.runId}/") || n.startsWith(s"rerun/${r2.runId}/")))
    val rerunUnits = items.keys.filter(_.startsWith(s"rerun/${r3.runId}/data/")).toVector.sorted
    assertEquals(rerunUnits, Vector(0, 1, 2).map(d => SealedNames.rerunData(WorkUnit(cell("C0"), d, arm("a0")), r3.runId)))
    // the scored (first) attempt of every dataset is its one Scheduled ledger record
    (0 until 4).foreach(d => assertEquals(ledgersOf(items, WorkUnit(cell("C0"), d, arm("a0"))).map(_.phase), Vector(CommitPhase.Scheduled)))
  }

  test("soft stop with D >= minimum accepts a uniform reverse-index drop; plaintext selection holds D, df, UCL only") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(2, 6, 1, min = 2)
    val rep = runner(plan, out, new Counting(clk, 10.0, ok), clk, CpuGuard(45.0 / 3600, 60.0)).run().fold(r => fail(r.message), identity)
    assert(rep.outcome.isInstanceOf[PilotOutcome.Partial])
    val d = rep.outcome.decision
    assertEquals(d.D, 2)
    assertEquals(d.dropped(cell("C0")), Vector(2))
    assertEquals(d.dropped(cell("C1")), Vector.empty[Int])
    val sel = ujson.read(Files.readString(out.resolve("selection.json")))
    assertEquals(sel("D").num.toInt, 2)
    assertEquals(sel("df").num.toInt, 1)
    assertEquals(sel.obj.keySet.toSet, Set("D", "df", "ucl_factor"))
  }

  test("partial pilot below the minimum D is refused") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(2, 20, 1, min = 15)
    val r = runner(plan, out, new Counting(clk, 10.0, ok), clk, CpuGuard(45.0 / 3600, 60.0)).run()
    assertEquals(r.left.toOption.map(_.isInstanceOf[PilotRefusal.TooFewDatasets]), Some(true))
  }

  test("stamp mismatch refuses resume naming the field before any work; ceilings and threads are not stamped") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    runner(plan, out, new Counting(clk, 1.0, ok), clk).run().fold(r => fail(r.message), identity)
    val other = PilotStamp(Vector("git_sha" -> "abc", "java_version" -> "17"))
    val ex = new Counting(clk, 1.0, ok)
    val refused = runner(plan, out, ex, clk, st = other).run()
    assertEquals(refused.left.toOption.map(_.message.contains("java_version")), Some(true))
    assertEquals(refused.left.toOption.map(_.isInstanceOf[PilotRefusal.StampMismatch]), Some(true))
    assertEquals(ex.calls.get(), 0)
    val again = runner(plan, out, ex, clk, guard = CpuGuard(5.0, 9.0), threads = 3).run()
    assertEquals(again.toOption.map(_.rerunJobs), Some(2), "a completed pilot resumed under other ceilings recomputes its kept jobs for the scorer")
    assertEquals(ex.calls.get(), 2)
  }

  test("L2/F7: the stamp binds the owner key fingerprint and the store path (hashed in plaintext, full in meta/stamp); another key or store refuses") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    val r = runner(plan, out, new Counting(clk, 1.0, ok), clk)
    r.run().fold(x => fail(x.message), identity)
    val onDisk = Files.readString(out.resolve("stamp.json"))
    val fields = PilotStamp.parse(onDisk).fold(e => fail(e), identity).fields.toMap
    val path = out.resolve("sealed").toAbsolutePath.normalize.toString
    assertEquals(fields("recipient_fp"), owner.fingerprint)
    assertEquals(fields("sealed_store_path_sha256"), Fs.sha256(path.getBytes(UTF_8)))
    assert(!fields.contains("sealed_store_path") && !onDisk.contains(path), "the plaintext stamp never names the custodian's directory")
    val sealedStamp = PilotStamp.parse(new String(read(out)(SealedNames.Stamp), UTF_8)).fold(e => fail(e), identity)
    assertEquals(sealedStamp.fields, PilotStamp.parse(onDisk).toOption.get.fields :+ ("sealed_store_path" -> path))
    val otherKey = TestOwner.random()
    val byKey = runner(plan, out, new Counting(clk, 1.0, ok), clk, store = Some(openStore(out, key = otherKey))).run()
    assertEquals(byKey.left.toOption, Some(PilotRefusal.StampMismatch("recipient_fp")))
    val elsewhere = SealedStore.open(out.resolve("sealed-2"), owner.recipient, owner.fingerprint).fold(e => fail(e.message), identity)
    val byPath = runner(plan, out, new Counting(clk, 1.0, ok), clk, store = Some(elsewhere)).run()
    assertEquals(byPath.left.toOption, Some(PilotRefusal.StampMismatch("sealed_store_path_sha256")))
    intercept[IllegalArgumentException](runner(plan, tmp(), new Counting(clk, 1.0, ok), clk, st = PilotStamp(Vector("recipient_fp" -> "x"))))
  }

  test("output with sealed data but no stamp is refused") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    runner(plan, out, new Counting(clk, 1.0, ok), clk).run().fold(r => fail(r.message), identity)
    Files.delete(out.resolve("stamp.json"))
    assert(runner(plan, out, new Counting(clk, 1.0, ok), clk).run().isLeft)
  }

  for stage <- CommitStage.values do
    test(s"atomicity: crash at $stage leaves only orphans; resume cleans them; owner content matches a clean run") {
      val clean = tmp(); val cc = clock(); val plan = mkPlan(2, 3, 2)
      runner(plan, clean, new Counting(cc, 1.0, ok), cc).run().fold(r => fail(r.message), identity)
      val out = tmp(); val clk = clock()
      val hook: CommitHook = (s, j) => if s == stage && j.dataset == 1 then throw new SimulatedCrash
      intercept[SimulatedCrash](runner(plan, out, new Counting(clk, 1.0, ok), clk, hook = hook).run())
      val markers = Fs.listFiles(out.resolve("progress")).map(_.toString)
      stage match
        case CommitStage.MarkerWritten =>
          assert(markers.exists(_.endsWith("d0001.done")) && !markers.exists(_.endsWith("d0001.done.sha256")), "marker written, sha missing")
        case CommitStage.DataSealed | CommitStage.LedgerSealed =>
          assert(!markers.exists(_.contains("d0001.done")), "no completion marker before the job's blobs are sealed")
          assert(markers.exists(_.endsWith("d0001.dispatched.sha256")), "the dispatch marker precedes the job's first seal")
        case CommitStage.MarkerSealed => assert(markers.exists(_.endsWith("d0001.done.sha256")))
      val ex = new Counting(clk, 1.0, ok)
      val rep = runner(plan, out, ex, clk).run().fold(r => fail(r.message), identity)
      assertEquals(rep.outcome.decision.D, 3)
      assert(Fs.listFiles(out.resolve("progress")).forall(p => !p.toString.endsWith(".tmp")))
      assert(Fs.listFiles(out.resolve("progress")).filter(_.toString.endsWith(".done")).forall(p => Files.exists(Fs.shaPath(p))))
      assertEquals(logicalDigest(out), logicalDigest(clean))
      assertOnlyAllowedPlaintext(out)
    }

  test("crash inside the sealed store between tmp write and rename: tmp debris is ciphertext, resume completes, close removes it") {
    val clean = tmp(); val cc = clock(); val plan = mkPlan(2, 3, 2)
    runner(plan, clean, new Counting(cc, 1.0, ok), cc).run().fold(r => fail(r.message), identity)
    val out = tmp(); val clk = clock()
    val n = new AtomicInteger(0)
    val storeHook = new StoreHook:
      override def at(s: StoreStage, name: String): Unit =
        if s == StoreStage.TmpWritten && name.startsWith("data/C1/d0001") && n.incrementAndGet() == 1 then throw new SimulatedCrash
    intercept[SimulatedCrash](runner(plan, out, new Counting(clk, 1.0, ok), clk, store = Some(openStore(out, hook = storeHook))).run())
    assertEquals(SealedStore.listTmp(out.resolve("sealed/blobs")).length, 1)
    val r2 = runner(plan, out, new Counting(clk, 1.0, ok), clk)
    r2.run().fold(r => fail(r.message), identity)
    r2.closeStore().fold(r => fail(r.message), identity)
    assertEquals(SealedStore.listTmp(out.resolve("sealed/blobs")), Vector.empty[Path])
    assertEquals(logicalDigest(out), logicalDigest(clean))
    assert(OwnerReader.readAll(out.resolve("sealed"), owner.priv).isRight)
  }

  test("a journal marker that disagrees with its sha refuses (corruption is not an orphan)") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    runner(plan, out, new Counting(clk, 1.0, ok), clk).run().fold(r => fail(r.message), identity)
    val marker = Fs.listFiles(out.resolve("progress")).find(_.toString.endsWith(".done")).get
    Files.writeString(marker, "tampered\n")
    val r = runner(plan, out, new Counting(clk, 1.0, ok), clk).run()
    assertEquals(r.left.toOption.map(_.isInstanceOf[PilotRefusal.LedgerCorrupt]), Some(true))
  }

  for threads <- Seq(1, 4) do
    test(s"CPU guard hard stop at a tiny ceiling (threads=$threads): refuses, nothing dispatched after the trip, resumable with an owner-authorized ceiling") {
      val out = tmp(); val clk = clock(); val plan = mkPlan(2, 4, 2)
      val tiny = CpuGuard(1.0 / 3600, 5.0 / 3600)
      val ex = new Counting(clk, 10.0, ok)
      runner(plan, out, ex, clk, tiny, threads).run() match
        case Left(PilotRefusal.CpuCeilingReached(cpu, ceiling)) =>
          assert(cpu >= 10.0)
          assertEqualsDouble(ceiling, 5.0 / 3600, 1e-12)
        case other => fail(s"expected CpuCeilingReached, got $other")
      assert(ex.calls.get() <= threads)
      assertOnlyAllowedPlaintext(out)
      assert(cost(out)("cpu_seconds_total").num >= 10.0)
      val again = new Counting(clk, 10.0, ok)
      assert(runner(plan, out, again, clk, tiny, threads).run().isLeft)
      assertEquals(again.calls.get(), 0)
      val fin = new Counting(clk, 10.0, ok)
      // resuming past a reached ceiling needs the owner's typed authorization; a plain number above 60 is refused
      intercept[IllegalArgumentException](CpuGuard(45.0, 61.0))
      // the raise authorizes exactly the third invocation (two were refused at the ceiling) and is recorded
      val outputId = runner(plan, out, fin, clk).outputIdentity.getOrElse(fail("the first invocation created the output identity"))
      assertEquals(outputId, Fs.sha256(Files.readAllBytes(out.resolve("stamp.json")) ++ Files.readAllBytes(out.resolve("output-id"))),
        "the owner can compute it as cat stamp.json output-id | sha256sum")
      val raise = OwnerCeilingRaise.of(61.0, "owner-bb", "test: resume after a forced low-ceiling stop", outputId, 3).fold(e => fail(e), identity)
      val lines = new java.util.concurrent.ConcurrentLinkedQueue[String]()
      val r3 = new PilotRunner(plan, out, stamp, openStore(out), root, fin, CpuGuard.raised(45.0, raise), clk, threads, log = lines.add(_): Unit)
      val rep = r3.run().fold(x => fail(x.message), identity)
      val logged = lines.toArray.map(_.toString).find(_.startsWith("PILOT_CEILING_RAISE")).getOrElse(fail("no raise log line"))
      assert(logged.contains(s"run_id=${r3.runId}") && logged.contains("forced low-ceiling stop") && logged.contains("approver=owner-bb"), logged)
      val recorded = cost(out)("ceiling_raises").arr
      assertEquals(recorded.map(r => (r("invocation").num.toInt, r("run_id").str, r("approver").str)).toVector, Vector((3, r3.runId, "owner-bb")))
      assert(read(out).contains(SealedNames.ceilingRaise(r3.runId)), "the raise is sealed with the run")
      // the same authorization cannot be used by a later invocation
      val reuse = runner(plan, out, new Counting(clk, 10.0, ok), clk, CpuGuard.raised(45.0, raise), threads).run()
      assertEquals(reuse.left.toOption.map(_.isInstanceOf[PilotRefusal.CeilingNotAuthorized]), Some(true))
      assertEquals(rep.outcome.decision.D, 4)
    }

  test("projection after the probe wave refuses when it exceeds the ceiling") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(2, 10, 1)
    val ex = new Counting(clk, 100.0, ok)
    runner(plan, out, ex, clk, CpuGuard(0.15, 0.2)).run() match
      case Left(PilotRefusal.ProjectionExceedsCeiling(p, c)) =>
        assertEqualsDouble(p, 2000.0 / 3600, 1e-9)
        assertEqualsDouble(c, 0.2, 1e-12)
      case other => fail(s"expected ProjectionExceedsCeiling, got $other")
    assertEquals(ex.calls.get(), 4)
  }

  test("dispatch is round-robin across cells in index order (single thread)") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(3, 3, 1)
    val seen = scala.collection.mutable.ArrayBuffer.empty[(String, Int)]
    val ex: ArmRunner = ctx => { seen += ((ctx.unit.cell.value, ctx.unit.dataset)); ArmResult.Done() }
    runner(plan, out, ex, clk).run().fold(x => fail(x.message), identity)
    assertEquals(seen.toVector, for d <- (0 until 3).toVector; c <- Vector("C0", "C1", "C2") yield (c, d))
  }

  test("retries: failed twice then done; always-failing capped at 2 retries; refusal not retried; one ledger and one timing record per unit") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 3)
    val behaviour: WorkUnit => Int => ArmResult = u => attempt =>
      u.arm.value match
        case "a0" => if attempt < 3 then ArmResult.Failed("flaky") else ArmResult.Done()
        case "a1" => ArmResult.Failed("always")
        case _ => ArmResult.Refused("rank_deficient")
    val ex = new Counting(clk, 1.0, behaviour)
    val r = runner(plan, out, ex, clk)
    r.run().fold(x => fail(x.message), identity)
    assertEquals(ex.calls.get(), 2 * (3 + 3 + 1))
    def u(d: Int, a: String) = WorkUnit(cell("C0"), d, arm(a))
    assertEquals((ledgerOf(out, u(0, "a0")).status, ledgerOf(out, u(0, "a0")).attempts), (UnitStatus.Done, 3))
    assertEquals((ledgerOf(out, u(0, "a1")).status, ledgerOf(out, u(0, "a1")).attempts), (UnitStatus.Failed, 3))
    assertEquals(ledgerOf(out, u(0, "a2")).status, UnitStatus.Refused)
    assertEquals(ledgerOf(out, u(1, "a1")).code, "always")
    assertEquals(ledgerOf(out, u(1, "a1")).payload, SealedNames.data(u(1, "a1"), r.runId))
    val items = read(out)
    val timing = ujson.read(new String(items(SealedNames.timing(u(0, "a0"), r.runId)), UTF_8))
    assertEquals(timing("attempts").arr.map(_("status").str).toVector, Vector("retried", "retried", "done"))
    for d <- 0 until 2; a <- Vector("a0", "a1", "a2") do
      assertEquals(items.keys.count(_.contains(unitPath(u(d, a)))), 3, s"fixed blob set for ${u(d, a)}")
    assertEquals(UnitPayload.decode(items(SealedNames.data(u(0, "a2"), r.runId))).map(_._1), Right(UnitStatus.Refused))
    assertOnlyAllowedPlaintext(out)
  }

  test("M6: the sealed blob count does not depend on refusals, failures or retries") {
    val plan = mkPlan(2, 3, 3)
    def blobs(behaviour: WorkUnit => Int => ArmResult): Int =
      val out = tmp(); val clk = clock()
      val ex: ArmRunner = ctx =>
        val r = behaviour(ctx.unit)(ctx.attempt)
        if r == ArmResult.Done() then ctx.emit("out.bin", blobBytes(ctx.unit)) // refusals and failures emit nothing
        r
      runner(plan, out, ex, clk).run().fold(x => fail(x.message), identity)
      SealedStore.listBlobs(out.resolve("sealed/blobs")).length
    val mixedBehaviour: WorkUnit => Int => ArmResult = u =>
      attempt =>
        (u.arm.value, u.dataset) match
          case ("a0", _) => ArmResult.Refused("rank")
          case ("a1", 0) => if attempt < 3 then ArmResult.Failed("flaky") else ArmResult.Done()
          case ("a1", _) => ArmResult.Failed("hard")
          case _ => ArmResult.Done()
    val allDone = blobs(ok)
    val mixed = blobs(mixedBehaviour)
    assertEquals(mixed, allDone)
    assertEquals(allDone, 2 + 2 * 3 * 3 * 3, "root check, stamp, then payload, ledger and timing per unit")
  }

  test("an arm exception is a failure, not a crash") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    val ex: ArmRunner = _ => throw new IllegalStateException("kaboom")
    runner(plan, out, ex, clk).run().fold(x => fail(x.message), identity)
    assertEquals(ledgerOf(out, WorkUnit(cell("C0"), 0, arm("a0"))).status, UnitStatus.Failed)
  }

  test("unit records carry attempt-unique names; a resumed job adds new names, never a duplicate; payload hashes reveal (non)determinism") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    def steppedWall(step: Double): () => Double =
      var wall = 0.0
      () => { wall += step; wall }
    val crashHook: CommitHook = (s, j) => if s == CommitStage.MarkerWritten && j.dataset == 1 then throw new SimulatedCrash
    intercept[SimulatedCrash](runner(plan, out, new Counting(clk, 1.0, ok), clk, runId = "run0", wall = steppedWall(1.5), hook = crashHook).run())
    runner(plan, out, new Counting(clk, 1.0, ok), clk, runId = "run1", wall = steppedWall(2.5)).run().fold(x => fail(x.message), identity)
    val items = read(out) // refuses on any differing duplicate
    val u = WorkUnit(cell("C0"), 1, arm("a0"))
    assertEquals(ledgersOf(items, u).map(r => (r.invocation, r.runId, r.phase)), Vector((1, "run0", CommitPhase.Scheduled), (2, "run1", CommitPhase.Scheduled)))
    assertEquals(ledgersOf(items, u).map(_.payload), Vector(SealedNames.data(u, "run0"), SealedNames.data(u, "run1")))
    assertEquals(ledgersOf(items, u).map(_.payloadSha256).distinct, Vector(Fs.sha256(items(SealedNames.data(u, "run0")))))
    assertNotEquals(items(SealedNames.timing(u, "run0")).toSeq, items(SealedNames.timing(u, "run1")).toSeq)
    assert(items.contains(SealedNames.rerunData(WorkUnit(cell("C0"), 0, arm("a0")), "run1")), "dataset 0 was recomputed for the scorer")
  }

  test("F2: a nondeterministic Done payload regenerated after a crash never poisons the store; the owner sees the mismatch for that unit only") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    val flip = new AtomicInteger(0)
    val arms: ArmRunner = ctx => { ctx.emit("o", s"${ctx.unit.dataset}-${if ctx.unit.dataset == 1 then flip.incrementAndGet() else 0}".getBytes(UTF_8)); ArmResult.Done() }
    val crashHook: CommitHook = (s, j) => if s == CommitStage.MarkerWritten && j.dataset == 1 then throw new SimulatedCrash
    intercept[SimulatedCrash](runner(plan, out, arms, clk, runId = "first", hook = crashHook).run())
    val rep = runner(plan, out, arms, clk, runId = "second").run().fold(x => fail(x.message), identity)
    val items = OwnerReader.readAll(out.resolve("sealed"), owner.priv, allowPartial = true).fold(e => fail(s"the store must stay readable: $e"), identity)
    val u1 = WorkUnit(cell("C0"), 1, arm("a0"))
    val shas = ledgersOf(items, u1).map(_.payloadSha256)
    assertEquals(shas.length, 2)
    assertNotEquals(shas(0), shas(1), "the owner detects the nondeterminism of this unit by comparing payload hashes")
    // the scorer consumed the second invocation's commit; the owner scores the first: the record lets it name the unit
    assertEquals(rep.scorerInputs.find(_.unit == u1).map(_.payloadSha256), Some(shas(1)))
    assertEquals(ledgersOf(items, WorkUnit(cell("C0"), 0, arm("a0"))).map(_.payloadSha256).distinct.length, 1)
  }

  test("resuming under a different root is visible to the owner as a differing duplicate; the root never reaches disk") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    runner(plan, out, new Counting(clk, 1.0, ok), clk).run().fold(x => fail(x.message), identity)
    Files.delete(out.resolve("progress/C0/d0000.done"))
    runner(plan, out, new Counting(clk, 1.0, ok), clk, rt = new PilotRoot(42L)).run().fold(x => fail(x.message), identity)
    assertEquals(OwnerReader.readAll(out.resolve("sealed"), owner.priv, allowPartial = true).left.toOption, Some("logical name sealed twice with different plaintext"))
    val ackHex = Fs.sha256(s"${root.value}\n".getBytes(UTF_8))
    Fs.listFiles(out).filterNot(_.toString.contains("/blobs/")).foreach { p =>
      val t = Files.readString(p, java.nio.charset.StandardCharsets.ISO_8859_1)
      assert(!t.contains(root.value.toString) && !t.contains(ackHex), s"root material in ${out.relativize(p)}")
    }
  }

  test("a refused seal write surfaces as SealFailed and the unit is not marked complete") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    val bad = new StoreHook:
      override def corruptBlob(b: Array[Byte]): Array[Byte] = { val c = b.clone(); c(100) = (c(100) ^ 1).toByte; c }
    val r = runner(plan, out, new Counting(clk, 1.0, ok), clk, store = Some(openStore(out, hook = bad))).run()
    assertEquals(r.left.toOption.map(_.isInstanceOf[PilotRefusal.SealFailed]), Some(true))
    assert(!Files.exists(out.resolve("progress")) || Fs.listFiles(out.resolve("progress")).isEmpty)
  }

  test("whole-runner determinism (F11): identical decrypted content and identical sealed-tree digests at 1 and 4 threads") {
    val plan = mkPlan(5, 8, 3)
    def behaviour(u: WorkUnit): Int => ArmResult = attempt =>
      val h = math.abs((u.cell.value + u.dataset + u.arm.value).hashCode % 7)
      if h == 0 then ArmResult.Refused("rank")
      else if h == 1 then (if attempt < 2 then ArmResult.Failed("flaky") else ArmResult.Done(0.5))
      else if h == 2 then ArmResult.Failed("hard")
      else ArmResult.Done(0.5)
    val base = tmp()
    def go(threads: Int): (String, String) =
      val out = base.resolve("run") // one path for every run: the store path is part of the sealed stamp
      Fs.deleteTree(out)
      val clk = clock()
      val ex: ArmRunner = ctx =>
        Thread.sleep(math.abs((ctx.unit.cell.value + ctx.unit.dataset).hashCode % 4).toLong + (if threads > 1 then util.Random.nextInt(3) else 0))
        clk.advance(0.01)
        ctx.emit("out.bin", blobBytes(ctx.unit))
        behaviour(ctx.unit)(ctx.attempt)
      val r = runner(plan, out, ex, clk, threads = threads, store = Some(openStore(out, entropy = new DerivedEntropy("seed"))), runId = "fixed", wall = () => 0.0)
      r.run().fold(x => fail(x.message), identity)
      r.closeStore().fold(x => fail(x.message), identity)
      assertOnlyAllowedPlaintext(out)
      val all = Fs.sha256(read(out).toVector.sortBy(_._1).map((n, d) => s"$n ${Fs.sha256(d)}").mkString("\n").getBytes(UTF_8))
      (all, SealedStore.digest(out.resolve("sealed")))
    val a = go(1); val b = go(4); val c = go(4)
    assertEquals(b, a)
    assertEquals(c, a)
  }

  test("ArmContext: unsafe or duplicate blob names are refused; one scorer contribution per attempt; RawBlob and PilotRoot never print their contents") {
    val ctx = new ArmContext(WorkUnit(cell("C0"), 0, arm("a0")), 1, root, () => false)
    intercept[IllegalArgumentException](ctx.emit("../x", Array[Byte](1)))
    ctx.emit("ok", Array[Byte](1))
    intercept[IllegalArgumentException](ctx.emit("ok", Array[Byte](2)))
    intercept[IllegalArgumentException](ctx.emit(ScorerContribution.EntryName, Array[Byte](3)))
    ctx.contribute(Array[Byte](4))
    intercept[IllegalArgumentException](ctx.contribute(Array[Byte](5)))
    assertEquals(ctx.emitted.map(_.name), Vector("ok", ScorerContribution.EntryName))
    assertEquals(ctx.emitted.head.toString, "RawBlob(redacted)")
    assert(!root.toString.contains("0123"))
  }

  // ---- review findings (reproducers R1-R4 of the S7 review; R5 is in PilotAggregationSuite) ----

  test("R1 (H1): a transient failure, a crash before the marker and a rerun with a different attempt count leave a store the owner reads") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    val run1: ArmRunner = ctx =>
      ctx.emit("out.bin", s"${ctx.unit.dataset}".getBytes(UTF_8))
      if ctx.unit.dataset == 1 && ctx.attempt == 1 then ArmResult.Failed("transient") else ArmResult.Done()
    val crash: CommitHook = (s, j) => if s == CommitStage.LedgerSealed && j.dataset == 1 then throw new SimulatedCrash
    intercept[SimulatedCrash](runner(plan, out, run1, clk, hook = crash).run())
    val run2: ArmRunner = ctx =>
      ctx.emit("out.bin", s"${ctx.unit.dataset}".getBytes(UTF_8))
      ArmResult.Done()
    runner(plan, out, run2, clk).run().fold(r => fail(r.message), identity)
    val r = OwnerReader.readAll(out.resolve("sealed"), owner.priv, allowPartial = true)
    assert(r.isRight, s"owner reader refused the store: ${r.left.toOption}")
    val attempts = ledgersOf(r.toOption.get, WorkUnit(cell("C0"), 1, arm("a0"))).map(rec => (rec.invocation, rec.attempts))
    assertEquals(attempts, Vector((1, 2), (2, 1)), "both commits are kept; the owner scores invocation 1")
  }

  test("R2 (D2): a resume under an exceeded soft ceiling finishes the hole first; every cell keeps the prefix 0..D-1") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(2, 4, 1)
    val okArm: ArmRunner = ctx => { clk.advance(10.0); ctx.emit("o", Array[Byte](1)); ArmResult.Done() }
    runner(plan, out, okArm, clk).run().fold(r => fail(r.message), identity)
    Files.delete(out.resolve("progress/C0/d0001.done.sha256"))
    Files.delete(out.resolve("progress/C0/d0001.done"))
    val rep = runner(plan, out, okArm, clock(), CpuGuard(30.0 / 3600, 60.0)).run().fold(r => fail(r.message), identity)
    val d = rep.outcome.decision
    assertEquals(d.D, 4)
    assertEquals(d.kept(cell("C0")), (0 until d.D).toVector)
    assertEquals(d.kept(cell("C1")), (0 until d.D).toVector)
  }

  test("D2: past the soft stop a resume finishes only the jobs below the highest completed index, then drops to the shortest prefix") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(2, 6, 1)
    val okArm = new Counting(clk, 10.0, ok)
    runner(plan, out, okArm, clk).run().fold(r => fail(r.message), identity)
    // a crash abandoned (C1, 2) and (C0, 4); everything at index 5 and below completed otherwise
    Vector("progress/C1/d0002.done", "progress/C1/d0002.done.sha256", "progress/C0/d0004.done", "progress/C0/d0004.done.sha256").foreach(p => Files.delete(out.resolve(p)))
    val seen = java.util.concurrent.ConcurrentHashMap.newKeySet[(String, Int)]()
    val arms: ArmRunner = ctx => { seen.add((ctx.unit.cell.value, ctx.unit.dataset)); ArmResult.Done() }
    val rep = runner(plan, out, arms, clock(), CpuGuard(1.0 / 3600, 60.0)).run().fold(r => fail(r.message), identity)
    assertEquals(rep.outcome.decision.D, 6)
    assert(seen.contains(("C1", 2)) && seen.contains(("C0", 4)))
  }

  test("R3 (M3): a missing or unreadable cost.json next to existing progress refuses to resume instead of resetting the CPU guard") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 4, 1)
    val calls = new AtomicInteger(0)
    val okArm: ArmRunner = _ => { calls.incrementAndGet(); clk.advance(10.0); ArmResult.Done() }
    val g = CpuGuard(15.0 / 3600, 15.0 / 3600)
    assert(runner(plan, out, okArm, clk, g).run().left.toOption.exists(_.isInstanceOf[PilotRefusal.CpuCeilingReached]))
    val before = calls.get()
    val saved = Files.readString(out.resolve("cost.json"))
    Files.delete(out.resolve("cost.json"))
    val again = runner(plan, out, okArm, clk, g).run()
    assert(again.isLeft, "resume without cost.json must refuse")
    assertEquals(calls.get(), before, "no arm may run after the guard state was lost")
    assertEquals(again.left.toOption.map(_.isInstanceOf[PilotRefusal.CostStateLost]), Some(true))
    Files.writeString(out.resolve("cost.json"), "{\"cpu_seconds_total\": \"NaN\"")
    assertEquals(runner(plan, out, okArm, clk, g).run().left.toOption.map(_.isInstanceOf[PilotRefusal.CostStateLost]), Some(true))
    Files.writeString(out.resolve("cost.json"), saved)
    assertEquals(runner(plan, out, okArm, clk, g).run().left.toOption.map(_.isInstanceOf[PilotRefusal.CpuCeilingReached]), Some(true))
    assertEquals(calls.get(), before)
  }

  test("R4 (M4): after one worker crashes, run() returns only when every worker has stopped; nothing is written afterwards") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    val slowFinished = new java.util.concurrent.atomic.AtomicBoolean(false)
    val arms: ArmRunner = ctx =>
      if ctx.unit.dataset == 1 then
        val t0 = System.nanoTime()
        var spins = 0L
        while System.nanoTime() - t0 < 1500000000L do spins += 1
        assert(spins >= 0L)
        clk.advance(123.0)
        slowFinished.set(true)
      ArmResult.Done()
    val crash: CommitHook = (s, j) => if s == CommitStage.DataSealed && j.dataset == 0 then { Thread.sleep(100L); throw new SimulatedCrash }
    intercept[SimulatedCrash](runner(plan, out, arms, clk, threads = 2, hook = crash).run())
    assert(slowFinished.get(), "run() returned while the other worker's arm was still running")
    def snapshot(): Option[String] = Option.when(Files.exists(out.resolve("cost.json")))(Files.readString(out.resolve("cost.json")))
    val atReturn = snapshot()
    Thread.sleep(3000L)
    assertEquals(snapshot(), atReturn, "a worker wrote cost.json after run() returned")
  }

  test("M4: a commit in flight when another worker crashes is abandoned at its next seal; nothing of it is sealed after the crash") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    val bSealedPayload = new CountDownLatch(1)
    val aCrashed = new CountDownLatch(1)
    val hook: CommitHook = (s, j) =>
      if s == CommitStage.DataSealed && j.dataset == 0 then
        assert(bSealedPayload.await(10, TimeUnit.SECONDS))
        aCrashed.countDown()
        throw new SimulatedCrash
      else if s == CommitStage.DataSealed && j.dataset == 1 then
        bSealedPayload.countDown()
        assert(aCrashed.await(10, TimeUnit.SECONDS))
        Thread.sleep(300L) // let worker A's crash reach the crash flag
    val arms: ArmRunner = ctx => { ctx.emit("o", blobBytes(ctx.unit)); ArmResult.Done() }
    intercept[SimulatedCrash](runner(plan, out, arms, clk, threads = 2, hook = hook, runId = "m4run").run())
    val items = read(out)
    val b = WorkUnit(cell("C0"), 1, arm("a0"))
    assert(items.contains(SealedNames.data(b, "m4run")), "B sealed its payload before the crash")
    assertEquals(ledgersOf(items, b), Vector.empty[LedgerRecord], "B's ledger record was sealed after A crashed")
    assert(!items.keys.exists(_.startsWith(s"timing/${unitPath(b)}/")))
    assert(!Files.exists(out.resolve("progress/C0/d0001.done")))
  }

  test("L4: an arm that returns Failed because shouldAbort fired is not committed; the unit stays incomplete and resumes cleanly") {
    val out = tmp(); val clk = clock()
    val plan = mkPlan(1, 2, 1).copy(maxRetries = 0) // a Failed result would otherwise commit at once
    val aborted = new AtomicInteger(0)
    val arms: ArmRunner = ctx =>
      ctx.emit("o", blobBytes(ctx.unit))
      if ctx.unit.dataset == 0 then { clk.advance(5.0); ArmResult.Done() }
      else
        var r: ArmResult = ArmResult.Done()
        var go = true
        while go do
          clk.advance(5.0)
          if ctx.shouldAbort then
            aborted.incrementAndGet()
            r = ArmResult.Failed("aborted_by_guard")
            go = false
        r
    val g = CpuGuard(15.0 / 3600, 15.0 / 3600)
    runner(plan, out, arms, clk, g).run() match
      case Left(PilotRefusal.CpuCeilingReached(cpu, _)) => assert(cpu >= 15.0)
      case other => fail(s"expected CpuCeilingReached, got $other")
    assertEquals(aborted.get(), 1)
    val items = read(out)
    val u1 = WorkUnit(cell("C0"), 1, arm("a0"))
    assert(!items.keys.exists(_.contains(unitPath(u1))), s"nothing of the aborted unit is sealed: ${items.keys.filter(_.contains(unitPath(u1)))}")
    assert(!Files.exists(out.resolve("progress/C0/d0001.done")))
    val fin = runner(plan, out, new Counting(clk, 1.0, ok), clk, roomy).run().fold(x => fail(x.message), identity)
    assertEquals(fin.outcome.decision.D, 2)
    assertEquals(ledgerOf(out, u1).status, UnitStatus.Done)
  }

  test("L3: the hard ceiling is checked around every unit attempt; the attempt that crosses it is discarded and no later unit starts") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 3)
    val ex = new Counting(clk, 10.0, ok)
    runner(plan, out, ex, clk, CpuGuard(25.0 / 3600, 25.0 / 3600)).run() match
      case Left(_: PilotRefusal.CpuCeilingReached) => ()
      case other => fail(s"expected CpuCeilingReached, got $other")
    assertEquals(ex.calls.get(), 3, "units a0, a1 ran (20 s); a2 ran into the ceiling and was discarded; nothing after")
    val items = read(out)
    assertEquals(items.keys.count(_.startsWith("ledger/")), 2)
  }

  test("closeStore refuses while a run is in progress, and run refuses while a close is in progress") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val arms: ArmRunner = _ => { entered.countDown(); assert(release.await(10, TimeUnit.SECONDS)); ArmResult.Done() }
    val r = runner(plan, out, arms, clk)
    val t = new Thread(() => { val _ = r.run() })
    t.start()
    assert(entered.await(10, TimeUnit.SECONDS))
    assertEquals(r.closeStore().left.toOption.map(_.isInstanceOf[PilotRefusal.Failure]), Some(true))
    release.countDown()
    t.join(10000L)
    assert(r.closeStore().isRight)
  }

  // ---- rev 3 re-review (F1, F5) ----

  test("F1: run() is single-shot; after a caught crash a second run() on the same instance is refused and the store stays readable") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    val crashes = new AtomicInteger(0)
    val crashOnce: CommitHook = (s, j) => if s == CommitStage.LedgerSealed && j.dataset == 1 && crashes.incrementAndGet() == 1 then throw new SimulatedCrash
    val arms: ArmRunner = ctx => { ctx.emit("o", blobBytes(ctx.unit)); ArmResult.Done() }
    val r = runner(plan, out, arms, clk, hook = crashOnce)
    intercept[SimulatedCrash](r.run())
    val second = r.run()
    assertEquals(second.left.toOption, Some(PilotRefusal.RunnerReused))
    assert(OwnerReader.readAll(out.resolve("sealed"), owner.priv, allowPartial = true).isRight, "the store must stay readable")
    // the resume is a new instance: a new run id and the next invocation
    val r2 = runner(plan, out, arms, clk)
    val rep = r2.run().fold(x => fail(x.message), identity)
    assertEquals(rep.invocation, 2)
    assertNotEquals(r2.runId, r.runId)
    assert(OwnerReader.readAll(out.resolve("sealed"), owner.priv, allowPartial = true).isRight)
  }

  test("F1: the run id is drawn inside run(), once per instance") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    val draws = new AtomicInteger(0)
    val r = new PilotRunner(plan, out, stamp, openStore(out), root, new Counting(clk, 1.0, ok), roomy, clk, runIds = () => s"rid${draws.incrementAndGet()}")
    intercept[IllegalStateException](r.runId)
    assertEquals(draws.get(), 0)
    val rep = r.run().fold(x => fail(x.message), identity)
    assertEquals((draws.get(), r.runId, rep.runId), (1, "rid1", "rid1"))
    assertEquals(r.run().left.toOption, Some(PilotRefusal.RunnerReused))
    assertEquals(draws.get(), 1)
  }

  test("F5: an interrupt of the run() thread joins every worker, restores the interrupt flag and returns Interrupted") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    val entered = new CountDownLatch(1)
    val armFinished = new java.util.concurrent.atomic.AtomicBoolean(false)
    val arms: ArmRunner = ctx =>
      entered.countDown()
      val t0 = System.nanoTime()
      var spins = 0L
      while System.nanoTime() - t0 < 700000000L do spins += 1 // not interruptible: the worker must be joined, not abandoned
      assert(spins >= 0L)
      armFinished.set(true)
      ctx.emit("o", blobBytes(ctx.unit))
      ArmResult.Done()
    val r = runner(plan, out, arms, clk)
    @volatile var result: Option[Either[PilotRefusal, PilotReport]] = None
    @volatile var finishedAtReturn = false
    @volatile var flagAfter = false
    val t = new Thread(() =>
      result = Some(r.run())
      finishedAtReturn = armFinished.get()
      flagAfter = Thread.currentThread().isInterrupted
    )
    t.start()
    assert(entered.await(10, TimeUnit.SECONDS))
    t.interrupt()
    t.join(20000L)
    assert(!t.isAlive)
    assertEquals(result.flatMap(_.left.toOption), Some(PilotRefusal.Interrupted))
    assert(finishedAtReturn, "run() returned before the in-flight worker finished")
    assert(flagAfter, "the interrupt flag is restored for the caller")
    assert(!Files.exists(out.resolve("progress/C0/d0001.done")), "nothing after the interrupt is marked complete")
  }

  // ---- independent probes of the 2026-10-03 blocker notes (H1, M2, M4), kept as regressions ----

  test("probe H1: successful bytes that change per invocation, crashed after LedgerSealed, resume into a store the owner reads; both scheduled commits survive") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    def arms(tag: String): ArmRunner = ctx => { ctx.emit("o", s"${ctx.unit.dataset}-$tag".getBytes(UTF_8)); ArmResult.Done() }
    val crash: CommitHook = (s, j) => if s == CommitStage.LedgerSealed && j.dataset == 1 then throw new SimulatedCrash
    intercept[SimulatedCrash](runner(plan, out, arms("inv1"), clk, hook = crash, runId = "h1first").run())
    val rep = runner(plan, out, arms("inv2"), clk, runId = "h1second").run().fold(x => fail(x.message), identity)
    val items = OwnerReader.readAll(out.resolve("sealed"), owner.priv, allowPartial = true).fold(e => fail(s"the store must stay readable: $e"), identity)
    val u1 = WorkUnit(cell("C0"), 1, arm("a0"))
    val recs = ledgersOf(items, u1)
    assertEquals(recs.map(r => (r.invocation, r.runId)), Vector((1, "h1first"), (2, "h1second")))
    assertNotEquals(recs(0).payloadSha256, recs(1).payloadSha256)
    assert(!items.contains(SealedNames.timing(u1, "h1first")), "the crash hit between ledger and timing; the scheduled record counts without it")
    assertEquals(Fs.sha256(items(recs(0).payload)), recs(0).payloadSha256)
    // the earlier completed dataset was recomputed with other bytes under rerun/: no differing duplicate either
    val u0 = WorkUnit(cell("C0"), 0, arm("a0"))
    assertNotEquals(Fs.sha256(items(SealedNames.rerunData(u0, "h1second"))), ledgersOf(items, u0).head.payloadSha256)
    assertEquals(rep.scorerInputs.map(_.unit).toSet, Set(u0, u1))
  }

  test("probe M2a: a job in flight at the highest completed index when the run crashed is finished on a resume past the soft stop") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(2, 4, 1)
    val crash: CommitHook = (s, j) => if s == CommitStage.DataSealed && j == Job(cell("C1"), 2) then throw new SimulatedCrash
    intercept[SimulatedCrash](runner(plan, out, new Counting(clk, 10.0, ok), clk, hook = crash).run())
    // C0 completed 0..2, C1 completed 0..1; (C1, 2) was dispatched and in flight
    val seen = java.util.concurrent.ConcurrentHashMap.newKeySet[(String, Int)]()
    val arms: ArmRunner = ctx => { seen.add((ctx.unit.cell.value, ctx.unit.dataset)); ArmResult.Done() }
    val rep = runner(plan, out, arms, clock(), CpuGuard(30.0 / 3600, 60.0)).run().fold(r => fail(r.message), identity)
    assert(seen.contains(("C1", 2)), "the in-flight job is finished before the soft stop is honoured")
    assert(!seen.contains(("C0", 3)) && !seen.contains(("C1", 3)), "no new dataset is dispatched past the soft stop")
    assertEquals(rep.outcome.decision.D, 3)
  }

  test("probe M2b: every job in flight when the run crashed is finished on a resume past the soft stop, even above the highest completed index") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(2, 4, 1)
    val c0Entered = new CountDownLatch(1)
    val arms1: ArmRunner = ctx =>
      clk.advance(10.0)
      ctx.unit match
        case WorkUnit(c, 2, _) if c.value == "C0" =>
          c0Entered.countDown()
          val t0 = System.nanoTime()
          while !ctx.shouldAbort && System.nanoTime() - t0 < 10000000000L do Thread.sleep(5L)
        case WorkUnit(c, 2, _) if c.value == "C1" => assert(c0Entered.await(10, TimeUnit.SECONDS))
        case _ => ()
      ArmResult.Done()
    val crash: CommitHook = (s, j) => if s == CommitStage.DataSealed && j == Job(cell("C1"), 2) then throw new SimulatedCrash
    intercept[SimulatedCrash](runner(plan, out, arms1, clk, threads = 2, hook = crash).run())
    assert(!Files.exists(out.resolve("progress/C0/d0002.done")) && !Files.exists(out.resolve("progress/C1/d0002.done")))
    val seen = java.util.concurrent.ConcurrentHashMap.newKeySet[(String, Int)]()
    val arms2: ArmRunner = ctx => { seen.add((ctx.unit.cell.value, ctx.unit.dataset)); ArmResult.Done() }
    val rep = runner(plan, out, arms2, clock(), CpuGuard(30.0 / 3600, 60.0)).run().fold(r => fail(r.message), identity)
    assert(seen.contains(("C0", 2)) && seen.contains(("C1", 2)), s"both in-flight jobs are finished: $seen")
    assert(!seen.contains(("C0", 3)) && !seen.contains(("C1", 3)), "no new dataset is dispatched past the soft stop")
    assertEquals(rep.outcome.decision.D, 3)
  }

  test("probe M4: repeated interrupts of the run() thread never return it while a latch-blocked worker is alive") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    val entered = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val armFinished = new java.util.concurrent.atomic.AtomicBoolean(false)
    @volatile var worker: Thread = null
    val arms: ArmRunner = ctx =>
      worker = Thread.currentThread()
      entered.countDown()
      var released = false
      while !released do
        try released = release.await(30, TimeUnit.SECONDS)
        catch case _: InterruptedException => () // a worker interrupt must not end the arm early either
      armFinished.set(true)
      ctx.emit("o", blobBytes(ctx.unit))
      ArmResult.Done()
    val r = runner(plan, out, arms, clk)
    @volatile var result: Option[Either[PilotRefusal, PilotReport]] = None
    @volatile var finishedAtReturn = false
    val t = new Thread(() =>
      result = Some(r.run())
      finishedAtReturn = armFinished.get()
    )
    t.start()
    assert(entered.await(10, TimeUnit.SECONDS))
    (1 to 3).foreach { _ =>
      t.interrupt()
      Thread.sleep(200L)
      assert(t.isAlive, "run() returned while its worker was still blocked")
    }
    release.countDown()
    t.join(20000L)
    assert(!t.isAlive)
    assertEquals(result.flatMap(_.left.toOption), Some(PilotRefusal.Interrupted))
    assert(finishedAtReturn, "run() returned before the blocked worker finished")
    worker.join(5000L)
    assert(!worker.isAlive, "the worker thread terminates once run() has returned")
    assert(!Files.exists(out.resolve("progress/C0/d0000.done")), "nothing after the interrupt is marked complete")
  }

  // ---- independent re-review 2026-10-10 (failure 2) ----

  test("review 2: a commit exception after the attempt's CPU was metered checkpoints that CPU durably before the crash propagates") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    val arms: ArmRunner = ctx => { clk.advance(10.0); ctx.emit("o", blobBytes(ctx.unit)); ArmResult.Done(7.0) }
    val crashed = new AtomicInteger(0)
    val hook: CommitHook = (s, _) => if s == CommitStage.DataSealed && crashed.incrementAndGet() == 1 then throw new SimulatedCrash
    intercept[SimulatedCrash](runner(plan, out, arms, clk, hook = hook).run())
    assertEqualsDouble(cost(out)("cpu_seconds_total").num, 17.0, 1e-9)
  }

  // ---- second independent re-review 2026-10-10 (failures 1, 2, 3, 5) ----

  test("re-review 1a: a failed cost checkpoint leaves accounting.open; the resume refuses until the owner charges that run") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 3, 1)
    val arms: ArmRunner = ctx =>
      clk.advance(10.0)
      if ctx.unit.dataset == 1 && !Files.isDirectory(out.resolve("cost.json")) then
        // the cost file becomes unwritable for the rest of the run: every checkpoint fails from here on
        Files.delete(out.resolve("cost.json"))
        Files.createDirectories(out.resolve("cost.json").resolve("blocker")): Unit
      ctx.emit("o", blobBytes(ctx.unit))
      ArmResult.Done()
    val r1 = runner(plan, out, arms, clk, runId = "uncertain1")
    intercept[java.io.IOException](r1.run())
    assertEquals(Files.readString(out.resolve("accounting.open")).trim, "uncertain1")
    Fs.deleteTree(out.resolve("cost.json"))
    Files.writeString(out.resolve("cost.json"), "{\"cpu_seconds_total\":0,\"invocations\":1}\n") // stale but parseable
    assertEquals(runner(plan, out, new Counting(clk, 1.0, ok), clk).run().left.toOption, Some(PilotRefusal.AccountingUncertain("uncertain1")))
    val wrong = OwnerAccountingRecovery.of("someother", 100.0, "owner-bb", "power loss").fold(e => fail(e), identity)
    val w = new PilotRunner(plan, out, stamp, openStore(out), root, new Counting(clk, 1.0, ok), roomy, clk, accountingRecovery = Some(wrong))
    assertEquals(w.run().left.toOption, Some(PilotRefusal.AccountingRecoveryMismatch("someother", "uncertain1")))
    val rec = OwnerAccountingRecovery.of("uncertain1", 100.0, "owner-bb", "cost checkpoint failed").fold(e => fail(e), identity)
    val r = new PilotRunner(plan, out, stamp, openStore(out), root, new Counting(clk, 1.0, ok), roomy, clk, accountingRecovery = Some(rec))
    val rep = r.run().fold(x => fail(x.message), identity)
    assert(rep.totalCpuSeconds >= 100.0, s"the charged CPU counts: ${rep.totalCpuSeconds}")
    assert(!Files.exists(out.resolve("accounting.open")))
    assertEquals(cost(out)("accounting_recoveries").arr.map(_("uncertain_run_id").str).toVector, Vector("uncertain1"))
    assert(read(out).contains(SealedNames.accountingRecovery("uncertain1")))
  }

  test("re-review 1b: a process death mid-run (accounting.open on disk) makes the resume refuse uncertain accounting") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 3, 1)
    val snap = tmp()
    val arms: ArmRunner = ctx =>
      clk.advance(10.0)
      if ctx.unit.dataset == 1 then
        // what the disk holds if the power fails now
        Files.copy(out.resolve("cost.json"), snap.resolve("cost.json"))
        Files.copy(out.resolve("accounting.open"), snap.resolve("accounting.open")): Unit
      ctx.emit("o", blobBytes(ctx.unit))
      ArmResult.Done()
    val r1 = runner(plan, out, arms, clk, runId = "died1")
    r1.run().fold(x => fail(x.message), identity)
    assert(!Files.exists(out.resolve("accounting.open")), "a clean exit closes the accounting")
    Files.copy(snap.resolve("cost.json"), out.resolve("cost.json"), java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    Files.copy(snap.resolve("accounting.open"), out.resolve("accounting.open"))
    assertEquals(runner(plan, out, new Counting(clk, 1.0, ok), clk).run().left.toOption, Some(PilotRefusal.AccountingUncertain("died1")))
  }

  test("re-review 5b: a cost.json whose invocation count would overflow is refused, never incremented") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    Files.createDirectories(out)
    Files.writeString(out.resolve("cost.json"), s"{\"cpu_seconds_total\":0,\"invocations\":${Int.MaxValue}}\n")
    runner(plan, out, new Counting(clk, 1.0, ok), clk).run() match
      case Left(_: PilotRefusal.CostStateLost) => ()
      case other => fail(s"expected CostStateLost, got $other")
    assertEquals(cost(out)("invocations").num, Int.MaxValue.toDouble)
  }

  test("re-review 3: CpuGuard and the owner authorizations are not Serializable, and reflective construction re-checks every invariant") {
    for c <- Vector(classOf[CpuGuard], classOf[OwnerCeilingRaise], classOf[OwnerAccountingRecovery]) do
      assert(!classOf[java.io.Serializable].isAssignableFrom(c), s"${c.getSimpleName} must not be Serializable")
    val ctor = classOf[OwnerCeilingRaise].getDeclaredConstructors.head
    ctor.setAccessible(true)
    val e = intercept[java.lang.reflect.InvocationTargetException](ctor.newInstance(Double.box(Double.PositiveInfinity), "", "", "", Int.box(1)))
    assert(e.getCause.isInstanceOf[IllegalArgumentException])
    val g = classOf[CpuGuard].getDeclaredConstructors.head
    g.setAccessible(true)
    val e2 = intercept[java.lang.reflect.InvocationTargetException](g.newInstance(Double.box(45.0), Double.box(61.0), None))
    assert(e2.getCause.isInstanceOf[IllegalArgumentException])
  }

  // ---- third independent review 2026-10-10 (H2, M1, L1, L2, L3, hardening) ----

  test("review3 H2: an owner ceiling raise is bound to one output: accepted for output A, refused for output B") {
    val plan = mkPlan(1, 2, 1); val clk = clock()
    val a = tmp(); val b = tmp()
    assert(runner(plan, a, new Counting(clk, 1.0, ok), clk).run().isRight)
    assert(runner(plan, b, new Counting(clk, 1.0, ok), clk).run().isRight)
    val idA = runner(plan, a, new Counting(clk, 1.0, ok), clk).outputIdentity.getOrElse(fail("no identity"))
    assertNotEquals(Some(idA), runner(plan, b, new Counting(clk, 1.0, ok), clk).outputIdentity)
    val raise = OwnerCeilingRaise.of(70.0, "owner-bb", "output A only", idA, 2).fold(e => fail(e), identity)
    assert(runner(plan, a, new Counting(clk, 1.0, ok), clk, CpuGuard.raised(45.0, raise)).run().isRight)
    runner(plan, b, new Counting(clk, 1.0, ok), clk, CpuGuard.raised(45.0, raise)).run() match
      case Left(PilotRefusal.CeilingNotAuthorized(d)) => assert(d.contains("another output"), d)
      case other => fail(s"expected CeilingNotAuthorized, got $other")
  }

  private def craftedCost(cpu: Double, invocations: Int, recovered: Option[String]): String =
    val rec = recovered.fold("")(id =>
      s""","accounting_recoveries":[{"invocation":$invocations,"uncertain_run_id":"$id","charged_cpu_seconds":$cpu,"approver":"owner-bb","reason":"power loss"}]""")
    s"""{"cpu_seconds_total":$cpu,"invocations":$invocations$rec}\n"""

  test("review3 M1: an accounting recovery already recorded for the uncertain run is not charged twice") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    runner(plan, out, new Counting(clk, 0.0, ok), clk).run().fold(x => fail(x.message), identity)
    // the state a death leaves between the recovery's cost write and the marker replacement
    Files.writeString(out.resolve("cost.json"), craftedCost(100.0, 2, Some("dead")))
    Files.writeString(out.resolve("accounting.open"), "dead\n")
    val rec = OwnerAccountingRecovery.of("dead", 100.0, "owner-bb", "power loss").fold(e => fail(e), identity)
    val r = new PilotRunner(plan, out, stamp, openStore(out), root, new Counting(clk, 0.0, ok), roomy, clk, accountingRecovery = Some(rec))
    r.run().fold(x => fail(x.message), identity)
    assertEqualsDouble(cost(out)("cpu_seconds_total").num, 100.0, 1e-9)
    assertEquals(cost(out)("accounting_recoveries").arr.length, 1)
    assert(!Files.exists(out.resolve("accounting.open")))
  }

  test("review3 L2: a recovery whose charge makes the total non-finite is refused; the marker and the cost stay") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    runner(plan, out, new Counting(clk, 0.0, ok), clk).run().fold(x => fail(x.message), identity)
    Files.writeString(out.resolve("cost.json"), craftedCost(1e308, 2, None))
    Files.writeString(out.resolve("accounting.open"), "dead\n")
    val before = Files.readString(out.resolve("cost.json"))
    val rec = OwnerAccountingRecovery.of("dead", 1e308, "owner-bb", "power loss").fold(e => fail(e), identity)
    val r = new PilotRunner(plan, out, stamp, openStore(out), root, new Counting(clk, 0.0, ok), roomy, clk, accountingRecovery = Some(rec))
    r.run() match
      case Left(_: PilotRefusal.AccountingRecoveryInvalid) => ()
      case other => fail(s"expected AccountingRecoveryInvalid, got $other")
    assertEquals(Files.readString(out.resolve("cost.json")), before)
    assertEquals(Files.readString(out.resolve("accounting.open")).trim, "dead")
  }

  test("review3 L1: the runner never writes an invocation count its parser rejects") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    Files.createDirectories(out)
    val last = s"{\"cpu_seconds_total\":0,\"invocations\":${Int.MaxValue - 1}}\n"
    Files.writeString(out.resolve("cost.json"), last)
    assertEquals(runner(plan, out, new Counting(clk, 1.0, ok), clk).run().left.toOption, Some(PilotRefusal.InvocationsExhausted(Int.MaxValue - 1)))
    assertEquals(Files.readString(out.resolve("cost.json")), last)
  }

  test("review3 hardening: cost.json authorization records are validated; L3: the retry cap is bounded") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    Files.createDirectories(out)
    val badRaise = """{"cpu_seconds_total":0,"invocations":1,"ceiling_raises":[{"invocation":0,"run_id":"../x","output_identity":"zz","hard_core_hours":1e6,"approver":"","reason":""}]}"""
    Files.writeString(out.resolve("cost.json"), badRaise + "\n")
    runner(plan, out, new Counting(clk, 1.0, ok), clk).run() match
      case Left(_: PilotRefusal.CostStateLost) => ()
      case other => fail(s"expected CostStateLost, got $other")
    intercept[IllegalArgumentException](mkPlan(1, 2, 1).copy(maxRetries = Int.MaxValue))
    assertEquals(mkPlan(1, 2, 1).copy(maxRetries = PilotPlan.MaxRetries).maxRetries, PilotPlan.MaxRetries)
  }

  // ---- fourth independent review 2026-10-10 (H2, M, L2) ----

  test("review4 H2: an output recreated at the same path has a new identity, so the old output's raise is refused") {
    val plan = mkPlan(1, 2, 1); val clk = clock()
    val base = tmp()
    val out = base.resolve("output")
    assert(runner(plan, out, new Counting(clk, 1.0, ok), clk).run().isRight)
    val id = runner(plan, out, new Counting(clk, 1.0, ok), clk).outputIdentity.getOrElse(fail("no identity"))
    val raise = OwnerCeilingRaise.of(70.0, "owner-bb", "the original output", id, 2).fold(e => fail(e), identity)
    // move the whole output away, then build a fresh one at the same path with the same key and stamp inputs
    Files.move(out, base.resolve("archive"))
    assert(runner(plan, out, new Counting(clk, 1.0, ok), clk).run().isRight)
    assertEquals(Files.readString(out.resolve("stamp.json")), Files.readString(base.resolve("archive/stamp.json")), "identical stamp bytes")
    runner(plan, out, new Counting(clk, 1.0, ok), clk, CpuGuard.raised(45.0, raise)).run() match
      case Left(PilotRefusal.CeilingNotAuthorized(d)) => assert(d.contains("another output"), d)
      case other => fail(s"expected CeilingNotAuthorized, got $other")
    // the original output (moved back) still accepts it
    Fs.deleteTree(out)
    Files.move(base.resolve("archive"), out)
    assert(runner(plan, out, new Counting(clk, 1.0, ok), clk, CpuGuard.raised(45.0, raise)).run().isRight)
  }

  test("review4 H2: an adopted output without a well-formed output-id is refused") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    assert(runner(plan, out, new Counting(clk, 1.0, ok), clk).run().isRight)
    Files.writeString(out.resolve("output-id"), "not-an-id\n")
    assert(runner(plan, out, new Counting(clk, 1.0, ok), clk).run().left.toOption.exists(_.isInstanceOf[PilotRefusal.Failure]))
  }

  test("review4 M: a replayed recovery completes its sealed accounting record") {
    val out = tmp(); val clk = clock(); val plan = mkPlan(1, 2, 1)
    runner(plan, out, new Counting(clk, 0.0, ok), clk).run().fold(x => fail(x.message), identity)
    Files.writeString(out.resolve("cost.json"), craftedCost(100.0, 2, Some("dead")))
    Files.writeString(out.resolve("accounting.open"), "dead\n")
    val rec = OwnerAccountingRecovery.of("dead", 100.0, "owner-bb", "power loss").fold(e => fail(e), identity)
    new PilotRunner(plan, out, stamp, openStore(out), root, new Counting(clk, 0.0, ok), roomy, clk, accountingRecovery = Some(rec))
      .run().fold(x => fail(x.message), identity)
    val sealedRecord = read(out).get(SealedNames.accountingRecovery("dead")).map(b => ujson.read(new String(b, UTF_8)))
    assertEquals(sealedRecord.map(r => (r("uncertain_run_id").str, r("invocation").num.toInt)), Some(("dead", 2)))
  }

  test("review4 L2: journal names and recovery agree for every dataset index a plan accepts") {
    intercept[IllegalArgumentException](mkPlan(1, SealedNames.MaxDatasets + 1, 1))
    val plan = mkPlan(1, SealedNames.MaxDatasets, 1)
    val out = tmp()
    val progress = new PilotProgress(out)
    val last = Job(cell("C0"), plan.datasets - 1)
    progress.markDispatched(last)
    assertEquals(progress.recover(Set(cell("C0"))).map(_.dispatched), Right(Set(last)))
  }
