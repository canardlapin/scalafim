package scalafim.phrfcmp.exec

import java.nio.file.{Files, Path}
import scalafim.phrfcmp.ingest.*
import scalafim.phrfcmp.prep.CommonPreparation
import scalafim.phrfcmp.run.*
import scalafim.phrfcmp.score.PilotCell as ScoreCell

/** A deliberately tiny ceiling around real condition fits on harness inputs. */
object RealCpuGuardRehearsal:
  private def ok[A](e: Either[?, A]): A = e.fold(x => throw new IllegalStateException(x.toString), identity)
  def main(args: Array[String]): Unit =
    require(args.length == 3, "RealCpuGuardRehearsal <harness-inputs> <new-output> <python>")
    val input = Path.of(args(0)); val out = Path.of(args(1))
    require(!Files.exists(out))
    Files.createDirectories(out)
    val inventory = ujson.read(Files.readString(input.resolve("rehearsal-inputs.json")))
    val harness = "7a3c91d50b44e2f1"
    require(inventory("root_kind").str == "harness" && inventory("root_hex").str == harness)
    val cell = ScoreCell.CTX5
    val plan = PilotPlan(Vector(PilotCell(ok(CellId.parse(cell.id)),
      PilotCorpusAssembler.arms(cell).map((a, _) => ok(ArmId.parse(a))))), 2, probeDatasets = 1, minDatasets = 2, maxRetries = 0)
    val loaded = (0 until 2).map: d =>
      val expectation = IngestExpectation(RootKind.Harness, harness, inventory("generator_code_sha256").str,
        CellSpec(cell.id, CellKind.Condition, 40, 0, 1.0, false), d, Seeds.ProtocolDenylist)
      val bound = ok(PhrfDatasetLoader.load(input, f"${cell.id}__d$d%04d", expectation))
      new LoadedDataset(bound.fit, ok(CommonPreparation.prepare(bound.fit, CellKind.Condition)), None)
    val source: PilotDatasetSource = j => Right(loaded(j.dataset))
    val config = GlmSingleConfig(Path.of(args(2)), Path.of("tools/phrf-comparison/glmsingle/from_generator.py").toAbsolutePath, 900.0)
    val delegate = new PilotArmRunner(source, PilotEngines.real(config, ok(ThreadCpuClock.system)))
    var attempts = 0
    val arms: ArmRunner = context =>
      attempts += 1
      delegate.run(context)
    val owner = TestOwner.random()
    val store = ok(SealedStore.open(out.resolve("sealed"), owner.recipient, owner.fingerprint))
    val stamp = PilotStamp(Vector("role" -> "harness-real-cpu-guard", "inputs" -> Fs.sha256File(input.resolve("rehearsal-inputs.json"))))
    val root = new PilotRoot(java.lang.Long.parseUnsignedLong(harness, 16))
    def runner(guard: CpuGuard) = new PilotRunner(plan, out, stamp, store, root, arms, guard, CpuClock.system)
    val tiny = CpuGuard(0.5 / 3600, 0.5 / 3600)
    val first = runner(tiny).run()
    require(first.left.exists(_.isInstanceOf[PilotRefusal.CpuCeilingReached]), first.toString)
    val before = attempts
    require(before > 0, "guard must interrupt actual real-arm work, not only dispatch setup")
    require(runner(tiny).run().isLeft && attempts == before)
    val identity = runner(CpuGuard()).outputIdentity.get
    val raise = ok(OwnerCeilingRaise.of(61.0, "ephemeral-harness-test-owner",
      "rehearsal-only forced CPU stop", identity, 3))
    val report = ok(runner(CpuGuard.raised(45.0, raise)).run())
    require(report.contributions.length == 8 && attempts > before)
    val receipt = ujson.Obj("schema" -> "phrf-s10-real-cpu-guard/v1", "root_kind" -> "harness",
      "stop" -> first.left.toOption.get.productPrefix, "attempts_before_stop" -> before,
      "no_dispatch_on_unraised_resume" -> true, "resumed_units" -> report.contributions.length,
      "total_cpu_seconds" -> report.totalCpuSeconds, "pilot_launch_authority" -> false)
    Files.writeString(out.resolve("receipt.json"), ujson.write(receipt, indent = 2) + "\n")
    println(ujson.write(receipt))
