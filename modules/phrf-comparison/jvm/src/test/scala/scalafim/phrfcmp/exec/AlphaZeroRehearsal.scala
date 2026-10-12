package scalafim.phrfcmp.exec

import java.nio.file.{Files, Path}
import java.nio.charset.StandardCharsets.UTF_8
import scalafim.phrfcmp.ingest.*
import scalafim.phrfcmp.prep.{CommonPreparation, NativeArm}
import scalafim.phrfcmp.run.*

/** Original S10 alpha-zero check: ten harness datasets, y0 = y - signal + signal_condition_mean. */
object AlphaZeroRehearsal:
  private def ok[A](e: Either[?, A]): A = e.fold(x => throw new IllegalStateException(x.toString), identity)
  def main(args: Array[String]): Unit =
    require(args.length == 2, "AlphaZeroRehearsal <harness-inputs> <new-output.jsonl>")
    val input = Path.of(args(0)); val output = Path.of(args(1))
    require(!Files.exists(output))
    val inventory = ujson.read(Files.readString(input.resolve("rehearsal-inputs.json")))
    require(inventory("root_kind").str == "harness" && inventory("no_deviation_datasets").num == 10)
    val clock = ok(ThreadCpuClock.system)
    val selected = scala.collection.mutable.Map.empty[(String, String), Vector[Int]]
    for d <- 0 until 10 do
      val expected = IngestExpectation(RootKind.Harness, "7a3c91d50b44e2f1", inventory("generator_code_sha256").str,
        CellSpec("T-TX-fast", CellKind.Trial, 40, 4000, 1.0, true), d, Seeds.ProtocolDenylist)
      val bound = ok(PhrfDatasetLoader.load(input, f"T-TX-fast__d$d%04d", expected))
      for (kind, fit) <- Vector("persistent" -> bound.fit, "no-deviation" -> NoDeviationHarness.inputs(bound)) do
        val start = CpuClock.system.processCpuSeconds()
        val prep = ok(CommonPreparation.prepare(fit, CellKind.Trial))
        if d < 2 && kind == "persistent" then
          val probe = ok(PhrfCanProbe.run(fit, prep, clock = clock))
          val timing = probe.timings
          val row = ujson.Obj("dataset" -> d, "method" -> "phrf-can-timing-only",
            "selected_alpha" -> probe.selectedAlpha, "prepare_seconds" -> timing.foldPrepare.flatten.sum,
            "fit_seconds" -> timing.foldRun.flatten.sum, "final_seconds" -> (timing.finalPrepare + timing.finalRun),
            "pilot_launch_authority" -> false)
          Files.writeString(Path.of(args(1) + ".can.jsonl"), ujson.write(row) + "\n", UTF_8,
            java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND): Unit
        val phrf = PhrfTrialRunner.run(fit, prep, clock = clock)
        val settings = PhrfTrialRunner.rlssSettings(fit, prep)
        val native = settings.flatMap(s => TrialNativeRunner.run(NativeArm.Rlss, fit, prep, s)
          .left.map(e => PhrfTrialRefusal.Inconsistent(e.toString)))
        val choices = Vector("phrf-ml" -> phrf.toOption.map(_.tuning.selection),
          "rlss" -> native.toOption.flatMap(_.tuning).map(_.selection))
        for (method, choice) <- choices do
          choice.foreach(s => selected.update((kind, method), selected.getOrElse((kind, method), Vector.empty) :+ s.selected))
          val row = ujson.Obj("dataset" -> d, "truth_case" -> kind, "method" -> method,
            "input_sha256" -> bound.inputSha256, "selected_index" -> choice.fold[ujson.Value](ujson.Null)(s => ujson.Num(s.selected)),
            "refusal" -> (if method == "phrf-ml" then phrf.left.toOption.map(_.toString) else native.left.toOption.map(_.toString)).fold[ujson.Value](ujson.Null)(ujson.Str(_)),
            "pair_cpu_seconds" -> (CpuClock.system.processCpuSeconds() - start), "pilot_launch_authority" -> false)
          Files.writeString(output, ujson.write(row) + "\n", UTF_8, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND)
          println(ujson.write(row))
    val methods = Vector("phrf-ml", "rlss")
    val complete = methods.forall(m => Vector("persistent", "no-deviation").forall(k => selected.getOrElse((k, m), Vector.empty).length == 10))
    val nonconstant = methods.forall: m =>
      val all = selected.getOrElse(("persistent", m), Vector.empty) ++ selected.getOrElse(("no-deviation", m), Vector.empty)
      !all.forall(_ == 0) && !all.forall(_ == AlphaGrid.Pilot.size - 1)
    val summary = ujson.Obj("complete" -> complete, "not_constant_endpoint" -> nonconstant,
      "endpoint_counts" -> ujson.Arr.from(selected.toVector.sortBy(_._1).map { case ((kind, method), ix) =>
        ujson.Obj("truth_case" -> kind, "method" -> method, "selected_indices" -> ujson.Arr.from(ix),
          "lower" -> ix.count(_ == 0), "upper" -> ix.count(_ == AlphaGrid.Pilot.size - 1)) }),
      "pilot_launch_authority" -> false)
    Files.writeString(Path.of(args(1) + ".summary.json"), ujson.write(summary, indent = 2) + "\n")
    require(complete && nonconstant, "S10 endpoint check failed; retain all records")
