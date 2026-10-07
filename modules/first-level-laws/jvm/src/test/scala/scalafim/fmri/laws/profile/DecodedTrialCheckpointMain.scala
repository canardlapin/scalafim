package scalafim.fmri.laws.profile

import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal
import scalafim.fmri.design.hrf.{KernelBasisCompilation, TrialDesignLowering}
import scalafim.fmri.fit.profile.*
import scalafim.fmri.fit.profile.ProfileHrfTrialOutputsParallel.*
import scalafim.fmri.model.ProfileCriterion

/** Explicit opt-in diagnostic: Test/runMain ... <output.json> <dense|regular|tiny> <voxels> <trials> <workers>
  * <penalized|ml> <exact|corrected> <source-id> [nodes-per-axis] [baseline|expanded] [trial-block-size]. Writes non-admitted evidence even
  * when the public execution returns a refusal.
  */
object DecodedTrialCheckpointMain:
  import DecodedTrialCheckpoint.*

  def main(args: Array[String]): Unit =
    require(
      args.length >= 8 && args.length <= 11,
      "output.json geometry voxels trials workers criterion mode source-id [nodes-per-axis] [baseline|expanded] [trial-block-size]"
    )
    val budget = args.lift(9).getOrElse("baseline") match
      case "baseline" => DecodeBudget()
      case "expanded" => DecodeBudget(maxNewtonSteps = 6, maxJets = 8, maxExactEvaluations = 2)
      case other      => throw new IllegalArgumentException(s"unknown budget $other")
    val geometry = args(1) match
      case "dense" | "dense-blocked"     => Geometry.B0Dense
      case "regular" | "regular-blocked" => Geometry.B0Regular
      case "tiny"                        => Geometry.Tiny
      case other                         => throw new IllegalArgumentException(s"unknown geometry $other")
    val criterion = args(5) match
      case "penalized" => ProfileCriterion.PenalizedProfile(1.0)
      case "ml"        => ProfileCriterion.TrialRandomEffectsML(1.0)
      case other       => throw new IllegalArgumentException(s"unknown criterion $other")
    val mode = args(6) match
      case "exact"     => ProfileTrialReadoutMode.ExactShape
      case "corrected" => ProfileTrialReadoutMode.CorrectedReference
      case other       => throw new IllegalArgumentException(s"unknown readout $other")
    val config = Config(
      geometry,
      args(2).toInt,
      args(3).toInt,
      workers = args(4).toInt,
      gridAxisNodes = args.lift(8).map(_.toInt).getOrElse(2),
      budget = budget,
      trialPreparation = TrialPreparationPolicy(args.lift(10)
        .fold[TrialDesignLowering](TrialDesignLowering.Dense)(s => TrialDesignLowering.Blocked(s.toInt))),
      criterion = criterion,
      mode = mode,
      compilation =
        if args(1).endsWith("-blocked") then KernelBasisCompilation.BlockedPartial(96) else KernelBasisCompilation.Dense
    )
    val started = System.nanoTime()
    val receipt = ujson.Obj(
      "format" -> "phrf-decoded-diagnostic/1",
      "source" -> args(7),
      "qualification" -> "not-admitted",
      "geometry" -> geometry.toString,
      "voxels" -> config.voxels,
      "trials" -> config.trials,
      "workersRequested" -> config.workers,
      "criterion" -> criterion.toString,
      "readout" -> mode.toString,
      "java" -> System.getProperty("java.runtime.version"),
      "os" -> System.getProperty("os.name"),
      "arch" -> System.getProperty("os.arch"),
      "decodeBudget" -> config.budget.toString,
      "nodesPerAxis" -> config.gridAxisNodes,
      "budgetProfile" -> args.lift(9).getOrElse("baseline"),
      "compilation" -> config.compilation.toString,
      "trialPreparation" -> config.trialPreparation.toString,
      "limitations" -> ujson.Arr(
        "original-family certification unavailable",
        "no measured engine peak",
        "single diagnostic run, no warmup",
        "no scientific calibration",
        "input block recycled"
      )
    )
    try
      val f = fixture(config)
      receipt("basisSeconds") = f.basisNanos / 1e9
      receipt("fixtureSecondsIncludingBasis") = f.fixtureNanos / 1e9
      receipt("rows") = f.rows
      receipt("trials") = f.trials
      receipt("basisRank") = f.plan.basis.rank
      receipt("inputBlockVoxels") = f.inputBlockVoxels
      receipt("retainedInputBytes") = f.inputBytes.toDouble
      val prepareStarted = System.nanoTime()
      val prepared = f.prepare
      receipt("preparationSeconds") = (System.nanoTime() - prepareStarted) / 1e9
      prepared match
        case Left(error)  => receipt("preparationError") = error.message
        case Right(value) =>
          receipt("setup") = value.setup.toString
          receipt("trialPreparationReceipt") = value.setup.trial.fold[ujson.Value](ujson.Null)(product)
          receipt("retainedSourceDesignValues") = value.setup.expandedTrialLoweringDoubles
            .fold[ujson.Value](ujson.Null)(v => ujson.Num(v.toDouble))
          value.trialOutputs match
            case Left(error)    => receipt("outputPreparationError") = error.message
            case Right(outputs) =>
              receipt("originalFamilyAdmission") = outputs
                .executionDeclaration(request, mode, ProfileTrialEvidenceRequest.CertifiedOriginalEquations)
                .fold(_.message, _.toString)
              val readers = Vector.fill(config.workers)(new f.Reader)
              val sink = new Sink(f.trials)
              val runStarted = System.nanoTime()
              val result =
                if config.workers == 1 then outputs.run(readers.head, request, mode, sink)
                else outputs.runParallel(readers, request, mode, sink)
              receipt("executionSecondsIncludingReadsAndSink") = (System.nanoTime() - runStarted) / 1e9
              receipt("readerWallSecondsSummedAcrossWorkers") = readers.map(_.nanos).sum / 1e9
              receipt("readValues") = readers.map(_.values.toDouble).sum
              receipt("largestReaderSeriesValues") = readers.map(_.largestSeriesValues.toDouble).max
              receipt("sinkSeconds") = sink.nanos / 1e9
              receipt("attemptedDelivered") = sink.attempted.toDouble
              receipt("emitted") = sink.emitted.toDouble
              receipt("offNodeOutputs") = sink.offNode.toDouble
              receipt("float32OutputBytes") = (sink.outputValues * 4L).toDouble
              receipt("retainedSinkBytes") = sink.retainedOutputBytes.toDouble
              receipt("outputChecksum") = sink.outputChecksum
              receipt("maxFloat32AbsoluteError") = sink.maxFloatError
              receipt("maxPreparedBasisResidual") = sink.maxPreparedResidual
              receipt("statuses") = ujson.Obj.from(
                sink.statuses.toVector.sortBy(_._1.toString).map((k, v) => k.toString -> ujson.Num(v.toDouble))
              )
              receipt("budgetExits") = ujson.Obj.from(
                sink.exits.toVector.sortBy(_._1.toString).map((k, v) => k.toString -> ujson.Num(v.toDouble))
              )
              result match
                case Left(error)    => receipt("executionError") = error.message
                case Right(summary) =>
                  receipt("progress") = summary.progress.toString
                  receipt("workersUsed") = summary.progress.workersUsed
                  receipt("provenance") = summary.provenance
                  receipt("decoder") = product(summary.progress.decoder)
                  receipt("trialWork") = summary.progress.trial.fold[ujson.Value](ujson.Null)(v => product(v))
                  receipt("mlWork") = summary.progress.trialMl.fold[ujson.Value](ujson.Null)(v => product(v))
    catch case NonFatal(error) => receipt("failure") = error.toString
    receipt("totalSeconds") = (System.nanoTime() - started) / 1e9
    receipt("heapUsedAtEndBytes") = ManagementFactory.getMemoryMXBean.getHeapMemoryUsage.getUsed.toDouble
    val output = Path.of(args(0))
    Option(output.getParent).foreach(p => { val _ = Files.createDirectories(p) })
    val _ = Files.writeString(output, ujson.write(receipt, indent = 2) + "\n", UTF_8)
    println(s"PHRF diagnostic written to $output; qualification=not-admitted")

  private def product(value: Product): ujson.Value =
    ujson.Obj.from(
      value.productElementNames
        .zip(value.productIterator)
        .map((name, item) =>
          name -> (item match
            case nested: Product => product(nested)
            case number: Long    => ujson.Num(number.toDouble)
            case number: Int     => ujson.Num(number.toDouble)
            case number: Double  => ujson.Num(number)
            case other           => ujson.Str(other.toString))
        )
    )
