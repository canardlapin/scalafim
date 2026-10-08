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
  * <penalized|ml> <exact|corrected> <source-id> [nodes-per-axis] [baseline|expanded|repaired]
  * [trial-block-size] [noise-ratio] [execution-deadline-seconds] [prepared-residual-limit] [horizon-seconds] [basis-max-rank] [basis-subspace]. Writes non-admitted evidence even
  * when the public execution returns a refusal.
  */
object DecodedTrialCheckpointMain:
  import DecodedTrialCheckpoint.*

  def main(args: Array[String]): Unit =
    require(
      args.length >= 8 && args.length <= 17,
      "output.json geometry voxels trials workers criterion mode source-id [nodes-per-axis] [baseline|expanded|repaired] [trial-block-size] [noise-ratio] [execution-deadline-seconds] [prepared-residual-limit] [horizon-seconds] [basis-max-rank] [basis-subspace]"
    )
    val budget = args.lift(9).getOrElse("baseline") match
      case "baseline" => DecodeBudget()
      case "expanded" => DecodeBudget(maxNewtonSteps = 6, maxJets = 8, maxExactEvaluations = 2)
      case "repaired" => repairedBudget
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
      noiseRatio = args.lift(11).map(_.toDouble),
      horizonSeconds = args.lift(14).map(_.toDouble),
      basisMaxRank = args.lift(15).map(_.toInt).getOrElse(32),
      trialPreparation = TrialPreparationPolicy(args.lift(10)
        .fold[TrialDesignLowering](TrialDesignLowering.Dense)(s => TrialDesignLowering.Blocked(s.toInt))),
      criterion = criterion,
      mode = mode,
      compilation =
        if args(1).endsWith("-blocked") then KernelBasisCompilation.BlockedPartial(args.lift(16).map(_.toInt).getOrElse(96)) else KernelBasisCompilation.Dense
    )
    val deadlineSeconds = args.lift(12).map(_.toDouble)
    require(deadlineSeconds.forall(x => x.isFinite && x > 0.0))
    val evidence = args.lift(13).fold[ProfileTrialEvidenceRequest](ProfileTrialEvidenceRequest.PreparedBasisResidual)(s =>
      ProfileTrialEvidenceRequest.PreparedBasisResidualAtMost(ProfileTrialResidualLimit(s.toDouble)))
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
      "evidenceRequest" -> evidence.toString,
      "horizonSeconds" -> config.horizonSeconds.fold[ujson.Value](ujson.Null)(ujson.Num(_)),
      "basisMaximumRank" -> config.basisMaxRank,
      "java" -> System.getProperty("java.runtime.version"),
      "os" -> System.getProperty("os.name"),
      "arch" -> System.getProperty("os.arch"),
      "maximumJvmHeapBytes" -> Runtime.getRuntime.maxMemory().toDouble,
      "availableJvmProcessors" -> Runtime.getRuntime.availableProcessors(),
      "decodeBudget" -> config.budget.toString,
      "nodesPerAxis" -> config.gridAxisNodes,
      "budgetProfile" -> args.lift(9).getOrElse("baseline"),
      "compilation" -> config.compilation.toString,
      "trialPreparation" -> config.trialPreparation.toString,
      "noiseRatio" -> config.noiseRatio.getOrElse(if geometry == Geometry.Tiny then 0.01 else 2.0),
      "executionDeadlineSeconds" -> deadlineSeconds.fold[ujson.Value](ujson.Null)(ujson.Num(_)),
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
              val heap = new DiagnosticHeapSampler
              heap.start()
              def cancelled(): Boolean = deadlineSeconds.exists(limit => (System.nanoTime() - runStarted) / 1e9 >= limit)
              val result =
                try
                  if config.workers == 1 then outputs.run(readers.head, request, mode, sink, () => cancelled(), evidence = evidence)
                  else outputs.runParallel(readers, request, mode, sink, () => cancelled(), evidence = evidence) match
                    case Left(ProfileFitError.WorkersStillRunning(_, _, _, _, termination)) => Left(termination.awaitFinal())
                    case completed => completed
                finally heap.close()
              receipt("executionSecondsIncludingReadsAndSink") = (System.nanoTime() - runStarted) / 1e9
              receipt("sampledProcessHeapMaximumBytes") = heap.maximum.toDouble
              receipt("processHeapSamples") = heap.samples.toDouble
              receipt("memoryScope") = "whole sbt JVM used heap sampled every 10ms; includes build, fixture, garbage and engine; neither engine live memory nor an exact peak"
              receipt("readerWallSecondsSummedAcrossWorkers") = readers.map(_.nanos).sum / 1e9
              receipt("readValues") = readers.map(_.values.toDouble).sum
              receipt("largestReaderSeriesValues") = readers.map(_.largestSeriesValues.toDouble).max
              receipt("sinkSeconds") = sink.nanos / 1e9
              receipt("attemptedDelivered") = sink.attempted.toDouble
              receipt("emitted") = sink.emitted.toDouble
              receipt("readoutRefused") = sink.readoutRefused.toDouble
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
              receipt("completed") = result.isRight
              result.fold(errorProgress, summary => Some(summary.progress)).foreach: progress =>
                receipt("progress") = progress.toString
                receipt("workersUsed") = progress.workersUsed
                receipt("attemptedVoxels") = progress.attemptedVoxels
                receipt("attemptedStatuses") = ujson.Obj.from(progress.decodeStatuses.toVector
                  .sortBy(_._1.toString).map((k, v) => k.toString -> ujson.Num(v.toDouble)))
                receipt("decoder") = product(progress.decoder)
                receipt("trialWork") = progress.trial.fold[ujson.Value](ujson.Null)(v => product(v))
                receipt("mlWork") = progress.trialMl.fold[ujson.Value](ujson.Null)(v => product(v))
                receipt("publicReadoutWork") = progress.publicReadout.fold[ujson.Value](ujson.Null)(v => product(v))
              result match
                case Left(error) => receipt("executionError") = error.message
                case Right(summary) => receipt("provenance") = summary.provenance
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

  private def errorProgress(error: ProfileFitError): Option[ProfileRunProgress] = error match
    case ProfileFitError.Cancelled(p) => Some(p)
    case ProfileFitError.Dataset(_, p) => Some(p)
    case ProfileFitError.Backend(_, p) => Some(p)
    case ProfileFitError.TrialReadoutFailure(_, p) => Some(p)
    case ProfileFitError.TrialMlFailure(_, p) => Some(p)
    case ProfileFitError.SinkRefused(_, p) => Some(p)
    case ProfileFitError.SinkThrew(_, p) => Some(p)
    case _ => None

  /** Process instrumentation only. No engine-memory inference is made from these samples. */
  private final class DiagnosticHeapSampler extends AutoCloseable:
    @volatile private var active = true
    @volatile var maximum = 0L
    @volatile var samples = 0L
    private val thread = new Thread(() =>
      val bean = ManagementFactory.getMemoryMXBean
      while active do
        maximum = math.max(maximum, bean.getHeapMemoryUsage.getUsed)
        samples += 1
        try Thread.sleep(10)
        catch case _: InterruptedException => ()
    , "phrf-diagnostic-heap-sampler")
    thread.setDaemon(true)
    def start(): Unit = thread.start()
    def close(): Unit =
      active = false
      thread.interrupt()
      thread.join()
