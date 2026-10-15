package scalafim.fmri.laws.profile

import java.nio.file.{Files, Path}
import scalafim.fmri.design.hrf.KernelBasisCompilation
import scalafim.fmri.fit.profile.*

/** Compare declared production decoder configurations on the matched recovery ladder. */
object TrialRecoveryCandidate:
  def main(args: Array[String]): Unit =
    require(
      args.length >= 3 && args.length <= 5,
      "output.json voxels nodes-per-axis [center|bank] [stationarity-tolerance]"
    )
    val voxels = args(1).toInt
    val nodes = args(2).toInt
    val config = DecodedTrialCheckpoint.Config(
      DecodedTrialCheckpoint.Geometry.B0Dense,
      voxels,
      compilation = KernelBasisCompilation.BlockedPartial(96),
      gridAxisNodes = nodes,
      budget = DecodeBudget(
        maxNewtonSteps = 16,
        maxJets = 20,
        maxExactEvaluations = 40,
        maxCandidateAttempts = 12,
        stationarityStepTolerance = args.lift(4).map(_.toDouble).getOrElse(1e-8),
        initialization = args.lift(3).getOrElse("bank") match
          case "bank"   => DecodeInitialization.BankNode
          case "center" => DecodeInitialization.ChartCenterProbe
          case other    => throw new IllegalArgumentException(s"unknown initialization $other")
      )
    )
    val f = DecodedTrialCheckpoint.fixture(config)
    val ladder = TrialRecoveryFixture(f)
    val prepared = f.prepare.fold(e => throw new IllegalArgumentException(e.message), identity)
    val outputs = prepared.trialOutputs.fold(e => throw new IllegalArgumentException(e.message), identity)
    val records = ujson.Arr()
    val stages = ujson.Arr()
    TrialRecoveryFixture.noiseRatios.foreach: ratio =>
      val current = f.copy(rawBlock = ladder.response(ratio))
      val sink = new BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt]:
        def accept(block: VoxelBlock, payload: ProfileTrialOutputBlock): Either[String, ProfileFitReceipt] =
          payload.results.foreach: voxel =>
            val selected = voxel.selection
            records.value += ujson.Obj(
              "noiseRatio" -> ratio,
              "voxel" -> voxel.voxelId,
              "status" -> selected.status.toString,
              "coordinates" -> ujson.Arr.from(selected.coordinates),
              "energy" -> selected.energy,
              "hessian" -> ujson.Arr.from(selected.dataHessian),
              "budgetExit" -> selected.budgetExit.fold[ujson.Value](ujson.Null)(v => ujson.Str(v.toString))
            )
          Right(ProfileFitReceipt(block.index, payload.voxelIds))
      val started = System.nanoTime()
      val result = outputs
        .run(new current.Reader, DecodedTrialCheckpoint.request, ProfileTrialReadoutMode.ExactShape, sink)
        .fold(e => throw new IllegalArgumentException(e.message), identity)
      stages.value += ujson.Obj(
        "noiseRatio" -> ratio,
        "seconds" -> ((System.nanoTime() - started) / 1e9),
        "decoder" -> ujson.Obj.from(
          result.progress.decoder.productElementNames
            .zip(result.progress.decoder.productIterator)
            .map((k, v) => k -> ujson.Num(v.asInstanceOf[Long].toDouble))
        ),
        "trialWork" -> result.progress.trial.fold("")(_.toString),
        "readout" -> result.progress.publicReadout.fold("")(_.toString)
      )
      println(s"nodes=$nodes ratio=$ratio ${result.progress.decodeStatuses}")
    val receipt = ujson.Obj(
      "qualification" -> "not-admitted",
      "nodesPerAxis" -> nodes,
      "budget" -> config.budget.toString,
      "setup" -> prepared.setup.toString,
      "records" -> records,
      "stages" -> stages
    )
    val _ = Files.writeString(Path.of(args(0)), ujson.write(receipt, indent = 2) + "\n")
