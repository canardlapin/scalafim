package scalafim.fmri.laws.profile

import java.nio.file.{Files, Path}
import scalafim.fmri.design.hrf.KernelBasisCompilation
import scalafim.fmri.fit.profile.*

/** Actual public bounded search/readout on all matched noise-ladder cells. */
object TrialBoundedSearchCandidate:
  def main(args: Array[String]): Unit =
    require(args.length == 2, "output.json max-jets")
    val config = DecodedTrialCheckpoint.Config(
      DecodedTrialCheckpoint.Geometry.B0Dense,
      16,
      compilation = KernelBasisCompilation.BlockedPartial(96),
      budget = DecodeBudget(
        maxNewtonSteps = 16,
        maxJets = args(1).toInt,
        maxExactEvaluations = 40,
        maxCandidateAttempts = 30,
        stationarityStepTolerance = 1e-6,
        initialization = DecodeInitialization.BoundedMultistart
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
      val consumed = new DecodedTrialCheckpoint.Sink(f.trials)
      val sink = new BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt]:
        def accept(block: VoxelBlock, payload: ProfileTrialOutputBlock): Either[String, ProfileFitReceipt] =
          payload.results.foreach: voxel =>
            val selected = voxel.selection
            records.value += ujson.Obj(
              "noiseRatio" -> ratio,
              "voxel" -> voxel.voxelId,
              "coordinates" -> ujson.Arr.from(selected.coordinates),
              "energy" -> selected.energy,
              "status" -> selected.status.toString,
              "hessian" -> ujson.Arr.from(selected.dataHessian),
              "budgetExit" -> selected.budgetExit.fold[ujson.Value](ujson.Null)(v => ujson.Str(v.toString)),
              "search" -> selected.search.fold[ujson.Value](ujson.Null): receipt =>
                ujson.Obj(
                  "requestedStarts" -> receipt.requestedStarts,
                  "scale" -> receipt.objectiveScale,
                  "energyTieToleranceScaled" -> receipt.energyTieToleranceScaled,
                  "trajectories" -> ujson.Arr.from(receipt.trajectories.map: trajectory =>
                    val result = trajectory.result
                    ujson.Obj(
                      "initialUnit" -> ujson.Arr.from(trajectory.initialUnitCoordinates),
                      "status" -> result.status.toString,
                      "evaluations" -> result.work.evaluations,
                      "iterations" -> result.work.iterations,
                      "rejectedSteps" -> result.work.rejectedSteps,
                      "metricResets" -> result.work.metricResets,
                      "point" -> result.point.fold[ujson.Value](ujson.Null)(p =>
                        ujson.Obj(
                          "unit" -> ujson.Arr.from(p.coordinates),
                          "energyScaled" -> p.value,
                          "gradientScaled" -> ujson.Arr.from(p.gradient),
                          "projectedGradient" -> p.projectedGradientNorm
                        )
                      )
                    ))
                )
            )
          consumed.accept(block, payload)
      val started = System.nanoTime()
      val result = outputs
        .run(new current.Reader, DecodedTrialCheckpoint.request, ProfileTrialReadoutMode.ExactShape, sink)
        .fold(e => throw new IllegalArgumentException(e.message), identity)
      stages.value += ujson.Obj(
        "noiseRatio" -> ratio,
        "seconds" -> ((System.nanoTime() - started) / 1e9),
        "attempted" -> consumed.attempted,
        "emitted" -> consumed.emitted,
        "offNodeOutputs" -> consumed.offNode,
        "outputValues" -> consumed.outputValues,
        "float32BytesConsumed" -> (consumed.outputValues * 4L),
        "outputChecksum" -> consumed.outputChecksum,
        "maxFloat32Error" -> consumed.maxFloatError,
        "maxPreparedResidual" -> consumed.maxPreparedResidual,
        "retainedSinkBytes" -> consumed.retainedOutputBytes,
        "decoder" -> ujson.Obj.from(
          result.progress.decoder.productElementNames
            .zip(result.progress.decoder.productIterator)
            .map((k, v) => k -> ujson.Num(v.asInstanceOf[Long].toDouble))
        ),
        "trialWork" -> result.progress.trial.fold("")(_.toString),
        "readout" -> result.progress.publicReadout.fold("")(_.toString)
      )
      println(s"ratio=$ratio ${result.progress.decodeStatuses}")
    val _ = Files.writeString(
      Path.of(args(0)),
      ujson.write(
        ujson.Obj(
          "format" -> "phrf-bounded-search-diagnostic/1",
          "complete" -> true,
          "qualification" -> "not-admitted",
          "budget" -> config.budget.toString,
          "setup" -> prepared.setup.toString,
          "records" -> records,
          "stages" -> stages
        ),
        indent = 2
      ) + "\n"
    )
