package scalafim.fmri.laws.profile

import java.io.{BufferedOutputStream, DataOutputStream}
import java.nio.file.{Files, Path}
import gale.linalg.DMat
import scalafim.fmri.ar.WhiteningTransform
import scalafim.fmri.design.hrf.KernelBasisCompilation
import scalafim.fmri.fit.profile.*
import scalafim.fmri.hrf.family.ShapePoint

/** Export a bounded, matched B0 recovery experiment for an independent SciPy oracle. */
object TrialRecoveryExport:
  def main(args: Array[String]): Unit =
    require(args.length == 2, "output-directory voxels")
    val directory = Path.of(args(0))
    val _ = Files.createDirectories(directory)
    val f = DecodedTrialCheckpoint.fixture(
      DecodedTrialCheckpoint.Config(
        DecodedTrialCheckpoint.Geometry.B0Dense,
        args(1).toInt,
        compilation = KernelBasisCompilation.BlockedPartial(96)
      )
    )
    val ladder = TrialRecoveryFixture(f)
    val basis = f.plan.basis
    val metadata = ujson.Obj(
      "format" -> "phrf-recovery-input/1",
      "rows" -> f.rows,
      "trials" -> f.trials,
      "rank" -> basis.rank,
      "voxels" -> f.inputBlockVoxels,
      "truth" -> numbers(TrialRecoveryFixture.truth),
      "noiseRatios" -> numbers(TrialRecoveryFixture.noiseRatios),
      "signalRms" -> numbers(ladder.signalRms),
      "lower" -> numbers(basis.family.chart.lower),
      "upper" -> numbers(basis.family.chart.upper),
      "basis" -> basis.provenance.canonical,
      "layout" -> "big-endian float64 row-major; expanded columns are basis-major then trial"
    )
    val arrays = ujson.Obj()
    def write(name: String, matrix: DMat): Unit =
      val stream = new DataOutputStream(
        new BufferedOutputStream(Files.newOutputStream(directory.resolve(name + ".bin")))
      )
      try
        var r = 0
        while r < matrix.rows do
          var c = 0
          while c < matrix.cols do
            stream.writeDouble(matrix(r, c))
            c += 1
          r += 1
      finally stream.close()
      arrays(name) = ujson.Arr(matrix.rows, matrix.cols)
    require(f.expanded.blockCount == 1, "dense reference export requires a single trial design block")
    val denseSource = f.expanded.block(0).fold(e => throw new IllegalArgumentException(e.message), identity)
    write(
      "expanded",
      WhiteningTransform
        .matrix(f.whitening, gale(denseSource))
        .fold(e => throw new IllegalArgumentException(e.toString), identity)
    )
    write(
      "nuisance",
      WhiteningTransform
        .matrix(f.whitening, gale(f.baseline.designMatrix))
        .fold(e => throw new IllegalArgumentException(e.toString), identity)
    )
    write("phi", DMat.tabulate(basis.rank, basis.fineCount)((p, i) => basis.value(p, i)))
    write("lags", DMat.tabulate(1, basis.fineCount)((_, i) => basis.lags(i)))
    write("clean", ladder.whiten(ladder.clean))
    write("unit-noise", ladder.whiten(ladder.unitNoise))
    val coefficients = new Array[Double](basis.rank * 4)
    basis.coefficientJetInto(
      ShapePoint.unsafe(TrialRecoveryFixture.truth),
      new Array[Double](basis.fineCount * 10),
      coefficients,
      4
    )
    metadata("truthCoefficientJet") = numbers(coefficients.toVector)
    val preparation = TrialBandedPreparation
      .prepare(f.expanded, Some(f.whitening), Some(gale(f.baseline.designMatrix)), 1.0)
      .fold(e => throw new IllegalArgumentException(e.message), identity)
    val objective = preparation
      .objective(NodeGrid(basis.family.chart, Vector(2, 2, 2)))
      .fold(e => throw new IllegalArgumentException(e.message), identity)
    val stages = ujson.Arr()
    TrialRecoveryFixture.noiseRatios.foreach: ratio =>
      val raw = ladder.response(ratio)
      val responses = ladder.whiten(raw)
      val current = f.copy(rawBlock = raw)
      val outputs =
        current.prepare.flatMap(_.trialOutputs).fold(e => throw new IllegalArgumentException(e.message), identity)
      val decoded = ujson.Arr()
      val sink = new BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt]:
        def accept(block: VoxelBlock, payload: ProfileTrialOutputBlock): Either[String, ProfileFitReceipt] =
          payload.results.foreach: voxel =>
            val selected = voxel.selection
            val response = Array.tabulate(f.rows)(t => responses(t, voxel.voxelId))
            objective.pointAt(
              preparation.encodeWhitened(response).fold(e => throw new IllegalArgumentException(e.message), identity)
            )
            val jet = new ProfileJetBuffer(3, 3)
            require(objective.jetAt(TrialRecoveryFixture.truth.toArray, jet))
            decoded.value += ujson.Obj(
              "voxel" -> voxel.voxelId,
              "coordinates" -> numbers(selected.coordinates),
              "energy" -> selected.energy,
              "status" -> selected.status.toString,
              "budgetExit" -> selected.budgetExit.fold[ujson.Value](ujson.Null)(v => ujson.Str(v.toString)),
              "truthEnergy" -> jet.energy,
              "truthGradient" -> numbers(jet.gradient.toVector),
              "truthHessian" -> numbers(jet.hessian.toVector)
            )
          Right(ProfileFitReceipt(block.index, payload.voxelIds))
      val summary = outputs
        .run(new current.Reader, DecodedTrialCheckpoint.request, ProfileTrialReadoutMode.ExactShape, sink)
        .fold(e => throw new IllegalArgumentException(e.message), identity)
      stages.value += ujson.Obj("noiseRatio" -> ratio, "decoder" -> decoded, "progress" -> summary.progress.toString)
      println(s"exported noise ratio $ratio: ${summary.progress.decodeStatuses}")
    metadata("arrays") = arrays
    metadata("stages") = stages
    val _ = Files.writeString(directory.resolve("input.json"), ujson.write(metadata, indent = 2) + "\n")

  private def numbers(values: Vector[Double]): ujson.Arr = ujson.Arr.from(values)

  private def gale(matrix: scalafim.fmri.hrf.linalg.Mat): DMat =
    DMat.tabulate(matrix.rows, matrix.cols)((r, c) => matrix(r, c))
