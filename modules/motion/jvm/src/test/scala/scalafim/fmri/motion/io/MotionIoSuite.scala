package scalafim.fmri.motion.io

import bids4s.*
import scalafim.fmri.motion.*
import scalafim.image.*

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.file.{Files, Path}

class MotionIoSuite extends munit.FunSuite:

  test("NIfTI adapter reads 4D runs and 3D masks through existing image IO") {
    val dir = Files.createTempDirectory("scalafim-motion-nifti")
    val runPath = dir.resolve("run.nii")
    val maskPath = dir.resolve("mask.nii")

    writeFloat32Nifti(runPath, Vector(2, 1, 1, 3), Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0))
    writeFloat32Nifti(maskPath, Vector(2, 1, 1), Vector(1.0, 0.0))

    val run = MotionNiftiIo.readRun(runPath).fold(err => fail(err.message), identity)
    val mask = MotionNiftiIo.readMask(maskPath).fold(err => fail(err.message), identity)

    assertEquals(run.space.dims.take(4), Vector(2, 1, 1, 3))
    assertEquals(run.nVolumes, 3)
    assertEqualsDouble(run.valueAtCanonicalOrdinal(0), 1.0, 1e-12)
    assertEqualsDouble(run.valueAtCanonicalOrdinal(5), 6.0, 1e-12)
    assert(mask.valueAtCanonicalOrdinal(0))
    assert(!mask.valueAtCanonicalOrdinal(1))
  }

  test("NIfTI metadata adapter roundtrips affine, voxel size, TR, and slice timing") {
    val dir = Files.createTempDirectory("scalafim-motion-nifti-meta")
    val path = dir.resolve("sub-01_task-rest_bold.nii")
    val affine =
      DMat.fromRows(
        Vector(
          Vector(2.0, 0.0, 0.0, 10.0),
          Vector(0.0, 3.0, 0.0, 20.0),
          Vector(0.0, 0.0, 4.0, 30.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    val timing = SliceTiming.unsafe(Vector(0.0, 0.4))
    val run = tinyRun(affine)
    val metadata =
      MotionNiftiMetadata(
        path = path,
        dims = Vector(2, 1, 2, 2),
        voxelSize = Vector(2.0, 3.0, 4.0),
        affine = Some(affine),
        repetitionTime = Some(1.5),
        acquisitionTiming = Some(AcquisitionTiming.Slice(timing))
      )

    MotionNifti.write(path, run, Some(metadata)).fold(err => fail(err.message), identity)
    val loaded = MotionNifti.read(path).fold(err => fail(err.message), identity)

    assertEquals(loaded.metadata.path, path)
    assertEquals(loaded.metadata.dims, Vector(2, 1, 2, 2))
    assertEquals(loaded.metadata.voxelSize, Vector(2.0, 3.0, 4.0))
    assertEqualsDouble(loaded.metadata.repetitionTime.getOrElse(Double.NaN), 1.5, 1e-12)
    assertEquals(loaded.metadata.affine.map(_.toRows), Some(affine.toRows))
    loaded.metadata.acquisitionTiming match
      case Some(AcquisitionTiming.Slice(actual)) =>
        assertEquals(actual.offsetSeconds, Vector(0.0, 0.4))
      case other =>
        fail(s"expected slice timing, got $other")
    assertEqualsDouble(loaded.run.valueAtCanonicalOrdinal(0), 0.25, 1e-12)
    assertEqualsDouble(loaded.run.valueAtCanonicalOrdinal(7), 7.25, 1e-12)
  }

  test("NIfTI metadata adapter reports invalid sidecars") {
    val dir = Files.createTempDirectory("scalafim-motion-bad-sidecar")
    val path = dir.resolve("run.nii")
    writeFloat32Nifti(path, Vector(2, 1, 1, 2), Vector(1.0, 2.0, 3.0, 4.0))
    write(MotionNifti.defaultSidecar(path), """{"RepetitionTime":-1.0}""")

    val error = MotionNifti.read(path).left.getOrElse(fail("expected invalid sidecar"))
    assert(error.message.contains("RepetitionTime"))
  }

  test("BIDS adapter discovers raw scans with TR and slice timing sidecars") {
    val root = Files.createTempDirectory("scalafim-motion-bids")
    write(root.resolve("dataset_description.json"), """{"Name":"Motion Fixture","BIDSVersion":"1.10.0"}""")
    write(root.resolve("participants.tsv"), "participant_id\nsub-01\n")
    val func = root.resolve("sub-01/func")
    Files.createDirectories(func)
    Files.write(func.resolve("sub-01_task-rest_run-01_bold.nii.gz"), Array.emptyByteArray)
    write(
      func.resolve("sub-01_task-rest_run-01_bold.json"),
      """{"RepetitionTime":2.0,"SliceTiming":[0.0,0.5]}"""
    )
    val deriv = root.resolve("derivatives/fmriprep/sub-01/func")
    Files.createDirectories(deriv)
    write(root.resolve("derivatives/fmriprep/dataset_description.json"), """{"Name":"fMRIPrep","BIDSVersion":"1.10.0"}""")
    Files.write(deriv.resolve("sub-01_task-rest_run-01_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz"), Array.emptyByteArray)
    write(
      deriv.resolve("sub-01_task-rest_run-01_space-MNI152NLin2009cAsym_desc-preproc_bold.json"),
      """{"RepetitionTime":2.0}"""
    )

    val project = MotionBids.loadProject(root).fold(err => fail(err.message), identity)
    val scans =
      MotionBids
        .rawScans(project, subid = "01", task = "rest", run = "01")
        .fold(err => fail(err.message), identity)

    assertEquals(scans.length, 1)
    val scan = scans.head
    assertEquals(scan.path, func.resolve("sub-01_task-rest_run-01_bold.nii.gz").toAbsolutePath.normalize())
    assertEqualsDouble(scan.repetitionTimeSeconds.getOrElse(Double.NaN), 2.0, 1e-12)
    scan.acquisitionTiming match
      case AcquisitionTiming.Slice(timing) =>
        assertEquals(timing.offsetSeconds, Vector(0.0, 0.5))
      case other =>
        fail(s"expected slice timing, got $other")
    assertEquals(scan.plan().acquisitionTiming, scan.acquisitionTiming)

    val preproc =
      MotionBids
        .preprocScans(project, subid = "01", task = "rest", run = "01", space = "MNI152NLin2009cAsym")
        .fold(err => fail(err.message), identity)
    assertEquals(preproc.length, 1)
    assertEquals(preproc.head.repetitionTimeSeconds, Some(2.0))
    assertEquals(preproc.head.acquisitionTiming, AcquisitionTiming.Volume)
  }

  test("BIDS adapter rejects structurally invalid project identities") {
    val root = Files.createTempDirectory("scalafim-motion-bids-invalid")
    write(root.resolve("dataset_description.json"), """{"Name":"Invalid Motion Fixture","BIDSVersion":"1.10.0"}""")
    write(root.resolve("participants.tsv"), "participant_id\nsub-01\n")
    Files.createDirectories(root.resolve("sub-01/func"))
    Files.write(root.resolve("sub-01/func/sub-01_bold.nii.gz"), Array.emptyByteArray)

    val error = MotionBids.loadProject(root).left.getOrElse(fail("expected strict BIDS validation failure"))
    assert(error.message.contains("MissingRequiredField"), clues(error.message))
    assert(error.message.contains("sub-01/func/sub-01_bold.nii.gz"), clues(error.message))
  }

  test("report writer serializes motion, matrices, and summary bundle") {
    val trace =
      MotionTrace
        .unsafe(Vector(RigidPose.identity, RigidPose.unsafe(1.0, 0.0, 0.0, 0.0, 0.0, 0.0)))
    val estimate =
      MotionEstimate(
        trace = trace,
        diagnostics = Vector(
          FrameFitDiagnostics(1.0, 1.0, iterations = 0, overlap = 1.0, restarted = false, converged = true),
          FrameFitDiagnostics(2.0, 0.5, iterations = 3, overlap = 0.9, restarted = true, converged = true)
        ),
        control = MotionControl.default
    )
    val result = MotionCorrectionResult.estimateOnly(estimate, MotionPlan.default)
    val texts = MotionReportWriter.render(result)

    assert(texts.motionTsv.startsWith("frame\ttx\tty\ttz\trx\try\trz\tfd\t"))
    assert(texts.motionTsv.contains("1\t1.0\t0.0\t0.0\t0.0\t0.0\t0.0\t1.0\tNA\tNA\t2.0\t0.5\t3\t0.9\ttrue\ttrue\tfalse"))
    assert(texts.matricesCsv.contains("1,0,1.0,0.0,0.0,1.0"))
    assert(texts.summaryCsv.contains("n_volumes,2"))
    assert(texts.summaryCsv.contains("mean_fd,0.5"))

    val outDir = Files.createTempDirectory("scalafim-motion-report")
    val paths = MotionReportWriter.writeBundle(outDir, "sub-01_task-rest", result).fold(err => fail(err.message), identity)
    assert(Files.isRegularFile(paths.motionTsv))
    assert(Files.isRegularFile(paths.matricesCsv))
    assert(Files.isRegularFile(paths.summaryCsv))
    assertEquals(paths.summary.fdMean, Some(0.5))
  }

  test("CLI parser returns typed commands and rejects malformed flags") {
    MotionCli.parse(Vector("estimate", "--input", "run.nii", "--output-prefix", "out/sub-01")) match
      case Right(MotionCommand.Estimate(input, outputPrefix, plan)) =>
        assertEquals(input, Path.of("run.nii"))
        assertEquals(outputPrefix, Path.of("out/sub-01"))
        assertEquals(plan.engine, MotionEngine.RigidRobust)
      case other =>
        fail(s"expected estimate command, got $other")

    MotionCli.parse(Vector("apply", "--input", "run.nii", "--motion", "motion.tsv", "--output", "corrected.nii")) match
      case Right(MotionCommand.Apply(input, motionTsv, output, control)) =>
        assertEquals(input, Path.of("run.nii"))
        assertEquals(motionTsv, Path.of("motion.tsv"))
        assertEquals(output, Path.of("corrected.nii"))
        assertEquals(control, ApplyControl.linear)
      case other =>
        fail(s"expected apply command, got $other")

    MotionCli.parse(Vector("run", "--input", "run.nii", "--output-prefix", "out/sub-01")) match
      case Right(MotionCommand.Run(_, _, plan, applyControl)) =>
        assertEquals(plan.reference, ReferenceStrategy.Middle)
        assertEquals(applyControl, ApplyControl.linear)
      case other =>
        fail(s"expected run command, got $other")

    MotionCli.parse(Vector("report", "--motion", "motion.tsv", "--output-dir", "report", "--prefix", "sub-01")) match
      case Right(MotionCommand.Report(motionTsv, outputDir, prefix)) =>
        assertEquals(motionTsv, Path.of("motion.tsv"))
        assertEquals(outputDir, Path.of("report"))
        assertEquals(prefix, "sub-01")
      case other =>
        fail(s"expected report command, got $other")

    val duplicate = MotionCli.parse(Vector("estimate", "--input", "a.nii", "--input", "b.nii", "--output-prefix", "out"))
    assert(duplicate.left.exists(_.message.contains("duplicate flag")))
    val missing = MotionCli.parse(Vector("report", "--motion", "motion.tsv"))
    assert(missing.left.exists(_.message.contains("output-dir")))
  }

  test("CLI runner executes estimate, apply, run, and report commands") {
    val dir = Files.createTempDirectory("scalafim-motion-cli-run")
    val input = dir.resolve("run.nii")
    MotionNifti.write(input, estimatorRun(), None).fold(err => fail(err.message), identity)

    val estimate =
      MotionCli
        .run(Vector("estimate", "--input", input.toString, "--output-prefix", dir.resolve("estimate/sub-01").toString))
        .fold(err => fail(err.message), identity)
    val motionTsv = dir.resolve("estimate/sub-01_motion.tsv")
    assert(estimate.outputs.contains(motionTsv))
    assert(Files.isRegularFile(motionTsv))
    assert(Files.isRegularFile(dir.resolve("estimate/sub-01_matrices.csv")))
    assert(Files.isRegularFile(dir.resolve("estimate/sub-01_summary.csv")))

    val applyOut = dir.resolve("corrected.nii")
    val applied =
      MotionCli
        .run(Vector("apply", "--input", input.toString, "--motion", motionTsv.toString, "--output", applyOut.toString))
        .fold(err => fail(err.message), identity)
    assert(applied.outputs.contains(applyOut))
    assert(Files.isRegularFile(applyOut))
    assert(Files.isRegularFile(MotionNifti.defaultSidecar(applyOut)))
    assertEquals(MotionNifti.read(applyOut).fold(err => fail(err.message), _.run.nVolumes), 2)

    val reported =
      MotionCli
        .run(Vector("report", "--motion", motionTsv.toString, "--output-dir", dir.resolve("report").toString, "--prefix", "sub-01"))
        .fold(err => fail(err.message), identity)
    assertEquals(reported.outputs.length, 3)
    assert(Files.isRegularFile(dir.resolve("report/sub-01_motion.tsv")))
    assert(Files.isRegularFile(dir.resolve("report/sub-01_matrices.csv")))
    assert(Files.isRegularFile(dir.resolve("report/sub-01_summary.csv")))

    val run =
      MotionCli
        .run(Vector("run", "--input", input.toString, "--output-prefix", dir.resolve("run/sub-01").toString))
        .fold(err => fail(err.message), identity)
    assert(run.outputs.contains(dir.resolve("run/sub-01_corrected.nii")))
    assert(run.outputs.contains(dir.resolve("run/sub-01_motion.tsv")))
    assert(Files.isRegularFile(dir.resolve("run/sub-01_corrected.nii")))
    assert(Files.isRegularFile(dir.resolve("run/sub-01_motion.tsv")))
  }

  private def write(path: Path, text: String): Unit =
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)

  private def tinyRun(affine: DMat): NeuroVec[Double] =
    val spatial =
      NeuroSpace(
        Vector(2, 1, 2),
        spacing = Some(Vector(2.0, 3.0, 4.0)),
        origin = Some(Vector(10.0, 20.0, 30.0)),
        trans = Some(affine)
      )
    val data = PrimitiveBuffers.tabulate[Double](2 * 1 * 2 * 2)(i => i.toDouble + 0.25)
    NeuroVec.copyFromCanonicalArray(data, spatial.addDim(2, Some(Axis.Time)), "motion-io-fixture")

  private def estimatorRun(): NeuroVec[Double] =
    val dims = Vector(7, 5, 5)
    val nxyz = dims.product
    val space = NeuroSpace(dims)
    val frame =
      PrimitiveBuffers.tabulate[Double](nxyz) { lin =>
        val voxel = space.indexToVoxel3D(lin)
        val dx = voxel.x.toDouble - 3.0
        val dy = voxel.y.toDouble - 2.0
        val dz = voxel.z.toDouble - 2.0
        10.0 * math.exp(-(dx * dx / 5.0 + dy * dy / 3.0 + dz * dz / 4.0)) +
          0.4 * voxel.x.toDouble +
          0.2 * voxel.y.toDouble -
          0.15 * voxel.z.toDouble
      }
    val data = PrimitiveBuffers.ofSize[Double](nxyz * 2)
    var lin = 0
    while lin < nxyz do
      var t = 0
      while t < 2 do
        data(lin * 2 + t) = frame(lin)
        t += 1
      lin += 1
    NeuroVec.copyFromCanonicalArray(data, space.addDim(2, Some(Axis.Time)), "motion-cli-estimate-fixture")

  private def writeFloat32Nifti(path: Path, dims: Vector[Int], values: Vector[Double]): Unit =
    Files.createDirectories(path.getParent)
    val header = Array.fill[Byte](348)(0)
    val bb = ByteBuffer.wrap(header).order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(0, 348)
    bb.putShort(40, dims.length.toShort)
    var i = 0
    while i < 8 do
      val dim = if i < dims.length then dims(i) else 1
      bb.putShort(42 + i * 2, dim.toShort)
      i += 1
    bb.putShort(70, 16.toShort)
    bb.putShort(72, 32.toShort)
    bb.putFloat(80, 1.0f)
    bb.putFloat(84, 1.0f)
    bb.putFloat(88, 1.0f)
    bb.putFloat(108, 352.0f)
    bb.putFloat(112, 1.0f)
    bb.putFloat(116, 0.0f)
    bb.put(344, 'n'.toByte)
    bb.put(345, '+'.toByte)
    bb.put(346, '1'.toByte)
    bb.put(347, 0.toByte)

    val data = ByteBuffer.allocate(values.length * 4).order(ByteOrder.LITTLE_ENDIAN)
    values.foreach(value => data.putFloat(value.toFloat))
    Files.write(path, header ++ Array.fill[Byte](4)(0) ++ data.array())
