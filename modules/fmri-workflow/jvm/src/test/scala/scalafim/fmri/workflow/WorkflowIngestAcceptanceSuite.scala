package scalafim.fmri.workflow

import munit.FunSuite
import bids4s.*
import bids4s.io.BidsProjectLoader
import scalafim.dataset.*
import scalafim.dataset.io.NiftiStagingCache
import scalafim.image.{DMat, PrimitiveBuffers, NeuroSpace, NeuroVol}
import scalafim.image.io.Nifti

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.GZIPOutputStream
import scala.jdk.CollectionConverters.*
import scala.util.Using

class WorkflowIngestAcceptanceSuite extends FunSuite:
  test("header-first catalog opens mixed-byte multi-run BOLD lazily with compressed staging") {
    withFixture { root =>
      writeProject(root)
      val projectReport = BidsProjectLoader.loadChecked(root).toOption.get
      val project = projectReport.value
      val recipe = DatasetRecipe.unsafe(
        datasetId = DatasetId("acceptance"),
        project = WorkflowArtifactRef.unsafe[BidsProjectResource](root.toUri.toString.stripSuffix("/")),
        boldQuery = BidsQuery.from(
          filename = Vector("desc-preproc_bold\\.nii(\\.gz)?$"),
          scope = BidsScope.Derivatives,
          pipeline = Some(PipelineName("fmriprep"))
        ).toOption.get,
        maskPolicy = MaskPolicy.IntersectRunMasks,
        confounds = Some(ConfoundSelectionConfig(variables = Vector("motion6")))
      )

      val compilation = BidsStudyCompilerJvm.compileChecked(projectReport, recipe).toOption.get
      val catalog = compilation.catalog
      assertEquals(compilation.issues, Vector.empty)

      assertEquals(catalog.units.length, 1)
      val unit = catalog.units.head
      assertEquals(unit.runs.map(_.id.value), Vector("01", "02"))
      assertEquals(unit.runs.map(_.timepoints), Vector(2, 2))
      assert(unit.runs.forall(_.confounds.nonEmpty))
      assert(unit.mask.isInstanceOf[UnitMask.Intersection])

      val cache = NiftiStagingCache.unsafe(root.resolve("nifti-cache"))
      val opened =
        FirstLevelUnitSource
          .open(unit, staging = Some(cache))
          .fold(error => fail(error.message), identity)
      val block = opened.source.readBlock(
        DataSelection(
          time = TimepointSelection.indices(3, 0, 2),
          voxels = VoxelSelection.All
        )
      ).toOption.get

      assertEquals(opened.source.blockLengths, Vector(2, 2))
      assertEquals(opened.source.voxelDomain.indices, Vector(0, 2))
      assertEquals(block.timepoints, Vector(3, 0, 2))
      assertEquals(block.voxelIndices, Vector(0, 2))
      assertMatrixEquals(
        block.data,
        Vector(
          Vector(24.0, 29.0),
          Vector(3.0, 5.0),
          Vector(4.0, 9.0)
        )
      )

      val cached = Files.list(cache.root)
      try assertEquals(cached.iterator().asScala.count(_.toString.endsWith(".nii")), 1)
      finally cached.close()
    }
  }

  private def writeProject(root: Path): Unit =
    write(root.resolve("dataset_description.json"), """{"Name":"Workflow acceptance","BIDSVersion":"1.10.0","DatasetType":"raw"}""")
    write(root.resolve("participants.tsv"), "participant_id\tage\nsub-01\t30\n")
    write(
      root.resolve("derivatives/fmriprep/dataset_description.json"),
      """{"Name":"fMRIPrep","BIDSVersion":"1.10.0","DatasetType":"derivative"}"""
    )
    val space = NeuroSpace(Vector(2, 2, 1))

    Vector("01", "02").foreach { run =>
      val prefix = s"sub-01_task-demo_run-$run"
      val derivativePrefix = s"${prefix}_space-MNI"
      val func = root.resolve("derivatives/fmriprep/sub-01/func")
      val boldNii = func.resolve(s"${derivativePrefix}_desc-preproc_bold.nii")
      if run == "01" then
        writeRawSeries(
          boldNii,
          datatype = 4,
          bitpix = 16,
          rawValues = Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0),
          byteOrder = ByteOrder.LITTLE_ENDIAN,
          slope = 2.0f,
          intercept = 1.0f
        )
      else
        writeRawSeries(
          boldNii,
          datatype = 16,
          bitpix = 32,
          rawValues = Vector(10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0),
          byteOrder = ByteOrder.BIG_ENDIAN,
          slope = 0.5f,
          intercept = -1.0f
        )

      val boldPath =
        if run == "02" then
          val compressed = boldNii.resolveSibling(boldNii.getFileName.toString + ".gz")
          gzip(boldNii, compressed)
          Files.delete(boldNii)
          compressed
        else boldNii
      val sidecarName = boldPath.getFileName.toString.replace(".nii.gz", ".json").replace(".nii", ".json")
      write(boldPath.resolveSibling(sidecarName), """{"RepetitionTime":2.0}""")

      val maskValues =
        if run == "01" then Array(1.0, 1.0, 1.0, 0.0)
        else Array(1.0, 0.0, 1.0, 1.0)
      Nifti.writeVol(
        func.resolve(s"${derivativePrefix}_desc-brain_mask.nii"),
        NeuroVol.copyFromCanonicalArray(PrimitiveBuffers.fromArray(maskValues), space, s"mask-$run")
      )
      write(
        func.resolve(s"${prefix}_desc-confounds_timeseries.tsv"),
        "trans_x\ttrans_y\n0\t0\n0\t0\n"
      )
      write(
        root.resolve(s"sub-01/func/${prefix}_events.tsv"),
        "onset\tduration\ttrial_type\n0\t1\tstim\n"
      )
    }

  private def writeRawSeries(
      path: Path,
      datatype: Int,
      bitpix: Int,
      rawValues: Vector[Double],
      byteOrder: ByteOrder,
      slope: Float,
      intercept: Float
  ): Unit =
    Files.createDirectories(path.getParent)
    val bytesPerValue = bitpix / 8
    val bytes = new Array[Byte](352 + rawValues.length * bytesPerValue)
    val buffer = ByteBuffer.wrap(bytes).order(byteOrder)
    buffer.putInt(0, 348)
    buffer.putShort(40, 4.toShort)
    buffer.putShort(42, 2.toShort)
    buffer.putShort(44, 2.toShort)
    buffer.putShort(46, 1.toShort)
    buffer.putShort(48, 2.toShort)
    buffer.putShort(70, datatype.toShort)
    buffer.putShort(72, bitpix.toShort)
    buffer.putFloat(76, 1.0f)
    buffer.putFloat(80, 1.0f)
    buffer.putFloat(84, 1.0f)
    buffer.putFloat(88, 1.0f)
    buffer.putFloat(92, 2.0f)
    buffer.putFloat(108, 352.0f)
    buffer.putFloat(112, slope)
    buffer.putFloat(116, intercept)
    buffer.putShort(254, 1.toShort)
    var column = 0
    while column < 4 do
      buffer.putFloat(280 + column * 4, if column == 0 then 1.0f else 0.0f)
      buffer.putFloat(296 + column * 4, if column == 1 then 1.0f else 0.0f)
      buffer.putFloat(312 + column * 4, if column == 2 then 1.0f else 0.0f)
      column += 1
    val magic = "n+1".getBytes(StandardCharsets.US_ASCII)
    buffer.put(344, magic(0))
    buffer.put(345, magic(1))
    buffer.put(346, magic(2))
    buffer.position(352)
    rawValues.foreach { value =>
      datatype match
        case 4  => buffer.putShort(value.toInt.toShort)
        case 16 => buffer.putFloat(value.toFloat)
        case other => throw new IllegalArgumentException(s"unsupported acceptance fixture datatype $other")
    }
    Files.write(path, bytes)

  private def gzip(source: Path, target: Path): Unit =
    Using.Manager { use =>
      val input = use(Files.newInputStream(source))
      val output = use(new GZIPOutputStream(Files.newOutputStream(target)))
      input.transferTo(output)
    }.fold(error => throw error, _ => ())

  private def write(path: Path, value: String): Unit =
    Files.createDirectories(path.getParent)
    Files.writeString(path, value, StandardCharsets.UTF_8)

  private def assertMatrixEquals(actual: DMat, expected: Vector[Vector[Double]]): Unit =
    assertEquals(actual.rows, expected.length)
    assertEquals(actual.cols, expected.head.length)
    var row = 0
    while row < actual.rows do
      var column = 0
      while column < actual.cols do
        assertEqualsDouble(actual(row, column), expected(row)(column), 1e-6)
        column += 1
      row += 1

  private def withFixture[A](body: Path => A): A =
    val root = Files.createTempDirectory("scalafim-workflow-ingest-acceptance-")
    try body(root)
    finally
      val files = Files.walk(root)
      try files.iterator().asScala.toVector.reverse.foreach(Files.deleteIfExists)
      finally files.close()
