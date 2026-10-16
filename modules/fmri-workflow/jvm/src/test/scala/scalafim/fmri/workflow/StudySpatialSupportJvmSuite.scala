package scalafim.fmri.workflow

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path}
import java.util.zip.GZIPOutputStream
import scala.jdk.CollectionConverters.*
import scalafim.dataset.*
import scalafim.dataset.io.{NiftiResponseBlockSource, NiftiStagingCache}
import scalafim.image.{SampleSpaces, SomeSampleSpace, SomeScalarVolume, SomeScalarSeries}
import scalafim.image.SampleSpaces.*
import scalafim.image.world.{WorldSpace, SpaceEvidence}
import scalafim.image.io.Nifti
import image4s.nifti.{NiftiWriteOptions, NiftiCoordinateSystem}

class StudySpatialSupportJvmSuite extends munit.FunSuite:
  // Parent voxel (40,60,40) of the ds000001 MNI2009c 2 mm grid.
  private val world = WorldSpace.template("MNI152NLin2009cAsym").toOption.get
  private val evidence = SpaceEvidence(bidsSpace = Some("MNI152NLin2009cAsym"))
  private def geometry(origin: Double): SomeSampleSpace = SampleSpaces.inWorld(
    SampleSpaces(Vector(3, 2, 1), spacing = Some(Vector(2.0, 2.0, 2.0)),
      origin = Some(Vector(origin, -12.5, 1.5))), world).toOption.get
  private val space = geometry(-16.5)
  private val options = NiftiWriteOptions.default.withCoordinateSystem(NiftiCoordinateSystem.Mni152)
  private val limits = StudySpatialLimits.make(6, 2).toOption.get
  private def withRoot(f: Path => Unit): Unit =
    val root = Files.createTempDirectory("scalafim-mni-support-")
    try f(root)
    finally
      val paths = Files.walk(root)
      try paths.iterator().asScala.toVector.reverse.foreach(path => { val _ = Files.deleteIfExists(path) })
      finally paths.close()

  private def write(root: Path, name: String, values: Array[Double], geometry: SomeSampleSpace = space): Path =
    val path = root.resolve(name)
    Nifti.writeVolume(path, SomeScalarVolume.unsafeCopyFromCanonicalArray(values, geometry, name), options)
      .fold(error => fail(error.message), _ => path)

  private def unit(id: String, masks: Vector[Path]): FirstLevelUnit =
    val runs = masks.indices.map { i =>
      RunInput.unsafe(RunId(s"0${i + 1}"), RepetitionTime.unsafe(1.0), 96,
        WorkflowArtifactRef.unsafe[BoldImageResource](masks(i).resolveSibling(s"absent-$id-$i-bold.nii").toUri.toString),
        WorkflowArtifactRef.unsafe[EventsTableResource](masks(i).resolveSibling(s"absent-$id-$i-events.tsv").toUri.toString))
    }.toVector
    FirstLevelUnit.unsafe(FirstLevelUnitId.unsafe(id), SubjectId(id), None, TaskId("demo"),
      SpaceId("MNI152NLin2009cAsym"), DatasetShape.unsafe(space, 96 * masks.size), runs,
      UnitMask.unsafeIntersection(runs.zip(masks).map((run, path) =>
        run.id -> WorkflowArtifactRef.unsafe[MaskImageResource](path.toUri.toString))))

  private def withMask(unit: FirstLevelUnit, mask: UnitMask): FirstLevelUnit =
    FirstLevelUnit.unsafe(unit.id, unit.subject, unit.session, unit.task, unit.space, unit.shape,
      unit.runs, mask, unit.acquisition, unit.echo, unit.resolution, unit.pipeline)

  private def catalog(units: FirstLevelUnit*): StudyCatalog =
    StudyCatalog.make(DatasetId("mni-support"), units.toVector).toOption.get

  private def compress(path: Path): Path =
    val target = path.resolveSibling(path.getFileName.toString + ".gz")
    val stream = new GZIPOutputStream(Files.newOutputStream(target))
    try stream.write(Files.readAllBytes(path)) finally stream.close()
    target

  test("MNI mask files yield the hand-derived intersection and checked file-backed voxel order") {
    withRoot { root =>
      val a = write(root, "a.nii", Array(1, 1, 1, 1, 0, 1).map(_.toDouble))
      val b = write(root, "b.nii", Array(1, 0, 1, 1, 1, 1).map(_.toDouble))
      val c = write(root, "c.nii", Array(-1, 0, 2, 0, 1, 1).map(_.toDouble))
      val d = write(root, "d.nii", Array(1.0, Double.NaN, 1.0, 0.0, 0.0, 1.0))
      Vector(a, b, c, d).foreach { path =>
        val bytes = ByteBuffer.wrap(Files.readAllBytes(path)).order(ByteOrder.LITTLE_ENDIAN)
        assertEquals(bytes.getShort(252).toInt, 0) // qform intentionally unset
        assertEquals(bytes.getShort(254).toInt, 4) // MNI sform
        assertEquals(bytes.get(123).toInt & 7, 2) // millimeters
        Vector(80, 84, 88).foreach(offset => assertEqualsDouble(bytes.getFloat(offset).toDouble, 2.0, 0.0))
        Vector(292 -> -16.5, 308 -> -12.5, 324 -> 1.5).foreach { (offset, expected) =>
          assertEqualsDouble(bytes.getFloat(offset).toDouble, expected, 0.0)
        }
        val placed = Nifti.readHeader(path).left.map(_.message).flatMap(_.spaceIn(evidence).left.map(_.message))
          .fold(fail(_), identity)
        val headerShape = DatasetShape.unsafe(placed.spatialSpace, 1)
        assert(image4s.geometry.Grid.exactCongruence(headerShape.grid, DatasetShape.unsafe(space, 1).grid).isRight)
      }
      val study = catalog(unit("02", Vector(c, compress(d))), unit("01", Vector(a, b)))
      val cache = NiftiStagingCache.unsafe(root.resolve("cache"))
      val result = StudySpatialSupportJvm.compile(study, limits, _ => Right(evidence), Some(cache)).fold(e => fail(e.message), identity)
      assertEquals(result.selection.ordinals.toVector, Vector(0, 2, 5))
      assertEquals(result.units.map(_.selectedVoxels), Vector(4, 3))
      assertEquals(result.units.flatMap(_.masks.map(_.nonfiniteVoxels)), Vector(0, 0, 0, 1))
      assertEquals(result.reads.requests, 12L)
      assertEquals(result.reads.largestReadVoxels, 2)
      assert(result.units.forall(_.unit.runs.forall(run => !Files.exists(Path.of(java.net.URI.create(run.bold.location.value))))))
      val response = write(root, "response.nii", Array(0, 10, 20, 30, 40, 50).map(_.toDouble))
      // Independent NIfTI-1 disk-order oracle: x is fastest on disk, while
      // the values supplied above and dataset ordinals have z fastest.
      val raw = ByteBuffer.wrap(Files.readAllBytes(response)).order(ByteOrder.LITTLE_ENDIAN)
      assertEquals(raw.getShort(70).toInt, 64) // Float64
      val offset = raw.getFloat(108).toInt
      Vector(0.0, 20.0, 40.0, 10.0, 30.0, 50.0).zipWithIndex.foreach { (expected, index) =>
        assertEqualsDouble(raw.getDouble(offset + index * 8), expected, 0.0)
      }
      val source = NiftiResponseBlockSource.open(response, evidence).toOption.get
      val selection = result.selectionFor(study.units.head).toOption.get
      val block = source.readBlock(DataSelection(voxels = selection)).toOption.get
      assertEquals(block.voxelIndices, Vector(0, 2, 5))
      Vector(0.0, 20.0, 50.0).zipWithIndex.foreach((value, column) =>
        assertEqualsDouble(block.data(0, column), value, 0.0))
    }
  }

  test("compressed masks require an explicit staging cache") {
    withRoot { root =>
      val mask = compress(write(root, "mask.nii", Array.fill(6)(1.0)))
      val study = catalog(unit("01", Vector(mask)))
      assert(StudySpatialSupportJvm.compile(study, limits, _ => Right(evidence)).isLeft)
      assertEquals(StudySpatialSupportJvm.compile(study, limits, _ => Right(evidence),
        Some(NiftiStagingCache.unsafe(root.resolve("cache")))).toOption.get.selection.size, 6)
    }
  }

  test("wrong affine, multiple mask volumes and unsupported URI are refused") {
    withRoot { root =>
      val shifted = geometry(-14.5)
      val wrong = write(root, "shifted.nii", Array.fill(6)(1.0), shifted)
      val multi = root.resolve("multiple.nii")
      Nifti.writeSeries(multi, SomeScalarSeries.unsafeCopyFromCanonicalArray(
        Array.fill(12)(1.0), space.addDim(ProviderAxes.time(2)), "multiple"), options).fold(error => fail(error.message), identity)
      Vector(wrong, multi).foreach { path =>
        val error = StudySpatialSupportJvm.compile(catalog(unit("01", Vector(path))), limits, _ => Right(evidence)).left.toOption.get
        assert(error.message.contains("mask header"))
      }
      val missing = unit("01", Vector(root.resolve("missing.nii")))
      assert(StudySpatialSupportJvm.compile(catalog(missing), StudySpatialLimits.make(5, 2).toOption.get, _ => Right(evidence))
        .left.toOption.get.message.contains("limit"))
      val remote = withMask(missing, UnitMask.Single(WorkflowArtifactRef.unsafe[MaskImageResource]("https://example.invalid/mask.nii")))
      assert(StudySpatialSupportJvm.compile(catalog(remote), limits, _ => Right(evidence)).left.toOption.get.message.contains("local file URI"))
    }
  }

  test("cancellation before opening is propagated without being converted to a storage failure") {
    withRoot { root =>
      var cancelled = false
      var preserved = false
      Thread.currentThread().interrupt()
      try
        val _ = StudySpatialSupportJvm.compile(catalog(unit("01", Vector(root.resolve("missing.nii")))), limits, _ => Right(evidence))
      catch case _: InterruptedException =>
        cancelled = true
        preserved = Thread.currentThread().isInterrupted
      finally { val _ = Thread.interrupted() }
      assert(cancelled, "cancellation must escape the typed storage result")
      assert(preserved, "cancellation must preserve the caller's interrupt flag")
    }
  }
