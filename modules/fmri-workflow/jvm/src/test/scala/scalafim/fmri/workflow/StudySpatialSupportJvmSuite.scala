package scalafim.fmri.workflow

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path}
import java.util.zip.GZIPOutputStream
import scala.jdk.CollectionConverters.*
import scalafim.dataset.*
import scalafim.dataset.io.{NiftiResponseBlockSource, NiftiStagingCache}
import scalafim.image.{Axis, NeuroSpace, NeuroVec, NeuroVol, PrimitiveBuffers}
import scalafim.image.io.{Nifti, NiftiWriteOptions, NiftiCoordinateSystem, NiftiSpatialUnits}

class StudySpatialSupportJvmSuite extends munit.FunSuite:
  // Parent voxel (40,60,40) of the ds000001 MNI2009c 2 mm grid.
  private val space = NeuroSpace(Vector(3, 2, 1), spacing = Some(Vector(2.0, 2.0, 2.0)),
    origin = Some(Vector(-16.5, -12.5, 1.5)))
  private val options = NiftiWriteOptions(NiftiCoordinateSystem.Mni152, NiftiSpatialUnits.Millimeters)
  private val limits = StudySpatialLimits.make(6, 2).toOption.get
  private def withRoot(f: Path => Unit): Unit =
    val root = Files.createTempDirectory("scalafim-mni-support-")
    try f(root)
    finally
      val paths = Files.walk(root)
      try paths.iterator().asScala.toVector.reverse.foreach(path => { val _ = Files.deleteIfExists(path) })
      finally paths.close()

  private def write(root: Path, name: String, values: Array[Double], geometry: NeuroSpace = space): Path =
    Nifti.writeVol(root.resolve(name), NeuroVol.fromLinear(PrimitiveBuffers.fromArray(values), geometry, name), options)

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
        assertEquals(Nifti.readHeader(path).space, space)
      }
      val study = catalog(unit("02", Vector(c, compress(d))), unit("01", Vector(a, b)))
      val cache = NiftiStagingCache.unsafe(root.resolve("cache"))
      val result = StudySpatialSupportJvm.compile(study, limits, Some(cache)).fold(e => fail(e.message), identity)
      assertEquals(result.selection.voxelCoords.map(v => space.gridToIndex3D(v)), Vector(0, 2, 5))
      assertEquals(result.units.map(_.selectedVoxels), Vector(4, 3))
      assertEquals(result.units.flatMap(_.masks.map(_.nonfiniteVoxels)), Vector(0, 0, 0, 1))
      assertEquals(result.reads.requests, 12L)
      assertEquals(result.reads.largestReadVoxels, 2)
      assert(result.units.forall(_.unit.runs.forall(run => !Files.exists(Path.of(java.net.URI.create(run.bold.location.value))))))
      val response = write(root, "response.nii", Array(0, 10, 20, 30, 40, 50).map(_.toDouble))
      val source = NiftiResponseBlockSource.open(response).toOption.get
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
      assert(StudySpatialSupportJvm.compile(study, limits).isLeft)
      assertEquals(StudySpatialSupportJvm.compile(study, limits,
        Some(NiftiStagingCache.unsafe(root.resolve("cache")))).toOption.get.selection.size, 6)
    }
  }

  test("wrong affine, multiple mask volumes and unsupported URI are refused") {
    withRoot { root =>
      val shifted = NeuroSpace(Vector(3, 2, 1), spacing = Some(Vector(2.0, 2.0, 2.0)),
        origin = Some(Vector(-14.5, -12.5, 1.5)))
      val wrong = write(root, "shifted.nii", Array.fill(6)(1.0), shifted)
      val multi = Nifti.writeVec(root.resolve("multiple.nii"), NeuroVec.fromLinear(
        PrimitiveBuffers.fromArray(Array.fill(12)(1.0)), space.addDim(2, Some(Axis.Time)), "multiple"), options)
      Vector(wrong, multi).foreach { path =>
        val error = StudySpatialSupportJvm.compile(catalog(unit("01", Vector(path))), limits).left.toOption.get
        assert(error.message.contains("mask header"))
      }
      val missing = unit("01", Vector(root.resolve("missing.nii")))
      assert(StudySpatialSupportJvm.compile(catalog(missing), StudySpatialLimits.make(5, 2).toOption.get)
        .left.toOption.get.message.contains("limit"))
      val remote = withMask(missing, UnitMask.Single(WorkflowArtifactRef.unsafe[MaskImageResource]("https://example.invalid/mask.nii")))
      assert(StudySpatialSupportJvm.compile(catalog(remote), limits).left.toOption.get.message.contains("local file URI"))
    }
  }

  test("cancellation before opening is propagated without being converted to a storage failure") {
    withRoot { root =>
      var cancelled = false
      var preserved = false
      Thread.currentThread().interrupt()
      try
        val _ = StudySpatialSupportJvm.compile(catalog(unit("01", Vector(root.resolve("missing.nii")))), limits)
      catch case _: InterruptedException =>
        cancelled = true
        preserved = Thread.currentThread().isInterrupted
      finally { val _ = Thread.interrupted() }
      assert(cancelled, "cancellation must escape the typed storage result")
      assert(preserved, "cancellation must preserve the caller's interrupt flag")
    }
  }
