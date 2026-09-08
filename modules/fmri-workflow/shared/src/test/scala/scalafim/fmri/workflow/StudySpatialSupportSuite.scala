package scalafim.fmri.workflow

import scalafim.dataset.*
import scalafim.image.{DMat, NeuroSpace}

class StudySpatialSupportSuite extends munit.FunSuite:
  // Explicit MNI2009c 2 mm crop, x-fastest legacy voxel order.
  private val space = NeuroSpace(Vector(3, 2, 1), spacing = Some(Vector(2.0, 2.0, 2.0)),
    origin = Some(Vector(-16.5, -12.5, 1.5)))
  private val limits = StudySpatialLimits.make(6, 2).toOption.get
  private def unit(id: String, geometry: NeuroSpace = space, namedSpace: String = "MNI152NLin2009cAsym"): FirstLevelUnit =
    val runs = Vector("01", "02").map(run => RunInput.unsafe(RunId(run), RepetitionTime.unsafe(1), 96,
      WorkflowArtifactRef.unsafe[BoldImageResource](s"file:///$id/$run-bold.nii"),
      WorkflowArtifactRef.unsafe[EventsTableResource](s"file:///$id/$run-events.tsv")))
    FirstLevelUnit.unsafe(FirstLevelUnitId.unsafe(id), SubjectId(id), None, TaskId("demo"),
      SpaceId(namedSpace), DatasetShape.unsafe(geometry, 192), runs,
      UnitMask.unsafeIntersection(runs.reverse.map(run => run.id ->
        WorkflowArtifactRef.unsafe[MaskImageResource](s"file:///$id/${run.id.value}-mask.nii"))))
  private def withMask(unit: FirstLevelUnit, mask: UnitMask): FirstLevelUnit =
    FirstLevelUnit.unsafe(unit.id, unit.subject, unit.session, unit.task, unit.space, unit.shape,
      unit.runs, mask, unit.acquisition, unit.echo, unit.resolution, unit.pipeline)

  private def catalog(units: Vector[FirstLevelUnit]): StudyCatalog =
    StudyCatalog.make(DatasetId("mni-support"), units).toOption.get
  private val study = catalog(Vector(unit("02"), unit("01")))
  private val values = Map(
    "file:///01/01-mask.nii" -> Vector(1.0, 1.0, 1.0, 1.0, 0.0, 1.0),
    "file:///01/02-mask.nii" -> Vector(1.0, 0.0, 1.0, 1.0, 1.0, 1.0),
    "file:///02/01-mask.nii" -> Vector(-1.0, 0.0, 2.0, 0.0, 1.0, 1.0),
    "file:///02/02-mask.nii" -> Vector(1.0, Double.NaN, 1.0, 0.0, 0.0, 1.0))

  private final class Reader(data: Vector[Double], width: Int = 2,
      geometry: NeuroSpace = space, frames: Int = 1, reverse: Boolean = false,
      sparse: Boolean = false) extends ResponseBlockSource:
    val shape = DatasetShape.unsafe(geometry, frames)
    val voxelDomain = if sparse then
      VoxelDomain.fromMask(scalafim.image.Mask.fromIndices(geometry, Array(0)), shape).toOption.get
    else VoxelDomain.fullUnsafe(shape)
    val metadata = DatasetMetadata.Empty
    val requests = scala.collection.mutable.ArrayBuffer.empty[Vector[Int]]
    def readResolved(selection: ResolvedDataSelection): Either[DatasetError, FmriSeries] =
      assert(selection.nVoxels <= width, "oversized mask read")
      assertEquals(selection.timepointIndices.map(_.value), Vector(0))
      val indices = selection.voxelIndexValues.map(_.value)
      requests += indices
      val returned = if reverse then indices.reverse else indices
      FmriSeries.fromIntIndices(DMat.fromRows(Vector(returned.map(data))), returned, Vector(0), shape)

  test("strict intersection reads bounded windows in canonical source order and preserves MNI coordinates") {
    val opened = scala.collection.mutable.ArrayBuffer.empty[String]
    val readers = scala.collection.mutable.ArrayBuffer.empty[Reader]
    val result = StudySpatialSupport.compile(study, limits) { artifact =>
      if readers.nonEmpty then assertEquals(readers.last.requests.flatten.toVector, (0 until 6).toVector)
      opened += artifact.location.value
      val reader = new Reader(values(artifact.location.value))
      readers += reader
      Right(reader)
    }.fold(error => fail(error.message), identity)
    assertEquals(opened.toVector, Vector("file:///01/01-mask.nii", "file:///01/02-mask.nii",
      "file:///02/01-mask.nii", "file:///02/02-mask.nii"))
    assertEquals(result.selection.voxelCoords.map(v => Vector(v.x, v.y, v.z)),
      Vector(Vector(0, 0, 0), Vector(2, 0, 0), Vector(2, 1, 0)))
    assertEquals(result.selection.voxelCoords.map(v => space.gridToIndex3D(v)), Vector(0, 2, 5))
    assertEquals(result.selectionFor(study.units.head).toOption.get.resolve(6).toOption.get.map(_.value), Vector(0, 2, 5))
    assertEquals(result.units.map(_.selectedVoxels), Vector(4, 3))
    assertEquals(result.units.flatMap(_.masks.map(_.selectedVoxels)), Vector(5, 5, 4, 3))
    assertEquals(result.units.flatMap(_.masks.map(_.nonfiniteVoxels)), Vector(0, 0, 0, 1))
    assertEquals(result.units.map(_.unit.runIds.map(_.value)), Vector.fill(2)(Vector("01", "02")))
    assertEquals(result.reads.requests, 12L)
    assertEquals(result.reads.maskSourcesOpened, 4L)
    assertEquals(result.reads.largestReadVoxels, 2)
    assert(result.selectionFor(unit("03")).isLeft)
    assert(result.selectionFor(withMask(study.units.head, UnitMask.Single(
      WorkflowArtifactRef.unsafe[MaskImageResource]("file:///changed.nii")))).isLeft)
  }

  test("single common unit masks retain all run identities and are not deduplicated across units") {
    val artifact = WorkflowArtifactRef.unsafe[MaskImageResource]("file:///shared.nii")
    val shared = catalog(study.units.map(value => withMask(value, UnitMask.Single(artifact))))
    var count = 0
    val result = StudySpatialSupport.compile(shared, limits) { _ =>
      count += 1
      Right(new Reader(Vector(0, 1, 0, 0, 0, 0).map(_.toDouble)))
    }.toOption.get
    assertEquals(count, 2)
    assertEquals(result.selection.size, 1)
    assertEquals(result.units.map(_.masks.head.runs.map(_.value)), Vector.fill(2)(Vector("01", "02")))
    assertEquals(result.units.map(_.masks.size), Vector(1, 1))
  }

  test("catalog shape, named-space, empty selection and foreign run masks fail before any opener") {
    val shifted = NeuroSpace(Vector(3, 2, 1), spacing = Some(Vector(2.0, 2.0, 2.0)),
      origin = Some(Vector(-14.5, -12.5, 1.5)))
    val foreign = withMask(study.units.head, UnitMask.unsafeIntersection(Vector(RunId("foreign") ->
      WorkflowArtifactRef.unsafe[MaskImageResource]("file:///foreign.nii"))))
    val invalid = Vector(StudyCatalog.empty(DatasetId("empty")),
      catalog(Vector(unit("01"), unit("02", shifted))),
      catalog(Vector(unit("01"), unit("02", namedSpace = "other-template"))), catalog(Vector(foreign)))
    invalid.foreach { value =>
      var calls = 0
      assert(StudySpatialSupport.compile(value, limits) { _ => calls += 1; Right(new Reader(Vector.fill(6)(1))) }.isLeft)
      assertEquals(calls, 0)
    }
    assert(StudySpatialLimits.make(0, 1).isLeft)
    assert(StudySpatialLimits.make(1, 0).isLeft)
    var calls = 0
    assert(StudySpatialSupport.compile(study, StudySpatialLimits.make(5, 2).toOption.get) { _ =>
      calls += 1; Right(new Reader(Vector.fill(6)(1)))
    }.isLeft)
    assertEquals(calls, 0)
  }

  test("wrong volume count, geometry, active-mask domains and reordered read receipts are refused") {
    val shifted = NeuroSpace(Vector(3, 2, 1), spacing = Some(Vector(2.0, 2.0, 2.0)),
      origin = Some(Vector(-14.5, -12.5, 1.5)))
    Vector(new Reader(Vector.fill(6)(1), frames = 2), new Reader(Vector.fill(6)(1), geometry = shifted),
      new Reader(Vector.fill(6)(1), sparse = true), new Reader(Vector.fill(6)(1), reverse = true)).foreach { reader =>
      assert(StudySpatialSupport.compile(study, limits)(_ => Right(reader)).isLeft)
    }
    assert(StudySpatialSupport.compile(study, limits)(_ => Left(DatasetError.StorageFailure("unavailable")))
      .left.toOption.get.message.contains("unavailable"))
  }

  test("zero and nonfinite support never become filled voxels; empty intersection is refused") {
    assert(StudySpatialSupport.compile(study, limits)(_ => Right(new Reader(Vector.fill(6)(0)))).isLeft)
    assert(StudySpatialSupport.compile(study, limits)(_ => Right(new Reader(Vector.fill(6)(Double.PositiveInfinity)))).isLeft)
    val disjoint = StudySpatialSupport.compile(study, limits) { artifact =>
      val first = artifact.location.value.startsWith("file:///01/")
      Right(new Reader(if first then Vector(1, 0, 0, 0, 0, 0).map(_.toDouble) else Vector(0, 1, 0, 0, 0, 0).map(_.toDouble)))
    }
    assert(disjoint.left.toOption.get.message.contains("empty"))
  }

  test("larger and singleton read limits produce the same identified selection and coverage") {
    val results = Vector(1, 4, 20).map { width =>
      StudySpatialSupport.compile(study, StudySpatialLimits.make(6, width).toOption.get)(artifact =>
        Right(new Reader(values(artifact.location.value), width = width))).toOption.get
    }
    assert(results.forall(_.selection == results.head.selection))
    assert(results.forall(_.units.map(_.selectedVoxels) == Vector(4, 3)))
    assertEquals(results.map(_.reads.requests), Vector(24L, 8L, 4L))
    assertEquals(results.map(_.reads.largestReadVoxels), Vector(1, 4, 6))
  }
