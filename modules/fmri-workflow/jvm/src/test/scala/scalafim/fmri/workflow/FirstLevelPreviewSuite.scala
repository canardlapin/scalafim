package scalafim.fmri.workflow

import scalafim.dataset.*
import scalafim.dataset.io.NiftiStagingCache
import scalafim.fmri.fit.{FitPlanExecutor,RunwiseFmriFitResult}
import scalafim.fmri.hrf.*
import scalafim.fmri.model.{FitStrategy,ModelBuildSpec}
import image4s.SomeSampleSpace
import neuroim4s.image.{SampleSpaces, SomeScalarSeries, SomeScalarVolume}
import neuroim4s.image.io.Nifti
import neuroim4s.image.SampleSpaces.*
import neuroim4s.spaces.{SpaceEvidence, WorldSpace}
import java.nio.file.{Files, Path}
import java.util.zip.GZIPOutputStream
import scala.jdk.CollectionConverters.*

class FirstLevelPreviewSuite extends munit.FunSuite:
  private def cacheSize(cache: NiftiStagingCache): Long =
    val paths = Files.walk(cache.root)
    try paths.iterator().asScala.filter(path => Files.isRegularFile(path)).map(path => Files.size(path)).sum
    finally paths.close()
  private val world = WorldSpace.declare("first-level synthetic acquisition").toOption.get
  private val evidence = SpaceEvidence(assertion = Some(world))
  private def writeMask(path: Path, space: SomeSampleSpace, data: Array[Double]): Path =
    Nifti.writeVolume(path, SomeScalarVolume.unsafeCopyFromCanonicalArray(data, space, "mask"))
      .fold(error => fail(error.message), _ => path)
  private def writeBold(path: Path, space: SomeSampleSpace, data: Array[Double]): Path =
    Nifti.writeSeries(path, SomeScalarSeries.unsafeCopyFromCanonicalArray(data, space.typed.appendNonSpatial(ProviderAxes.time(96)).fold(error => throw new IllegalArgumentException(error.message), identity), "bold"))
      .fold(error => fail(error.message), _ => path)

  test("first deferred read rejects a replaced affine and retries after the reviewed file is restored"):
    val root = Files.createTempDirectory("scalafim-preview-replacement-")
    try
      def geometry(x: Double): SomeSampleSpace =
        SampleSpaces.inWorld(SampleSpaces(Vector(2, 1, 1), origin = Some(Vector(x, 0.0, 0.0))), world).toOption.get
      val space = geometry(0.0)
      val mask = writeMask(root.resolve("mask.nii"), space, Array(1.0, 1.0))
      val bold = writeBold(root.resolve("bold.nii"), space, Array.tabulate(192)(_.toDouble))
      val reviewedBytes = Files.readAllBytes(bold)
      val events = Files.writeString(root.resolve("events.tsv"), "onset\tduration\ttrial_type\n10\t0\tA\n")
      val run = RunInput.unsafe(RunId("01"), RepetitionTime.unsafe(1.0), 96,
        WorkflowArtifactRef.unsafe[BoldImageResource](bold.toUri.toString),
        WorkflowArtifactRef.unsafe[EventsTableResource](events.toUri.toString))
      val unit = FirstLevelUnit.unsafe(FirstLevelUnitId.unsafe("replacement"), SubjectId("01"), None,
        TaskId("demo"), SpaceId("declared"), DatasetShape.unsafe(space, 96), Vector(run),
        UnitMask.Single(WorkflowArtifactRef.unsafe[MaskImageResource](mask.toUri.toString)))
      val preview = FirstLevelUnitDataset.open(unit, DatasetId("replacement"), _ => Right(evidence),
        responseAccess = FirstLevelResponseAccess.OnFirstRead).fold(error => fail(error.message), identity)
      writeBold(bold, geometry(1.0), Array.fill(192)(999.0))
      val selection = DataSelection(time = TimepointSelection.indices(1, 0), voxels = VoxelSelection.indices(1))
      preview.dataset.seriesEither(selection) match
        case Left(DatasetError.Geometry(image4s.geometry.GeometryError.GridsNotCongruent(_))) => ()
        case other => fail(s"expected replaced-affine refusal, got $other")
      val _ = Files.write(bold, reviewedBytes)
      val restored = preview.dataset.seriesEither(selection).fold(error => fail(error.message), identity)
      assertEquals(restored.timepoints, Vector(1, 0))
      assertEquals(restored.voxelIndices, Vector(1))
      assertEqualsDouble(restored.data(0, 0), 97.0, 0.0)
      assertEqualsDouble(restored.data(1, 0), 96.0, 0.0)
    finally
      val paths = Files.walk(root)
      try paths.iterator().asScala.toVector.reverse.foreach(path => { val _ = Files.deleteIfExists(path) })
      finally paths.close()

  test("condition and FIR previews never touch unavailable BOLD; restored deferred fits equal eager fits") {
    val root = Files.createTempDirectory("scalafim-preview-")
    try
      val space = SampleSpaces.inWorld(SampleSpaces(Vector(2, 1, 1),
        spacing = Some(Vector(2.0, 2.0, 2.0)), origin = Some(Vector(-16.5, -12.5, 1.5))), world).toOption.get
      val mask = writeMask(root.resolve("mask.nii"), space, Array(1.0, 1.0))
      val bold = writeBold(root.resolve("bold.nii"), space, Array.tabulate(192)(i => math.sin(i * 0.21)))
      val bytes = Files.readAllBytes(bold)
      val gzip = root.resolve("bold.nii.gz")
      val output = new GZIPOutputStream(Files.newOutputStream(gzip))
      try output.write(bytes) finally output.close()
      val compressedBytes = Files.readAllBytes(gzip)
      val events = Files.writeString(root.resolve("events.tsv"),"onset\tduration\ttrial_type\n10\t0\tA\n30\t0\tB\n50\t0\tA\n70\t0\tB\n")
      val cache = NiftiStagingCache.make(root.resolve("cache")).toOption.get
      Vector(bold,gzip).foreach { path =>
        val run = RunInput.unsafe(RunId("01"),RepetitionTime.unsafe(1.0),96,
          WorkflowArtifactRef.unsafe[BoldImageResource](path.toUri.toString),WorkflowArtifactRef.unsafe[EventsTableResource](events.toUri.toString))
        val unit = FirstLevelUnit.unsafe(FirstLevelUnitId.unsafe("selected"),SubjectId("01"),None,TaskId("demo"),SpaceId("MNI152NLin2009cAsym"),
          DatasetShape.unsafe(space,96),Vector(run),UnitMask.Single(WorkflowArtifactRef.unsafe[MaskImageResource](mask.toUri.toString)))
        Files.delete(path)
        Vector(Hrfs.SPMG1,Hrfs.fir(nBasis=4,span=8.s)).foreach { hrf =>
          val beforeBytes = cacheSize(cache)
          val preview = FirstLevelUnitDataset.open(unit,DatasetId("preview"), _ => Right(evidence),staging=Some(cache),responseAccess=FirstLevelResponseAccess.OnFirstRead)
            .fold(e => fail(e.message),identity)
          val specification = ModelBuildSpec("onset ~ hrf(trial_type, id = task)",defaultHrf=hrf,precision=0.1.s,strategy=FitStrategy.RunwiseLeastSquares())
          val plan = preview.buildPlan(specification).fold(e => fail(e.message),identity)
          assert(!Files.exists(path))
          assertEquals(cacheSize(cache),beforeBytes)
          assert(preview.dataset.seriesEither(DataSelection(time=TimepointSelection.indices(0),voxels=VoxelSelection.indices(0))).isLeft)
          val _ = Files.write(path,if path == bold then bytes else compressedBytes)
          val eager = FirstLevelUnitDataset.open(unit,DatasetId("eager"), _ => Right(evidence),staging=Some(cache)).fold(e => fail(e.message),identity)
          val eagerPlan = eager.buildPlan(specification).fold(e => fail(e.message),identity)
          assertEquals(plan.model.designSchema.map(_.fingerprint),eagerPlan.model.designSchema.map(_.fingerprint))
          val left = FitPlanExecutor.fit(preview.dataset,plan).toOption.get.asInstanceOf[RunwiseFmriFitResult]
          val right = FitPlanExecutor.fit(eager.dataset,eagerPlan).toOption.get.asInstanceOf[RunwiseFmriFitResult]
          val a = left.runs.head.coefficients
          val b = right.runs.head.coefficients
          assertEquals(Vector.tabulate(a.predictors,a.voxels)(a.apply),Vector.tabulate(b.predictors,b.voxels)(b.apply))
          Files.delete(path)
        }
        val _ = Files.write(path,if path == bold then bytes else compressedBytes)
      }
    finally
      val paths = Files.walk(root)
      try paths.iterator().asScala.toVector.reverse.foreach(path => { val _ = Files.deleteIfExists(path) }) finally paths.close()
  }
