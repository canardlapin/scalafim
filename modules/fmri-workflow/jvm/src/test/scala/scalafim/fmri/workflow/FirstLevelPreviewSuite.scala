package scalafim.fmri.workflow

import scalafim.dataset.*
import scalafim.dataset.io.{NiftiStagingCache,NiftiStagingLimits}
import scalafim.fmri.fit.{FitPlanExecutor,RunwiseFmriFitResult}
import scalafim.fmri.hrf.*
import scalafim.fmri.model.{FitStrategy,ModelBuildSpec}
import scalafim.image.{NeuroSpace,NeuroVec,NeuroVol,PrimitiveBuffers}
import scalafim.image.io.{Nifti,NiftiWriteOptions,NiftiCoordinateSystem,NiftiSpatialUnits}
import java.nio.file.Files
import java.util.zip.GZIPOutputStream
import scala.jdk.CollectionConverters.*

class FirstLevelPreviewSuite extends munit.FunSuite:
  test("condition and FIR previews never touch unavailable BOLD; restored deferred fits equal eager fits") {
    val root = Files.createTempDirectory("scalafim-preview-")
    try
      val space = NeuroSpace(Vector(2,1,1),spacing=Some(Vector(2.0,2.0,2.0)),origin=Some(Vector(-16.5,-12.5,1.5)))
      val options = NiftiWriteOptions(NiftiCoordinateSystem.Mni152,NiftiSpatialUnits.Millimeters)
      val mask = Nifti.writeVol(root.resolve("mask.nii"),NeuroVol.fromLinear(PrimitiveBuffers.fromArray(Array(1.0,1.0)),space,"mask"),options)
      val bold = Nifti.writeVec(root.resolve("bold.nii"),NeuroVec.fromLinear(PrimitiveBuffers.fromArray(Array.tabulate(192)(i => math.sin(i*0.21))),space.addDim(96,Some(scalafim.image.Axis.Time)),"bold"),options)
      val bytes = Files.readAllBytes(bold)
      val gzip = root.resolve("bold.nii.gz")
      val output = new GZIPOutputStream(Files.newOutputStream(gzip))
      try output.write(bytes) finally output.close()
      val compressedBytes = Files.readAllBytes(gzip)
      val events = Files.writeString(root.resolve("events.tsv"),"onset\tduration\ttrial_type\n10\t0\tA\n30\t0\tB\n50\t0\tA\n70\t0\tB\n")
      val cache = NiftiStagingCache.make(root.resolve("cache"),NiftiStagingLimits(minimumFreeBytes=0)).toOption.get
      Vector(bold,gzip).foreach { path =>
        val run = RunInput.unsafe(RunId("01"),RepetitionTime.unsafe(1.0),96,
          WorkflowArtifactRef.unsafe[BoldImageResource](path.toUri.toString),WorkflowArtifactRef.unsafe[EventsTableResource](events.toUri.toString))
        val unit = FirstLevelUnit.unsafe(FirstLevelUnitId.unsafe("selected"),SubjectId("01"),None,TaskId("demo"),SpaceId("MNI152NLin2009cAsym"),
          DatasetShape.unsafe(space,96),Vector(run),UnitMask.Single(WorkflowArtifactRef.unsafe[MaskImageResource](mask.toUri.toString)))
        Files.delete(path)
        Vector(Hrfs.SPMG1,Hrfs.fir(nBasis=4,span=8.s)).foreach { hrf =>
          val beforeBytes = cache.sizeBytes.toOption.get
          val preview = FirstLevelUnitDataset.open(unit,DatasetId("preview"),staging=Some(cache),responseAccess=FirstLevelResponseAccess.OnFirstRead)
            .fold(e => fail(e.message),identity)
          val specification = ModelBuildSpec("onset ~ hrf(trial_type, id = task)",defaultHrf=hrf,precision=0.1.s,strategy=FitStrategy.RunwiseLeastSquares())
          val plan = preview.buildPlan(specification).fold(e => fail(e.message),identity)
          assert(!Files.exists(path))
          assertEquals(cache.sizeBytes.toOption.get,beforeBytes)
          assert(preview.dataset.seriesEither(DataSelection(time=TimepointSelection.indices(0),voxels=VoxelSelection.indices(0))).isLeft)
          val _ = Files.write(path,if path == bold then bytes else compressedBytes)
          val eager = FirstLevelUnitDataset.open(unit,DatasetId("eager"),staging=Some(cache)).fold(e => fail(e.message),identity)
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
