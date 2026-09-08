package scalafim.fmri.workflow

import bids4s.ConfoundSelectionConfig
import scalafim.dataset.*
import scalafim.fmri.fit.{FitPlanExecutor, RunwiseFmriFitResult}
import scalafim.fmri.hrf.*
import scalafim.fmri.model.{FitStrategy, ModelBuildSpec}
import scalafim.image.{NeuroSpace, NeuroVec, NeuroVol, PrimitiveBuffers}
import scalafim.image.io.{Nifti, NiftiWriteOptions, NiftiCoordinateSystem, NiftiSpatialUnits}
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

class FirstLevelUnitDatasetSuite extends munit.FunSuite:
  test("selected companion files bind into condition and FIR fits without discovering unrelated files") {
    val root = Files.createTempDirectory("scalafim-unit-dataset-")
    try
      // Crop at parent voxel (40,60,40) of the ds000001 MNI2009c 2 mm grid.
      val space = NeuroSpace(Vector(2, 1, 1), spacing = Some(Vector(2.0, 2.0, 2.0)),
        origin = Some(Vector(-16.5, -12.5, 1.5)))
      val options = NiftiWriteOptions(NiftiCoordinateSystem.Mni152, NiftiSpatialUnits.Millimeters)
      val mask = Nifti.writeVol(root.resolve("mask.nii"),
        NeuroVol.fromLinear(PrimitiveBuffers.fromArray(Array(1.0, 1.0)), space, "mask"), options)
      val _ = Files.writeString(root.resolve("unrelated_events.tsv"), "not an events table")
      val runs = Vector("02", "01").map { id =>
        val bold = Nifti.writeVec(root.resolve(s"$id-bold.nii"),
          NeuroVec.fromLinear(PrimitiveBuffers.fromArray(Array.tabulate(192)(i => math.sin(i * 0.21))),
            space.addDim(96, Some(scalafim.image.Axis.Time)), "bold"), options)
        val events = Files.writeString(root.resolve(s"$id-events.tsv"),
          "onset\tduration\ttrial_type\ttrial_id\tunused_modulation\n" +
            Vector.tabulate(4)(i => s"${10 + 20 * i}\t0\t${if i % 2 == 0 then "A" else "B"}\ttrial-${i + 1}\tn/a").mkString("\n") + "\n")
        val confounds = Files.writeString(root.resolve(s"$id-confounds.tsv"),
          "motion_x\n" + Vector.tabulate(96)(i => math.cos(i * 0.13).toString).mkString("\n") + "\n")
        RunInput.unsafe(RunId(id), RepetitionTime.unsafe(1.0), 96,
          WorkflowArtifactRef.unsafe[BoldImageResource](bold.toUri.toString),
          WorkflowArtifactRef.unsafe[EventsTableResource](events.toUri.toString),
          Some(WorkflowArtifactRef.unsafe[ConfoundsTableResource](confounds.toUri.toString)))
      }
      val unit = FirstLevelUnit.unsafe(FirstLevelUnitId.unsafe("selected"), SubjectId("01"), None,
        TaskId("demo"), SpaceId("MNI152NLin2009cAsym"), DatasetShape.unsafe(space, 192), runs,
        UnitMask.Single(WorkflowArtifactRef.unsafe[MaskImageResource](mask.toUri.toString)))
      val opened = FirstLevelUnitDataset.open(unit, DatasetId("selected"),
        confounds = Some(ConfoundSelectionConfig(variables = Vector("motion_x"), clean = Vector.empty)),
        eventColumns = Some(Vector("trial_type", "trial_id")))
        .fold(e => fail(e.message), identity)
      assert(opened.tables.sourceTables.values.forall(_.events.rows.forall(_.last.isEmpty)))
      assertEquals(opened.tables.runIds, Vector(RunId("02"), RunId("01")))
      Vector(Hrfs.SPMG1, Hrfs.fir(nBasis = 4, span = 8.s)).foreach { hrf =>
        val plan = opened.buildPlan(ModelBuildSpec(
          "onset ~ hrf(trial_type, parent = trial_id, phase = stimulus, id = task)",
          defaultHrf = hrf, precision = 0.1.s, strategy = FitStrategy.RunwiseLeastSquares()))
          .fold(e => fail(e.message), identity)
        val fit = FitPlanExecutor.fit(opened.dataset, plan).fold(e => fail(e.message), identity)
        fit match
          case runwise: RunwiseFmriFitResult =>
            assertEquals(runwise.runs.length, 2)
            assertEquals(runwise.voxelIndices, Vector(0, 1))
            assertEquals(runwise.runs.map(_.timepoints.length), Vector(96, 96))
          case other => fail(s"expected runwise fit, got ${other.engine}")
      }
    finally
      val paths = Files.walk(root)
      try paths.iterator().asScala.toVector.reverse.foreach(path => { val _ = Files.deleteIfExists(path) })
      finally paths.close()
  }
