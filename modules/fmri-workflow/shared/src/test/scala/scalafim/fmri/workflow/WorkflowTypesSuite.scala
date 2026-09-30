package scalafim.fmri.workflow

import scalafim.image.SampleSpaces

import munit.FunSuite
import scalafim.dataset.{DatasetId, DatasetShape, RunId, SpaceId, SubjectId, TaskId}
import scalafim.estimates.{EstimandId, ObservationId, PinnedUnit, ProductId, UnitId, UnitRevisionId}
import scalafim.archive.ContentDigest
import scalafim.fmri.group.{GroupMarginalUncertainty, InterceptPolicy}
import scalafim.image.SomeSampleSpace

class WorkflowTypesSuite extends FunSuite:
  test("workflow identifiers and artifact locations validate at construction") {
    assert(WorkflowId("bad/id").isLeft)
    assert(WorkflowId("  ").isLeft)
    assert(ArtifactLocation("\n").isLeft)
    assertEquals(WorkflowId.unsafe("study-01").value, "study-01")
    assertEquals(
      ArtifactLocation.unsafe("file:///results/").resolve("first-level", "unit-01").value,
      "file:///results/first-level/unit-01"
    )
  }

  test("contrast recipes reject degenerate rows and lower to fit contrasts") {
    assert(ContrastWorkflow.t("duplicate", Vector("x" -> 1.0, "x" -> -1.0)).isLeft)
    assert(ContrastWorkflow.t("zero", Vector("x" -> 0.0)).isLeft)
    assert(ContrastWorkflow.f("empty", Vector.empty).isLeft)

    val t = ContrastWorkflow.t("stim", Vector("b" -> -1.0, "a" -> 1.0)).toOption.get
    val f = ContrastWorkflow.f(
      "omnibus",
      Vector(Vector("a" -> 1.0), Vector("b" -> 1.0))
    ).toOption.get

    t.executable match
      case ExecutableContrast.T(value) =>
        assertEquals(value.name, "stim")
        assertEquals(value.weights, Map("a" -> 1.0, "b" -> -1.0))
      case other => fail(s"expected T contrast, got $other")

    f.executable match
      case ExecutableContrast.F(value) =>
        assertEquals(value.name, "omnibus")
        assertEquals(value.weights.length, 2)
      case other => fail(s"expected F contrast, got $other")
  }

  test("catalog construction rejects invalid run layout and duplicate units") {
    val shape = DatasetShape.unsafe(SampleSpaces(Vector(2, 2, 1)), 4)
    val run = runInput("run-1", 3)
    val mismatch = FirstLevelUnit.make(
      id = FirstLevelUnitId.unsafe("unit-01"),
      subject = SubjectId("01"),
      session = None,
      task = TaskId("demo"),
      space = SpaceId("MNI"),
      shape = shape,
      runs = Vector(run),
      mask = UnitMask.Single(WorkflowArtifactRef.unsafe[MaskImageResource]("file:///mask.nii.gz"))
    )

    assert(mismatch.isLeft)
    assert(RunInput.make(
      id = RunId("bad"),
      repetitionTime = RepetitionTime.unsafe(2.0),
      timepoints = 0,
      bold = WorkflowArtifactRef.unsafe[BoldImageResource]("file:///bold.nii.gz"),
      events = WorkflowArtifactRef.unsafe[EventsTableResource]("file:///events.tsv")
    ).isLeft)

    val valid = unit("unit-01")
    assert(StudyCatalog.make(DatasetId("demo"), Vector(valid, valid)).isLeft)
  }

  test("group workflow validates terms and output formats stay open") {
    val design = GroupDesignRecipe.make(Vector("age"), InterceptPolicy.Include).toOption.get
    val badContrast = GroupContrastWorkflow.make("age-effect", Vector("unknown" -> 1.0)).toOption.get
    val invalid = GroupWorkflow.make(
      id = GroupWorkflowId.unsafe("group"),
      inputs = Vector(WorkflowContrastId.unsafe("stim")),
      design = design,
      contrasts = Vector(badContrast)
    )
    assert(invalid.isLeft)

    val pluginFormat = OutputFormatId.unsafe("zarr-v1")
    val output = ResultOutputPolicy(
      root = ArtifactLocation.unsafe("s3://bucket/results"),
      format = pluginFormat,
      layout = EstimateMapLayout.BackendDefault
    )
    val bundle = output.firstLevel(WorkflowId.unsafe("study"), FirstLevelUnitId.unsafe("unit-01"))

    assertEquals(bundle.format.value, "zarr-v1")
    assertEquals(bundle.location.value, "s3://bucket/results/first-level/unit-01")
  }

  test("only a digest-pinned execution output can form a group input") {
    val selected = EstimateProductSelection.make(
      ObservationId("participant-01"), ProductId("effect"),
      Some(GroupMarginalUncertainty.StandardError(ProductId("standard-error"))), Vector(EstimandId("stim"))
    ).toOption.get
    val pinned = PinnedUnit(UnitId("00000000-0000-4000-8000-000000000001"), UnitRevisionId("00000000-0000-4000-8000-000000000002"),
      scalafim.estimates.FileReference("units/revision-01/estimates.json", ContentDigest.unsafeSha256("a" * 64), 42))
    val sealedReference = SealedEstimateReference.make(pinned, selected).toOption.get

    assertEquals(sealedReference.groupInput.reference, pinned)
    assertEquals(sealedReference.groupInput.effect, ProductId("effect"))
    assert(SealedEstimateReference.make(pinned.copy(manifest = pinned.manifest.copy(bytes = 0)), selected).isLeft)
    assert(EstimateProductSelection.make(selected.observation, selected.effect, selected.uncertainty,
      Vector(EstimandId("stim"), EstimandId("stim"))).isLeft)
  }

  private def runInput(id: String, timepoints: Int): RunInput =
    RunInput.unsafe(
      id = RunId(id),
      repetitionTime = RepetitionTime.unsafe(2.0),
      timepoints = timepoints,
      bold = WorkflowArtifactRef.unsafe[BoldImageResource](s"file:///$id/bold.nii.gz"),
      events = WorkflowArtifactRef.unsafe[EventsTableResource](s"file:///$id/events.tsv")
    )

  private def unit(id: String): FirstLevelUnit =
    FirstLevelUnit.unsafe(
      id = FirstLevelUnitId.unsafe(id),
      subject = SubjectId("01"),
      session = None,
      task = TaskId("demo"),
      space = SpaceId("MNI"),
      shape = DatasetShape.unsafe(SampleSpaces(Vector(2, 2, 1)), 4),
      runs = Vector(runInput("run-1", 4)),
      mask = UnitMask.Single(WorkflowArtifactRef.unsafe[MaskImageResource]("file:///mask.nii.gz"))
    )
