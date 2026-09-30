package scalafim.fmri.workflow

import java.nio.file.Files
import gale.linalg.Matrix
import scalafim.dataset.{DataSelection, DatasetError, DatasetSeriesReader, FmriDataset, FmriSeries, InMemoryDatasetBackend}
import scalafim.estimates.*
import scalafim.estimates.io.LocalEstimateStore
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.fit.{ChunkSize, EstimateOutput, EstimateUncertaintyRequest, FirstLevelEstimateRequest, FirstLevelEstimates}
import scalafim.fmri.fit.estimates.{EstimatePublicationIdentity, FitEstimateProducer}
import scalafim.fmri.group.{GroupEstimateAdmission, GroupGeometryEvidence, GroupMarginalUncertainty}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitPlan, FmriModel}
import scalafim.image.SampleSpaces

class LocalEstimateWorkflowSuite extends munit.FunSuite:
  private def right[A](value: Either[EstimateError, A]): A = value.fold(error => fail(error.message), identity)

  private val frame = SamplingFrame(blockLens = Seq(4), tr = Seq(1.0))
  private val dataset = FmriDataset.unsafe(InMemoryDatasetBackend(scalafim.dataset.DatasetId("workflow-fit"),
    Matrix.tabulate(4, 2)((t, v) => (if v == 0 then 1.0 + 2.0 * t else 5.0 - 3.0 * t) +
      (v + 1) * Vector(1.0, -1.0, -1.0, 1.0)(t)), SampleSpaces(Vector(2, 1, 1))), frame)
  private val event = EventModel(Vector.empty, frame, Mat.fromRows(Vector.tabulate(4)(t => Vector(t.toDouble))),
    Vector("task"), Vector(0 -> 1), Map("task" -> Vector(0)))
  private val fit = FitPlan(FmriModel(event,
    BaselineModel.build(frame, basis = BaselineBasis.Constant, intercept = Intercept.Global), dataset))
  private val ids = Vector(EstimandId("task"), EstimandId("intercept"))
  private val catalog = EstimandCatalog(ModelRevisionId("00000000-0000-4000-8000-000000000004"), ids.map(id =>
    EstimandDefinition(id, id.value, EstimandKind.Coefficient, "signal", "unit", id.value)))
  private final class CountingReader extends DatasetSeriesReader:
    var calls = 0
    val dataset = LocalEstimateWorkflowSuite.this.dataset
    def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
      calls += 1
      dataset.seriesEither(selection)
  private val admission = new GroupEstimateAdmission:
    def verify(units: Vector[EstimateUnit]) =
      if units.forall(_.domain.worldFrame == "scanner") then
        Right(GroupGeometryEvidence.Verified("scanner", "synthetic fixture grid", Vector.empty))
      else Left(EstimateError.Invalid("synthetic geometry admission refused"))

  private def producer(participant: String, revision: String, worldFrame: String = "scanner"): FitEstimateProducer =
    val request = right(FirstLevelEstimateRequest.make(fit.coefficientAxis.get.columnIds.map(EstimateOutput.Coefficient.apply),
      EstimateUncertaintyRequest.Marginal).left.map(error => EstimateError.Invalid(error.message)))
    val prepared = right(FirstLevelEstimates.prepare(fit, request, ChunkSize.unsafe(1)).left.map(error => EstimateError.Invalid(error.message)))
    val identity = EstimatePublicationIdentity(DatasetId("00000000-0000-4000-8000-000000000001"),
      UnitId(s"00000000-0000-4000-8000-0000000000$participant"), UnitRevisionId(revision),
      participant, ObservationId(s"subject-$participant"), Vector(AcquisitionId("run-1")), s"execution-$participant", "test", "signal", Vector.empty)
    right(FitEstimateProducer.shared(prepared, identity, catalog, ids, worldFrame))

  private def selection(producer: FitEstimateProducer): EstimateProductSelection =
    val effect = producer.unit.products.find(_.kind == ProductKind.Effect).get.id
    val uncertainty = producer.unit.products.find(_.kind == ProductKind.StandardError).get.id
    EstimateProductSelection.make(producer.unit.observations.head.id, effect,
      Some(GroupMarginalUncertainty.StandardError(uncertainty)), ids).toOption.get

  private def output(root: java.nio.file.Path, layout: EstimateMapLayout = EstimateMapLayout.BundledMaps): PlannedEstimateOutput =
    PlannedEstimateOutput.make(ResultBundleId.unsafe("study.unit"), ArtifactLocation.unsafe(root.toUri.toString), OutputFormatId.CoreNifti, layout)

  test("synthetic OLS seals, fresh store reopens, and group admission consumes only pinned references") {
    val root = Files.createTempDirectory("workflow-estimate-handoff-")
    val first = producer("01", "00000000-0000-4000-8000-000000000101")
    val second = producer("02", "00000000-0000-4000-8000-000000000102")
    val foreignFrame = producer("04", "00000000-0000-4000-8000-000000000104", "other-frame")
    val store = right(LocalEstimateStore.open(root))
    val reader = new CountingReader
    val sealedFirst = right(LocalEstimateWorkflow.produce(first, reader, store, 1, output(root), selection(first)))
    val sealedSecond = right(LocalEstimateWorkflow.produce(second, reader, store, 1, output(root), selection(second)))
    assert(LocalEstimateWorkflow.produce(foreignFrame, reader, store, 1, output(root), selection(foreignFrame)).isLeft)
    val reopened = right(LocalEstimateStore.open(root))

    val group = right(LocalEstimateWorkflow.prepareGroup(reopened, Vector(sealedFirst, sealedSecond), admission, 16))
    assertEquals(group.inputs.map(_.reference), Vector(sealedFirst.pinned, sealedSecond.pinned))
    val responses = group.readBlock(Vector(0, 1)).toOption.get.data.responses.values.toVector
    val task = responses(0)
    val intercept = responses(1)
    assertEqualsDouble(task.effects(0, 0), 2.0, 1e-12)
    assertEqualsDouble(task.effects(0, 1), -3.0, 1e-12)
    assertEqualsDouble(intercept.effects(0, 0), 1.0, 1e-12)
    assertEqualsDouble(intercept.effects(0, 1), 5.0, 1e-12)
    assertEqualsDouble(task.variances.get(0, 0), 0.4, 1e-12)
    assertEqualsDouble(task.variances.get(0, 1), 1.6, 1e-12)
    assertEqualsDouble(intercept.variances.get(0, 0), 1.4, 1e-12)
    assertEqualsDouble(intercept.variances.get(0, 1), 5.6, 1e-12)
    val missingRevision = SealedEstimateReference.make(
      sealedFirst.pinned.copy(revision = UnitRevisionId("00000000-0000-4000-8000-000000000199")), sealedFirst.selection).toOption.get
    assert(LocalEstimateWorkflow.prepareGroup(reopened, Vector(missingRevision, sealedSecond), admission, 8).isLeft)
    val wrongDigest = SealedEstimateReference.make(sealedFirst.pinned.copy(
      manifest = sealedFirst.pinned.manifest.copy(digest = scalafim.archive.ContentDigest.unsafeSha256("f" * 64))), sealedFirst.selection).toOption.get
    assert(LocalEstimateWorkflow.prepareGroup(reopened, Vector(wrongDigest, sealedSecond), admission, 8).isLeft)
    val wrongProduct = EstimateProductSelection.make(sealedFirst.selection.observation, ProductId("missing-product"),
      sealedFirst.selection.uncertainty, ids).toOption.get
    val wrongReference = SealedEstimateReference.make(sealedFirst.pinned, wrongProduct).toOption.get
    assert(LocalEstimateWorkflow.prepareGroup(reopened, Vector(wrongReference, sealedSecond), admission, 8).isLeft)
  }

  test("cancellation aborts the owned sink and returns no sealed reference") {
    val root = Files.createTempDirectory("workflow-estimate-cancel-")
    val value = producer("03", "00000000-0000-4000-8000-000000000103")
    val result = LocalEstimateWorkflow.produce(value, new CountingReader, right(LocalEstimateStore.open(root)), 1, output(root), selection(value), () => true)
    assertEquals(result, Left(EstimateError.Cancelled))
    assert(!Files.exists(root.resolve(s"units/${value.unit.revision.value}/estimates.json")))
  }

  test("unsupported planned format, layout and destination are refused before local sink allocation") {
    val root = Files.createTempDirectory("workflow-estimate-format-")
    val value = producer("05", "00000000-0000-4000-8000-000000000105")
    val reader = new CountingReader
    val unsupported = PlannedEstimateOutput.make(ResultBundleId.unsafe("unsupported"), ArtifactLocation.unsafe(root.toUri.toString),
      OutputFormatId.unsafe("other-format"), EstimateMapLayout.BundledMaps)
    val store = right(LocalEstimateStore.open(root))
    assertEquals(LocalEstimateWorkflow.produce(value, reader, store, 1, unsupported, selection(value)),
      Left(EstimateError.Unsupported("workflow output format 'other-format' is not implemented by LocalEstimateWorkflow")))
    assertEquals(LocalEstimateWorkflow.produce(value, reader, store, 1, output(root, EstimateMapLayout.IndividualNamedMaps), selection(value)),
      Left(EstimateError.Unsupported("workflow output layout 'IndividualNamedMaps' is not implemented by LocalEstimateWorkflow")))
    val absent = root.resolve("does-not-exist")
    val absentOutput = PlannedEstimateOutput.make(ResultBundleId.unsafe("absent"), ArtifactLocation.unsafe(absent.toUri.toString),
      OutputFormatId.CoreNifti, EstimateMapLayout.BundledMaps)
    assert(LocalEstimateWorkflow.produce(value, reader, store, 1, output(Files.createTempDirectory("wrong-output-")), selection(value)).isLeft)
    assert(LocalEstimateWorkflow.produce(value, reader, store, 1, absentOutput, selection(value)) match
      case Left(EstimateError.Invalid(detail)) => detail.startsWith("cannot compare planned local output:")
      case _ => false)
    val malformed = PlannedEstimateOutput.make(ResultBundleId.unsafe("malformed"), ArtifactLocation.unsafe("file:///bad path"),
      OutputFormatId.CoreNifti, EstimateMapLayout.BundledMaps)
    assert(LocalEstimateWorkflow.produce(value, reader, store, 1, malformed, selection(value)) match
      case Left(EstimateError.Invalid(detail)) => detail.startsWith("invalid local output location:")
      case _ => false)
    assertEquals(reader.calls, 0)
    assert(!Files.exists(root.resolve(".staging")))
  }

  test("invalid selected axes refuse before reading or publishing") {
    val value = producer("06", "00000000-0000-4000-8000-000000000106")
    val valid = selection(value)
    val invalid = Vector(
      EstimateProductSelection.make(ObservationId("missing-observation"), valid.effect, valid.uncertainty, ids).toOption.get ->
        "selected observation is not declared by the producer unit",
      EstimateProductSelection.make(valid.observation, valid.effect, valid.uncertainty, Vector(EstimandId("missing-estimand"))).toOption.get ->
        "selected estimands are not declared by the selected effect",
      EstimateProductSelection.make(valid.observation, ProductId("foreign-effect"), valid.uncertainty, ids).toOption.get ->
        "selected effect product is not declared as an effect",
      EstimateProductSelection.make(valid.observation, valid.effect,
        Some(GroupMarginalUncertainty.StandardError(ProductId("foreign-uncertainty"))), ids).toOption.get ->
        "selected marginal uncertainty is not associated with the selected effect axes"
    )
    invalid.foreach: (candidate, expected) =>
      val root = Files.createTempDirectory("workflow-estimate-invalid-")
      val reader = new CountingReader
      val result = LocalEstimateWorkflow.produce(value, reader, right(LocalEstimateStore.open(root)), 1, output(root), candidate)
      assertEquals(result, Left(EstimateError.Invalid(expected)))
      assertEquals(reader.calls, 0)
      assert(!Files.exists(root.resolve(s"units/${value.unit.revision.value}/estimates.json")))
  }
