package scalafim.fmri.workflow

import scalafim.dataset.*
import scalafim.fmri.design.{EventSupportDisposition, EventSupportPolicy, ResponseSupportRequest, ScanIndex}
import scalafim.fmri.fit.{FitPlanExecutor, RunwiseFmriFitResult}
import scalafim.fmri.hrf.*
import scalafim.fmri.model.{FitStrategy, ModelBuildSpec}
import scalafim.image.{Axis, NeuroSpace, NeuroVec, NeuroVol, PrimitiveBuffers}
import scalafim.image.io.{Nifti, NiftiWriteOptions, NiftiCoordinateSystem, NiftiSpatialUnits}
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

class FirstLevelSamplingReferenceSuite extends munit.FunSuite:
  test("public first-level grid changes boundary support and independently recovers MNI FIR responses") {
    val root = Files.createTempDirectory("scalafim-sampling-reference-")
    try
      // One-voxel crop at (40,60,40) of ds000001 MNI2009c 2 mm parent geometry.
      val space = NeuroSpace(Vector(1, 1, 1), spacing = Some(Vector(2.0, 2.0, 2.0)),
        origin = Some(Vector(-16.5, -12.5, 1.5)))
      val options = NiftiWriteOptions(NiftiCoordinateSystem.Mni152, NiftiSpatialUnits.Millimeters)
      val mask = Nifti.writeVol(root.resolve("mask.nii"),
        NeuroVol.fromLinear(PrimitiveBuffers.fromArray(Array(1.0)), space, "mask"), options)
      // Analytic unit-height 2-second FIR boxes at onset 2.5: midpoint samples 3,5,7,9.
      // The second event at 598.5 contributes only to the sample at 599.
      // This response is constructed directly, without ScalaFIM design/convolution code.
      val response = Array.fill(300)(2.0)
      Vector(1 -> 3.0, 2 -> 4.0, 3 -> 5.0, 4 -> 6.0, 299 -> 3.0).foreach { (index, beta) => response(index) += beta }
      val bold = Nifti.writeVec(root.resolve("bold.nii"),
        NeuroVec.fromLinear(PrimitiveBuffers.fromArray(response),
          space.addDim(300, Some(Axis.Time)), "bold"), options)
      // Verify the generated spatial declaration directly from NIfTI-1 bytes.
      // The model TR is declared by RunInput; the lightweight writer does not
      // yet expose temporal units/step (tracked separately in native Mote).
      Vector(mask, bold).foreach { path =>
        val header = java.nio.ByteBuffer.wrap(Files.readAllBytes(path)).order(java.nio.ByteOrder.LITTLE_ENDIAN)
        assertEquals(header.getShort(254).toInt, 4)
        assertEquals(header.get(123).toInt & 7, 2)
        assertEqualsDouble(header.getFloat(80).toDouble, 2.0, 0.0)
        assertEqualsDouble(header.getFloat(84).toDouble, 2.0, 0.0)
        assertEqualsDouble(header.getFloat(88).toDouble, 2.0, 0.0)
        assertEqualsDouble(header.getFloat(292).toDouble, -16.5, 0.0)
        assertEqualsDouble(header.getFloat(308).toDouble, -12.5, 0.0)
        assertEqualsDouble(header.getFloat(324).toDouble, 1.5, 0.0)
      }
      val originalEvents = "onset\tduration\ttrial_type\tunused\n2.5\t0\tA\tn/a\n598.5\t0\tA\tn/a\n"
      val events = Files.writeString(root.resolve("events.tsv"), originalEvents)
      val run = RunInput.unsafe(RunId("01"), RepetitionTime.unsafe(2.0), 300,
        WorkflowArtifactRef.unsafe[BoldImageResource](bold.toUri.toString),
        WorkflowArtifactRef.unsafe[EventsTableResource](events.toUri.toString),
        timing = RunTimingMetadata(Some(false)))
      val unit = FirstLevelUnit.unsafe(FirstLevelUnitId.unsafe("timing"), SubjectId("01"), None,
        TaskId("boundary"), SpaceId("MNI152NLin2009cAsym"), DatasetShape.unsafe(space, 300), Vector(run),
        UnitMask.Single(WorkflowArtifactRef.unsafe[MaskImageResource](mask.toUri.toString)))
      def open(reference: SamplingReference): OpenedFirstLevelDataset =
        FirstLevelUnitDataset.open(unit, DatasetId("timing"), eventColumns = Some(Vector("trial_type")),
          samplingReference = reference).fold(e => fail(e.message), identity)
      val midpoint = open(SamplingReference.VolumeMidpoint)
      val onset = open(SamplingReference.VolumeOnset)
      val legacy = FirstLevelUnitDataset.open(unit, DatasetId("default"), eventColumns = Some(Vector("trial_type")))
        .fold(e => fail(e.message), identity)
      assertEquals(legacy.sampling.declaration, SamplingReference.VolumeMidpoint)
      assertEqualsDouble(midpoint.sampling.runs.head.lastSample.value, 599.0, 0.0)
      assertEqualsDouble(onset.sampling.runs.head.lastSample.value, 598.0, 0.0)
      assertEquals(midpoint.tables.events, onset.tables.events)
      assertEquals(midpoint.unit.runs.head.timing.sliceTimingCorrected, Some(false))
      val scans = (1 to 300).map(ScanIndex.unsafeOneBased).toVector
      def spec(policy: EventSupportPolicy): ModelBuildSpec = ModelBuildSpec(
        "onset ~ hrf(trial_type, id = task)", defaultHrf = Hrfs.fir(nBasis = 4, span = 8.s),
        precision = 0.1.s, strategy = FitStrategy.RunwiseLeastSquares(),
        responseSupport = Some(ResponseSupportRequest(scans, policy)))
      val accepted = midpoint.buildPlan(spec(EventSupportPolicy.RejectUnobservedAllowPartial)).fold(e => fail(e.message), identity)
      assert(onset.buildPlan(spec(EventSupportPolicy.RejectUnobservedAllowPartial)).isLeft)
      val excluded = onset.buildPlan(spec(EventSupportPolicy.ExcludeUnobservedAllowPartial)).fold(e => fail(e.message), identity)
      val midDecisions = accepted.model.eventModel.designSchema.audit.responseSupport.flatMap(_.decisions)
      val startDecisions = excluded.model.eventModel.designSchema.audit.responseSupport.flatMap(_.decisions)
      assertEquals(midDecisions.map(_.disposition), Vector.fill(2)(EventSupportDisposition.Retained))
      assertEquals(startDecisions.map(_.disposition), Vector(EventSupportDisposition.Retained, EventSupportDisposition.Excluded))
      assertEquals(midDecisions.last.columns.map(_.observedSamples), Vector(1, 0, 0, 0))
      assertEquals(startDecisions.last.columns.map(_.observedSamples), Vector(0, 0, 0, 0))
      val fit = FitPlanExecutor.fit(midpoint.dataset, accepted).fold(e => fail(e.message), identity) match
        case result: RunwiseFmriFitResult => result
        case other => fail(s"expected runwise fit, got ${other.engine}")
      val betas = fit.runs.head.coefficients
      assertEquals(betas.predictors, 5)
      Vector(3.0, 4.0, 5.0, 6.0, 2.0).zipWithIndex.foreach { (expected, row) =>
        assertEqualsDouble(betas(row, 0), expected, 1e-10)
      }
      // On the onset grid the four early boxes sample rows 2..5, whose responses
      // are 6,7,8,2. The other 296 rows contain two unexplained increments of 3.
      val startFit = FitPlanExecutor.fit(onset.dataset, excluded).fold(e => fail(e.message), identity) match
        case result: RunwiseFmriFitResult => result
        case other => fail(s"expected runwise fit, got ${other.engine}")
      val baseline = 2.0 + 6.0 / 296.0
      val expectedStart = Vector(6.0 - baseline, 7.0 - baseline, 8.0 - baseline, 2.0 - baseline, baseline)
      expectedStart.zipWithIndex.foreach { (expected, row) =>
        assertEqualsDouble(startFit.runs.head.coefficients(row, 0), expected, 1e-10)
      }
      assertEquals(midpoint.dataset.samplingFrame.samples().map(_.value), (0 until 300).map(i => 1.0 + i * 2.0).toVector)
      assertEquals(onset.dataset.samplingFrame.samples().map(_.value), (0 until 300).map(i => i * 2.0).toVector)
      assertEquals(Files.readString(events), originalEvents)
      // Invalid identities must fail before opening even the first table.
      val missing = RunInput.unsafe(run.id, run.repetitionTime, run.timepoints, run.bold,
        WorkflowArtifactRef.unsafe[EventsTableResource](root.resolve("missing.tsv").toUri.toString))
      val missingUnit = FirstLevelUnit.unsafe(unit.id, unit.subject, unit.session, unit.task, unit.space,
        unit.shape, Vector(missing), unit.mask)
      val invalid = FirstLevelUnitDataset.open(missingUnit, DatasetId("invalid"),
        samplingReference = SamplingReference.PerRun(Map(RunId("1") -> SamplingFraction.VolumeOnset)))
      assert(invalid.left.toOption.get.message.contains("run identities must match exactly"))
    finally
      val paths = Files.walk(root)
      try paths.iterator().asScala.toVector.reverse.foreach(path => { val _ = Files.deleteIfExists(path) })
      finally paths.close()
  }
