package scalafim.fmri.mvpa

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import resample4s.core.*
import resample4s.designs.LeaveOneGroupOut
import scalafim.dataset.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.mvpa.predictive.*
import scalafim.image.{DMat, NeuroSpace}
import scalafim.response.*

import scala.concurrent.ExecutionContext.Implicits.global

final class DatasetEvidenceSuite extends munit.FunSuite:
  import PredictiveAnalysis.given

  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private def dataset: FmriDataset =
    FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(
        id = DatasetId("dataset-evidence-demo"),
        data = DMat.fromRows(
          Vector(
            Vector(2.0, 2.0, 0.0),
            Vector(-2.0, -2.0, 0.0),
            Vector(3.0, 1.8, 0.5),
            Vector(-3.0, -1.8, -0.5)
          )
        ),
        space = NeuroSpace(Vector(3, 1, 1))
      ),
      samplingFrame = SamplingFrame(blockLens = Seq(4), tr = Seq(1.0))
    )

  test("resolved dataset selection preserves exact sample and voxel identity and orientation"):
    val ingested =
      DatasetEvidence
        .fromDataset(
          dataset,
          DataSelection(
            time = IndexSelection.indices(1, 3),
            voxels = IndexSelection.indices(0, 2)
          )
        )
        .fold(error => fail(error.message), identity)

    assertEquals(ingested.timepoints.map(_.value), Vector(1, 3))
    assertEquals(ingested.voxels.map(_.value), Vector(0, 2))
    assertEquals(ingested.samples.keys.map(_.value), Vector("timepoint:1", "timepoint:3"))
    assertEquals(ingested.features.keys.map(_.value), Vector("voxel:0", "voxel:2"))

    val materialized =
      ingested.observations.evidence
        .materialize(MaterializationPolicy.Allow(MaterializationBudget.unsafe(4L)))
        .fold(error => fail(error.message), identity)
    assertEquals(
      matrixRows(materialized.value),
      Vector(Vector(-2.0, 0.0), Vector(-3.0, -0.5))
    )

    val run =
      Column(ingested.samples, Vector("run-a", "run-b"))
        .fold(error => fail(error.message), identity)
    assertEquals(run.rowIdentity, ingested.samples.identity)
    assertEquals(run.toVector, Vector("run-a", "run-b"))

  test("opened dataset ingestion lowers the attachment boundary into the same evidence core"):
    val attached = dataset
    val schemaId = ResponseSchemaId.unsafe("dataset-evidence-opened-schema")
    val schema =
      DatasetResponseSchema
        .fromDataset(
          attached,
          schemaId,
          scalafim.response.UnitId.unsafe("unit")
        )
        .fold(error => fail(error.message), identity)
    val source =
      InMemoryResponseSource
        .copyFromRowMajor[IO](
          SourceId.unsafe("dataset-evidence-opened-source"),
          schema,
          Array[Double](
            2.0, 2.0, 0.0, -2.0, -2.0, 0.0, 3.0, 1.8, 0.5, -3.0, -1.8, -0.5
          )
        )
        .fold(error => fail(error.message), identity)
    val acquisition =
      AcquisitionContext
        .volume(
          attached,
          DatasetKey.unsafe("sub-01"),
          ResponseKey.unsafe("bold"),
          schemaId,
          scalafim.response.UnitId.unsafe("unit")
        )
        .fold(error => fail(error.message), identity)
    val opened =
      OpenedDataset
        .attach(attached, source, acquisition)
        .toEither
        .fold(
          issues => fail(issues.toNonEmptyList.toList.map(_.message).mkString("; ")),
          identity
        )

    DatasetEvidence
      .fromOpened(
        opened,
        DatasetRunQuery.All,
        DataSelection(
          time = IndexSelection.indices(1, 3),
          voxels = IndexSelection.indices(0, 2)
        )
      )
      .value
      .unsafeToFuture()
      .map: evaluated =>
        val ingested = evaluated.fold(error => fail(error.message), identity)
        val materialized =
          ingested.observations.evidence
            .materialize(MaterializationPolicy.Allow(MaterializationBudget.unsafe(4L)))
            .fold(error => fail(error.message), identity)
        assertEquals(ingested.timepoints.map(_.value), Vector(1, 3))
        assertEquals(ingested.voxels.map(_.value), Vector(0, 2))
        assertEquals(
          matrixRows(materialized.value),
          Vector(Vector(-2.0, 0.0), Vector(-3.0, -0.5))
        )

  test("dataset evidence and an axis-bound target run through the typed classifier"):
    val ingested =
      DatasetEvidence
        .fromDataset(dataset)
        .fold(error => fail(error.message), identity)
    val face = ClassId.unsafe("face")
    val scene = ClassId.unsafe("scene")
    val classes =
      ClassAxis
        .create(
          AxisId.unsafe("dataset-evidence-classes"),
          Vector(face, scene),
          CoordinateProvenance.unsafe("dataset-evidence-suite", "v1")
        )
        .fold(error => fail(error.message), identity)
    val labels =
      Column(
        ingested.samples,
        Vector(face, scene, face, scene)
      ).fold(error => fail(error.message), identity)
    val target =
      CategoricalTarget(ingested.samples, classes, labels)
        .fold(error => fail(error.message), identity)
    val source: CategoricalObservationSource[
      ingested.samples.Id,
      ingested.features.Id,
      FeatureId
    ] =
      CategoricalObservationSource
        .fromObservations(ingested.observations, target)
        .fold(error => fail(error.message), identity)
    val design: ExactClassificationValidation[ingested.samples.Id] =
      validation[ingested.samples.Id](ingested.samples)
    val measurement =
      Measurement
        .identity(ingested.features, MeasurementId.unsafe("dataset-all-voxels"))
        .fold(error => fail(error.message), identity)
    val frame =
      MeasurementFrame(ingested.features)(
        Vector(MeasurementEntry(measurement, "whole-dataset"))
      ).fold(error => fail(error.message), identity)
    val configuration =
      ClassificationConfiguration(
        SwiftCentroid(PredictorScaling.None)
      ).fold(error => fail(error.message), identity)
    val result =
      Mvpa
        .run(source)(
          design,
          frame,
          source.classify(configuration),
          classificationStrategy
        )
        .fold(error => fail(error.message), identity)

    result.values.head.outcome match
      case MeasurementOutcome.Success(value, _) =>
        assertEqualsDouble(value.accuracy.value, 1.0, 1e-12)
        assertEquals(
          value.predictions.predicted.toVector,
          Vector(face, scene, face, scene)
        )
      case other => fail(s"expected typed classification success, obtained $other")

  private def validation[S <: multivar.core.SemanticSpace](
      samples: AxisRef.Aux[SampleId, S]
  ): ExactClassificationValidation[S] =
    val ordinal =
      LeaveOneGroupOut(
        Labels.dense(indices(0, 0, 1, 1), samples.size).toOption.get
      )
    val authority = SeedAuthority.fromLong(SeedDomain.Validation, 113L)
    val compiled =
      ordinal
        .compile(IndexSpace.of(samples.size).toOption.get, authority.seed)
        .toOption
        .get
    val schedule =
      BoundSchedule(
        compiled,
        samples,
        AxisPopulationFingerprint.fromAxis(samples).toOption.get,
        ScheduleLabels.fromDesign(samples, ordinal).toOption.get,
        authority
      ).toOption.get
    ValidationDesign(
      schedule,
      ScientificAxisName.unsafe("samples"),
      GeneralizationAxis(
        ScientificAxisName.unsafe("samples"),
        samples.identity
      )
    ).toOption.get

  private def classificationStrategy: ExecutionStrategy =
    ExecutionStrategy(
      BackendId.unsafe("alder-portable"),
      ExecutionRepresentation.Dense,
      NumericPrecision.Binary64,
      SolverChoice.NotApplicable,
      Vector.empty,
      Scheduling.serial,
      MaterializationPolicy.Allow(MaterializationBudget.unsafe(12L)),
      FallbackPolicy.forbidden,
      ResultDelivery.Collected,
      Vector("learner" -> "swift-centroid")
    ).toOption.get

  private def indices(values: Int*): IArray[Int] =
    IArray.unsafeFromArray(values.toArray)

  private def matrixRows(matrix: gale.linalg.DMat): Vector[Vector[Double]] =
    Vector.tabulate(matrix.rows): row =>
      Vector.tabulate(matrix.cols): column =>
        matrix(row, column)
