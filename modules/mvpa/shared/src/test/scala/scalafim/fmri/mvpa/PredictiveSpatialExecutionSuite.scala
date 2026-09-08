package scalafim.fmri.mvpa

import gale.linalg.DMat
import multivar.core.ValueId
import resample4s.core.*
import resample4s.designs.LeaveOneGroupOut
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.predictive.*
import scalafim.fmri.mvpa.predictive.PredictiveAnalysis.given
import scalafim.image.*
import scalafim.locus.Selection
import scalafim.locus.SpaceKey

final class PredictiveSpatialExecutionSuite extends munit.FunSuite:
  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private val volumeSpace =
    VolumeSpace(
      NeuroSpace(
        dims = Vector(3, 1, 1),
        spacing = Some(Vector(1.0, 1.0, 1.0)),
        origin = Some(Vector(0.0, 0.0, 0.0))
      )
    )
  private val packed =
    VolumeDomain.semantic(
      SpaceKey.unsafe("predictive-searchlight-volume"),
      volumeSpace
    )
  private type Voxel = packed.S
  private val domain: VolumeDomain[Voxel] = packed.value
  private val neural: IdentifiedLocusAxis[Voxel] =
    SpatialAxes.volume(domain).toOption.get
  private val samples =
    AxisRef
      .create(
        AxisId.unsafe("predictive-searchlight-samples"),
        AxisPurpose.Samples,
        Vector.tabulate(8)(index => SampleId.unsafe(s"sample-$index")),
        CoordinateBasis.unsafe("run-trial-table"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("predictive-searchlight-fixture", "v1")
      )
      .toOption
      .get
  private val cat = ClassId.unsafe("cat")
  private val dog = ClassId.unsafe("dog")
  private val classes =
    ClassAxis
      .create(
        AxisId.unsafe("predictive-searchlight-classes"),
        Vector(cat, dog),
        CoordinateProvenance.unsafe("predictive-searchlight-fixture", "v1")
      )
      .toOption
      .get
  private val target =
    CategoricalTarget(
      samples,
      classes,
      Column(
        samples,
        Vector(cat, dog, cat, dog, cat, dog, cat, dog)
      ).toOption.get
    ).toOption.get
  private val evidence =
    EvidenceTable
      .dense(
        samples,
        neural.features,
        DMat.dense(
          8,
          3,
          Vector(
            -4.0, -1.0, -2.0, 4.0, 1.0, 2.0, -3.0, -0.5, -3.0, 3.0, 0.5, 3.0, -2.0, -1.5, -4.0, 2.0, 1.5, 4.0, -5.0,
            -0.25, -5.0, 5.0, 0.25, 5.0
          )
        ),
        ValueId.unsafe("predictive-searchlight-patterns")
      )
      .toOption
      .get
  private val source: CategoricalObservationSource[
    samples.Id,
    neural.features.Id,
    FeatureId
  ] =
    CategoricalObservationSource(evidence, target).toOption.get
  private val design =
    val runs =
      Labels.dense(indices(0, 0, 1, 1, 2, 2, 3, 3), samples.size).toOption.get
    val ordinal = LeaveOneGroupOut(runs)
    val authority = SeedAuthority.fromLong(SeedDomain.Validation, 131L)
    val compiled = ordinal
      .compile(IndexSpace.of(samples.size).toOption.get, authority.seed)
      .toOption
      .get
    val schedule = BoundSchedule(
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
  private val configuration =
    ClassificationConfiguration(
      StandardizedNearestCentroid(
        StandardizationSpecification.CenterScaleRejectConstant
      )
    ).toOption.get
  private val centers =
    Selection
      .fromOrdinals(domain.finiteSpace, Vector(2, 0))
      .toOption
      .get
  private val frame =
    VolumeFrames
      .metricSearchlights(
        neural,
        domain,
        SearchlightRadius.make(0.0).toOption.get,
        centers
      )
      .toOption
      .get
  private val strategy =
    ExecutionStrategy(
      BackendId.unsafe("alder-portable"),
      ExecutionRepresentation.Dense,
      NumericPrecision.Binary64,
      SolverChoice.NotApplicable,
      Vector.empty,
      Scheduling.serial,
      MaterializationPolicy.Allow(MaterializationBudget.unsafe(8L)),
      FallbackPolicy.forbidden,
      ResultDelivery.Collected
    ).toOption.get

  private def indices(values: Int*): IArray[Int] =
    IArray.unsafeFromArray(values.toArray)

  private def execute[R](
      requestedFrame: MeasurementFrame[
        neural.features.Id,
        FeatureId,
        R
      ]
  ) =
    Mvpa
      .run(source)(
        design,
        requestedFrame,
        source.classify(configuration),
        strategy
      )
      .toOption
      .get

  private def result = execute(frame)

  test("searchlight execution scatters by typed compact-center rendition"):
    val analysis = result
    assertEquals(
      analysis.values.map(_.measurement.id.value),
      Vector("searchlight-0", "searchlight-2")
    )
    assertEquals(
      analysis.values.map(_.rendition.center.ordinal),
      Vector(1, 0)
    )

    val scattered =
      SpatialResultScatter
        .searchlights(centers.positions, analysis)
        .toOption
        .get
    val first = centers.positions.index(0).toOption.get
    val second = centers.positions.index(1).toOption.get
    assertEquals(
      scattered(first).receipt.measurement.id.value,
      "searchlight-2"
    )
    assertEquals(
      scattered(second).receipt.measurement.id.value,
      "searchlight-0"
    )
    scattered.toVector.foreach:
      case MeasurementOutcome.Success(value, receipt) =>
        assertEquals(value.accuracy.value, 1.0)
        assertEquals(receipt.materializedCells, 8L)
      case other => fail(s"expected successful searchlight, obtained $other")

  test("scatter rejects duplicate typed centers rather than using result order"):
    val repeatedRendition = frame.entries.head.rendition
    val duplicateFrame = MeasurementFrame(neural.features)(
      frame.entries.map: entry =>
        MeasurementEntry(entry.measurement, repeatedRendition)
    ).toOption.get
    val duplicate = execute(duplicateFrame)
    val rejected =
      SpatialResultScatter
        .searchlights(centers.positions, duplicate)
        .left
        .exists:
          case SpatialScatterError.DuplicateCenter(1, first, second) =>
            first.value == "searchlight-0" && second.value == "searchlight-2"
          case _ => false
    assert(rejected)
