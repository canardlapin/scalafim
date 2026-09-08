package scalafim.fmri.mvpa.benchmark

import gale.linalg.{DMat, Matrix}
import java.util.concurrent.TimeUnit
import multivar.core.{ValueId, ValueIdentity}
import org.openjdk.jmh.annotations.{Measurement as JmhMeasurement, *}
import org.openjdk.jmh.infra.Blackhole
import resample4s.core.*
import resample4s.designs.LeaveOneGroupOut
import scala.compiletime.uninitialized
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.ObservationRdm.given
import scalafim.fmri.mvpa.predictive.*
import scalafim.fmri.mvpa.predictive.PredictiveAnalysis.given

private object ConformanceEither:
  def orThrow[E, A](value: Either[E, A]): A =
    value.fold(
      error => throw new IllegalArgumentException(error.toString),
      identity
    )

private trait ConformanceBenchmarkPolicy:
  final val observationSamples = 96
  final val observationFeatures = 64
  final val relationPartitions = 8
  final val relationEffects = 8
  final val relationFeatures = 64
  final val predictiveRuns = 8
  final val predictiveClasses = 4
  final val predictiveFeatures = 32
  final val predictiveSupports = 16
  final val predictiveSupportWidth = 8

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(1)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@JmhMeasurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class ObservationRdmConformanceBenchmark extends ConformanceBenchmarkPolicy:
  private var fixture: ObservationConformanceFixture = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    fixture = new ObservationConformanceFixture(
      observationSamples,
      observationFeatures
    )
    fixture.verify()

  @Benchmark
  def correlationPublicCall(blackhole: Blackhole): Unit =
    blackhole.consume(fixture.run())

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(1)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@JmhMeasurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class RelationalRdmConformanceBenchmark extends ConformanceBenchmarkPolicy:
  private var fixture: RelationalConformanceFixture = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    fixture = new RelationalConformanceFixture(
      relationEffects,
      relationFeatures,
      relationPartitions
    )
    fixture.verify()

  @Benchmark
  def identityCrossvalidatedRdmPublicCall(blackhole: Blackhole): Unit =
    blackhole.consume(fixture.rdmChecksum())

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Fork(1)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@JmhMeasurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class RsaQueryConformanceBenchmark extends ConformanceBenchmarkPolicy:
  private var fixture: RelationalConformanceFixture = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    fixture = new RelationalConformanceFixture(
      relationEffects,
      relationFeatures,
      relationPartitions
    )
    fixture.verify()

  @Benchmark
  def pearsonQuery(blackhole: Blackhole): Unit =
    blackhole.consume(fixture.pearson())

  @Benchmark
  def interceptedOlsQuery(blackhole: Blackhole): Unit =
    blackhole.consume(fixture.regressionChecksum())

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(1)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@JmhMeasurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class PredictiveCentroidConformanceBenchmark extends ConformanceBenchmarkPolicy:
  private var fixture: PredictiveConformanceFixture = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    fixture = new PredictiveConformanceFixture(
      predictiveRuns,
      predictiveClasses,
      predictiveFeatures,
      predictiveSupports,
      predictiveSupportWidth
    )
    fixture.verifyWhole()

  @Benchmark
  def leaveOneRunOutPublicCall(blackhole: Blackhole): Unit =
    blackhole.consume(fixture.whole())

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(1)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@JmhMeasurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class PredictiveFrameConformanceBenchmark extends ConformanceBenchmarkPolicy:
  private var fixture: PredictiveConformanceFixture = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    fixture = new PredictiveConformanceFixture(
      predictiveRuns,
      predictiveClasses,
      predictiveFeatures,
      predictiveSupports,
      predictiveSupportWidth
    )
    fixture.verifyFrame()

  @Benchmark
  def fixedSupportFramePublicCall(blackhole: Blackhole): Unit =
    blackhole.consume(fixture.frame())

private final class ObservationConformanceFixture(
    sampleCount: Int,
    featureCount: Int
):
  import ConformanceEither.orThrow

  private val samples = orThrow(
    AxisRef.create(
      AxisId.unsafe("conformance-benchmark-observation-samples"),
      AxisPurpose.Samples,
      Vector.tabulate(sampleCount)(index => SampleId.unsafe(s"sample-$index")),
      CoordinateBasis.unsafe("sample-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("mvpa-conformance-benchmark", "v1")
    )
  )
  private val neural = orThrow(
    AxisRef.create(
      AxisId.unsafe("conformance-benchmark-observation-features"),
      AxisPurpose.NeuralFeatures,
      Vector.tabulate(featureCount)(index => FeatureId.unsafe(s"feature-$index")),
      CoordinateBasis.unsafe("feature-order"),
      Some(AxisUnits.unsafe("activation")),
      AxisScale.nominal,
      CoordinateProvenance.unsafe("mvpa-conformance-benchmark", "v1")
    )
  )
  private val values = DMat.tabulate(sampleCount, featureCount): (row, column) =>
    math.sin((row + 1).toDouble * 0.13 + (column + 1).toDouble * 0.07) +
      0.25 * math.cos((row + 1).toDouble * (column + 1).toDouble * 0.003)
  private val evidence = orThrow(
    EvidenceTable.dense(
      samples,
      neural,
      values,
      ValueId.unsafe("conformance-benchmark-observation-values")
    )
  )
  private val source = orThrow(Observations(evidence))
  private val design = orThrow(ObservationFitDesign.entireTable(samples))
  private val measurement = orThrow(
    Measurement.identity(
      neural,
      MeasurementId.unsafe("conformance-benchmark-observation-whole")
    )
  )
  private val frame = orThrow(
    MeasurementFrame(neural)(Vector(MeasurementEntry(measurement, "whole")))
  )
  private val strategy = orThrow(
    ExecutionStrategy(
      BackendId.unsafe("gale-portable-conformance-benchmark"),
      ExecutionRepresentation.Dense,
      NumericPrecision.Binary64,
      SolverChoice.NotApplicable,
      Vector.empty,
      Scheduling.serial,
      MaterializationPolicy.Allow(
        MaterializationBudget.unsafe(sampleCount.toLong * featureCount.toLong)
      ),
      FallbackPolicy.forbidden,
      ResultDelivery.Collected
    )
  )
  private val estimand = ObservationRdm(source, ObservationDistance.Correlation)

  def run() =
    orThrow(Mvpa.run(source)(design, frame, estimand, strategy))

  def verify(): Unit =
    val result = run()
    require(result.counts.expected == 1)
    result.values.head.outcome match
      case MeasurementOutcome.Success(value, _) =>
        require(value.distances.toVector.length == sampleCount * (sampleCount - 1) / 2)
        require(
          math.abs(value.distances.toVector.sum - 4571.000223431536) <= 1e-10
        )
      case other => throw new IllegalArgumentException(other.toString)

private final class RelationalConformanceFixture(
    effectCount: Int,
    neuralCount: Int,
    partitionCount: Int
):
  import ConformanceEither.orThrow

  private val effects = orThrow(
    AxisRef.create(
      AxisId.unsafe("conformance-benchmark-effects"),
      AxisPurpose.Effects,
      Vector.tabulate(effectCount)(index => AxisKey.unsafe(s"condition-$index")),
      CoordinateBasis.unsafe("condition-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("mvpa-conformance-benchmark", "v1")
    )
  )
  private val neural = orThrow(
    AxisRef.create(
      AxisId.unsafe("conformance-benchmark-relational-features"),
      AxisPurpose.NeuralFeatures,
      Vector.tabulate(neuralCount)(index => FeatureId.unsafe(s"feature-$index")),
      CoordinateBasis.unsafe("feature-order"),
      Some(AxisUnits.unsafe("activation")),
      AxisScale.nominal,
      CoordinateProvenance.unsafe("mvpa-conformance-benchmark", "v1")
    )
  )
  private val partitions = orThrow(
    AxisRef.create(
      AxisId.unsafe("conformance-benchmark-runs"),
      AxisPurpose.Partitions,
      Vector.tabulate(partitionCount)(index => PartitionId.unsafe(s"run-$index")),
      CoordinateBasis.unsafe("run-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("mvpa-conformance-benchmark", "v1")
    )
  )
  private val partitionAxis = orThrow(
    PartitionAxis(ScientificAxisName.unsafe("runs"), partitions)
  )
  private val training = orThrow(
    AxisRef.create(
      AxisId.unsafe("conformance-benchmark-relation-training"),
      AxisPurpose.Samples,
      Vector(SampleId.unsafe("fit-a"), SampleId.unsafe("fit-b")),
      CoordinateBasis.unsafe("fit-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("mvpa-conformance-benchmark", "v1")
    )
  )
  private val fitDesign = orThrow(
    DesignIdentity(DesignKind.unsafe("conformance-benchmark-relation-fit"))
  )
  private val matrices = Vector.tabulate(partitionCount): partition =>
    Matrix.tabulate(effectCount, neuralCount): (effect, coordinate) =>
      math.sin((effect + 1).toDouble * 0.31 + (coordinate + 1).toDouble * 0.017) +
        0.02 * partition.toDouble * math.cos((coordinate + 1).toDouble * 0.11)
  private val capabilities = orThrow(EstimateOnlyCapabilities(neural))
  private val source =
    val entries = partitions.keys.zipWithIndex.map: (partition, position) =>
      val estimate = orThrow(
        EvidenceTable.dense(
          effects,
          neural,
          matrices(position),
          ValueId.unsafe(s"conformance-benchmark-estimate-${partition.value}")
        )
      )
      val receipt = orThrow(
        RelationFitReceipt(
          ValueIdentity.source(
            ValueId.unsafe(s"conformance-benchmark-source-${partition.value}")
          ),
          fitDesign,
          orThrow(Estimability(effects, Vector.fill(effectCount)(true))),
          NormalizationIdentity.none,
          training
        )
      )
      PartitionRelation(partition, orThrow(Relation(estimate, receipt, capabilities)))
    orThrow(PartitionedRelations(partitionAxis, effects, neural, entries))
  private val evidenceIdentity = orThrow(
    PartitionEvidenceIdentity(source.identity, partitionAxis)
  )
  private val independentPairs =
    for
      left <- 0 until partitionCount - 1
      right <- left + 1 until partitionCount
    yield DeclaredIndependentPair(partitions.keys(left), partitions.keys(right))
  private val independence = orThrow(
    PartitionIndependenceDeclaration(
      orThrow(IndependenceDeclarationId("conformance-benchmark-independent-runs")),
      evidenceIdentity,
      evidenceIdentity,
      independentPairs.toVector
    )
  )
  private val pairing = orThrow(
    PairingDesign.allOrdered(
      partitionAxis,
      PairingReducer.WeightedMean,
      GeneralizationAxis(ScientificAxisName.unsafe("runs"), partitions.identity),
      independence
    )
  )
  private val measurement = orThrow(
    Measurement.identity(
      neural,
      MeasurementId.unsafe("conformance-benchmark-relational-whole")
    )
  )
  private val domain = orThrow(WithinPairDomain(effects))
  private val query = orThrow(
    RelationalFitQuery.identityPrecision(
      source,
      domain,
      RdmNormalization.DivideByNeuralDimension
    )
  )
  private val materialization = MaterializationPolicy.Allow(
    MaterializationBudget.unsafe(
      partitionCount.toLong * effectCount.toLong * neuralCount.toLong
    )
  )
  private val fit = executeFit()
  private val modelValues =
    (for
      left <- 0 until effectCount - 1
      right <- left + 1 until effectCount
    yield if left % 2 != right % 2 then 1.0 else 0.0).toVector
  private val model = orThrow(
    SecondOrderModel.signal(
      domain,
      SecondOrderModelName.unsafe("alternating-category"),
      modelValues
    )
  )

  private def executeFit() =
    orThrow(
      RelationalFit.query(
        query,
        pairing,
        measurement,
        materialization
      )
    )

  def rdmChecksum(): Double =
    orThrow(executeFit().rdm).distances.toVector.sum

  def pearson(): Double =
    orThrow(RelationalRsa.pearson(fit, model)).correlation

  def regressionChecksum(): Double =
    orThrow(
      RelationalRsa.regression(
        fit,
        Vector(model),
        Vector.empty,
        intercept = true
      )
    ).coefficients.map(_.value).sum

  def verify(): Unit =
    require(math.abs(rdmChecksum() - 6.874590519956278) <= 1e-10)
    require(math.abs(pearson() - -0.11748100095173242) <= 1e-10)
    require(math.abs(regressionChecksum() - 0.22064725063086724) <= 1e-10)

private final class PredictiveConformanceFixture(
    runCount: Int,
    classCount: Int,
    featureCount: Int,
    supportCount: Int,
    supportWidth: Int
):
  import ConformanceEither.orThrow

  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private val sampleCount = runCount * classCount
  private val samples = orThrow(
    AxisRef.create(
      AxisId.unsafe("conformance-benchmark-predictive-samples"),
      AxisPurpose.Samples,
      Vector.tabulate(sampleCount)(index => SampleId.unsafe(s"sample-$index")),
      CoordinateBasis.unsafe("run-class-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("mvpa-conformance-benchmark", "v1")
    )
  )
  private val neural = orThrow(
    AxisRef.create(
      AxisId.unsafe("conformance-benchmark-predictive-features"),
      AxisPurpose.NeuralFeatures,
      Vector.tabulate(featureCount)(index => FeatureId.unsafe(s"feature-$index")),
      CoordinateBasis.unsafe("feature-order"),
      Some(AxisUnits.unsafe("activation")),
      AxisScale.nominal,
      CoordinateProvenance.unsafe("mvpa-conformance-benchmark", "v1")
    )
  )
  private val classes = orThrow(
    ClassAxis.create(
      AxisId.unsafe("conformance-benchmark-classes"),
      Vector.tabulate(classCount)(index => ClassId.unsafe(s"class-$index")),
      CoordinateProvenance.unsafe("mvpa-conformance-benchmark", "v1")
    )
  )
  private val labels = Vector.tabulate(sampleCount)(row => classes.keys(row % classCount))
  private val target = orThrow(
    CategoricalTarget(samples, classes, orThrow(Column(samples, labels)))
  )
  private val values = DMat.tabulate(sampleCount, featureCount): (row, feature) =>
    val category = row % classCount
    val signal = if feature % classCount == category then 2.0 else -0.4
    signal + 0.15 * math.sin((row + 1).toDouble * 0.37 + (feature + 1).toDouble * 0.19)
  private val evidence = orThrow(
    EvidenceTable.dense(
      samples,
      neural,
      values,
      ValueId.unsafe("conformance-benchmark-predictive-values")
    )
  )
  private val source = orThrow(CategoricalObservationSource(evidence, target))
  private val design =
    val labels = orThrow(
      Labels.dense(
        IArray.unsafeFromArray(Array.tabulate(sampleCount)(_ / classCount)),
        sampleCount
      )
    )
    val ordinal = LeaveOneGroupOut(labels)
    val authority = SeedAuthority.fromLong(SeedDomain.Validation, 20260826L)
    val compiled = orThrow(
      ordinal.compile(orThrow(IndexSpace.of(sampleCount)), authority.seed)
    )
    val schedule = orThrow(
      BoundSchedule(
        compiled,
        samples,
        orThrow(AxisPopulationFingerprint.fromAxis(samples)),
        orThrow(ScheduleLabels.fromDesign(samples, ordinal)),
        authority
      )
    )
    orThrow(
      ValidationDesign(
        schedule,
        ScientificAxisName.unsafe("samples"),
        GeneralizationAxis(ScientificAxisName.unsafe("samples"), samples.identity)
      )
    )
  private val wholeMeasurement = orThrow(
    Measurement.identity(
      neural,
      MeasurementId.unsafe("conformance-benchmark-predictive-whole")
    )
  )
  private val wholeFrame = orThrow(
    MeasurementFrame(neural)(Vector(MeasurementEntry(wholeMeasurement, "whole")))
  )
  private val supportMeasurements = Vector.tabulate(supportCount): center =>
    val ordinals = Array.tabulate(supportWidth): offset =>
      (center * 2 + offset) % featureCount
    orThrow(
      Measurement.hardSelection(
        neural,
        MeasurementId.unsafe(s"conformance-benchmark-support-$center"),
        orThrow(
          Injection.from(
            IArray.unsafeFromArray(ordinals),
            orThrow(IndexSpace.of(featureCount))
          )
        )
      )
    )
  private val supportFrame = orThrow(
    MeasurementFrame(neural)(
      supportMeasurements.zipWithIndex.map: (measurement, index) =>
        MeasurementEntry(measurement, s"support-$index")
    )
  )
  private val configuration = orThrow(
    ClassificationConfiguration(
      StandardizedNearestCentroid(
        StandardizationSpecification.CenterScaleRejectConstant
      )
    )
  )
  private val estimand = source.classify(configuration)
  private val strategy = orThrow(
    ExecutionStrategy(
      BackendId.unsafe("alder-portable-conformance-benchmark"),
      ExecutionRepresentation.Dense,
      NumericPrecision.Binary64,
      SolverChoice.NotApplicable,
      Vector.empty,
      Scheduling.serial,
      MaterializationPolicy.Allow(
        MaterializationBudget.unsafe(
          sampleCount.toLong * featureCount.toLong * supportCount.toLong
        )
      ),
      FallbackPolicy.forbidden,
      ResultDelivery.Collected
    )
  )

  def whole(): Double =
    val result = execute(wholeFrame)
    result.values.map: value =>
      value.outcome match
        case MeasurementOutcome.Success(estimate, _) => estimate.accuracy.value
        case other => throw new IllegalArgumentException(other.toString)
    .sum

  def frame(): Double =
    val result = execute(supportFrame)
    result.values.map: value =>
      value.outcome match
        case MeasurementOutcome.Success(estimate, _) => estimate.accuracy.value
        case other => throw new IllegalArgumentException(other.toString)
    .sum

  def verifyWhole(): Unit =
    require(math.abs(whole() - 1.0) <= 1e-12)

  def verifyFrame(): Unit =
    require(math.abs(frame() - supportCount.toDouble) <= 1e-12)

  private def execute[R](frame: MeasurementFrame[neural.Id, FeatureId, R]) =
    orThrow(Mvpa.run(source)(design, frame, estimand, strategy))
