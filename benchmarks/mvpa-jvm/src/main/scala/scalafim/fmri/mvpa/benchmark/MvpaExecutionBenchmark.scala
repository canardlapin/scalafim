package scalafim.fmri.mvpa.benchmark

import gale.linalg.{DMat, DVec, DoubleLinearOperator, Matrix, MutableDVec}
import java.util.concurrent.TimeUnit
import multivar.core.{ValueId, ValueIdentity}
import org.openjdk.jmh.annotations.{Measurement as JmhMeasurement, *}
import org.openjdk.jmh.infra.Blackhole
import resample4s.core.*
import resample4s.designs.LeaveOneGroupOut
import scala.compiletime.uninitialized
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.predictive.*
import scalafim.fmri.mvpa.predictive.OperatorRidgeAnalysis.given

private object BenchmarkEither:
  def orThrow[E, A](value: Either[E, A]): A =
    value.fold(
      error => throw new IllegalArgumentException(error.toString),
      identity
    )

/** End-to-end operator-ridge validation with all axes, evidence, schedules, and execution policy prepared outside the
  * timed method.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(1)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@JmhMeasurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class PredictiveExecutionBenchmark:

  @Param(Array("24", "48"))
  var samples: Int = 0

  @Param(Array("16", "64"))
  var features: Int = 0

  private var fixture: PredictiveFixture = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    fixture = new PredictiveFixture(samples, features)
    fixture.verify()

  @Benchmark
  def operatorRidgeValidation(blackhole: Blackhole): Unit =
    fixture.run(blackhole)

/** One exact relational estimand compiled from equivalent dense and matrix-free evidence. Only execution representation
  * and materialization policy differ; setup verifies the resulting RDMs before timing begins.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Fork(1)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@JmhMeasurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
class RelationalQueryBenchmark:

  @Param(Array("12"))
  var effects: Int = 0

  @Param(Array("64"))
  var neuralCoordinates: Int = 0

  @Param(Array("4"))
  var partitions: Int = 0

  private var fixture: RelationalFixture = uninitialized

  @Setup(Level.Trial)
  def setup(): Unit =
    fixture = new RelationalFixture(effects, neuralCoordinates, partitions)
    fixture.verifyEquivalent()

  @Benchmark
  def explicitDenseMaterialization(blackhole: Blackhole): Unit =
    blackhole.consume(fixture.denseRdm())

  @Benchmark
  def matrixFreeProjection(blackhole: Blackhole): Unit =
    blackhole.consume(fixture.operatorRdm())

private final class PredictiveFixture(
    sampleCount: Int,
    featureCount: Int
):
  import BenchmarkEither.orThrow

  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  require(sampleCount >= 12 && sampleCount % 12 == 0)
  require(featureCount > 0)

  private val samples = orThrow(
    AxisRef.create(
      AxisId.unsafe("benchmark-predictive-samples"),
      AxisPurpose.Samples,
      Vector.tabulate(sampleCount)(index => SampleId.unsafe(s"sample-$index")),
      CoordinateBasis.unsafe("group-trial-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("mvpa-benchmark", "predictive-v1")
    )
  )
  private val features = orThrow(
    AxisRef.create(
      AxisId.unsafe("benchmark-predictive-features"),
      AxisPurpose.NeuralFeatures,
      Vector.tabulate(featureCount)(index => FeatureId.unsafe(s"feature-$index")),
      CoordinateBasis.unsafe("feature-order"),
      Some(AxisUnits.unsafe("standardized-signal")),
      AxisScale.nominal,
      CoordinateProvenance.unsafe("mvpa-benchmark", "predictive-v1")
    )
  )
  private val classes = orThrow(
    ClassAxis.create(
      AxisId.unsafe("benchmark-predictive-classes"),
      Vector(ClassId.unsafe("a"), ClassId.unsafe("b"), ClassId.unsafe("c")),
      CoordinateProvenance.unsafe("mvpa-benchmark", "predictive-v1")
    )
  )
  private val labels =
    Vector.tabulate(sampleCount)(row => classes.keys(row % classes.size))
  private val categoricalTarget = orThrow(
    CategoricalTarget(
      samples,
      classes,
      orThrow(Column(samples, labels))
    )
  )
  private val target = orThrow(ClassMembershipTarget.hard(categoricalTarget))
  private val values = DMat.tabulate(sampleCount, featureCount): (row, column) =>
    val klass = row % classes.size
    val signal = if column % classes.size == klass then 2.0 else -0.4
    signal +
      0.15 * math.sin((row + 1).toDouble * 0.37 + (column + 1).toDouble * 0.19)
  private val evidence = orThrow(
    EvidenceTable.dense(
      samples,
      features,
      values,
      ValueId.unsafe("benchmark-predictive-values")
    )
  )
  private val design =
    val groupSize = sampleCount / 4
    val groupLabels = orThrow(
      Labels.dense(
        IArray.unsafeFromArray(Array.tabulate(sampleCount)(_ / groupSize)),
        sampleCount
      )
    )
    val ordinal = LeaveOneGroupOut(groupLabels)
    val authority = SeedAuthority.fromLong(SeedDomain.Validation, 743L)
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
  private val measurement = orThrow(
    Measurement.identity(
      features,
      MeasurementId.unsafe("benchmark-predictive-global")
    )
  )
  private val frame = orThrow(
    MeasurementFrame(features)(
      Vector(MeasurementEntry(measurement, NoRendition))
    )
  )
  private val source = orThrow(MembershipObservationSource(evidence, target))
  private val configuration = orThrow(OperatorRidgeConfiguration.fixed(0.7))
  private val solver = orThrow(
    OperatorRidgeSolverSettings(tolerance = 1e-8, maxIterations = 500)
  )
  private val strategy = orThrow(
    ExecutionStrategy(
      BackendId.unsafe("gale-portable"),
      ExecutionRepresentation.Operator,
      NumericPrecision.Binary64,
      OperatorRidgeSolverSettings.choice(solver),
      Vector.empty,
      Scheduling.serial,
      MaterializationPolicy.Reject,
      FallbackPolicy.forbidden,
      ResultDelivery.Collected,
      Vector("kernel" -> "operator-ridge")
    )
  )
  private val estimand = source.operatorRidge(configuration)

  def run(blackhole: Blackhole): Unit =
    blackhole.consume(execute())

  private def execute() =
    orThrow(
      Mvpa.run(source)(
        design,
        frame,
        estimand,
        strategy
      )
    )

  def verify(): Unit =
    val result = execute()
    val receipt = result.receipt
    require(result.counts.expected == 1)
    require(result.counts.visited == 1)
    require(result.counts.completion == TraversalCompletion.Exhausted)
    require(receipt.work.operatorApplications > 0L)
    require(receipt.work.materializedCells == 0L)
    require(receipt.convergence.nonEmpty)
    require(receipt.convergence.forall(_.iterations > 0))

private final class RelationalFixture(
    effectCount: Int,
    neuralCount: Int,
    partitionCount: Int
):
  import BenchmarkEither.orThrow

  require(effectCount >= 2)
  require(neuralCount > 0)
  require(partitionCount >= 2)

  private val effects = orThrow(
    AxisRef.create(
      AxisId.unsafe("benchmark-relational-effects"),
      AxisPurpose.Effects,
      Vector.tabulate(effectCount)(index => AxisKey.unsafe(s"effect-$index")),
      CoordinateBasis.unsafe("effect-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("mvpa-benchmark", "relational-v1")
    )
  )
  private val neural = orThrow(
    AxisRef.create(
      AxisId.unsafe("benchmark-relational-neural"),
      AxisPurpose.NeuralFeatures,
      Vector.tabulate(neuralCount)(index => FeatureId.unsafe(s"voxel-$index")),
      CoordinateBasis.unsafe("voxel-order"),
      Some(AxisUnits.unsafe("percent-signal-change")),
      AxisScale.nominal,
      CoordinateProvenance.unsafe("mvpa-benchmark", "relational-v1")
    )
  )
  private val partitions = orThrow(
    AxisRef.create(
      AxisId.unsafe("benchmark-relational-partitions"),
      AxisPurpose.Partitions,
      Vector.tabulate(partitionCount)(index => PartitionId.unsafe(s"run-$index")),
      CoordinateBasis.unsafe("run-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("mvpa-benchmark", "relational-v1")
    )
  )
  private val namedPartitions = orThrow(
    PartitionAxis(ScientificAxisName.unsafe("runs"), partitions)
  )
  private val training = orThrow(
    AxisRef.create(
      AxisId.unsafe("benchmark-relational-training"),
      AxisPurpose.Samples,
      Vector(SampleId.unsafe("training-a"), SampleId.unsafe("training-b")),
      CoordinateBasis.unsafe("training-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("mvpa-benchmark", "relational-v1")
    )
  )
  private val matrices = Vector.tabulate(partitionCount): partition =>
    Matrix.tabulate(effectCount, neuralCount): (effect, coordinate) =>
      val base = math.sin((effect + 1).toDouble * 0.31 + (coordinate + 1).toDouble * 0.017)
      base + 0.02 * partition.toDouble * math.cos((coordinate + 1).toDouble * 0.11)
  private val fitDesign = orThrow(
    DesignIdentity(DesignKind.unsafe("benchmark-relational-fit"))
  )
  private val measurement =
    val localCount = math.max(1, neuralCount / 2)
    val injection = orThrow(
      Injection.from(
        IArray.unsafeFromArray(Array.tabulate(localCount)(identity)),
        orThrow(IndexSpace.of(neural.size))
      )
    )
    orThrow(
      Measurement.hardSelection(
        neural,
        MeasurementId.unsafe("benchmark-relational-half-features"),
        injection
      )
    )
  private val denseSource = source(operatorBacked = false)
  private val operatorSource = source(operatorBacked = true)
  private val domain = orThrow(WithinPairDomain(effects))
  private val denseRequest = orThrow(
    RelationalFitQuery.identityPrecision(
      denseSource,
      domain,
      RdmNormalization.Raw
    )
  )
  private val operatorRequest = orThrow(
    RelationalFitQuery.identityPrecision(
      operatorSource,
      domain,
      RdmNormalization.Raw
    )
  )
  private val design = pairingFor(denseSource)
  private val denseMaterialization = MaterializationPolicy.Allow(
    MaterializationBudget.unsafe(
      partitionCount.toLong * effectCount.toLong * neuralCount.toLong
    )
  )

  def denseRdm(): Vector[Double] =
    val fit = orThrow(
      RelationalFit.query(
        denseRequest,
        design,
        measurement,
        denseMaterialization
      )
    )
    orThrow(fit.rdm).distances.toVector

  def operatorRdm(): Vector[Double] =
    val fit = orThrow(
      RelationalFit.query(
        operatorRequest,
        design,
        measurement
      )
    )
    orThrow(fit.rdm).distances.toVector

  def verifyEquivalent(): Unit =
    require(denseSource.identity == operatorSource.identity)
    val dense = denseRdm()
    val operator = operatorRdm()
    require(dense.length == operator.length)
    var index = 0
    while index < dense.length do
      require(math.abs(dense(index) - operator(index)) <= 1e-10)
      index += 1

  private def source(
      operatorBacked: Boolean
  ): PartitionedRelations[
    partitions.Id,
    effects.Id,
    neural.Id,
    AxisKey,
    FeatureId,
    EstimateOnlyCapabilities[neural.Id, FeatureId]
  ] =
    val capabilities = orThrow(EstimateOnlyCapabilities(neural))
    val entries = partitions.keys.zipWithIndex.map: (partition, position) =>
      val valueId = ValueId.unsafe(s"benchmark-relation-${partition.value}")
      val estimate =
        if operatorBacked then
          orThrow(
            EvidenceTable.operator(
              effects,
              neural,
              new MatrixFree(matrices(position)),
              valueId
            )
          )
        else
          orThrow(
            EvidenceTable.dense(
              effects,
              neural,
              matrices(position),
              valueId
            )
          )
      val receipt = orThrow(
        RelationFitReceipt(
          ValueIdentity.source(ValueId.unsafe(s"benchmark-source-${partition.value}")),
          fitDesign,
          orThrow(Estimability(effects, Vector.fill(effectCount)(true))),
          NormalizationIdentity.none,
          training
        )
      )
      PartitionRelation(
        partition,
        orThrow(Relation(estimate, receipt, capabilities))
      )
    orThrow(
      PartitionedRelations(
        namedPartitions,
        effects,
        neural,
        entries
      )
    )

  private def pairingFor(
      evidence: PartitionedRelations[
        partitions.Id,
        effects.Id,
        neural.Id,
        AxisKey,
        FeatureId,
        EstimateOnlyCapabilities[neural.Id, FeatureId]
      ]
  ): PairingDesign[partitions.Id, partitions.Id] =
    val partitionEvidence = orThrow(
      PartitionEvidenceIdentity(evidence.identity, namedPartitions)
    )
    val pairs = Vector.newBuilder[DeclaredIndependentPair]
    var left = 0
    while left < partitionCount - 1 do
      var right = left + 1
      while right < partitionCount do
        pairs += DeclaredIndependentPair(
          partitions.keys(left),
          partitions.keys(right)
        )
        right += 1
      left += 1
    val declaration = orThrow(
      PartitionIndependenceDeclaration(
        orThrow(IndependenceDeclarationId("benchmark-independent-runs")),
        partitionEvidence,
        partitionEvidence,
        pairs.result()
      )
    )
    orThrow(
      PairingDesign.allOrdered(
        namedPartitions,
        PairingReducer.WeightedMean,
        GeneralizationAxis(
          ScientificAxisName.unsafe("runs"),
          partitions.identity
        ),
        declaration
      )
    )

private final class MatrixFree(matrix: DMat) extends DoubleLinearOperator:
  override def rows: Int = matrix.rows
  override def cols: Int = matrix.cols

  override def applyTo(input: DVec, output: MutableDVec): Unit =
    var row = 0
    while row < rows do
      var sum = 0.0
      var column = 0
      while column < cols do
        sum += matrix(row, column) * input(column)
        column += 1
      output(row) = sum
      row += 1

  override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
    var column = 0
    while column < cols do
      var sum = 0.0
      var row = 0
      while row < rows do
        sum += matrix(row, column) * input(row)
        row += 1
      output(column) = sum
      column += 1
