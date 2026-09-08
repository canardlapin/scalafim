package scalafim.fmri.mvpa

import gale.linalg.{DMat, DVec, DoubleLinearOperator, Matrix, MutableDVec}
import multivar.core.{ValueId, ValueIdentity}
import resample4s.core.*
import resample4s.designs.LeaveOneGroupOut
import scalafim.fmri.mvpa.predictive.*
import scalafim.fmri.mvpa.predictive.OperatorRidgeAnalysis.given
import scalafim.fmri.mvpa.predictive.PredictiveAnalysis.given

final class MvpaWorkAccountingSuite extends munit.FunSuite:
  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private val sampleCount = 12
  private val featureCount = 4
  private val classCount = 3
  private val samples = right(
    AxisRef.create(
      AxisId.unsafe("work-court-samples"),
      AxisPurpose.Samples,
      Vector.tabulate(sampleCount)(index => SampleId.unsafe(s"sample-$index")),
      CoordinateBasis.unsafe("run-trial-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("mvpa-work-court", "v1")
    )
  )
  private val features = right(
    AxisRef.create(
      AxisId.unsafe("work-court-features"),
      AxisPurpose.NeuralFeatures,
      Vector.tabulate(featureCount)(index => FeatureId.unsafe(s"feature-$index")),
      CoordinateBasis.unsafe("feature-order"),
      Some(AxisUnits.unsafe("standardized-signal")),
      AxisScale.nominal,
      CoordinateProvenance.unsafe("mvpa-work-court", "v1")
    )
  )
  private val classes = right(
    ClassAxis.create(
      AxisId.unsafe("work-court-classes"),
      Vector(ClassId.unsafe("a"), ClassId.unsafe("b"), ClassId.unsafe("c")),
      CoordinateProvenance.unsafe("mvpa-work-court", "v1")
    )
  )
  private val labels =
    Vector.tabulate(sampleCount)(row => classes.keys(row % classCount))
  private val categoricalTarget = right(
    CategoricalTarget(
      samples,
      classes,
      right(Column(samples, labels))
    )
  )
  private val membershipTarget = right(
    ClassMembershipTarget.hard(categoricalTarget)
  )
  private val patternValues = DMat.tabulate(sampleCount, featureCount): (row, column) =>
    val signal = if column % classCount == row % classCount then 2.0 else -0.5
    signal + 0.1 * math.sin((row + 1).toDouble * 0.7 + (column + 1).toDouble * 0.3)
  private val evidence = right(
    EvidenceTable.dense(
      samples,
      features,
      patternValues,
      ValueId.unsafe("work-court-patterns")
    )
  )
  private val validation =
    val groupLabels = right(
      Labels.dense(
        IArray.unsafeFromArray(Array.tabulate(sampleCount)(_ / classCount)),
        sampleCount
      )
    )
    val ordinal = LeaveOneGroupOut(groupLabels)
    val authority = SeedAuthority.fromLong(SeedDomain.Validation, 811L)
    val compiled = right(
      ordinal.compile(right(IndexSpace.of(sampleCount)), authority.seed)
    )
    val schedule = right(
      BoundSchedule(
        compiled,
        samples,
        right(AxisPopulationFingerprint.fromAxis(samples)),
        right(ScheduleLabels.fromDesign(samples, ordinal)),
        authority
      )
    )
    right(
      ValidationDesign(
        schedule,
        ScientificAxisName.unsafe("samples"),
        GeneralizationAxis(ScientificAxisName.unsafe("samples"), samples.identity)
      )
    )
  private val categoricalSource = right(
    CategoricalObservationSource(evidence, categoricalTarget)
  )
  private val membershipSource = right(
    MembershipObservationSource(evidence, membershipTarget)
  )
  private val classification = right(
    ClassificationConfiguration(
      StandardizedNearestCentroid(
        StandardizationSpecification.CenterScaleRejectConstant
      )
    )
  )
  private val ridge = right(OperatorRidgeConfiguration.fixed(0.7))
  private val ridgeSolver = right(
    OperatorRidgeSolverSettings(tolerance = 1e-10, maxIterations = 1000)
  )

  test("collected work scales exactly with equivalent dense measurements"):
    val one = denseResult(1)
    val two = denseResult(2)

    assertReconciled(one.receipt, one.counts)
    assertReconciled(two.receipt, two.counts)
    assertEquals(one.counts.completion, TraversalCompletion.Exhausted)
    assertEquals(two.counts.completion, TraversalCompletion.Exhausted)
    assertEquals(one.receipt.work.planned, 1)
    assertEquals(two.receipt.work.planned, 2)
    assertEquals(one.receipt.work.operatorApplications, 1L)
    assertEquals(two.receipt.work.operatorApplications, 2L)
    assertEquals(
      one.receipt.work.materializedCells,
      sampleCount.toLong * featureCount.toLong
    )
    assertEquals(
      two.receipt.work.materializedCells,
      2L * one.receipt.work.materializedCells
    )

  test("streaming early stop distinguishes planned from visited and completed work"):
    val traversal = streamingTraversal(3)

    assertReconciled(traversal.receipt, traversal.counts)
    assertEquals(traversal.state, 1)
    assertEquals(traversal.counts.expected, 3)
    assertEquals(traversal.counts.visited, 1)
    assertEquals(traversal.counts.succeeded, 1)
    assertEquals(traversal.counts.completion, TraversalCompletion.StoppedEarly)
    assertEquals(traversal.receipt.work.planned, 3)
    assertEquals(traversal.receipt.work.attempted, 1)
    assertEquals(traversal.receipt.measurements.length, 1)
    assertEquals(traversal.receipt.work.operatorApplications, 1L)
    assertEquals(
      traversal.receipt.work.materializedCells,
      sampleCount.toLong * featureCount.toLong
    )

  test("real operator-ridge convergence and operator work scale by measurement"):
    val one = ridgeResult(1)
    val two = ridgeResult(2)
    val oneIterations = iterationTotal(one.receipt)
    val twoIterations = iterationTotal(two.receipt)

    assertReconciled(one.receipt, one.counts)
    assertReconciled(two.receipt, two.counts)
    assert(one.receipt.work.operatorApplications > 0L)
    assertEquals(
      two.receipt.work.operatorApplications,
      2L * one.receipt.work.operatorApplications
    )
    assertEquals(one.receipt.work.materializedCells, 0L)
    assertEquals(two.receipt.work.materializedCells, 0L)
    assertEquals(one.receipt.convergence.length, 4 * classCount)
    assertEquals(two.receipt.convergence.length, 8 * classCount)
    assert(oneIterations > 0L)
    assertEquals(twoIterations, 2L * oneIterations)
    assert(one.receipt.convergence.forall(_.outcome == ConvergenceOutcome.Converged))
    assert(two.receipt.convergence.forall(_.outcome == ConvergenceOutcome.Converged))

  test("relational representations answer one estimand and expose exact projection work"):
    val twoPartitions = new RelationalWorkFixture(2)
    val fourPartitions = new RelationalWorkFixture(4)
    val twoDense = twoPartitions.dense()
    val twoOperator = twoPartitions.operator()
    val fourDense = fourPartitions.dense()
    val fourOperator = fourPartitions.operator()

    assertEquivalent(twoDense, twoOperator)
    assertEquivalent(fourDense, fourOperator)
    assertRelationScale(twoDense, partitions = 2, materialized = true)
    assertRelationScale(twoOperator, partitions = 2, materialized = false)
    assertRelationScale(fourDense, partitions = 4, materialized = true)
    assertRelationScale(fourOperator, partitions = 4, materialized = false)
    assertEquals(
      fourOperator.operatorApplications,
      2L * twoOperator.operatorApplications
    )
    assertEquals(
      fourDense.materializedCells,
      2L * twoDense.materializedCells
    )

  private def measurements(count: Int) =
    Vector.tabulate(count): index =>
      MeasurementEntry(
        right(
          Measurement.identity(
            features,
            MeasurementId.unsafe(s"work-court-global-$index")
          )
        ),
        NoRendition
      )

  private def frame(count: Int) =
    right(MeasurementFrame(features)(measurements(count)))

  private def denseStrategy(delivery: ResultDelivery) =
    right(
      ExecutionStrategy(
        BackendId.unsafe("alder-work-court"),
        ExecutionRepresentation.Dense,
        NumericPrecision.Binary64,
        SolverChoice.NotApplicable,
        Vector.empty,
        Scheduling.serial,
        MaterializationPolicy.Allow(MaterializationBudget.unsafe(64L)),
        FallbackPolicy.forbidden,
        delivery
      )
    )

  private def ridgeStrategy =
    right(
      ExecutionStrategy(
        BackendId.unsafe("gale-work-court"),
        ExecutionRepresentation.Operator,
        NumericPrecision.Binary64,
        OperatorRidgeSolverSettings.choice(ridgeSolver),
        Vector.empty,
        Scheduling.serial,
        MaterializationPolicy.Reject,
        FallbackPolicy.forbidden,
        ResultDelivery.Collected,
        Vector("kernel" -> "operator-ridge")
      )
    )

  private def denseResult(measurementCount: Int) =
    right(
      Mvpa.run(categoricalSource)(
        validation,
        frame(measurementCount),
        categoricalSource.classify(classification),
        denseStrategy(ResultDelivery.Collected)
      )
    )

  private def ridgeResult(measurementCount: Int) =
    right(
      Mvpa.run(membershipSource)(
        validation,
        frame(measurementCount),
        membershipSource.operatorRidge(ridge),
        ridgeStrategy
      )
    )

  private def streamingTraversal(measurementCount: Int) =
    val specification = right(
      Mvpa.specify(categoricalSource)(
        validation,
        frame(measurementCount),
        categoricalSource.classify(classification)
      )
    )
    val scientific = right(Mvpa.bind(specification))
    val plan = right(
      Mvpa.plan(scientific, denseStrategy(ResultDelivery.Streaming))
    )
    val execution = right(Mvpa.execute(plan))
    right(
      execution.visit(0): (visited, _) =>
        VisitDecision.Stop(visited + 1)
    )

  private def assertReconciled(
      receipt: ExecutionReceipt,
      counts: TraversalCounts
  ): Unit =
    assertEquals(receipt.work.planned, counts.expected)
    assertEquals(receipt.work.attempted, counts.visited)
    assertEquals(receipt.work.succeeded, counts.succeeded)
    assertEquals(receipt.work.rejected, counts.rejected)
    assertEquals(receipt.work.failed, counts.failed)
    assertEquals(
      receipt.work.operatorApplications,
      receipt.measurements.map(_.operatorApplications).sum
    )
    assertEquals(
      receipt.work.materializedCells,
      receipt.measurements.map(_.materializedCells).sum
    )
    assertEquals(
      receipt.work.succeeded + receipt.work.rejected + receipt.work.failed,
      receipt.work.attempted
    )

  private def iterationTotal(receipt: ExecutionReceipt): Long =
    receipt.convergence.foldLeft(0L)((total, value) => total + value.iterations.toLong)

  private def assertEquivalent(
      dense: RelationWorkSummary,
      operator: RelationWorkSummary
  ): Unit =
    assertEquals(dense.source, operator.source)
    assertEquals(dense.fit, operator.fit)
    assertEquals(dense.values.length, operator.values.length)
    dense.values
      .zip(operator.values)
      .foreach: (left, right) =>
        assertEqualsDouble(left, right, 1e-10)

  private def assertRelationScale(
      value: RelationWorkSummary,
      partitions: Int,
      materialized: Boolean
  ): Unit =
    val effects = 4L
    val neural = 6L
    val local = 3L
    assertEquals(value.operatorApplications, partitions.toLong)
    assertEquals(value.projections, partitions)
    assertEquals(value.edges, partitions * (partitions - 1))
    assertEquals(value.outputCells, effects * effects)
    assertEquals(
      value.avoidedFullRelationCells,
      partitions.toLong * effects * (neural - local)
    )
    assertEquals(
      value.materializedCells,
      if materialized then partitions.toLong * effects * local else 0L
    )

  private def right[A](value: Either[?, A]): A =
    value.fold(error => fail(s"expected Right, obtained $error"), identity)

  private final case class RelationWorkSummary(
      source: ScientificSourceIdentity,
      fit: ScientificComponentFingerprint,
      values: Vector[Double],
      operatorApplications: Long,
      materializedCells: Long,
      projections: Int,
      edges: Int,
      outputCells: Long,
      avoidedFullRelationCells: Long
  )

  private final class RelationalWorkFixture(partitionCount: Int):
    private val effectCount = 4
    private val neuralCount = 6
    private val localCount = 3
    private val effects = right(
      AxisRef.create(
        AxisId.unsafe(s"work-relation-effects-$partitionCount"),
        AxisPurpose.Effects,
        Vector.tabulate(effectCount)(index => AxisKey.unsafe(s"effect-$index")),
        CoordinateBasis.unsafe("effect-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("mvpa-work-court", "relation-v1")
      )
    )
    private val neural = right(
      AxisRef.create(
        AxisId.unsafe(s"work-relation-neural-$partitionCount"),
        AxisPurpose.NeuralFeatures,
        Vector.tabulate(neuralCount)(index => FeatureId.unsafe(s"voxel-$index")),
        CoordinateBasis.unsafe("voxel-order"),
        Some(AxisUnits.unsafe("percent-signal-change")),
        AxisScale.nominal,
        CoordinateProvenance.unsafe("mvpa-work-court", "relation-v1")
      )
    )
    private val partitions = right(
      AxisRef.create(
        AxisId.unsafe(s"work-relation-partitions-$partitionCount"),
        AxisPurpose.Partitions,
        Vector.tabulate(partitionCount)(index => PartitionId.unsafe(s"run-$index")),
        CoordinateBasis.unsafe("run-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("mvpa-work-court", "relation-v1")
      )
    )
    private val namedPartitions = right(
      PartitionAxis(ScientificAxisName.unsafe("runs"), partitions)
    )
    private val training = right(
      AxisRef.create(
        AxisId.unsafe(s"work-relation-training-$partitionCount"),
        AxisPurpose.Samples,
        Vector(SampleId.unsafe("training-a"), SampleId.unsafe("training-b")),
        CoordinateBasis.unsafe("training-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("mvpa-work-court", "relation-v1")
      )
    )
    private val matrices = Vector.tabulate(partitionCount): partition =>
      Matrix.tabulate(effectCount, neuralCount): (effect, coordinate) =>
        math.sin((effect + 1).toDouble * 0.41 + (coordinate + 1).toDouble * 0.17) +
          partition.toDouble * 0.03
    private val fitDesign = right(
      DesignIdentity(DesignKind.unsafe("work-relation-fit"))
    )
    private val measurement =
      val injection = right(
        Injection.from(
          IArray.unsafeFromArray(Array.tabulate(localCount)(identity)),
          right(IndexSpace.of(neuralCount))
        )
      )
      right(
        Measurement.hardSelection(
          neural,
          MeasurementId.unsafe("work-relation-local"),
          injection
        )
      )
    private val denseSource = source(operatorBacked = false)
    private val operatorSource = source(operatorBacked = true)
    private val domain = right(WithinPairDomain(effects))
    private val design = pairing(denseSource)

    def dense(): RelationWorkSummary =
      summarize(
        denseSource,
        MaterializationPolicy.Allow(
          MaterializationBudget.unsafe(
            partitionCount.toLong * effectCount.toLong * localCount.toLong
          )
        )
      )

    def operator(): RelationWorkSummary =
      summarize(operatorSource, MaterializationPolicy.Reject)

    private def summarize[
        C <: RelationCapabilities[neural.Id, FeatureId]
    ](
        source: PartitionedRelations[
          partitions.Id,
          effects.Id,
          neural.Id,
          AxisKey,
          FeatureId,
          C
        ],
        materialization: MaterializationPolicy
    ): RelationWorkSummary =
      val request = right(
        RelationalFitQuery.identityPrecision(
          source,
          domain,
          RdmNormalization.Raw
        )
      )
      val fit = right(
        RelationalFit.query(
          request,
          design,
          measurement,
          materialization
        )
      )
      val values = right(fit.rdm).distances.toVector
      val work = fit.computation
      RelationWorkSummary(
        fit.sourceIdentity,
        fit.identity,
        values,
        work.operatorApplications,
        work.materializedCells,
        work.projections.length,
        work.edges.length,
        work.outputCells,
        work.avoidedFullRelationCells
      )

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
      val capabilities = right(EstimateOnlyCapabilities(neural))
      val entries = partitions.keys.zipWithIndex.map: (partition, position) =>
        val valueId = ValueId.unsafe(s"work-relation-${partition.value}")
        val estimate =
          if operatorBacked then
            right(
              EvidenceTable.operator(
                effects,
                neural,
                new MatrixFree(matrices(position)),
                valueId
              )
            )
          else
            right(
              EvidenceTable.dense(
                effects,
                neural,
                matrices(position),
                valueId
              )
            )
        val receipt = right(
          RelationFitReceipt(
            ValueIdentity.source(ValueId.unsafe(s"work-source-${partition.value}")),
            fitDesign,
            right(Estimability(effects, Vector.fill(effectCount)(true))),
            NormalizationIdentity.none,
            training
          )
        )
        PartitionRelation(
          partition,
          right(Relation(estimate, receipt, capabilities))
        )
      right(
        PartitionedRelations(
          namedPartitions,
          effects,
          neural,
          entries
        )
      )

    private def pairing(
        source: PartitionedRelations[
          partitions.Id,
          effects.Id,
          neural.Id,
          AxisKey,
          FeatureId,
          EstimateOnlyCapabilities[neural.Id, FeatureId]
        ]
    ): PairingDesign[partitions.Id, partitions.Id] =
      val evidenceIdentity = right(
        PartitionEvidenceIdentity(source.identity, namedPartitions)
      )
      val pairs = Vector.newBuilder[DeclaredIndependentPair]
      var left = 0
      while left < partitionCount - 1 do
        var rightPosition = left + 1
        while rightPosition < partitionCount do
          pairs += DeclaredIndependentPair(
            partitions.keys(left),
            partitions.keys(rightPosition)
          )
          rightPosition += 1
        left += 1
      val independence = right(
        PartitionIndependenceDeclaration(
          right(IndependenceDeclarationId(s"work-independent-$partitionCount")),
          evidenceIdentity,
          evidenceIdentity,
          pairs.result()
        )
      )
      right(
        PairingDesign.allOrdered(
          namedPartitions,
          PairingReducer.WeightedMean,
          GeneralizationAxis(
            ScientificAxisName.unsafe("runs"),
            partitions.identity
          ),
          independence
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
