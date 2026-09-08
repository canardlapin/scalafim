package scalafim.fmri.mvpa

import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.DoubleLinearOperator
import gale.linalg.MutableDVec
import multivar.core.ValueId
import multivar.core.ValueIdentity
import resample4s.core.IndexSpace
import resample4s.core.Injection
import scalafim.fmri.mvpa.FirstOrderAnalysis.given

final class FirstOrderAnalysisSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private val effects = right(
    AxisRef.create(
      AxisId.unsafe("first-order-effects"),
      AxisPurpose.Effects,
      Vector(AxisKey.unsafe("a"), AxisKey.unsafe("b"), AxisKey.unsafe("unused")),
      CoordinateBasis.unsafe("effect-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("first-order-suite", "v1")
    )
  )

  private val queries = right(
    AxisRef.create(
      AxisId.unsafe("first-order-queries"),
      AxisPurpose.unsafe("effect-queries"),
      Vector(AxisKey.unsafe("b-minus-a"), AxisKey.unsafe("a-level")),
      CoordinateBasis.unsafe("declared-linear-queries"),
      None,
      right(AxisScale.named("interval")),
      CoordinateProvenance.unsafe("first-order-suite", "v1")
    )
  )

  private val neural = right(
    AxisRef.create(
      AxisId.unsafe("first-order-neural"),
      AxisPurpose.NeuralFeatures,
      Vector(FeatureId.unsafe("x"), FeatureId.unsafe("y"), FeatureId.unsafe("z")),
      CoordinateBasis.unsafe("voxel-order"),
      Some(AxisUnits.unsafe("percent-signal-change")),
      right(AxisScale.named("interval")),
      CoordinateProvenance.unsafe("first-order-suite", "v1")
    )
  )

  private val partitions = right(
    AxisRef.create(
      AxisId.unsafe("first-order-runs"),
      AxisPurpose.Partitions,
      Vector(PartitionId.unsafe("run-1"), PartitionId.unsafe("run-2")),
      CoordinateBasis.unsafe("run-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("first-order-suite", "v1")
    )
  )

  private val partitionAxis = right(
    PartitionAxis(ScientificAxisName.unsafe("runs"), partitions)
  )

  private val training = right(
    AxisRef.create(
      AxisId.unsafe("first-order-fit-samples"),
      AxisPurpose.Samples,
      Vector(SampleId.unsafe("t1"), SampleId.unsafe("t2")),
      CoordinateBasis.unsafe("time-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("first-order-suite", "v1")
    )
  )

  private val relationDesign = right(
    DesignIdentity(DesignKind.unsafe("first-order-relation-fit"))
  )

  private val matrices = Vector(
    GaleTestMatrix.fromRows(
      Seq(
        Seq(1.0, 2.0, 3.0),
        Seq(4.0, 5.0, 6.0),
        Seq(99.0, 99.0, 99.0)
      )
    ),
    GaleTestMatrix.fromRows(
      Seq(
        Seq(3.0, 4.0, 5.0),
        Seq(8.0, 8.0, 10.0),
        Seq(-99.0, -99.0, -99.0)
      )
    )
  )

  private val query = right(
    EffectQuery.dense(
      effects,
      queries,
      GaleTestMatrix.fromRows(
        Seq(
          Seq(-1.0, 1.0, 0.0),
          Seq(1.0, 0.0, 0.0)
        )
      ),
      ValueId.unsafe("first-order-query")
    )
  )

  private val design = right(
    PartitionReductionDesign.weighted(
      partitionAxis,
      Vector(
        right(PartitionWeight(partitions.keys(0), 1.0)),
        right(PartitionWeight(partitions.keys(1), 3.0))
      ),
      PartitionReducer.WeightedMean,
      PartitionScope.FixedObserved
    )
  )

  private val measurement =
    val injection = right(
      Injection.from(
        IArray.unsafeFromArray(Array(2, 0)),
        right(IndexSpace.of(neural.size))
      )
    )
    right(
      Measurement.hardSelection(
        neural,
        MeasurementId.unsafe("z-then-x"),
        injection
      )
    )

  private val frame = right(
    MeasurementFrame(neural)(Vector(MeasurementEntry(measurement, NoRendition)))
  )

  private val strategy = right(
    ExecutionStrategy(
      BackendId.unsafe("first-order-portable"),
      ExecutionRepresentation.Operator,
      NumericPrecision.Binary64,
      SolverChoice.NotApplicable,
      Vector.empty,
      Scheduling.serial,
      MaterializationPolicy.Reject,
      FallbackPolicy.forbidden,
      ResultDelivery.Collected
    )
  )

  private def source(
      operatorBacked: Boolean,
      estimable: Vector[Boolean] = Vector(true, true, false),
      probes: Vector[Probe] = Vector(Probe(), Probe())
  ): PartitionedRelations[
    partitions.Id,
    effects.Id,
    neural.Id,
    AxisKey,
    FeatureId,
    EstimateOnlyCapabilities[neural.Id, FeatureId]
  ] =
    val entries = partitions.keys.zipWithIndex.map: (partition, position) =>
      val estimate =
        if operatorBacked then
          right(
            EvidenceTable.operator(
              effects,
              neural,
              CountingOperator(matrices(position), probes(position)),
              ValueId.unsafe(s"first-order-estimate-${partition.value}")
            )
          )
        else
          right(
            EvidenceTable.dense(
              effects,
              neural,
              matrices(position),
              ValueId.unsafe(s"first-order-estimate-${partition.value}")
            )
          )
      val receipt = right(
        RelationFitReceipt(
          ValueIdentity.source(ValueId.unsafe(s"first-order-source-${partition.value}")),
          relationDesign,
          right(Estimability(effects, estimable)),
          NormalizationIdentity.none,
          training
        )
      )
      PartitionRelation(
        partition,
        right(Relation(estimate, receipt, right(EstimateOnlyCapabilities(neural))))
      )
    right(PartitionedRelations(partitionAxis, effects, neural, entries))

  private def onlySuccess(
      result: AnalysisResult[
        MeasuredEffectMap[queries.Id, AxisKey],
        FirstOrderBindRejection,
        FirstOrderTaskFailure,
        NoRendition.type
      ]
  ): MeasuredEffectMap[queries.Id, AxisKey] =
    result.values match
      case Vector(MeasurementValue(_, _, MeasurementOutcome.Success(value, _))) => value
      case other => fail(s"expected one success, obtained $other")

  test("weighted contrast effect map matches the independent R oracle"):
    val evidence = source(operatorBacked = false)
    val result = right(
      Mvpa.run(evidence)(
        design,
        frame,
        FirstOrderAnalysis.contrast(evidence, query),
        strategy
      )
    )
    val effectMap = onlySuccess(result)

    // Generated independently by tools/reference/mvpa-first-order-oracle.R.
    assertMatrix(
      effectMap.values,
      GaleTestMatrix.fromRows(Seq(Seq(4.5, 4.5), Seq(4.5, 2.5)))
    )
    assertEquals(effectMap.queries.identity, queries.identity)
    assert(effectMap.queries.evidence eq queries.evidence)
    assertEquals(effectMap.local, measurement.local.identity)
    assertEquals(effectMap.units.label, "percent-signal-change")
    assertEquals(
      effectMap.units.coefficientUnits,
      EffectCoefficientUnits.DimensionlessLinearCombination
    )
    assertEquals(result.plan.design, design.identity)
    assertEquals(result.values.head.measurement, measurement.identity)
    assertEquals(
      result.plan.estimand.fields.find(_.name == "query").map(_.value),
      Some(query.identity.value)
    )
    assertEquals(effectMap.computation.partitions.map(_.coefficient), Vector(0.25, 0.75))
    assertEquals(effectMap.computation.outputCells, 4L)
    assertEquals(result.receipt.work.operatorApplications, 4L)
    assertEqualsDouble(right(effectMap.value(queries.keys(0), 0)), 4.5, 1e-12, "query lookup")

  test("dense and matrix-free relations have identical first-order output and honest work"):
    val probes = Vector(Probe(), Probe())
    val denseSource = source(operatorBacked = false)
    val operatorSource = source(operatorBacked = true, probes = probes)
    val denseAnalysis = right(
      Mvpa.run(denseSource)(
        design,
        frame,
        FirstOrderAnalysis.contrast(denseSource, query),
        strategy
      )
    )
    val operatorAnalysis = right(
      Mvpa.run(operatorSource)(
        design,
        frame,
        FirstOrderAnalysis.contrast(operatorSource, query),
        strategy
      )
    )
    val dense = onlySuccess(denseAnalysis)
    val operator = onlySuccess(operatorAnalysis)

    assertMatrix(operator.values, dense.values)
    assertEquals(probes.map(_.transposeCalls), Vector(2, 2))
    assertEquals(operatorAnalysis.receipt.work.operatorApplications, 4L)
    assertEquals(operator.computation.outputCells, 4L)

  test("estimability is checked on exact query support rather than every stored effect"):
    val unusedNonEstimable = source(operatorBacked = false)
    assert(
      Mvpa
        .run(unusedNonEstimable)(
          design,
          frame,
          FirstOrderAnalysis.contrast(unusedNonEstimable, query),
          strategy
        )
        .isRight
    )

    val usedNonEstimable = source(
      operatorBacked = false,
      estimable = Vector(true, false, true)
    )
    Mvpa.run(usedNonEstimable)(
      design,
      frame,
      FirstOrderAnalysis.contrast(usedNonEstimable, query),
      strategy
    ) match
      case Left(
            MvpaRunError.Binding(
              BindError.EstimandRejected(
                _,
                FirstOrderBindRejection.NonEstimableEffect(partition, effect),
                _
              )
            )
          ) =>
        assertEquals(partition, partitions.keys.head)
        assertEquals(effect, "b")
      case other => fail(s"expected query-specific bind rejection, obtained $other")

  test("coefficient values, reduction scope, and sampling claim enter scientific identity"):
    val changedQuery = right(
      EffectQuery.dense(
        effects,
        queries,
        GaleTestMatrix.fromRows(
          Seq(
            Seq(-2.0, 2.0, 0.0),
            Seq(1.0, 0.0, 0.0)
          )
        ),
        ValueId.unsafe("first-order-query")
      )
    )
    val sampled = right(
      PartitionReductionDesign.weighted(
        partitionAxis,
        design.weights,
        PartitionReducer.WeightedMean,
        PartitionScope.SampledPopulation(
          GeneralizationAxis(ScientificAxisName.unsafe("runs"), partitions.identity)
        )
      )
    )

    assertNotEquals(query.identity, changedQuery.identity)
    assertNotEquals(design.identity, sampled.identity)
    assertEquals(design.referencedAxes, Vector(partitionAxis.reference))
    assertEquals(sampled.referencedAxes, Vector(partitionAxis.reference))

  test("partition reduction is total, canonical, and method-specific"):
    val reversed = right(
      PartitionReductionDesign.weighted(
        partitionAxis,
        design.weights.reverse,
        PartitionReducer.WeightedMean,
        PartitionScope.FixedObserved
      )
    )
    assertEquals(reversed.weights.map(_.partition), partitions.keys)
    assertEquals(reversed.identity, design.identity)

    PartitionReductionDesign.weighted(
      partitionAxis,
      Vector(design.weights.head),
      PartitionReducer.WeightedMean,
      PartitionScope.FixedObserved
    ) match
      case Left(PartitionReductionDesignError.MissingPartition(partition)) =>
        assertEquals(partition, partitions.keys(1))
      case other => fail(s"expected missing-partition rejection, obtained $other")

  private def assertMatrix(actual: DMat, expected: DMat): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var column = 0
      while column < actual.cols do
        assertEqualsDouble(actual(row, column), expected(row, column), 1e-12)
        column += 1
      row += 1

  private final class Probe:
    var transposeCalls: Int = 0

  private object Probe:
    def apply(): Probe = new Probe

  private final class CountingOperator(matrix: DMat, probe: Probe) extends DoubleLinearOperator:
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
      probe.transposeCalls += 1
      var column = 0
      while column < cols do
        var sum = 0.0
        var row = 0
        while row < rows do
          sum += matrix(row, column) * input(row)
          row += 1
        output(column) = sum
        column += 1

  private object CountingOperator:
    def apply(matrix: DMat, probe: Probe): CountingOperator =
      new CountingOperator(matrix, probe)
