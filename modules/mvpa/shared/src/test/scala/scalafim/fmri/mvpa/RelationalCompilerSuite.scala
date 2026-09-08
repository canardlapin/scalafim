package scalafim.fmri.mvpa

import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.DoubleLinearOperator
import gale.linalg.MutableDVec
import multivar.core.ValueId
import multivar.core.ValueIdentity
import resample4s.core.IndexSpace
import resample4s.core.Injection

class RelationalCompilerSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private val effectAxis = right(
    AxisRef.create(
      AxisId.unsafe("compiler-effects"),
      AxisPurpose.Effects,
      Vector(AxisKey.unsafe("a"), AxisKey.unsafe("b"), AxisKey.unsafe("c")),
      CoordinateBasis.unsafe("condition-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("relational-compiler-suite", "v1")
    )
  )

  private val neuralAxis = right(
    AxisRef.create(
      AxisId.unsafe("compiler-neural"),
      AxisPurpose.NeuralFeatures,
      Vector(
        FeatureId.unsafe("v1"),
        FeatureId.unsafe("v2"),
        FeatureId.unsafe("v3"),
        FeatureId.unsafe("v4")
      ),
      CoordinateBasis.unsafe("voxel-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("relational-compiler-suite", "v1")
    )
  )

  private val partitionAxis = right(
    AxisRef.create(
      AxisId.unsafe("compiler-runs"),
      AxisPurpose.Partitions,
      Vector(
        PartitionId.unsafe("run-1"),
        PartitionId.unsafe("run-2"),
        PartitionId.unsafe("run-3")
      ),
      CoordinateBasis.unsafe("run-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("relational-compiler-suite", "v1")
    )
  )

  private val namedPartitions = right(
    PartitionAxis(ScientificAxisName.unsafe("runs"), partitionAxis)
  )

  private val trainingAxis = right(
    AxisRef.create(
      AxisId.unsafe("compiler-training"),
      AxisPurpose.Samples,
      Vector(SampleId.unsafe("t1"), SampleId.unsafe("t2")),
      CoordinateBasis.unsafe("time-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("relational-compiler-suite", "v1")
    )
  )

  private val matrices = Vector(
    GaleTestMatrix.fromRows(
      Seq(
        Seq(1.0, 2.0, 3.0, 4.0),
        Seq(0.0, 1.0, 1.0, 2.0),
        Seq(2.0, 0.0, 1.0, 1.0)
      )
    ),
    GaleTestMatrix.fromRows(
      Seq(
        Seq(1.5, 1.0, 2.0, 3.0),
        Seq(0.5, 1.5, 0.0, 1.0),
        Seq(1.0, 0.5, 2.0, 0.0)
      )
    ),
    GaleTestMatrix.fromRows(
      Seq(
        Seq(0.5, 2.5, 1.0, 2.0),
        Seq(1.0, 0.5, 1.5, 0.0),
        Seq(1.5, 1.0, 0.5, 2.5)
      )
    )
  )

  private val fitDesign = right(
    DesignIdentity(DesignKind.unsafe("runwise-relation"), Vector("model" -> "conditions"))
  )

  private def measurement(
      positions: Int*
  ): Measurement[neuralAxis.Id, FeatureId, FeatureId] =
    val injection = right(
      Injection.from(
        IArray.unsafeFromArray(positions.toArray),
        right(IndexSpace.of(neuralAxis.size))
      )
    )
    right(
      Measurement.hardSelection(
        neuralAxis,
        MeasurementId.unsafe(s"selection-${positions.mkString("-")}"),
        injection
      )
    )

  private def allOrdered[C <: RelationCapabilities[neuralAxis.Id, FeatureId]](
      evidence: PartitionedRelations[
        partitionAxis.Id,
        effectAxis.Id,
        neuralAxis.Id,
        AxisKey,
        FeatureId,
        C
      ]
  ): PairingDesign[partitionAxis.Id, partitionAxis.Id] =
    val independence = right(
      PartitionIndependenceTestSupport.declareAllPairs(
        evidence.identity,
        namedPartitions,
        "compiler-independent-runs"
      )
    )
    right(
      PairingDesign.allOrdered(
        namedPartitions,
        PairingReducer.WeightedMean,
        GeneralizationAxis(ScientificAxisName.unsafe("runs"), partitionAxis.identity),
        independence
      )
    )

  private def source(
      operatorBacked: Boolean,
      probes: Vector[OperatorProbe] = Vector.fill(3)(OperatorProbe())
  ): PartitionedRelations[
    partitionAxis.Id,
    effectAxis.Id,
    neuralAxis.Id,
    AxisKey,
    FeatureId,
    EstimateOnlyCapabilities[neuralAxis.Id, FeatureId]
  ] =
    val entries = partitionAxis.keys.zipWithIndex.map: (partition, position) =>
      val estimate =
        if operatorBacked then
          right(
            EvidenceTable.operator(
              effectAxis,
              neuralAxis,
              CountingMatrixOperator(matrices(position), probes(position)),
              ValueId.unsafe(s"operator-relation-${partition.value}")
            )
          )
        else
          right(
            EvidenceTable.dense(
              effectAxis,
              neuralAxis,
              matrices(position),
              ValueId.unsafe(s"dense-relation-${partition.value}")
            )
          )
      val estimability = right(Estimability(effectAxis, Vector(true, true, true)))
      val receipt = right(
        RelationFitReceipt(
          ValueIdentity.source(ValueId.unsafe(s"source-${partition.value}")),
          fitDesign,
          estimability,
          NormalizationIdentity.none,
          trainingAxis
        )
      )
      val relation = right(
        Relation(
          estimate,
          receipt,
          right(EstimateOnlyCapabilities(neuralAxis))
        )
      )
      PartitionRelation(partition, relation)
    right(
      PartitionedRelations(
        namedPartitions,
        effectAxis,
        neuralAxis,
        entries
      )
    )

  test("an independent direct contrast oracle equals the sufficient effect form"):
    val evidence = source(operatorBacked = true)
    val local = measurement(0, 2)
    val metric = right(NeuralQuery.identity(local.local))
    val pairing = allOrdered(evidence)
    val domain = right(WithinPairDomain(effectAxis))
    val form = right(
      RelationalCompiler.effectForm(
        evidence,
        pairing,
        local,
        metric
      )
    )
    val selectedCoordinates = Vector(0, 2)
    val direct = domain.pairs.map: pair =>
      pairing.edges
        .map: edge =>
          val leftPartition = partitionAxis
            .positionOf(edge.left)
            .fold(
              fail(s"missing left partition ${edge.left.value}")
            )(identity)
          val rightPartition = partitionAxis
            .positionOf(edge.right)
            .fold(
              fail(s"missing right partition ${edge.right.value}")
            )(identity)
          val contribution = selectedCoordinates
            .map: coordinate =>
              val leftContrast =
                matrices(leftPartition)(pair.firstPosition, coordinate) -
                  matrices(leftPartition)(pair.secondPosition, coordinate)
              val rightContrast =
                matrices(rightPartition)(pair.firstPosition, coordinate) -
                  matrices(rightPartition)(pair.secondPosition, coordinate)
              leftContrast * rightContrast
            .sum
          edge.weight * contribution
        .sum / 6.0

    domain.pairs.zipWithIndex.foreach: (pair, position) =>
      val expected =
        form.value(pair.firstPosition, pair.firstPosition) +
          form.value(pair.secondPosition, pair.secondPosition) -
          form.value(pair.firstPosition, pair.secondPosition) -
          form.value(pair.secondPosition, pair.firstPosition)
      assertEqualsDouble(direct(position), expected, 1e-12)

    assertEquals(form.receipt.reducer, PairingReducer.WeightedMean)
    assertEquals(form.receipt.reducerDenominator, 6.0)
    assert(form.receipt.measurementComposedBeforeProjection)
    assertEquals(form.receipt.materializedCells, 0L)
    assertEquals(form.receipt.projections.map(_.localNeuralCoordinates), Vector(2, 2, 2))
    assertEquals(form.receipt.avoidedFullRelationCells, 18L)

  test("dense, operator, and explicit local materialization paths agree"):
    val local = measurement(3, 1)
    val metric = right(
      NeuralQuery.fixedPrecision(
        local.local,
        GaleTestMatrix.fromRows(Seq(Seq(2.0, 0.25), Seq(0.25, 1.5))),
        ValueId.unsafe("local-fixed-precision")
      )
    )
    val operatorSource = source(operatorBacked = true)
    val denseSource = source(operatorBacked = false)
    val operatorResult = right(
      RelationalCompiler.effectForm(
        operatorSource,
        allOrdered(operatorSource),
        local,
        metric
      )
    )
    val denseResult = right(
      RelationalCompiler.effectForm(
        denseSource,
        allOrdered(denseSource),
        local,
        metric
      )
    )
    val materializedResult = right(
      RelationalCompiler.effectForm(
        operatorSource,
        allOrdered(operatorSource),
        local,
        metric,
        MaterializationPolicy.Allow(MaterializationBudget.unsafe(100L))
      )
    )

    assertMatrix(operatorResult.value, denseResult.value)
    assertMatrix(operatorResult.value, materializedResult.value)
    assertEquals(
      operatorResult.receipt.projectionMode,
      RelationalProjectionMode.OperatorProjection
    )
    assertEquals(
      materializedResult.receipt.projectionMode,
      RelationalProjectionMode.ExplicitLocalMaterialization
    )
    assertEquals(materializedResult.receipt.materializations.length, 3)
    assertEquals(materializedResult.receipt.materializedCells, 18L)
    assertEquals(operatorResult.receipt.operatorApplications, 4L)
    assertEquals(materializedResult.receipt.operatorApplications, 4L)

  test("only partitions named by oriented edges are projected"):
    val probes = Vector.fill(3)(OperatorProbe())
    val evidence = source(operatorBacked = true, probes)
    val edge = right(PairingEdge(partitionAxis.keys(0), partitionAxis.keys(1), 2.0))
    val pairing = right(
      PairingDesign(
        namedPartitions,
        namedPartitions,
        Vector(edge),
        PairingReducer.WeightedSum,
        GeneralizationAxis(ScientificAxisName.unsafe("runs"), partitionAxis.identity),
        right(
          PartitionIndependenceTestSupport.declarePairs(
            evidence.identity,
            namedPartitions,
            Vector(DeclaredIndependentPair(edge.left, edge.right)),
            "oriented-edge"
          )
        )
      )
    )
    val local = measurement(0)
    val result = right(
      RelationalCompiler.effectForm(
        evidence,
        pairing,
        local,
        right(NeuralQuery.identity(local.local))
      )
    )

    assertEquals(
      result.receipt.edges,
      Vector(
        PairEdgeReceipt(
          partitionAxis.keys(0),
          partitionAxis.keys(1),
          2.0
        )
      )
    )
    assertEquals(result.receipt.reducerDenominator, 1.0)
    assertEquals(result.receipt.projections.map(_.partition), partitionAxis.keys.take(2))
    assertEquals(probes.map(_.transposeApplications), Vector(3, 3, 0))
    assertEquals(probes.map(_.forwardApplications), Vector(0, 0, 0))

  test("materialization budget failures remain explicit"):
    val local = measurement(0, 1, 2)
    val evidence = source(operatorBacked = true)
    val result = RelationalCompiler.effectForm(
      evidence,
      allOrdered(evidence),
      local,
      right(NeuralQuery.identity(local.local)),
      MaterializationPolicy.Allow(MaterializationBudget.unsafe(2L))
    )
    assert(result.left.exists:
      case RelationalCompilationError.Evidence(
            EvidenceTableError.MaterializationBudgetExceeded(required, budget)
          ) =>
        required == 9L && budget.maxElements == 2L
      case _ => false)

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

  private final class OperatorProbe:
    var forwardApplications: Int = 0
    var transposeApplications: Int = 0

  private object OperatorProbe:
    def apply(): OperatorProbe = new OperatorProbe

  private final class CountingMatrixOperator(
      matrix: DMat,
      probe: OperatorProbe
  ) extends DoubleLinearOperator:
    override def rows: Int = matrix.rows
    override def cols: Int = matrix.cols

    override def applyTo(input: DVec, output: MutableDVec): Unit =
      probe.forwardApplications += 1
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
      probe.transposeApplications += 1
      var column = 0
      while column < cols do
        var sum = 0.0
        var row = 0
        while row < rows do
          sum += matrix(row, column) * input(row)
          row += 1
        output(column) = sum
        column += 1

  private object CountingMatrixOperator:
    def apply(matrix: DMat, probe: OperatorProbe): CountingMatrixOperator =
      new CountingMatrixOperator(matrix, probe)
