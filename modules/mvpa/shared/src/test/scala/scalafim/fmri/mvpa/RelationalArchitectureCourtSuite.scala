package scalafim.fmri.mvpa

import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.DoubleLinearOperator
import gale.linalg.Matrix
import gale.linalg.MutableDVec
import multivar.core.ValueId
import multivar.core.ValueIdentity
import resample4s.core.IndexSpace
import resample4s.core.Injection

final class RelationalArchitectureCourtSuite extends munit.FunSuite:
  private val a = AxisKey.unsafe("a")
  private val b = AxisKey.unsafe("b")
  private val c = AxisKey.unsafe("c")
  private val x = FeatureId.unsafe("x")
  private val y = FeatureId.unsafe("y")
  private val z = FeatureId.unsafe("z")
  private val run1 = PartitionId.unsafe("run-1")
  private val run2 = PartitionId.unsafe("run-2")
  private val run3 = PartitionId.unsafe("run-3")

  private val canonicalEffects = Vector(a, b, c)
  private val canonicalFeatures = Vector(x, y, z)
  private val canonicalPartitions = Vector(run1, run2, run3)

  private val baseCells = cells(
    canonicalPartitions,
    canonicalEffects,
    canonicalFeatures,
    Vector(
      Vector(Vector(1.0, 2.0, 3.0), Vector(0.0, 1.0, 1.0), Vector(2.0, 0.0, 1.0)),
      Vector(Vector(1.5, 1.0, 2.0), Vector(0.5, 1.5, 0.0), Vector(1.0, 0.5, 2.0)),
      Vector(Vector(0.5, 2.5, 1.0), Vector(1.0, 0.5, 1.5), Vector(1.5, 1.0, 0.5))
    )
  )

  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  test("pair reversal transposes the effect form and reverses its edge ledger"):
    val fixture = Fixture(
      canonicalEffects,
      canonicalFeatures,
      canonicalPartitions,
      baseCells,
      "pair-reversal",
      operatorBacked = false
    )
    val forward = right(
      PairingDesign.forwardOnly(
        fixture.namedPartitions,
        PairingReducer.WeightedMean,
        fixture.generalization,
        fixture.independenceFor(fixture.source)
      )
    )
    val reversed = right(forward.reverse)
    val measurement = right(
      Measurement.identity(fixture.neural, MeasurementId.unsafe("whole-space"))
    )
    val metric = right(NeuralQuery.identity(measurement.local))
    val forwardForm = right(
      RelationalCompiler.effectForm(fixture.source, forward, measurement, metric)
    )
    val reversedForm = right(
      RelationalCompiler.effectForm(fixture.source, reversed, measurement, metric)
    )

    assertTranspose(forwardForm.value, reversedForm.value)
    assertEquals(
      reversedForm.receipt.edges,
      forwardForm.receipt.edges.map(edge => PairEdgeReceipt(edge.right, edge.left, edge.weight))
    )
    assertEquals(reversedForm.receipt.design, reversed.identity)

  test("public query, independent direct closure, and complete-form contraction agree"):
    val fixture = Fixture(
      canonicalEffects,
      canonicalFeatures,
      canonicalPartitions,
      baseCells,
      "direct-query",
      operatorBacked = true
    )
    val design = fixture.allOrdered
    val measurement = fixture.identityMeasurement("whole-space")
    val metric = right(NeuralQuery.identity(measurement.local))
    val domain = right(WithinPairDomain(fixture.effects))
    val form = right(
      RelationalCompiler.effectForm(fixture.source, design, measurement, metric)
    )
    val direct = fixture.directIdentityDistances(design)
    val queried = right(fixture.identityFit("whole-space").rdm).distances.toVector

    domain.pairs.zipWithIndex.foreach: (pair, position) =>
      val expected =
        form.value(pair.firstPosition, pair.firstPosition) +
          form.value(pair.secondPosition, pair.secondPosition) -
          form.value(pair.firstPosition, pair.secondPosition) -
          form.value(pair.secondPosition, pair.firstPosition)
      assertEqualsDouble(direct(position), expected, 1e-12)
      assertEqualsDouble(queried(position), expected, 1e-12)

  test("partition, effect, and feature reorderings preserve the keyed estimand"):
    val baseline = Fixture(
      canonicalEffects,
      canonicalFeatures,
      canonicalPartitions,
      baseCells,
      "reorder-baseline",
      operatorBacked = false
    )
    val partitionReordered = Fixture(
      canonicalEffects,
      canonicalFeatures,
      Vector(run3, run1, run2),
      baseCells,
      "reorder-partitions",
      operatorBacked = false
    )
    val effectReordered = Fixture(
      Vector(c, a, b),
      canonicalFeatures,
      canonicalPartitions,
      baseCells,
      "reorder-effects",
      operatorBacked = false
    )
    val featureReordered = Fixture(
      canonicalEffects,
      Vector(z, x, y),
      canonicalPartitions,
      baseCells,
      "reorder-features",
      operatorBacked = false
    )

    val expected = canonicalForm(baseline)(baseline.form)
    assertVector(expected, canonicalForm(partitionReordered)(partitionReordered.form))
    assertVector(expected, canonicalForm(effectReordered)(effectReordered.form))
    assertVector(expected, canonicalForm(featureReordered)(featureReordered.form))
    assertNotEquals(baseline.source.identity, partitionReordered.source.identity)
    assertNotEquals(baseline.source.identity, effectReordered.source.identity)
    assertNotEquals(baseline.source.identity, featureReordered.source.identity)

  test("composed measurement agrees with explicitly projected relations"):
    val fixture = Fixture(
      canonicalEffects,
      canonicalFeatures,
      canonicalPartitions,
      baseCells,
      "measurement-composition-source",
      operatorBacked = true
    )
    val localKeys = Vector(FeatureId.unsafe("component-u"), FeatureId.unsafe("component-v"))
    val local = right(
      AxisRef.create(
        AxisId.unsafe("measurement-composition-local"),
        AxisPurpose.NeuralFeatures,
        localKeys,
        CoordinateBasis.unsafe("fixed-components"),
        Some(AxisUnits.unsafe("activation")),
        AxisScale.nominal,
        CoordinateProvenance.unsafe("relational-court", "measurement-composition-v1")
      )
    )
    val weights = matrix(
      Vector(
        Vector(1.0, 0.0, 1.0),
        Vector(0.0, 2.0, 0.0)
      )
    )
    val measurement = right(
      Measurement.fixedProjection(
        fixture.neural,
        local,
        MeasurementId.unsafe("fixed-components"),
        weights
      )
    )
    val composed = right(
      RelationalCompiler.effectForm(
        fixture.source,
        fixture.allOrdered,
        measurement,
        right(NeuralQuery.identity(measurement.local))
      )
    )

    val projectedCells = (for
      partition <- canonicalPartitions
      effect <- canonicalEffects
      (localKey, localPosition) <- localKeys.zipWithIndex
    yield
      val value = canonicalFeatures.zipWithIndex.map: (feature, sourcePosition) =>
        baseCells((partition, effect, feature)) * weights(localPosition, sourcePosition)
      (partition, effect, localKey) -> value.sum
    ).toMap
    val projected = Fixture(
      canonicalEffects,
      localKeys,
      canonicalPartitions,
      projectedCells,
      "measurement-composition-projected",
      operatorBacked = false
    )
    val explicit = projected.form

    assertVector(canonicalForm(fixture)(composed), canonicalForm(projected)(explicit))
    assert(composed.receipt.measurementComposedBeforeProjection)

  test("independent orthogonal partition errors cancel while overlapping errors reveal bias"):
    val effects = Vector(a, b)
    val features = Vector(x, y)
    val partitions = Vector(run1, run2)
    val orthogonalCells = cells(
      partitions,
      effects,
      features,
      Vector(
        Vector(Vector(0.0, 0.0), Vector(1.0, 0.0)),
        Vector(Vector(0.0, 0.0), Vector(0.0, 1.0))
      )
    )
    val overlappingCells = cells(
      partitions,
      effects,
      features,
      Vector(
        Vector(Vector(0.0, 0.0), Vector(1.0, 0.0)),
        Vector(Vector(0.0, 0.0), Vector(1.0, 0.0))
      )
    )
    val orthogonal = Fixture(
      effects,
      features,
      partitions,
      orthogonalCells,
      "independent-orthogonal",
      operatorBacked = false
    )
    val overlapping = Fixture(
      effects,
      features,
      partitions,
      overlappingCells,
      "overlapping-errors",
      operatorBacked = false
    )
    val orthogonalFit = orthogonal.identityFit("whole-space")
    val overlappingFit = overlapping.identityFit("whole-space")

    assertEqualsDouble(right(orthogonalFit.rdm).distances.values(0), 0.0, 1e-12)
    assertEqualsDouble(right(overlappingFit.rdm).distances.values(0), 1.0, 1e-12)

  test("certified crossnobis precision matches an independent bilinear oracle"):
    val effects = Vector(a, b)
    val features = Vector(x, y)
    val partitions = Vector(run1, run2)
    val relationCells = cells(
      partitions,
      effects,
      features,
      Vector(
        Vector(Vector(0.0, 0.0), Vector(1.0, 2.0)),
        Vector(Vector(0.0, 0.0), Vector(3.0, 4.0))
      )
    )
    val fixture = Fixture(
      effects,
      features,
      partitions,
      relationCells,
      "precision-oracle",
      operatorBacked = false
    )
    val precision = Map(
      (x, x) -> 2.0,
      (x, y) -> 0.5,
      (y, x) -> 0.5,
      (y, y) -> 1.0
    )
    val source = fixture.precisionSource(precision)
    val measurement = fixture.identityMeasurement("whole-space")
    val domain = right(WithinPairDomain(source.effects))
    val query = right(
      RelationalFitQuery.crossnobis(source, domain, RdmNormalization.Raw)
    )
    val fit = right(
      RelationalFit.query(query, fixture.allOrderedFor(source), measurement)
    )
    val result = right(fit.distance)

    val left = Vector(1.0, 2.0)
    val rightVector = Vector(3.0, 4.0)
    val oracle =
      left(0) * (2.0 * rightVector(0) + 0.5 * rightVector(1)) +
        left(1) * (0.5 * rightVector(0) + rightVector(1))
    assertEqualsDouble(oracle, 19.0, 1e-12)
    assertEqualsDouble(result.rdm.distances.values(0), oracle, 1e-12)
    assertEquals(result.precision.operatorApplications, 4L)
    assertEquals(result.precision.partitions.map(_.partition), partitions)

  test("dense and operator execution agree with exact logical-work receipts"):
    val dense = Fixture(
      canonicalEffects,
      canonicalFeatures,
      canonicalPartitions,
      baseCells,
      "representation-parity",
      operatorBacked = false
    )
    val operator = Fixture(
      canonicalEffects,
      canonicalFeatures,
      canonicalPartitions,
      baseCells,
      "representation-parity",
      operatorBacked = true
    )
    val denseMeasurement = dense.selection("selected-x-z", Vector(0, 2))
    val operatorMeasurement = operator.selection("selected-x-z", Vector(0, 2))
    val denseForm = right(
      RelationalCompiler.effectForm(
        dense.source,
        dense.allOrdered,
        denseMeasurement,
        right(NeuralQuery.identity(denseMeasurement.local))
      )
    )
    val operatorForm = right(
      RelationalCompiler.effectForm(
        operator.source,
        operator.allOrdered,
        operatorMeasurement,
        right(NeuralQuery.identity(operatorMeasurement.local))
      )
    )
    val materialized = right(
      RelationalCompiler.effectForm(
        operator.source,
        operator.allOrdered,
        operatorMeasurement,
        right(NeuralQuery.identity(operatorMeasurement.local)),
        MaterializationPolicy.Allow(MaterializationBudget.unsafe(6L))
      )
    )

    assertMatrix(denseForm.value, operatorForm.value)
    assertMatrix(denseForm.value, materialized.value)
    assertEquals(dense.source.identity, operator.source.identity)
    assertEquals(denseForm.receipt.operatorApplications, 3L)
    assertEquals(operatorForm.receipt.operatorApplications, 3L)
    assertEquals(operatorForm.receipt.outputCells, 9L)
    assertEquals(operatorForm.receipt.avoidedFullRelationCells, 9L)
    assertEquals(operatorForm.receipt.materializedCells, 0L)
    assertEquals(materialized.receipt.materializedCells, 18L)
    assertEquals(materialized.receipt.materializations.length, 3)
    assertEquals(operator.probes.map(_.transposeApplications), Vector(3, 3, 3))
    assertEquals(operator.probes.map(_.forwardApplications), Vector(2, 2, 2))

  private final class Fixture(
      effectOrder: Vector[AxisKey],
      featureOrder: Vector[FeatureId],
      partitionOrder: Vector[PartitionId],
      cellValues: Map[(PartitionId, AxisKey, FeatureId), Double],
      token: String,
      operatorBacked: Boolean
  ):
    val effects = right(
      AxisRef.create(
        AxisId.unsafe(s"court-effects-$token"),
        AxisPurpose.Effects,
        effectOrder,
        CoordinateBasis.unsafe("condition-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("relational-court", token)
      )
    )
    val neural = right(
      AxisRef.create(
        AxisId.unsafe(s"court-neural-$token"),
        AxisPurpose.NeuralFeatures,
        featureOrder,
        CoordinateBasis.unsafe("neural-order"),
        Some(AxisUnits.unsafe("activation")),
        AxisScale.nominal,
        CoordinateProvenance.unsafe("relational-court", token)
      )
    )
    val partitions = right(
      AxisRef.create(
        AxisId.unsafe(s"court-partitions-$token"),
        AxisPurpose.Partitions,
        partitionOrder,
        CoordinateBasis.unsafe("partition-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("relational-court", token)
      )
    )
    val namedPartitions = right(
      PartitionAxis(ScientificAxisName.unsafe("runs"), partitions)
    )
    val generalization =
      GeneralizationAxis(ScientificAxisName.unsafe("runs"), partitions.identity)
    private val training = right(
      AxisRef.create(
        AxisId.unsafe(s"court-training-$token"),
        AxisPurpose.Samples,
        Vector(SampleId.unsafe("time-1"), SampleId.unsafe("time-2")),
        CoordinateBasis.unsafe("time-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("relational-court", token)
      )
    )
    private val relationDesign = right(
      DesignIdentity(DesignKind.unsafe("court-relation-fit"), Vector("fixture" -> token))
    )
    val probes: Vector[OperatorProbe] = Vector.fill(partitions.size)(OperatorProbe())

    private def relationMatrix(partition: PartitionId): DMat =
      matrix(
        effectOrder.map: effect =>
          featureOrder.map: feature =>
            cellValues((partition, effect, feature))
      )

    private def receipt(partition: PartitionId) =
      right(
        RelationFitReceipt(
          ValueIdentity.source(ValueId.unsafe(s"court-source-$token-${partition.value}")),
          relationDesign,
          right(Estimability(effects, Vector.fill(effects.size)(true))),
          NormalizationIdentity.none,
          training
        )
      )

    val source: PartitionedRelations[
      partitions.Id,
      effects.Id,
      neural.Id,
      AxisKey,
      FeatureId,
      EstimateOnlyCapabilities[neural.Id, FeatureId]
    ] =
      val entries = partitionOrder.zipWithIndex.map: (partition, position) =>
        val value = relationMatrix(partition)
        val estimate =
          if operatorBacked then
            right(
              EvidenceTable.operator(
                effects,
                neural,
                CountingOperator(value, probes(position)),
                ValueId.unsafe(s"court-estimate-$token-${partition.value}")
              )
            )
          else
            right(
              EvidenceTable.dense(
                effects,
                neural,
                value,
                ValueId.unsafe(s"court-estimate-$token-${partition.value}")
              )
            )
        PartitionRelation(
          partition,
          right(Relation(estimate, receipt(partition), right(EstimateOnlyCapabilities(neural))))
        )
      right(PartitionedRelations(namedPartitions, effects, neural, entries))

    def independenceFor[C <: RelationCapabilities[neural.Id, FeatureId]](
        evidence: PartitionedRelations[
          partitions.Id,
          effects.Id,
          neural.Id,
          AxisKey,
          FeatureId,
          C
        ]
    ): PartitionIndependenceDeclaration[partitions.Id, partitions.Id] =
      right(
        PartitionIndependenceTestSupport.declareAllPairs(
          evidence.identity,
          namedPartitions,
          s"court-independent-$token"
        )
      )

    def allOrderedFor[C <: RelationCapabilities[neural.Id, FeatureId]](
        evidence: PartitionedRelations[
          partitions.Id,
          effects.Id,
          neural.Id,
          AxisKey,
          FeatureId,
          C
        ]
    ): PairingDesign[partitions.Id, partitions.Id] =
      right(
        PairingDesign.allOrdered(
          namedPartitions,
          PairingReducer.WeightedMean,
          generalization,
          independenceFor(evidence)
        )
      )

    def allOrdered: PairingDesign[partitions.Id, partitions.Id] =
      allOrderedFor(source)

    def identityMeasurement(id: String) =
      right(Measurement.identity(neural, MeasurementId.unsafe(id)))

    def selection(id: String, positions: Vector[Int]) =
      val injection = right(
        Injection.from(
          IArray.unsafeFromArray(positions.toArray),
          right(IndexSpace.of(neural.size))
        )
      )
      right(Measurement.hardSelection(neural, MeasurementId.unsafe(id), injection))

    def directIdentityDistances(
        design: PairingDesign[partitions.Id, partitions.Id]
    ): Vector[Double] =
      val domain = right(WithinPairDomain(effects))
      val denominator = design.reducer match
        case PairingReducer.WeightedSum  => 1.0
        case PairingReducer.WeightedMean => design.edges.map(_.weight).sum
      domain.pairs.map: pair =>
        design.edges
          .map: edge =>
            val contribution = featureOrder
              .map: feature =>
                val left =
                  cellValues((edge.left, pair.first, feature)) -
                    cellValues((edge.left, pair.second, feature))
                val right =
                  cellValues((edge.right, pair.first, feature)) -
                    cellValues((edge.right, pair.second, feature))
                left * right
              .sum
            edge.weight * contribution / denominator
          .sum

    def identityFit(id: String) =
      val domain = right(WithinPairDomain(effects))
      val query = right(
        RelationalFitQuery.identityPrecision(
          source,
          domain,
          RdmNormalization.Raw
        )
      )
      val measurement = identityMeasurement(id)
      right(RelationalFit.query(query, allOrdered, measurement))

    def form: PairedEffectForm[effects.Id, AxisKey] =
      val measurement = identityMeasurement("whole-space")
      right(
        RelationalCompiler.effectForm(
          source,
          allOrdered,
          measurement,
          right(NeuralQuery.identity(measurement.local))
        )
      )

    def precisionSource(
        entries: Map[(FeatureId, FeatureId), Double]
    ): PartitionedRelations[
      partitions.Id,
      effects.Id,
      neural.Id,
      AxisKey,
      FeatureId,
      PrecisionFitCapabilities[neural.Id, FeatureId]
    ] =
      val precisionMatrix = matrix(
        featureOrder.map: row =>
          featureOrder.map(column => entries((row, column)))
      )
      val residualMatrix = DMat.eye(neural.size)
      val relations = partitionOrder.map: partition =>
        val estimate = right(
          EvidenceTable.dense(
            effects,
            neural,
            relationMatrix(partition),
            ValueId.unsafe(s"court-estimate-$token-${partition.value}")
          )
        )
        val residual = right(
          EvidenceTable.dense(
            neural,
            neural,
            residualMatrix,
            ValueId.unsafe(s"court-residual-$token-${partition.value}")
          )
        )
        val precision = right(
          EvidenceTable.dense(
            neural,
            neural,
            precisionMatrix,
            ValueId.unsafe(s"court-precision-$token-${partition.value}")
          )
        )
        val certifiedPrecision = right(CertifiedNoisePrecision(precision))
        val certifiedResidual = right(CertifiedResidualMoments(residual))
        val capabilities = right(
          PrecisionFitCapabilities(
            certifiedResidual,
            certifiedPrecision,
            ResidualDegreesOfFreedom.unsafe(20.0)
          )
        )
        PartitionRelation(
          partition,
          right(Relation(estimate, receipt(partition), capabilities))
        )
      right(PartitionedRelations(namedPartitions, effects, neural, relations))

  private object Fixture:
    def apply(
        effects: Vector[AxisKey],
        features: Vector[FeatureId],
        partitions: Vector[PartitionId],
        cells: Map[(PartitionId, AxisKey, FeatureId), Double],
        token: String,
        operatorBacked: Boolean
    ): Fixture =
      new Fixture(effects, features, partitions, cells, token, operatorBacked)

  private final class OperatorProbe:
    var forwardApplications: Int = 0
    var transposeApplications: Int = 0

  private object OperatorProbe:
    def apply(): OperatorProbe = new OperatorProbe

  private final class CountingOperator(matrix: DMat, probe: OperatorProbe) extends DoubleLinearOperator:
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

  private def cells(
      partitions: Vector[PartitionId],
      effects: Vector[AxisKey],
      features: Vector[FeatureId],
      values: Vector[Vector[Vector[Double]]]
  ): Map[(PartitionId, AxisKey, FeatureId), Double] =
    (for
      (partition, partitionPosition) <- partitions.zipWithIndex
      (effect, effectPosition) <- effects.zipWithIndex
      (feature, featurePosition) <- features.zipWithIndex
    yield (partition, effect, feature) ->
      values(partitionPosition)(effectPosition)(featurePosition)).toMap

  private def matrix(rows: Vector[Vector[Double]]): DMat =
    val builder = Matrix.newBuilder(rows.length, rows.head.length)
    var row = 0
    while row < rows.length do
      var column = 0
      while column < rows(row).length do
        builder(row, column) = rows(row)(column)
        column += 1
      row += 1
    builder.result()

  private def canonicalForm(fixture: Fixture)(
      form: PairedEffectForm[fixture.effects.Id, AxisKey]
  ): Vector[Double] =
    for
      left <- canonicalEffects.filter(fixture.effects.positionOf(_).nonEmpty)
      rightKey <- canonicalEffects.filter(fixture.effects.positionOf(_).nonEmpty)
    yield form.value(
      fixture.effects.positionOf(left).get,
      fixture.effects.positionOf(rightKey).get
    )

  private def assertTranspose(left: DMat, right: DMat): Unit =
    assertEquals(left.rows, right.cols)
    assertEquals(left.cols, right.rows)
    var row = 0
    while row < left.rows do
      var column = 0
      while column < left.cols do
        assertEqualsDouble(left(row, column), right(column, row), 1e-12)
        column += 1
      row += 1

  private def assertMatrix(left: DMat, right: DMat): Unit =
    assertEquals(left.rows, right.rows)
    assertEquals(left.cols, right.cols)
    var row = 0
    while row < left.rows do
      var column = 0
      while column < left.cols do
        assertEqualsDouble(left(row, column), right(row, column), 1e-12)
        column += 1
      row += 1

  private def assertVector(left: Vector[Double], right: Vector[Double]): Unit =
    assertEquals(left.length, right.length)
    left
      .zip(right)
      .foreach: (expected, actual) =>
        assertEqualsDouble(actual, expected, 1e-12)
