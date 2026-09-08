package scalafim.fmri.mvpa

import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.DoubleLinearOperator
import gale.linalg.MutableDVec
import multivar.core.OperatorRepresentation
import multivar.core.ValueId
import resample4s.core.Draw
import resample4s.core.IndexSpace
import resample4s.core.Injection

class EvidenceTableSuite extends munit.FunSuite:

  private val sampleIds =
    Vector("sample-101", "sample-205", "sample-309").map(SampleId.unsafe)

  private val featureIds =
    Vector("voxel-17", "voxel-41").map(FeatureId.unsafe)

  private val samples =
    sampleAxis("all-samples", sampleIds)

  private val features =
    featureAxis("brain-features", featureIds)

  private val matrix =
    GaleTestMatrix.fromRows(
      Vector(
        Vector(1.0, 10.0),
        Vector(2.0, 20.0),
        Vector(3.0, 30.0)
      )
    )

  private def sampleAxis(id: String, keys: Vector[SampleId]): AxisRef[SampleId] =
    AxisRef
      .create(
        AxisId.unsafe(id),
        AxisPurpose.Samples,
        keys,
        CoordinateBasis.unsafe("trial-table"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("fixture", "v1")
      )
      .toOption
      .get

  private def featureAxis(id: String, keys: Vector[FeatureId]): AxisRef[FeatureId] =
    AxisRef
      .create(
        AxisId.unsafe(id),
        AxisPurpose.NeuralFeatures,
        keys,
        CoordinateBasis.unsafe("voxel-table"),
        Some(AxisUnits.unsafe("percent-signal-change")),
        AxisScale.nominal,
        CoordinateProvenance.unsafe("mask", "v1")
      )
      .toOption
      .get

  private def indices(values: Int*): IArray[Int] =
    IArray.unsafeFromArray(values.toArray)

  private def dense(
      id: String = "dense-evidence"
  ): EvidenceTable[samples.Id, features.Id, SampleId, FeatureId] =
    EvidenceTable
      .dense(samples, features, matrix, ValueId.unsafe(id))
      .toOption
      .get

  test("dense and matrix-free evidence expose the same exact axes and numerical actions"):
    val counting = CountingOperator(matrix)
    val denseEvidence = dense()
    val operatorEvidence = EvidenceTable
      .operator(samples, features, counting, ValueId.unsafe("operator-evidence"))
      .toOption
      .get
    val weights = GaleTestMatrix.fromRows(
      Vector(Vector(2.0, -1.0), Vector(0.5, 3.0))
    )
    val rowScores = GaleTestMatrix.fromRows(
      Vector(Vector(1.0), Vector(-2.0), Vector(0.25))
    )

    assert(denseEvidence.rows eq operatorEvidence.rows)
    assert(denseEvidence.columns eq operatorEvidence.columns)
    assertEquals(denseEvidence.representation, OperatorRepresentation.Dense)
    assertEquals(operatorEvidence.representation, OperatorRepresentation.MatrixFree)
    assertMatrix(
      operatorEvidence.rightMultiply(weights).toOption.get,
      denseEvidence.rightMultiply(weights).toOption.get
    )
    assertMatrix(
      operatorEvidence.transposeMultiply(rowScores).toOption.get,
      denseEvidence.transposeMultiply(rowScores).toOption.get
    )
    assert(counting.forwardCalls > 0)
    assert(counting.transposeCalls > 0)

  test("the structural adjoint satisfies the real inner-product law"):
    val evidence = dense()
    val columnVector = GaleTestMatrix.fromRows(Vector(Vector(2.0), Vector(-0.25)))
    val rowVector = GaleTestMatrix.fromRows(Vector(Vector(1.5), Vector(-2.0), Vector(0.75)))
    val forward = evidence.rightMultiply(columnVector).toOption.get
    val adjoint = evidence.transposeMultiply(rowVector).toOption.get

    var left = 0.0
    var row = 0
    while row < forward.rows do
      left += forward(row, 0) * rowVector(row, 0)
      row += 1

    var right = 0.0
    var column = 0
    while column < adjoint.rows do
      right += columnVector(column, 0) * adjoint(column, 0)
      column += 1

    assertEqualsDouble(left, right, 1e-12)

  test("row restriction and draw use the named typed relation without losing identity"):
    val space = IndexSpace.of(samples.size).toOption.get
    val injection = ReindexingLeg
      .injection(samples, Injection.from(indices(2, 0), space).toOption.get)
      .toOption
      .get
    val draw = ReindexingLeg
      .draw(samples, Draw.from(indices(2, 0, 2), space).toOption.get)
      .toOption
      .get
    val denseEvidence = dense()
    val operatorEvidence = EvidenceTable
      .operator(samples, features, CountingOperator(matrix), ValueId.unsafe("restricted-operator"))
      .toOption
      .get
    val weights = GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(1.0)))

    val denseInjected = denseEvidence.restrictRows(injection).toOption.get
    val operatorInjected = operatorEvidence.restrictRows(injection).toOption.get
    val denseDrawn = denseEvidence.restrictRows(draw).toOption.get
    val operatorDrawn = operatorEvidence.restrictRows(draw).toOption.get

    assertEquals(denseInjected.rows.identity, injection.child.identity)
    assertEquals(denseDrawn.rows.identity, draw.child.identity)
    assertMatrix(
      denseInjected.rightMultiply(weights).toOption.get,
      operatorInjected.rightMultiply(weights).toOption.get
    )
    assertMatrix(denseDrawn.rightMultiply(weights).toOption.get, operatorDrawn.rightMultiply(weights).toOption.get)
    assertEquals(denseInjected.rightMultiply(weights).toOption.get.toRows, Vector(Vector(33.0), Vector(11.0)))
    assertEquals(
      denseDrawn.rightMultiply(weights).toOption.get.toRows,
      Vector(Vector(33.0), Vector(11.0), Vector(33.0))
    )

  test("an explicit row permutation is the same numerical reorder for dense and matrix-free evidence"):
    val permutation = samples.reorder(Vector(2, 0, 1)).toOption.get
    val densePermuted = dense().restrictRows(permutation).toOption.get
    val operatorPermuted = EvidenceTable
      .operator(samples, features, CountingOperator(matrix), ValueId.unsafe("permuted-operator"))
      .toOption
      .get
      .restrictRows(permutation)
      .toOption
      .get

    val materializationPolicy =
      MaterializationPolicy.Allow(MaterializationBudget.unsafe(6))
    val expected = GaleTestMatrix.fromRows(
      Vector(
        Vector(3.0, 30.0),
        Vector(1.0, 10.0),
        Vector(2.0, 20.0)
      )
    )

    assertEquals(densePermuted.rows.identity, permutation.child.identity)
    assertMatrix(densePermuted.materialize(materializationPolicy).toOption.get.value, expected)
    assertMatrix(operatorPermuted.materialize(materializationPolicy).toOption.get.value, expected)

  test("dense materialization is explicit, budgeted, receipted, and not triggered by construction"):
    val counting = CountingOperator(matrix)
    val evidence = EvidenceTable
      .operator(samples, features, counting, ValueId.unsafe("lazy-evidence"))
      .toOption
      .get

    assertEquals(counting.forwardCalls, 0)
    assert(
      evidence
        .materialize(MaterializationPolicy.Reject)
        .left
        .exists:
          case EvidenceTableError.MaterializationRejected => true
          case _                                          => false
    )
    assertEquals(counting.forwardCalls, 0)

    val tooSmall = MaterializationBudget.unsafe(5)
    assert(
      evidence
        .materialize(MaterializationPolicy.Allow(tooSmall))
        .left
        .exists:
          case EvidenceTableError.MaterializationBudgetExceeded(6L, budget) => budget == tooSmall
          case _                                                            => false
    )
    assertEquals(counting.forwardCalls, 0)

    val budget = MaterializationBudget.unsafe(6)
    val materialized = evidence.materialize(MaterializationPolicy.Allow(budget)).toOption.get

    assertMatrix(materialized.value, matrix)
    assert(materialized.rows eq samples.evidence)
    assert(materialized.columns eq features.evidence)
    assertEquals(materialized.receipt.rowIdentity, samples.identity.fingerprint)
    assertEquals(materialized.receipt.columnIdentity, features.identity.fingerprint)
    assertEquals(materialized.receipt.sourceRepresentation, OperatorRepresentation.MatrixFree)
    assertEquals(materialized.receipt.elements, 6L)
    assertEquals(materialized.receipt.budget, budget)
    assertEquals(counting.forwardCalls, features.size)

  test("row stacking preserves exact axes and agrees for dense and matrix-free parts"):
    val firstRows = sampleAxis("first-run", sampleIds.take(2))
    val secondRows = sampleAxis("second-run", sampleIds.drop(2))
    val stackedRows = sampleAxis("stacked-runs", sampleIds)
    val firstMatrix = GaleTestMatrix.fromRows(Vector(Vector(1.0, 10.0), Vector(2.0, 20.0)))
    val secondMatrix = GaleTestMatrix.fromRows(Vector(Vector(3.0, 30.0)))

    val denseParts = Vector(
      EvidenceTable.dense(firstRows, features, firstMatrix, ValueId.unsafe("dense-first")).toOption.get,
      EvidenceTable.dense(secondRows, features, secondMatrix, ValueId.unsafe("dense-second")).toOption.get
    )
    val operatorParts = Vector(
      EvidenceTable
        .operator(firstRows, features, CountingOperator(firstMatrix), ValueId.unsafe("op-first"))
        .toOption
        .get,
      EvidenceTable
        .operator(secondRows, features, CountingOperator(secondMatrix), ValueId.unsafe("op-second"))
        .toOption
        .get
    )
    val denseStack = EvidenceTable.stackRows(stackedRows, features, denseParts).toOption.get
    val operatorStack = EvidenceTable.stackRows(stackedRows, features, operatorParts).toOption.get
    val weights = GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(-0.5)))

    assertEquals(denseStack.rows.identity, stackedRows.identity)
    assertEquals(denseStack.columns.identity, features.identity)
    assertMatrix(denseStack.rightMultiply(weights).toOption.get, operatorStack.rightMultiply(weights).toOption.get)
    assertMatrix(
      operatorStack.materialize(MaterializationPolicy.Allow(MaterializationBudget.unsafe(6))).toOption.get.value,
      matrix
    )

  test("stacking and construction reject mismatched scientific shapes and identities"):
    assert(
      EvidenceTable
        .dense(
          samples,
          features,
          GaleTestMatrix.fromRows(Vector(Vector(1.0, 2.0))),
          ValueId.unsafe("wrong-shape")
        )
        .left
        .exists:
          case EvidenceTableError.ShapeMismatch(3, 2, 1, 2) => true
          case _                                            => false
    )
    val nonFinite = GaleTestMatrix.fromRows(
      Vector(Vector(1.0, 2.0), Vector(3.0, Double.NaN), Vector(5.0, 6.0))
    )
    assert(
      EvidenceTable
        .dense(samples, features, nonFinite, ValueId.unsafe("non-finite"))
        .left
        .exists:
          case EvidenceTableError.NonFiniteValue("dense evidence", 3, value) => value.isNaN
          case _                                                             => false
    )
    assert(MaterializationBudget(0).isLeft)
    assert(
      EvidenceTable
        .stackRows(samples, features, Vector.empty)
        .left
        .exists:
          case EvidenceTableError.EmptyStack => true
          case _                             => false
    )

    val foreignFeatures = featureAxis("foreign-features", featureIds)
    val firstRows = sampleAxis("first-part", sampleIds.take(2))
    val secondRows = sampleAxis("second-part", sampleIds.drop(2))
    val parts = Vector(
      EvidenceTable
        .dense(
          firstRows,
          features,
          GaleTestMatrix.fromRows(Vector(Vector(1.0, 10.0), Vector(2.0, 20.0))),
          ValueId.unsafe("part-a")
        )
        .toOption
        .get,
      EvidenceTable
        .dense(
          secondRows,
          foreignFeatures,
          GaleTestMatrix.fromRows(Vector(Vector(3.0, 30.0))),
          ValueId.unsafe("part-b")
        )
        .toOption
        .get
    )
    assert(
      EvidenceTable
        .stackRows(samples, features, parts)
        .left
        .exists:
          case EvidenceTableError.StackColumnMismatch(1, _, _) => true
          case _                                               => false
    )

  private def assertMatrix(actual: DMat, expected: DMat, tolerance: Double = 1e-12): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var column = 0
      while column < actual.cols do
        assertEqualsDouble(actual(row, column), expected(row, column), tolerance)
        column += 1
      row += 1

  private final class CountingOperator private (matrix: DMat) extends DoubleLinearOperator:
    var forwardCalls: Int = 0
    var transposeCalls: Int = 0

    override def rows: Int =
      matrix.rows

    override def cols: Int =
      matrix.cols

    override def applyTo(input: DVec, output: MutableDVec): Unit =
      forwardCalls += 1
      var row = 0
      while row < rows do
        var total = 0.0
        var column = 0
        while column < cols do
          total += matrix(row, column) * input(column)
          column += 1
        output(row) = total
        row += 1

    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
      transposeCalls += 1
      var column = 0
      while column < cols do
        var total = 0.0
        var row = 0
        while row < rows do
          total += matrix(row, column) * input(row)
          row += 1
        output(column) = total
        column += 1

  private object CountingOperator:
    def apply(matrix: DMat): CountingOperator =
      new CountingOperator(matrix)
