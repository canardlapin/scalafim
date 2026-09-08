package scalafim.fmri.mvpa

import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.DoubleLinearOperator
import gale.linalg.MutableDVec
import multivar.core.ValueId
import resample4s.core.IndexSpace
import resample4s.core.Injection

class MeasurementSuite extends munit.FunSuite:

  private val sampleIds =
    Vector("sample-a", "sample-b", "sample-c").map(SampleId.unsafe)

  private val featureIds =
    Vector("voxel-a", "voxel-b", "voxel-c").map(FeatureId.unsafe)

  private val samples =
    sampleAxis("all-samples", sampleIds)

  private val features =
    featureAxis("whole-brain", featureIds, AxisPurpose.NeuralFeatures)

  private val components =
    featureAxis(
      "local-components",
      Vector("component-a", "component-b").map(FeatureId.unsafe),
      AxisPurpose.Components
    )

  private val patterns =
    GaleTestMatrix.fromRows(
      Vector(
        Vector(1.0, 10.0, 100.0),
        Vector(2.0, 20.0, 200.0),
        Vector(3.0, 30.0, 300.0)
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

  private def featureAxis(
      id: String,
      keys: Vector[FeatureId],
      purpose: AxisPurpose
  ): AxisRef[FeatureId] =
    AxisRef
      .create(
        AxisId.unsafe(id),
        purpose,
        keys,
        CoordinateBasis.unsafe("neural-coordinate-table"),
        Some(AxisUnits.unsafe("percent-signal-change")),
        AxisScale.nominal,
        CoordinateProvenance.unsafe("mask", "v1")
      )
      .toOption
      .get

  private def indices(values: Int*): IArray[Int] =
    IArray.unsafeFromArray(values.toArray)

  private def hardSelection(
      id: String,
      positions: Int*
  ): Measurement[features.Id, FeatureId, FeatureId] =
    val space = IndexSpace.of(features.size).toOption.get
    val injection = Injection.from(indices(positions*), space).toOption.get
    Measurement
      .hardSelection(features, MeasurementId.unsafe(id), injection)
      .toOption
      .get

  test("hard selections are sparse linear legs with exact ordered support"):
    val measurement = hardSelection("posterior-roi", 2, 0)
    val coordinateWeights = GaleTestMatrix.fromRows(
      Vector(Vector(1.0), Vector(10.0), Vector(100.0))
    )

    assertEquals(measurement.local.keys.map(_.value), Vector("voxel-c", "voxel-a"))
    assertEquals(measurement.identity.source, features.identity.fingerprint)
    assertEquals(measurement.identity.local, measurement.local.identity.fingerprint)
    assertEquals(measurement.identity.kind, MeasurementKind.HardSelection)
    assertEquals(
      measurement.leg.apply(coordinateWeights).toOption.get.toRows,
      Vector(Vector(100.0), Vector(1.0))
    )

  test("weighted-region identity canonically includes exact support and weights"):
    val id = MeasurementId.unsafe("weighted-language-roi")
    val first = Measurement
      .weightedRegion(
        features,
        id,
        Vector(featureIds(2) -> 2.0, featureIds(0) -> 0.5)
      )
      .toOption
      .get
    val reordered = Measurement
      .weightedRegion(
        features,
        id,
        Vector(featureIds(0) -> 0.5, featureIds(2) -> 2.0)
      )
      .toOption
      .get
    val changedWeight = Measurement
      .weightedRegion(
        features,
        id,
        Vector(featureIds(0) -> 0.5, featureIds(2) -> 3.0)
      )
      .toOption
      .get
    val changedSupport = Measurement
      .weightedRegion(
        features,
        id,
        Vector(featureIds(0) -> 0.5, featureIds(1) -> 2.0)
      )
      .toOption
      .get

    assertEquals(first.identity.fingerprint, reordered.identity.fingerprint)
    assertEquals(first.local.identity.fingerprint, reordered.local.identity.fingerprint)
    assertNotEquals(first.identity.fingerprint, changedWeight.identity.fingerprint)
    assertNotEquals(first.identity.fingerprint, changedSupport.identity.fingerprint)
    assertEquals(
      first.leg.apply(GaleTestMatrix.fromRows(Vector(Vector(2.0), Vector(5.0), Vector(7.0)))).toOption.get.toRows,
      Vector(Vector(15.0))
    )
    assert(Measurement.weightedRegion(features, id, Vector.empty).isLeft)
    assert(Measurement.weightedRegion(features, id, Vector(featureIds(0) -> Double.NaN)).isLeft)
    assert(Measurement.weightedRegion(features, id, Vector(featureIds(0) -> 0.0)).isLeft)
    assert(Measurement.weightedRegion(features, id, Vector(featureIds(0) -> 1.0, featureIds(0) -> 2.0)).isLeft)
    assert(Measurement.weightedRegion(features, id, Vector(FeatureId.unsafe("absent") -> 1.0)).isLeft)

  test("fixed projections validate their declared spaces and finite weights"):
    val weights = GaleTestMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0, -1.0),
        Vector(0.0, 0.5, 0.5)
      )
    )
    val id = MeasurementId.unsafe("two-component-projection")
    val fixed = Measurement.fixedProjection(features, components, id, weights).toOption.get
    assertEquals(
      fixed.leg.apply(GaleTestMatrix.fromRows(Vector(Vector(2.0), Vector(4.0), Vector(8.0)))).toOption.get.toRows,
      Vector(Vector(-6.0), Vector(6.0))
    )

    val wrongShape = GaleTestMatrix.fromRows(Vector(Vector(1.0, 0.0)))
    assert(
      Measurement
        .fixedProjection(features, components, id, wrongShape)
        .left
        .exists:
          case MeasurementError.ShapeMismatch(2, 3, 1, 2) => true
          case _                                          => false
    )

    val nonFinite = GaleTestMatrix.fromRows(
      Vector(
        Vector(1.0, Double.NaN, -1.0),
        Vector(0.0, 0.5, 0.5)
      )
    )
    assert(
      Measurement
        .fixedProjection(features, components, id, nonFinite)
        .left
        .exists:
          case MeasurementError.InvalidWeight(_, value) => value.isNaN
          case _                                        => false
    )

  test("frames are deterministic while typed rendition metadata remains non-scientific"):
    final case class RoiLabel(value: String)
    final case class SearchlightCenter(vertex: Int)

    val alpha = hardSelection("alpha", 0, 1)
    val zeta = hardSelection("zeta", 2)
    val alphaEntry = MeasurementEntry(alpha, RoiLabel("left-language"))
    val zetaEntry = MeasurementEntry(zeta, SearchlightCenter(42))
    val forward = MeasurementFrame(features)(Vector(alphaEntry, zetaEntry)).toOption.get
    val reverse = MeasurementFrame(features)(Vector(zetaEntry, alphaEntry)).toOption.get
    val labelView = MeasurementFrame(features)(Vector(MeasurementEntry(alpha, RoiLabel("display-a")))).toOption.get
    val centerView = MeasurementFrame(features)(Vector(MeasurementEntry(alpha, SearchlightCenter(99)))).toOption.get

    assertEquals(forward.entries.map(_.measurement.identity.id.value), Vector("alpha", "zeta"))
    assertEquals(reverse.entries.map(_.measurement.identity.id.value), Vector("alpha", "zeta"))
    assertEquals(forward.identity, reverse.identity)
    assertEquals(labelView.identity, centerView.identity)
    assert(MeasurementFrame(features)(Vector.empty).left.exists:
      case MeasurementError.EmptyFrame => true
      case _                           => false)

    val duplicateId = hardSelection("alpha", 2)
    assert(MeasurementFrame(features)(Vector(alphaEntry, MeasurementEntry(duplicateId, NoRendition))).left.exists:
      case MeasurementError.DuplicateMeasurementId(id) => id.value == "alpha"
      case _                                           => false)

  test("evidence measurement agrees for dense and matrix-free sources without eager materialization"):
    val measurement = hardSelection("sparse-measurement", 2, 0)
    val dense = EvidenceTable
      .dense(samples, features, patterns, ValueId.unsafe("dense-patterns"))
      .toOption
      .get
    val probe = ProbeOperator(patterns)
    val matrixFree = EvidenceTable
      .operator(samples, features, probe, ValueId.unsafe("matrix-free-patterns"))
      .toOption
      .get

    val denseMeasured = dense.measureColumns(measurement).toOption.get
    val matrixFreeMeasured = matrixFree.measureColumns(measurement).toOption.get
    assertEquals(probe.forwardCalls, 0)
    assertEquals(denseMeasured.columns.identity, measurement.local.identity)
    assertEquals(matrixFreeMeasured.columns.identity, measurement.local.identity)

    val localWeights = GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(1.0)))
    val expected = GaleTestMatrix.fromRows(
      Vector(Vector(101.0), Vector(202.0), Vector(303.0))
    )
    assertMatrix(denseMeasured.rightMultiply(localWeights).toOption.get, expected)
    assertMatrix(matrixFreeMeasured.rightMultiply(localWeights).toOption.get, expected)
    assert(probe.forwardCalls > 0)

  test("foreign source spaces are rejected by static composition"):
    val foreignSourceCannotMeasure = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace
      def illegal[R <: SemanticSpace, C1 <: SemanticSpace, C2 <: SemanticSpace, RK, CK](
          table: EvidenceTable[R, C1, RK, CK],
          measurement: Measurement[C2, CK, CK]
      ) = table.measureColumns(measurement)
    """)

    assert(foreignSourceCannotMeasure.nonEmpty)

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

  private final class ProbeOperator private (matrix: DMat) extends DoubleLinearOperator:
    var forwardCalls: Int = 0

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
      var column = 0
      while column < cols do
        var total = 0.0
        var row = 0
        while row < rows do
          total += matrix(row, column) * input(row)
          row += 1
        output(column) = total
        column += 1

  private object ProbeOperator:
    def apply(matrix: DMat): ProbeOperator =
      new ProbeOperator(matrix)
