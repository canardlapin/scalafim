package scalafim.fmri.mvpa.measurement

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import resample4s.core.{IndexSpace, Injection}
import scala.compiletime.testing.typeCheckErrors
import scalafim.fmri.mvpa.{AxisRef, EvidenceSource, Observations}
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class MeasurementLegSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private def axis(name: String, keys: Vector[String], units: String = "psc"): AxisRef[String] =
    right(AxisRef.fromStableKeys(name, SpaceRole.Observed, keys, "voxel-order", units, "raw", Vector("fixture:v1")))

  private def source: EvidenceSource =
    val id = SourceId.unsafe("fixture-source")
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe("fixture-root"), id)))

  private def observations(samples: AxisRef[String], neural: AxisRef[String], values: DoubleLinearOperator): Observations[samples.Id, neural.Id] =
    right(Observations.fromOperator(samples, neural, values, ValueIdentity.source(ValueId.unsafe("fixture-values")), source))

  private def matrixEquals(actual: DMat, expected: Vector[Vector[Double]]): Unit =
    assertEquals(actual.rows, expected.length)
    assertEquals(actual.cols, expected.head.length)
    for row <- expected.indices; column <- expected(row).indices do
      assertEqualsDouble(actual(row, column), expected(row)(column), 1e-12)

  test("identity and sparse selection equal explicit dense and matrix-free slicing without construction reads"):
    val samples = axis("samples", Vector("s1", "s2"))
    val neural = axis("neural", Vector("a", "b", "c"))
    val values = DMat.dense(2, 3, Vector(1.0, 10.0, 100.0, 2.0, 20.0, 200.0))
    var reads = 0
    val operator = new DoubleLinearOperator:
      val rows = 2
      val cols = 3
      def applyTo(input: DVec, output: MutableDVec): Unit =
        reads += 1
        values.applyTo(input, output)
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
        reads += 1
        values.transposeApplyTo(input, output)
    val whole = right(MeasurementLeg.identity(neural, MeasurementId.unsafe("all")))
    val population = right(IndexSpace.of(neural.size))
    val selected = right(MeasurementLeg.hardSelection(neural, MeasurementId.unsafe("tail-first"), right(Injection.from(IArray(2, 0), population))))
    val measured = right(selected.measure(observations(samples, neural, operator)))
    assertEquals(reads, 0)
    matrixEquals(right(right(whole.measure(observations(samples, neural, values))).patterns(DMat.eye(3))), Vector(Vector(1.0, 10.0, 100.0), Vector(2.0, 20.0, 200.0)))
    matrixEquals(right(measured.patterns(DMat.eye(2))), Vector(Vector(100.0, 1.0), Vector(200.0, 2.0)))
    assertEquals(reads, 2)

  test("weighted maps match independent arithmetic and distinguish coefficient changes while ignoring rendition"):
    val samples = axis("samples", Vector("s1", "s2"))
    val neural = axis("neural", Vector("a", "b", "c"))
    val values = DMat.dense(2, 3, Vector(1.0, 10.0, 100.0, 2.0, 20.0, 200.0))
    val id = MeasurementId.unsafe("weighted")
    val leg = right(MeasurementLeg.weightedRegion(neural, id, Vector("a" -> 0.5, "c" -> 2.0)))
    matrixEquals(right(right(leg.measure(observations(samples, neural, values))).patterns(DMat.eye(1))), Vector(Vector(200.5), Vector(401.0)))
    val reordered = right(MeasurementLeg.weightedRegion(neural, id, Vector("c" -> 2.0, "a" -> 0.5), Vector("label" -> "renamed")))
    val changed = right(MeasurementLeg.weightedRegion(neural, id, Vector("a" -> 0.5, "c" -> 3.0)))
    assertEquals(leg.descriptor.semanticId, reordered.descriptor.semanticId)
    assert(leg.descriptor.semanticId != changed.descriptor.semanticId)
    assert(MeasurementLeg.weightedRegion(neural, id, Vector("a" -> Double.NaN)).isLeft)
    assert(MeasurementLeg.weightedRegion(neural, id, Vector("a" -> 1.0, "a" -> 2.0)).isLeft)

  test("rank-deficient basis maps preserve explicit metric requirements and reject unverified isometry"):
    val samples = axis("samples", Vector("s1", "s2"))
    val neural = axis("neural", Vector("a", "b", "c"))
    val local = axis("basis", Vector("u", "v"))
    val weights = DMat.dense(2, 3, Vector(1.0, 0.0, 1.0, 2.0, 0.0, 2.0))
    val metric = MetricCapability.DeclaredMetricRequired("rank deficient; precision requires admission")
    val cost = MaterializationCost.LinearOperatorApplications(2, "two local coordinates")
    val map = right(MeasurementLeg.basisMap(neural, local, MeasurementId.unsafe("basis"), weights, metric, cost))
    val values = DMat.dense(2, 3, Vector(1.0, 10.0, 100.0, 2.0, 20.0, 200.0))
    matrixEquals(right(right(map.measure(observations(samples, neural, values))).patterns(DMat.eye(2))), Vector(Vector(101.0, 202.0), Vector(202.0, 404.0)))
    assertEquals(map.descriptor.metric, metric)
    val changedMetric = right(MeasurementLeg.basisMap(neural, local, MeasurementId.unsafe("basis"), weights, MetricCapability.DeclaredMetricRequired("different precision policy"), cost))
    assertNotEquals(map.descriptor.semanticId, changedMetric.descriptor.semanticId)
    assertEquals(MeasurementLeg.basisMap(neural, local, MeasurementId.unsafe("basis"), weights, MetricCapability.CoordinateIsometry, cost), Left(MeasurementError.UnverifiedMetric))
    val otherUnits = axis("neural", Vector("a", "b", "c"), "tesla")
    val different = right(MeasurementLeg.identity(otherUnits, MeasurementId.unsafe("all")))
    assert(right(MeasurementLeg.identity(neural, MeasurementId.unsafe("all"))).descriptor.semanticId != different.descriptor.semanticId)

  test("a foreign nominal neural space cannot enter a measurement even with equal dimensions"):
    val errors = typeCheckErrors("""
import scalafim.fmri.mvpa.Observations
import scalafim.fmri.mvpa.measurement.MeasurementLeg
import multivar.core.SemanticSpace
def foreign[S <: SemanticSpace, N <: SemanticSpace, M <: SemanticSpace, L <: SemanticSpace](leg: MeasurementLeg[N, String, L], evidence: Observations[S, M]) = leg.measure(evidence)
""")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("evidence")))
