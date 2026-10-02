package scalafim.fmri.mvpa.dataset.predictive

import alder.kernel.DataFingerprint
import gale.linalg.DMat
import multivar.core.SpaceRole
import resample4s.core.{DigestAlgorithm, Labels}
import resample4s.designs.FixedPartitions
import scalafim.fmri.mvpa.*

class AlderCorrelationCentroidSuite extends munit.FunSuite:
  given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)

  private val values = Vector(
    Vector(2.0, 1.0, 0.0), Vector(-2.0, -1.0, 0.5),
    Vector(-1.0, -2.0, 0.0), Vector(1.0, 2.0, -0.5),
    Vector(2.5, 0.5, 0.0), Vector(-2.5, -0.5, 0.0)
  )
  private val labels = Vector(0.0, 1.0, 1.0, 0.0, 0.0, 1.0)
  private val blocks = Array(0, 0, 1, 1, 2, 2)
  private val coding = right(SwiftTargetCoding(Vector(0.0 -> "left", 1.0 -> "right")))

  private def rows(input: Vector[Vector[Double]]): AlderMaterializedRows[String] =
    val keys = input.indices.map(i => s"sample:$i").toVector
    val axis = right(AxisRef.fromStableKeys("samples", SpaceRole.Samples, keys, "trial", "none", "one"))
    val response = right(AxisRef.fromStableKeys("response", SpaceRole.Observed, Vector("class"), "class", "none", "code"))
    val mapping = right(NativeAxisMapping.fromAxis(axis, keys.indices.map(i => 50L + i).toVector, DataFingerprint.external("correlation-centroid-suite")))
    right(AlderPredictiveAdmission.materialized(axis.descriptor, DMat.dense(input.length, input.head.length, input.flatten),
      DMat.dense(input.length, 1, labels), keys, mapping, right(MaterializationBudget(1000L))))

  private def design(rows: AlderMaterializedRows[String]): ValidationDesign[?, ? , resample4s.core.Coverage.ExactOnce] =
    val axis = right(AxisRef.fromStableKeys("samples", SpaceRole.Samples, rows.mapping.entriesByOrdinal.map(_.stableKey), "trial", "none", "one"))
    right(ValidationDesign.bind(axis, right(FixedPartitions.once(right(Labels.retained(IArray.unsafeFromArray(blocks.clone()))))), ScientificSeed.fromLong(17L)))

  test("correlation centroid uses the Alder fit lifecycle and preserves row-offset invariance distinct from Swift"):
    val baselineRows = rows(values)
    val baseline = right(AlderCorrelationCentroid.crossValidate(baselineRows, design(baselineRows), coding))
    val shiftedValues = values.zipWithIndex.map((row, index) => row.map(_ + (index - 2) * 11.0))
    val shiftedRows = rows(shiftedValues)
    val shifted = right(AlderCorrelationCentroid.crossValidate(shiftedRows, design(shiftedRows), coding))
    (0 until baseline.probabilities.rows).foreach: row =>
      baseline.classes.indices.foreach: column =>
        assertEqualsDouble(shifted.probabilities(row, column), baseline.probabilities(row, column), 1e-12)
    assertEquals(baseline.fits.map(_.audit.component.id.render), Vector.fill(3)("scalafim.correlation-centroid"))
    val swift = right(AlderSwiftCentroid.crossValidate(baselineRows, design(baselineRows), coding))
    assert((0 until baseline.probabilities.rows).exists(row => math.abs(baseline.probabilities(row, 0) - swift.probabilities(row, 0)) > 1e-8))


  test("repeated exact assessments average correlation probabilities by stable row"):
    val materialized = rows(values)
    val axis = right(AxisRef.fromStableKeys("samples", SpaceRole.Samples, materialized.mapping.entriesByOrdinal.map(_.stableKey), "trial", "none", "one"))
    val first = right(Labels.retained(IArray.unsafeFromArray(blocks.clone())))
    val second = right(Labels.retained(IArray(0, 1, 2, 0, 1, 2)))
    val onceSecond = right(ValidationDesign.bind(axis, right(FixedPartitions.once(second)), ScientificSeed.fromLong(17L)))
    val repeated = right(ValidationDesign.bind(axis, right(FixedPartitions.repeated(IArray(first, second))), ScientificSeed.fromLong(17L)))
    val a = right(AlderCorrelationCentroid.crossValidate(materialized, design(materialized), coding))
    val b = right(AlderCorrelationCentroid.crossValidate(materialized, onceSecond, coding))
    val combined = right(AlderCorrelationCentroid.crossValidate(materialized, repeated, coding))
    assertEquals(combined.rows.map(_.stableKey), materialized.mapping.entriesByOrdinal.map(_.stableKey))
    assertEquals(combined.fits.length, a.fits.length + b.fits.length)
    assert((0 until a.probabilities.rows).exists(row => math.abs(a.probabilities(row, 0) - b.probabilities(row, 0)) > 1e-8))
    combined.rows.foreach: row =>
      assertEquals(row.assessments.map(_.unit.repeat), Vector(0, 1))
      assertEquals(row.assessments.length, 2)
      row.assessments.foreach: contribution =>
        assert(!contribution.trainingStableKeys.contains(row.stableKey))
        assertEquals(contribution.trainingStableKeys, combined.fits.find(_.unit == contribution.unit).get.trainingStableKeys)
    combined.rows.indices.foreach: row =>
      coding.classes.indices.foreach: column =>
        assertEqualsDouble(combined.probabilities(row, column), (a.probabilities(row, column) + b.probabilities(row, column)) / 2.0, 1e-12)

  test("native correlation refuses a one-feature measurement"):
    val materialized = rows(values.map(_.take(1)))
    assert(AlderCorrelationCentroid.crossValidate(materialized, design(materialized), coding).isLeft)
