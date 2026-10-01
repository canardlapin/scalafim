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
