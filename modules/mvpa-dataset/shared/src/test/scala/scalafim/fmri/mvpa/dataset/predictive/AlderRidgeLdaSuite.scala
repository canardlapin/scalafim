package scalafim.fmri.mvpa.dataset.predictive

import alder.kernel.DataFingerprint
import gale.linalg.DMat
import multivar.core.SpaceRole
import resample4s.core.{DigestAlgorithm, Labels}
import resample4s.designs.FixedPartitions
import scalafim.fmri.mvpa.*

class AlderRidgeLdaSuite extends munit.FunSuite:
  given DigestAlgorithm = DigestAlgorithm.fnv1a64
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)

  test("ridge LDA uses the Alder OOF lifecycle and exposes dense named results"):
    val values = Vector(Vector(2.0, 1.0), Vector(-2.0, -1.0), Vector(-1.0, -2.0), Vector(1.0, 2.0), Vector(2.5, 0.5), Vector(-2.5, -0.5))
    val labels = Vector(0.0, 1.0, 1.0, 0.0, 0.0, 1.0)
    val keys = values.indices.map(i => s"sample:$i").toVector
    val axis = right(AxisRef.fromStableKeys("samples", SpaceRole.Samples, keys, "trial", "none", "one"))
    val mapping = right(NativeAxisMapping.fromAxis(axis, keys.indices.map(i => 500L + i).toVector, DataFingerprint.external("ridge-lda-suite")))
    val materialized = right(AlderPredictiveAdmission.materialized(axis.descriptor, DMat.dense(values.length, 2, values.flatten), DMat.dense(labels.length, 1, labels), keys, mapping, right(MaterializationBudget(1000L))))
    val design = right(ValidationDesign.bind(axis, right(FixedPartitions.once(right(Labels.retained(IArray.unsafeFromArray(Array(0, 0, 1, 1, 2, 2)))))), ScientificSeed.fromLong(17L)))
    val coding = right(SwiftTargetCoding(Vector(0.0 -> "left", 1.0 -> "right")))
    val result = right(AlderRidgeLda.crossValidate(materialized, design, right(RidgePenalty(0.5)), coding))
    assertEquals(result.probabilities.rows, values.length)
    assertEquals(result.probabilities.cols, 2)
    assertEquals(result.fits.map(_.audit.component.id.render), Vector.fill(3)("scalafim.ridge-lda"))
    (0 until result.probabilities.rows).foreach(row => assertEqualsDouble(result.probabilities(row, 0) + result.probabilities(row, 1), 1.0, 1e-12))
    result.fits.foreach: fit =>
      val unit = right(design.at(fit.unit))
      val train = unit.analysis.ordinals.toVector
      val test = unit.assessment.ordinals.toVector
      val trainMatrix = DMat.dense(train.length, 2, train.flatMap(values))
      val model = right(RidgeLdaClassifier(0.5).fit(trainMatrix, train.map(index => coding.label(labels(index)).get)))
      val prediction = right(model.predict(DMat.dense(test.length, 2, test.flatMap(values))))
      val expected = right(Classification.reorderProbabilities(prediction.classes, prediction.probabilities, coding.classes))
      test.zipWithIndex.foreach: (ordinal, local) =>
        coding.classes.indices.foreach: column =>
          assertEqualsDouble(result.probabilities(ordinal, column), expected(local, column), 1e-12)
    val other = right(AlderRidgeLda.crossValidate(materialized, design, right(RidgePenalty(0.75)), coding))
    assertNotEquals(result.fits.head.audit, other.fits.head.audit)
    val foreign = right(AxisRef.fromStableKeys("foreign-samples", SpaceRole.Samples, keys.reverse, "trial", "none", "one"))
    val foreignDesign = right(ValidationDesign.bind(foreign, right(FixedPartitions.once(right(Labels.retained(IArray(0, 0, 1, 1, 2, 2))))), ScientificSeed.fromLong(17L)))
    assert(AlderRidgeLda.crossValidate(materialized, foreignDesign, right(RidgePenalty(0.5)), coding).isLeft)


  test("ridge LDA averages repeated exact assessments through the shared native lifecycle"):
    val values = Vector(Vector(2.0, 1.0), Vector(-2.0, -1.0), Vector(-1.0, -2.0), Vector(1.0, 2.0), Vector(2.5, 0.5), Vector(-2.5, -0.5))
    val targets = Vector(0.0, 1.0, 1.0, 0.0, 0.0, 1.0)
    val keys = values.indices.map(i => s"repeat:$i").toVector
    val axis = right(AxisRef.fromStableKeys("samples", SpaceRole.Samples, keys, "trial", "none", "one"))
    val mapping = right(NativeAxisMapping.fromAxis(axis, keys.indices.map(i => 700L + i).toVector, DataFingerprint.external("ridge-lda-repeat")))
    val rows = right(AlderPredictiveAdmission.materialized(axis.descriptor, DMat.dense(6, 2, values.flatten), DMat.dense(6, 1, targets), keys, mapping, right(MaterializationBudget(1000L))))
    val first = right(Labels.retained(IArray(0, 0, 1, 1, 2, 2)))
    val second = right(Labels.retained(IArray(0, 1, 2, 0, 1, 2)))
    val onceFirst = right(ValidationDesign.bind(axis, right(FixedPartitions.once(first)), ScientificSeed.fromLong(17L)))
    val onceSecond = right(ValidationDesign.bind(axis, right(FixedPartitions.once(second)), ScientificSeed.fromLong(17L)))
    val repeated = right(ValidationDesign.bind(axis, right(FixedPartitions.repeated(IArray(first, second))), ScientificSeed.fromLong(17L)))
    val coding = right(SwiftTargetCoding(Vector(0.0 -> "left", 1.0 -> "right")))
    val penalty = right(RidgePenalty(0.5))
    val a = right(AlderRidgeLda.crossValidate(rows, onceFirst, penalty, coding))
    val b = right(AlderRidgeLda.crossValidate(rows, onceSecond, penalty, coding))
    val combined = right(AlderRidgeLda.crossValidate(rows, repeated, penalty, coding))
    assertEquals(combined.rows.map(_.stableKey), keys)
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
