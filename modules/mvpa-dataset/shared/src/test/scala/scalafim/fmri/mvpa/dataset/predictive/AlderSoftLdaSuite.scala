package scalafim.fmri.mvpa.dataset.predictive

import alder.kernel.DataFingerprint
import gale.linalg.{DMat, DVec, DoubleLinearOperator, Matrix, MutableDVec}
import multivar.core.SpaceRole
import multivar.family.canonical.{TraceRidgeFraction, TrialNuisanceDesign, WithinScatterPolicy}
import resample4s.core.{DigestAlgorithm, Labels}
import resample4s.designs.FixedPartitions
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.fit.*

class AlderSoftLdaSuite extends munit.FunSuite:
  given DigestAlgorithm = DigestAlgorithm.fnv1a64
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)

  private val values = Vector(
    Vector(2.2, 0.1, 0.2), Vector(0.0, 2.0, -0.2), Vector(-2.0, -1.8, 0.1),
    Vector(2.0, -0.1, 0.0), Vector(0.2, 2.2, 0.1), Vector(-2.2, -2.0, -0.1),
    Vector(2.3, 0.2, -0.1), Vector(-0.1, 1.9, 0.2), Vector(-1.9, -2.1, 0.0),
    Vector(1.9, 0.0, 0.1), Vector(0.1, 2.1, -0.1), Vector(-2.1, -1.9, 0.2)
  )
  private val targets = Vector(0.0, 1.0, 2.0, 0.0, 1.0, 2.0, 0.0, 1.0, 2.0, 0.0, 1.0, 2.0)
  private val coding = right(SwiftTargetCoding(Vector(0.0 -> "a", 1.0 -> "b", 2.0 -> "c")))
  private val config = SoftLdaConfig(WithinScatterPolicy.FixedTraceScaledRidge(TraceRidgeFraction.unsafe(0.05)))

  private final class PoisonReorderedOperator extends DoubleLinearOperator:
    val rows = 1
    val cols = 3
    var forwardCalls = 0
    def applyTo(input: DVec, output: MutableDVec): Unit =
      forwardCalls += 1
      throw IllegalStateException("reordered feature operator must not be read")

  private def fixture(
      input: Vector[Vector[Double]] = values,
      response: Vector[Double] = targets,
      nuisance: Option[TrialNuisanceDesign] = None
  ): (AlderSoftLdaRows, ValidationDesign[?, ?, resample4s.core.Coverage.ExactOnce], PatternOperator) =
    val keys = input.indices.map(index => s"sample:$index").toVector
    val axis = right(AxisRef.fromStableKeys("soft-lda-samples", SpaceRole.Samples, keys, "trial", "none", "one"))
    val mapping = right(NativeAxisMapping.fromAxis(axis, keys.indices.map(index => 1100L + index).toVector, DataFingerprint.external("alder-soft-lda-suite")))
    val matrix = PatternMatrix.fromRows(input)
    val operator = right(PatternOperator.fromMatrix(matrix))
    val rows = right(AlderSoftLda.admit(operator, response, mapping, right(MaterializationBudget(256L)), nuisance))
    val design = right(ValidationDesign.bind(axis, right(FixedPartitions.once(right(Labels.retained(IArray.unsafeFromArray(Array(0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3)))))), ScientificSeed.fromLong(29L)))
    (rows, design, operator)

  test("Alder soft LDA matches frozen legacy fold-local probabilities"):
    val (rows, design, operator) = fixture()
    val alder = right(AlderSoftLda.crossValidate(rows, design, coding, config))
    val membership = right(ClassMembership.hard(targets.map(value => coding.label(value).get)))
    val legacy = right(SoftLda.crossValidate(operator, membership, right(FoldPlan.leaveOneBlockOut(Vector(0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3))), config))
    assertMatrixClose(alder.prediction.probabilities, legacy.prediction.probabilities)
    assertEqualsDouble(alder.targetMse, legacy.targetMse, 1e-12)
    assertEqualsDouble(alder.targetArgmaxAccuracy, legacy.targetArgmaxAccuracy, 1e-12)
    assertEquals(alder.fits.map(_.audit.component.id.render), Vector.fill(4)("scalafim.soft-lda"))

  test("held-out features, targets, and nuisance do not alter the corresponding fitted fold"):
    val nuisance = right(TrialNuisanceDesign.from(Matrix.tabulate(values.length, 1)((row, _) => row.toDouble / 3.0)))
    val (baselineRows, design, _) = fixture(nuisance = Some(nuisance))
    val changedValues = values.updated(0, Vector(200.0, -100.0, 50.0)).updated(1, Vector(-200.0, 100.0, -50.0)).updated(2, Vector(150.0, 80.0, -30.0))
    val changedTargets = targets.updated(0, 2.0).updated(1, 0.0).updated(2, 1.0)
    val changedNuisance = right(TrialNuisanceDesign.from(Matrix.tabulate(values.length, 1)((row, _) => if row < 3 then 1000.0 + row else row.toDouble / 3.0)))
    val (changedRows, changedDesign, _) = fixture(changedValues, changedTargets, Some(changedNuisance))
    val baseline = right(AlderSoftLda.crossValidate(baselineRows, design, coding, config))
    val changed = right(AlderSoftLda.crossValidate(changedRows, changedDesign, coding, config))
    val baselineWeights = right(baseline.fits.find(_.unit.fold == 0).get.model.fit.functionalFrame.weights.toDense)
    val changedWeights = right(changed.fits.find(_.unit.fold == 0).get.model.fit.functionalFrame.weights.toDense)
    assertMatrixClose(baselineWeights, changedWeights)

  test("simplex targets remain soft through Alder fit roles"):
    val (_, design, operator) = fixture()
    val membership = right(ClassMembership.simplex(coding.classes, Matrix.tabulate(values.length, coding.classes.length): (row, column) =>
      val primary = targets(row).toInt
      if column == primary then 0.70 else 0.15
    ))
    val keys = values.indices.map(index => s"sample:$index").toVector
    val axis = right(AxisRef.fromStableKeys("soft-lda-samples", SpaceRole.Samples, keys, "trial", "none", "one"))
    val mapping = right(NativeAxisMapping.fromAxis(axis, keys.indices.map(index => 1100L + index).toVector, DataFingerprint.external("alder-soft-lda-suite")))
    val rows = right(AlderSoftLda.admitSoft(operator, membership, mapping, right(MaterializationBudget(256L))))
    val result = right(AlderSoftLda.crossValidate(rows, design, coding, config))
    assert(result.targetMse >= 0.0)
    assert(result.fits.forall(_.model.targetKind == ClassMembershipKind.Simplex))

  test("single-fit soft LDA rejects reordered feature axes before reading the operator"):
    val (_, _, operator) = fixture()
    val membership = right(ClassMembership.hard(targets.map(value => coding.label(value).get)))
    val model = right(SoftLda.fit(operator, membership, config))
    val poison = new PoisonReorderedOperator
    val reordered = right(PatternOperator.fromOperator(
      SampleAxis.unsafe(1),
      Vector(FeatureIndex(2), FeatureIndex(1), FeatureIndex(0)),
      poison,
      PatternOperatorProvenance.composed
    ))
    model.predict(reordered) match
      case Left(SoftLdaError.FeatureAxisMismatch(expected, actual)) =>
        assertEquals(expected, operator.featureIndices)
        assertEquals(actual, reordered.featureIndices)
      case other => fail(s"expected feature-axis rejection, got $other")
    assertEquals(poison.forwardCalls, 0)

  private def assertMatrixClose(actual: DMat, expected: DMat, tolerance: Double = 1e-10): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1
