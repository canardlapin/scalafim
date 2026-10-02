package scalafim.fmri.mvpa.fit

import gale.linalg.{DMat, Matrix}
import multivar.family.canonical.{LdaObjective, TraceRidgeFraction, TrialNuisanceDesign, WithinScatterPolicy}
import scalafim.fmri.mvpa.*

class SoftLdaSuite extends munit.FunSuite:
  private def config: SoftLdaConfig = SoftLdaConfig(WithinScatterPolicy.FixedTraceScaledRidge(TraceRidgeFraction.unsafe(0.05)))
  private def labels = Vector("a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c").map(ClassLabel.apply)
  private def matrix = DMat.dense(12, 3, Vector(
    2.2,0.1,0.2, 0.0,2.0,-0.2, -2.0,-1.8,0.1,
    2.0,-0.1,0.0, 0.2,2.2,0.1, -2.2,-2.0,-0.1,
    2.3,0.2,-0.1, -0.1,1.9,0.2, -1.9,-2.1,0.0,
    1.9,0.0,0.1, 0.1,2.1,-0.1, -2.1,-1.9,0.2
  ))
  private def membership = ClassMembership.hard(labels).toOption.get
  private def operator = PatternOperator.fromMatrix(PatternMatrix(matrix, Vector.tabulate(12)(SampleIndex.apply), Vector.tabulate(3)(FeatureIndex.apply))).toOption.get

  test("single-fit soft LDA agrees for composed and explicit pattern operators") {
    val composed = PatternOperator.fromOperator(SampleAxis.unsafe(12), operator.featureIndices, operator.linear, PatternOperatorProvenance.composed).toOption.get
    val left = SoftLda.fit(composed, membership, config).toOption.get.predict(composed).toOption.get
    val right = SoftLda.fit(operator, membership, config).toOption.get.predict(operator).toOption.get
    assertMatrixClose(left.probabilities, right.probabilities)
    assertEquals(left.predicted, right.predicted)
  }

  test("single-fit soft LDA retains simplex target roles and nuisance/components policy") {
    val soft = ClassMembership.simplex(membership.classes, Matrix.tabulate(12, 3): (row, column) =>
      if labels(row) == membership.classes(column) then 0.70 else 0.15
    ).toOption.get
    val nuisance = TrialNuisanceDesign.from(Matrix.tabulate(12, 1)((row, _) => row.toDouble / 3.0)).toOption.get
    val fitted = SoftLda.fit(operator, soft, config.copy(objective = LdaObjective.TraceRatio, components = SoftLdaComponents.Fixed(multivar.core.ComponentCount.unsafe(1)), trialNuisance = Some(nuisance))).toOption.get
    val prediction = fitted.predict(operator).toOption.get
    assertEquals(fitted.targetKind, ClassMembershipKind.Simplex)
    assertEquals(fitted.trialNuisanceColumns, 1)
    assertEquals(fitted.fit.programFit.program.objective.label, "trace-ratio")
    assertEquals(prediction.probabilities.rows, 12)
  }

  test("single-fit soft LDA rejects feature mismatch before reading a reordered source") {
    val model = SoftLda.fit(operator, membership, config).toOption.get
    val wrong = PatternOperator.fromMatrix(PatternMatrix(DMat.dense(1, 2, Vector(1.0, 2.0)), Vector(SampleIndex(0)), Vector(FeatureIndex(1), FeatureIndex(0)))).toOption.get
    assert(model.predict(wrong).left.toOption.exists(_.isInstanceOf[SoftLdaError.FeatureAxisMismatch]))
  }

  private def assertMatrixClose(left: DMat, right: DMat): Unit =
    var row = 0
    while row < left.rows do
      var column = 0
      while column < left.cols do
        assertEqualsDouble(left(row, column), right(row, column), 1e-9)
        column += 1
      row += 1
