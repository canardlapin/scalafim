package scalafim.fmri.mvpa

class EstimandExtensibilitySuite extends munit.FunSuite:

  test("a downstream estimand preserves its own result and error types"):
    val specification = ScientificPlanFixtures.specification()

    val result: specification.Result = ToyEstimate(0.75)
    val rejection: specification.Rejection = ToyRejection.Degenerate
    val failure: specification.Failure = ToyFailure.Numerical

    assertEquals(result, ToyEstimate(0.75))
    assertEquals(rejection, ToyRejection.Degenerate)
    assertEquals(failure, ToyFailure.Numerical)
    assertEquals(
      specification.boundaries.values.map(_.id.value),
      Vector("estimate")
    )

  test("compatible source and design types construct without a registry"):
    val errors = compileErrors("""
      import scalafim.fmri.mvpa.*

      trait MySource extends ScientificSource
      trait MyDesign extends EvidenceDesign
      trait MyEstimand extends Estimand[MySource, MyDesign]

      def valid(
          source: MySource,
          design: MyDesign,
          frame: MeasurementFrame[source.Neural, source.NeuralKey, NoRendition.type],
          estimand: MyEstimand
      ) = ScientificSpecification(source)(design, frame, estimand)
    """)

    assertEquals(errors, "")

  test("an estimand cannot bind to an incompatible source"):
    val errors = compileErrors("""
      import scalafim.fmri.mvpa.*

      trait SourceA extends ScientificSource
      trait SourceB extends ScientificSource
      trait DesignA extends EvidenceDesign
      trait QuestionA extends Estimand[SourceA, DesignA]

      def invalid(
          source: SourceB,
          design: DesignA,
          frame: MeasurementFrame[source.Neural, source.NeuralKey, NoRendition.type],
          estimand: QuestionA
      ) = ScientificSpecification(source)(design, frame, estimand)
    """)

    assert(errors.nonEmpty)

  test("an estimand cannot bind to an incompatible design"):
    val errors = compileErrors("""
      import scalafim.fmri.mvpa.*

      trait SourceA extends ScientificSource
      trait DesignA extends EvidenceDesign
      trait DesignB extends EvidenceDesign
      trait QuestionA extends Estimand[SourceA, DesignA]

      def invalid(
          source: SourceA,
          design: DesignB,
          frame: MeasurementFrame[source.Neural, source.NeuralKey, NoRendition.type],
          estimand: QuestionA
      ) = ScientificSpecification(source)(design, frame, estimand)
    """)

    assert(errors.nonEmpty)
