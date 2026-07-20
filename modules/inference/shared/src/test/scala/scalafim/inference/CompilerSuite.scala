package scalafim.inference

import scala.compiletime.testing.typeCheckErrors

class CompilerSuite extends munit.FunSuite:

  private def accepted[A](value: Either[InferenceError, A]): A =
    value.fold(error => fail(error.message), identity)

  private val rows = accepted(RowCount(20))
  private val fixed = MonteCarloPolicy.Fixed(accepted(MonteCarloDraws(99)))
  private val design = ResamplingDesign.exchangeableRows(rows)

  test("PCA variance-root inference compiles to an inspectable typed program") {
    val spec = InferenceSpec.of(
      FitDescriptor.Pca,
      TargetSpec.VarianceRoots,
      NullSpec.PermuteRows,
      design,
      UnitPolicy.SingleAxes,
      fixed,
      RequestedEvidence.SignificanceOnly,
      RootSeed(1729L)
    )
    val program = InferenceCompiler.compile(spec)

    assertEquals(program.summary.fit, "pca")
    assertEquals(program.summary.target.value, "ordered-variance-roots")
    assertEquals(program.summary.nullHypothesis.value, "row-permutation")
    assertEquals(program.summary.rows.value, 20)
    assertEquals(program.validity, ValidityClaim.Exact)
  }

  test("PLSC covariance and CCA correlation targets remain distinct") {
    val plsc = InferenceCompiler.compile(InferenceSpec.of(
      FitDescriptor.Plsc,
      TargetSpec.CovarianceRoots,
      NullSpec.BreakY,
      design,
      UnitPolicy.SingleAxes,
      fixed,
      RequestedEvidence.SignificanceAndStability,
      RootSeed(1L)
    ))
    val cca = InferenceCompiler.compile(InferenceSpec.of(
      FitDescriptor.Cca,
      TargetSpec.CanonicalCorrelations,
      NullSpec.BreakX,
      design,
      UnitPolicy.SingleAxes,
      fixed,
      RequestedEvidence.SignificanceOnly,
      RootSeed(2L)
    ))

    assertEquals(plsc.summary.target.value, "ordered-covariance-roots")
    assertEquals(cca.summary.target.value, "ordered-canonical-correlations")
    assertNotEquals(plsc.summary.target, cca.summary.target)
  }

  test("unsupported reduced-rank-regression target combinations do not typecheck") {
    val errors = typeCheckErrors("""
      import scalafim.inference.*
      val rows = RowCount(20).toOption.get
      val spec = InferenceSpec.of(
        FitDescriptor.ReducedRankRegression,
        TargetSpec.CovarianceRoots,
        NullSpec.BreakY,
        ResamplingDesign.exchangeableRows(rows),
        UnitPolicy.SingleAxes,
        MonteCarloPolicy.Fixed(MonteCarloDraws(99).toOption.get),
        RequestedEvidence.SignificanceOnly,
        RootSeed(1L)
      )
      InferenceCompiler.compile(spec)
    """)

    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("SupportsTarget")))
  }

  test("a null incompatible with the target does not typecheck") {
    val errors = typeCheckErrors("""
      import scalafim.inference.*
      val rows = RowCount(20).toOption.get
      val spec = InferenceSpec.of(
        FitDescriptor.Pca,
        TargetSpec.VarianceRoots,
        NullSpec.BreakY,
        ResamplingDesign.exchangeableRows(rows),
        UnitPolicy.SingleAxes,
        MonteCarloPolicy.Fixed(MonteCarloDraws(99).toOption.get),
        RequestedEvidence.SignificanceOnly,
        RootSeed(1L)
      )
      InferenceCompiler.compile(spec)
    """)

    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("SupportsNull")))
  }

  test("a design without an implemented validity rule does not typecheck") {
    val errors = typeCheckErrors("""
      import scalafim.inference.*
      val rows = RowCount(4).toOption.get
      val blocks = RowPartition.from(rows, Vector(Vector(0, 1), Vector(2, 3))).toOption.get
      trait CustomNull extends NullKind
      val customNull = new NullSpec[CustomNull]:
        val label = NullLabel("custom-null").toOption.get
      given SupportsNull[TargetKind.VarianceRoots, CustomNull] with
        val validity = ValidityClaim.Exact
      val spec = InferenceSpec.of(
        FitDescriptor.Pca,
        TargetSpec.VarianceRoots,
        customNull,
        ResamplingDesign.withinBlocks(blocks),
        UnitPolicy.SingleAxes,
        MonteCarloPolicy.Fixed(MonteCarloDraws(99).toOption.get),
        RequestedEvidence.SignificanceOnly,
        RootSeed(1L)
      )
      InferenceCompiler.compile(spec)
    """)

    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("SupportsDesign")))
  }

  test("structured and conditioned designs compile with explicit validity") {
    val blockRows = accepted(RowCount(4))
    val blocks = accepted(RowPartition.from(blockRows, Vector(Vector(0, 1), Vector(2, 3))))
    val reference = accepted(ConditioningRef("nuisance-v1"))

    val structured = InferenceCompiler.compile(InferenceSpec.of(
      FitDescriptor.Pca,
      TargetSpec.VarianceRoots,
      NullSpec.PermuteRows,
      ResamplingDesign.withinBlocks(blocks),
      UnitPolicy.SingleAxes,
      fixed,
      RequestedEvidence.SignificanceOnly,
      RootSeed(3L)
    ))
    val conditioned = InferenceCompiler.compile(InferenceSpec.of(
      FitDescriptor.Pca,
      TargetSpec.VarianceRoots,
      NullSpec.PermuteRows,
      ResamplingDesign.nuisanceAdjustedWithinBlocks(
        blocks,
        reference,
        WhiteningRequirement.Required
      ),
      UnitPolicy.SingleAxes,
      fixed,
      RequestedEvidence.SignificanceOnly,
      RootSeed(4L)
    ))

    assertEquals(structured.validity, ValidityClaim.Exact)
    assertEquals(conditioned.validity, ValidityClaim.Conditional)
  }

  test("dynamic within-block compilation is supported for proved protocols") {
    val blockRows = accepted(RowCount(4))
    val blocks = accepted(RowPartition.from(blockRows, Vector(Vector(0, 1), Vector(2, 3))))
    val spec = DynamicInferenceSpec(
      BuiltInFit.Plsc,
      BuiltInTarget.CovarianceRoots,
      BuiltInNull.BreakY,
      DynamicDesign.WithinBlocks(blocks),
      UnitPolicy.SingleAxes,
      fixed,
      RequestedEvidence.SignificanceOnly,
      RootSeed(5L)
    )

    InferenceCompiler.compileDynamic(spec) match
      case Right(_: AnyInferenceProgram.PlscBlocks) => ()
      case other => fail(s"expected a within-block PLSC program, got $other")
  }

  test("dynamic compilation rejects rather than constructing unsupported programs") {
    val unsupported = DynamicInferenceSpec(
      BuiltInFit.ReducedRankRegression,
      BuiltInTarget.CovarianceRoots,
      BuiltInNull.BreakY,
      DynamicDesign.ExchangeableRows(rows),
      UnitPolicy.SingleAxes,
      fixed,
      RequestedEvidence.SignificanceOnly,
      RootSeed(1L)
    )

    InferenceCompiler.compileDynamic(unsupported) match
      case Left(_: InferenceError.UnsupportedProblem) => ()
      case other                                      => fail(s"expected unsupported problem, got $other")
  }

  test("downstream protocols extend the compiler through typed evidence") {
    final class CustomFit
    trait CustomTargetKind extends TargetKind
    trait CustomNullKind extends NullKind

    val customFit = new FitDescriptor[CustomFit]:
      override val label: String = "custom-fit"
    val customTarget = new TargetSpec[CustomTargetKind]:
      override val label: TargetLabel = accepted(TargetLabel("custom-target"))
      override val alternative: Alternative = Alternative.Greater
      override val invariance: TargetInvariance = TargetInvariance.AxisOrientation
    val customNull = new NullSpec[CustomNullKind]:
      override val label: NullLabel = accepted(NullLabel("custom-null"))

    given SupportsTarget[CustomFit, CustomTargetKind] with {}
    given SupportsNull[CustomTargetKind, CustomNullKind] with
      override val validity: ValidityClaim = ValidityClaim.Heuristic
    given SupportsDesign[CustomNullKind, DesignKind.ExchangeableRows] with {}

    val program = InferenceCompiler.compile(InferenceSpec.of(
      customFit,
      customTarget,
      customNull,
      design,
      UnitPolicy.SingleAxes,
      fixed,
      RequestedEvidence.SignificanceOnly,
      RootSeed(11L)
    ))

    assertEquals(program.summary.fit, "custom-fit")
    assertEquals(program.validity, ValidityClaim.Heuristic)
  }

  test("an inference specification is data-free and serializable in shape") {
    val spec = InferenceSpec.of(
      FitDescriptor.Pca,
      TargetSpec.VarianceRoots,
      NullSpec.PermuteRows,
      design,
      UnitPolicy.SingleAxes,
      fixed,
      RequestedEvidence.SignificanceOnly,
      RootSeed(1729L)
    )

    def containsRuntimeHandle(value: Any): Boolean =
      value match
        case _: Function0[?]    => true
        case _: Function1[?, ?] => true
        case product: Product   => product.productIterator.exists(containsRuntimeHandle)
        case iterable: Iterable[?] => iterable.exists(containsRuntimeHandle)
        case _ => false

    assert(!containsRuntimeHandle(spec))
    assertEquals(spec.productArity, 8)
  }
