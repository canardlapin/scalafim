package scalafim.fmri.design

import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.linalg.Mat

class WeightedContrastDiagnosticsSuite extends munit.FunSuite:
  private def schema(task: Vector[Double]): DesignSchema =
    val rows = RowLayout(Vector.fill(task.size)(RunIndex.unsafeOneBased(1)), task.indices.map(i => Seconds(i.toDouble)).toVector, task.indices.map(i => ScanIndex.unsafeOneBased(i + 1)).toVector)
    val columns = Vector("task", "intercept").zipWithIndex.map: (name, index) =>
      StructuralColumn.fromOrigin(index + 1, StructuralColumnOrigin.Nuisance(TermId.unsafe("example"), ModulatorId.unsafe(name), RunScope.Global), name).toOption.get
    DesignSchema.validated(Mat.unsafe(task.size, 2, task.flatMap(value => Vector(value, 1.0)).toArray), rows, columns).toOption.get

  test("known unequal run sigmas give inverse-variance weighting for t and F"):
    val design = schema(Vector(-0.5, 0.5, -0.5, 0.5)) // sum(task^2)=1, orthogonal to intercept
    val contrast = ColumnContrast(Vector(design.columnIds.head -> 1.0))
    val f = FColumnContrast(Vector(contrast))
    val run = ContrastDiagnostics.analyze(design, Vector(contrast), Vector(f)).toOption.get
    Vector(RunContrastCombination.FixedEffects, RunContrastCombination.Concatenated(Vector(design.columnIds.head))).foreach: policy =>
      val t = WeightedContrastDiagnostics.t(Vector(run, run), contrast, Vector(1.0, 2.0), policy).toOption.get
      assertEqualsDouble(t.variance.get, 0.8, 1e-12) // 1 / (1/1 + 1/4)
      assertEquals(t.contributingRuns, Vector(RunIndex.unsafeOneBased(1), RunIndex.unsafeOneBased(2)))
      val result = WeightedContrastDiagnostics.f(Vector(run, run), f, Vector(1.0, 2.0), policy).toOption.get
      assertEqualsDouble(result.worstDirectionSe.get, math.sqrt(0.8), 1e-12)

  test("empty task run is reported and never supplies invented information"):
    val first = schema(Vector(-0.5, 0.5, -0.5, 0.5))
    val empty = schema(Vector.fill(4)(0.0))
    val contrast = ColumnContrast(Vector(first.columnIds.head -> 1.0))
    val runs = Vector(first, empty).map(ContrastDiagnostics.analyze(_, Vector(contrast), Vector.empty).toOption.get)
    val result = WeightedContrastDiagnostics.t(runs, contrast, Vector(2.0, 3.0), RunContrastCombination.FixedEffects).toOption.get
    assertEquals(result.contributingRuns, Vector(RunIndex.unsafeOneBased(1)))
    assertEquals(result.excludedRuns, Vector(RunIndex.unsafeOneBased(2)))
    assertEqualsDouble(result.variance.get, 4.0, 1e-12)
    assertEquals(
      WeightedContrastDiagnostics.t(runs, contrast, Vector(1.0), RunContrastCombination.FixedEffects),
      Left(DesignDiagnosticsError.ResidualSigmaCountMismatch(2, 1))
    )
    assertEquals(
      WeightedContrastDiagnostics.t(runs, contrast, Vector(1.0, 0.0), RunContrastCombination.FixedEffects),
      Left(DesignDiagnosticsError.InvalidResidualSigma(RunIndex.unsafeOneBased(2), 0.0))
    )

  test("a non-default rank policy is honoured by weighted combinations"):
    // The second column differs from the task by 1e-7: a coarse policy treats
    // the pair as one direction, so the task alone is no longer estimable.
    val rows = RowLayout(Vector.fill(4)(RunIndex.unsafeOneBased(1)), (0 until 4).map(i => Seconds(i.toDouble)).toVector, (1 to 4).map(ScanIndex.unsafeOneBased).toVector)
    val columns = Vector("task", "near").zipWithIndex.map: (name, index) =>
      StructuralColumn.fromOrigin(index + 1, StructuralColumnOrigin.Nuisance(TermId.unsafe("example"), ModulatorId.unsafe(name), RunScope.Global), name).toOption.get
    val design = DesignSchema.validated(Mat.unsafe(4, 2, Array(1.0, 1.0 + 1e-7, -1.0, -1.0 - 1e-7, 1.0, 1.0 - 1e-7, -1.0, -1.0 + 1e-7)), rows, columns).toOption.get
    val contrast = ColumnContrast(Vector(design.columnIds.head -> 1.0))
    val coarse = RankPolicy.relative(1e-5).toOption.get
    val runs = Vector.fill(2)(ContrastDiagnostics.analyze(design, Vector(contrast), Vector.empty, rankPolicy = coarse).toOption.get)
    val result = WeightedContrastDiagnostics.t(runs, contrast, Vector(1.0, 2.0), RunContrastCombination.FixedEffects).toOption.get
    assertEquals(result.variance, None)
    assertEquals(result.excludedRuns, Vector(RunIndex.unsafeOneBased(1), RunIndex.unsafeOneBased(2)))
    val defaultRuns = Vector.fill(2)(ContrastDiagnostics.analyze(design, Vector(contrast), Vector.empty).toOption.get)
    val estimable = WeightedContrastDiagnostics.t(defaultRuns, contrast, Vector(1.0, 2.0), RunContrastCombination.FixedEffects).toOption.get
    assertEquals(estimable.contributingRuns, Vector(RunIndex.unsafeOneBased(1), RunIndex.unsafeOneBased(2)))
    assertEquals(
      WeightedContrastDiagnostics.t(Vector(runs.head, defaultRuns.head), contrast, Vector(1.0, 2.0), RunContrastCombination.FixedEffects),
      Left(DesignDiagnosticsError.RankPolicyMismatch(Vector(coarse.label, RankPolicy.Default.label)))
    )
