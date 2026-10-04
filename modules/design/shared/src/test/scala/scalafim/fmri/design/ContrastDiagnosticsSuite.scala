package scalafim.fmri.design

import scalafim.fmri.design.fixtures.{ContrastDiagnosticsFixture, DurableContrastMatrixFixture}
import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.linalg.Mat

class ContrastDiagnosticsSuite extends munit.FunSuite:
  test("frozen gamble design loses eight residual degrees of freedom for eight independent spikes"):
    // Independent numpy.linalg.matrix_rank from the generated oracle fixture.
    val run = DurableContrastMatrixFixture.gamble.head
    val names = Vector.tabulate(run.columns)(i => s"original-$i")
    val before = schema(run.values, names)
    val augmented = Array.tabulate(run.rows * (run.columns + 8)): index =>
      val row = index / (run.columns + 8)
      val column = index % (run.columns + 8)
      if column < run.columns then run.values(row * run.columns + column)
      else if row == column - run.columns then 1.0 else 0.0
    val after = schema(augmented, names ++ Vector.tabulate(8)(i => s"spike-$i"))
    val original = ContrastDiagnostics.analyze(before, Vector.empty, Vector.empty).toOption.get
    val censored = ContrastDiagnostics.analyze(after, Vector.empty, Vector.empty).toOption.get
    assertEquals(original.rank, ContrastDiagnosticsFixture.DurableAudit.gambleRank)
    assertEquals(censored.rank, ContrastDiagnosticsFixture.DurableAudit.gambleRankWithEightSpikes)
    assertEquals(censored.rank - original.rank, 8)

  private def schema(values: Array[Double], columnNames: Vector[String]): DesignSchema =
    val n = values.length / columnNames.length
    val rows = RowLayout(Vector.fill(n)(RunIndex.unsafeOneBased(1)), (0 until n).toVector.map(i => Seconds(i.toDouble)), (1 to n).toVector.map(ScanIndex.unsafeOneBased))
    val columns = columnNames.zipWithIndex.map { (name, index) =>
      StructuralColumn.fromOrigin(index + 1, StructuralColumnOrigin.Nuisance(TermId.unsafe("fixture"), ModulatorId.unsafe(name), RunScope.Global), name).toOption.get
    }
    DesignSchema.validated(Mat.unsafe(n, columnNames.length, values), rows, columns).toOption.get

  test("duplicate columns retain estimable sums and reject their difference") {
    val schema = this.schema(ContrastDiagnosticsFixture.duplicateDesign, Vector("i", "u", "u2", "w"))
    val individual = ColumnContrast(Vector(schema.columnIds(1) -> 1.0))
    val tinyIndividual = ColumnContrast(Vector(schema.columnIds(1) -> 1e-14))
    val sum = ColumnContrast(Vector(schema.columnIds(1) -> 1.0, schema.columnIds(2) -> 1.0))
    val difference = ColumnContrast(Vector(schema.columnIds(1) -> 1.0, schema.columnIds(2) -> -1.0))
    val scaledSum = ColumnContrast(Vector(schema.columnIds(1) -> 1000.0, schema.columnIds(2) -> 1000.0))
    val result = ContrastDiagnostics.analyze(schema, Vector(individual, tinyIndividual, sum, difference, scaledSum), Vector.empty).toOption.get
    // A single duplicated column is half outside the row space; the
    // difference is wholly outside it.
    assertAliased(result.t.head.estimability, math.sqrt(0.5))
    assertAliased(result.t(1).estimability, math.sqrt(0.5))
    assertEquals(result.t(2).estimability, ContrastEstimability.Estimable)
    assertEqualsDouble(result.t(2).varianceOverSigmaSquared.get, 1.0, 1e-10)
    assertAliased(result.t(3).estimability, 1.0)
    assertEquals(result.t(4).estimability, ContrastEstimability.Estimable)
    assertEqualsDouble(result.t(4).varianceOverSigmaSquared.get, 1e6, 1e-4)
    assertEquals(result.aliases.length, 1)
  }

  test("F worst direction is invariant to rescaling, duplicate rows, reordering, and mixing") {
    val s = schema(Array(
      1, 1, 0, 1, 0, 1,
      1, 0, 1, 1, 1, 0,
      1, 1, 1, 1, -1, 0,
      1, 2, 1, 0, 1, 1,
      1, 1, 2, -1, 0, 1,
      1, 2, 2, 0, -1, -1
    ).map(_.toDouble), Vector("i", "a", "b"))
    val a = s.columnIds(1); val b = s.columnIds(2)
    val base = FColumnContrast(Vector(ColumnContrast(Vector(a -> 1.0, b -> 0.0)), ColumnContrast(Vector(a -> 0.0, b -> 1.0))))
    val changed = FColumnContrast(Vector(ColumnContrast(Vector(a -> 3.0, b -> 3.0)), ColumnContrast(Vector(a -> 6.0, b -> 6.0)), ColumnContrast(Vector(a -> 2.0, b -> -1.0))))
    val x = ContrastDiagnostics.analyze(s, Vector.empty, Vector(base)).toOption.get.f.head.worstDirectionSeOverSigma.get
    val y = ContrastDiagnostics.analyze(s, Vector.empty, Vector(changed)).toOption.get.f.head.worstDirectionSeOverSigma.get
    assertEqualsDouble(x, y, 1e-10)
    val extreme = FColumnContrast(Vector(ColumnContrast(Vector(a -> 1e200, b -> 0.0)), ColumnContrast(Vector(a -> 0.0, b -> 1e-200))))
    val z = ContrastDiagnostics.analyze(s, Vector.empty, Vector(extreme)).toOption.get.f.head.worstDirectionSeOverSigma.get
    assertEqualsDouble(x, z, 1e-10)
  }

  test("concatenated task information ignores a wholly nuisance-confounded run") {
    val one = schema(ContrastDiagnosticsFixture.singularTaskRun, Vector("task", "nuisance"))
    val two = schema(ContrastDiagnosticsFixture.informativeTaskRun, Vector("task", "nuisance"))
    val contrast = ColumnContrast(Vector(one.columnIds.head -> 1.0))
    val r1 = ContrastDiagnostics.analyze(one, Vector(contrast), Vector.empty).toOption.get
    val r2 = ContrastDiagnostics.analyze(two, Vector(contrast), Vector.empty).toOption.get
    val result = ContrastDiagnostics.concatenatedTaskT(Vector(r1, r2), Vector(one.columnIds.head), contrast).toOption.get
    assertEquals(result.contributingRuns, Vector(RunIndex.unsafeOneBased(2)))
    assertEquals(result.excludedRuns, Vector(RunIndex.unsafeOneBased(1)))
    assertEquals(result.runNuisance.map(_.nuisanceRank), Vector(1, 1))
    assertEqualsDouble(result.varianceOverSigmaSquared.get, 1.0, 1e-10)
  }

  test("preflight rejects a design with no residual degrees of freedom") {
    val saturated = schema(Array(1, 0, 0, 1).map(_.toDouble), Vector("a", "b"))
    assertEquals(ContrastDiagnostics.preflight(saturated), Left(DesignDiagnosticsError.InsufficientResidualDof(2, 2)))
    val duplicated = schema(Array(1, 1, 0, 0).map(_.toDouble), Vector("a", "a-copy"))
    assertEquals(ContrastDiagnostics.preflight(duplicated), Right(()))
  }

  test("preflight uses numerical rank while the column budget remains an explicit guard") {
    val duplicate = schema(Array(
      1, 1, 0,
      0, 0, 1,
      1, 1, 1
    ).map(_.toDouble), Vector("a", "a-copy", "b"))
    assertEquals(ContrastDiagnostics.preflight(duplicate), Right(()))
    assertEquals(ContrastDiagnostics.checkColumnBudget(duplicate, 2), Left(DesignDiagnosticsError.ColumnBudgetExceeded(3, 2)))
    assertEquals(ContrastDiagnostics.checkColumnBudget(duplicate, -1), Left(DesignDiagnosticsError.InvalidColumnBudget(-1)))
    assertEquals(ContrastDiagnostics.checkColumnBudget(duplicate, 3), Right(()))
  }

  test("high-pass comparison rejects a changed retained column") {
    val withCosine = schema(Array(
      1, 0, 1,
      0, 1, -1,
      1, 1, 1,
      0, 0, -1
    ).map(_.toDouble), Vector("task", "nuisance", "cosine"))
    val withoutCosine = schema(Array(
      1, 0,
      0, 1,
      1, 1,
      0, 0
    ).map(_.toDouble), Vector("task", "nuisance"))
    val changedRetained = schema(Array(
      1, 0,
      0, 1,
      2, 1,
      0, 0
    ).map(_.toDouble), Vector("task", "nuisance"))
    val contrast = ColumnContrast(Vector(withCosine.columnIds.head -> 1.0))
    val withResult = ContrastDiagnostics.analyze(withCosine, Vector(contrast), Vector.empty).toOption.getOrElse(fail("with-cosine diagnostic"))
    val withoutResult = ContrastDiagnostics.analyze(withoutCosine, Vector(contrast), Vector.empty).toOption.getOrElse(fail("without-cosine diagnostic"))
    val changedResult = ContrastDiagnostics.analyze(changedRetained, Vector(contrast), Vector.empty).toOption.getOrElse(fail("changed diagnostic"))
    assert(ContrastDiagnostics.highPassVarianceRatio(withResult, withoutResult, contrast, Vector(withCosine.columnIds(2))).isRight)
    assertEquals(
      ContrastDiagnostics.highPassVarianceRatio(withResult, changedResult, contrast, Vector(withCosine.columnIds(2))),
      Left(DesignDiagnosticsError.HighPassRetainedColumnDiffers(withCosine.columnIds.head))
    )
  }

  test("exact m2b raw matrices reproduce the independently recomputed row-orthonormalized F efficiencies") {
    val cases = Vector(
      DurableContrastMatrixFixture.block -> ContrastDiagnosticsFixture.DurableAudit.blockFixedEffectsFWorstSe,
      DurableContrastMatrixFixture.stop -> ContrastDiagnosticsFixture.DurableAudit.stopFixedEffectsFWorstSe,
      DurableContrastMatrixFixture.dense -> ContrastDiagnosticsFixture.DurableAudit.denseFixedEffectsFWorstSe
    )
    cases.foreach { (audit, expected) =>
      val runSchemas = audit.runs.map(auditSchema)
      val contrast = fContrast(runSchemas.head, audit.contrastRows)
      val diagnostics = runSchemas.map(value => ContrastDiagnostics.analyze(value, Vector.empty, Vector(contrast)).toOption.getOrElse(fail("raw audit F diagnostic")))
      val combined = ContrastDiagnostics.fixedEffectsF(diagnostics, contrast).toOption.getOrElse(fail("raw audit F fixed effects"))
      assertEquals(combined.contributingRuns, runSchemas.indices.toVector.map(index => RunIndex.unsafeOneBased(index + 1)))
      assertEqualsDouble(combined.worstDirectionSeOverSigma.getOrElse(fail("raw audit F value")), expected, OracleTolerance * expected)
    }
  }

  test("exact m2b DMS matrices reproduce fixed-effects probe-minus-sample efficiency") {
    val audit = DurableContrastMatrixFixture.dms
    val runSchemas = audit.runs.map(auditSchema)
    val contrast = tContrast(runSchemas.head, audit.contrastRows.head)
    val diagnostics = runSchemas.map(value => ContrastDiagnostics.analyze(value, Vector(contrast), Vector.empty).toOption.getOrElse(fail("raw audit DMS diagnostic")))
    val combined = ContrastDiagnostics.fixedEffectsT(diagnostics, contrast).toOption.getOrElse(fail("raw audit DMS fixed effects"))
    val expected = ContrastDiagnosticsFixture.DurableAudit.dmsFixedEffectsSe
    assertEqualsDouble(combined.varianceOverSigmaSquared.map(math.sqrt).getOrElse(fail("raw audit DMS value")), expected, OracleTolerance * expected)
    assertEquals(diagnostics.map(result => result.selectedRows.length - result.rank), ContrastDiagnosticsFixture.DurableAudit.dmsResidualDf)
  }

  test("exact m3 and m4 raw duplicate matrices retain the documented rank-deficient receipts") {
    val m3 = DurableContrastMatrixFixture.dupState.map(auditSchema)
    val audit = ContrastDiagnosticsFixture.DurableAudit
    m3.zip(audit.dupStateRank.zip(audit.dupStateNullity)).foreach { (value, expected) =>
      val result = ContrastDiagnostics.analyze(value, Vector.empty, Vector.empty).toOption.getOrElse(fail("m3 duplicate diagnostic"))
      assertEquals(result.rank, expected._1)
      assertEquals(result.aliases.length, expected._2)
      assert(result.aliases.forall(_.residual <= AliasResidualBound), result.aliases.map(_.residual).max)
    }
    DurableContrastMatrixFixture.dup2State.map(auditSchema).zip(audit.dup2StateNullity).foreach { (value, nullity) =>
      val result = ContrastDiagnostics.analyze(value, Vector.empty, Vector.empty).toOption.getOrElse(fail("m4 duplicate diagnostic"))
      assertEquals(result.aliases.length, nullity)
      assert(result.aliases.forall(_.residual <= AliasResidualBound), result.aliases.map(_.residual).max)
    }
  }

  test("tiny supported columns and tiny residual task information remain estimable under the rank policy") {
    val scaled = schema(Array(
      -5e-12, -0.5,
      -5e-12, -0.5,
      5e-12, 0.5,
      5e-12, 0.5
    ), Vector("tiny", "base"))
    val cancellation = ColumnContrast(Vector(scaled.columnIds(0) -> 1e-11, scaled.columnIds(1) -> 1.0))
    val diagnostic = ContrastDiagnostics.analyze(scaled, Vector(cancellation), Vector.empty).toOption.getOrElse(fail("scaled-column diagnostic"))
    assertEquals(diagnostic.t.head.estimability, ContrastEstimability.Estimable)
    assertEqualsDouble(diagnostic.t.head.varianceOverSigmaSquared.getOrElse(fail("scaled-column variance")), 1.0, 1e-9)

    val almostConfounded = schema(Array(
      -0.500000000005, -0.5,
      -0.499999999995, -0.5,
      0.499999999995, 0.5,
      0.500000000005, 0.5
    ), Vector("task", "nuisance"))
    val task = ColumnContrast(Vector(almostConfounded.columnIds.head -> 1.0))
    val run = ContrastDiagnostics.analyze(almostConfounded, Vector(task), Vector.empty).toOption.getOrElse(fail("near-confounded diagnostic"))
    val combined = ContrastDiagnostics.concatenatedTaskT(Vector(run), Vector(almostConfounded.columnIds.head), task).toOption.getOrElse(fail("near-confounded concatenation"))
    val variance = combined.varianceOverSigmaSquared.getOrElse(fail("near-confounded task information was discarded"))
    assert(math.abs(variance / 1e22 - 1.0) < 2e-4, s"near-confounded variance was $variance")
  }

  /** Relative agreement with the independent NumPy pseudo-inverse oracle. */
  private val OracleTolerance = 1e-11

  /** Scale-invariant alias residual `||Xc|| / (sigmaMax ||c||)` for exact
    * duplicates: a few hundred machine epsilons.
    */
  private val AliasResidualBound = 1e-13

  private def assertAliased(estimability: ContrastEstimability, residual: Double)(using munit.Location): Unit =
    estimability match
      case ContrastEstimability.Lost(LostEstimability.Aliased(value)) => assertEqualsDouble(value, residual, 1e-12)
      case other => fail(s"expected an aliased contrast, got $other")

  private def auditSchema(run: DurableContrastMatrixFixture.AuditRun): DesignSchema =
    schema(run.values, (0 until run.columns).toVector.map(index => s"audit-$index"))

  private def tContrast(schema: DesignSchema, weights: Array[Double]): ColumnContrast =
    ColumnContrast(weights.zipWithIndex.collect { case (weight, index) if weight != 0.0 => schema.columnIds(index) -> weight }.toVector)

  private def fContrast(schema: DesignSchema, rows: Vector[Array[Double]]): FColumnContrast =
    FColumnContrast(rows.map(tContrast(schema, _)))

  test("rank preflight validates selected scan identities before matrix access"):
    val design = schema(Array(1.0, 0.0, 0.0, 1.0, 1.0, 1.0), Vector("a", "b"))
    assertEquals(ContrastDiagnostics.preflight(design, Vector(ScanIndex.unsafeOneBased(1), ScanIndex.unsafeOneBased(1))), Left(DesignDiagnosticsError.DuplicateRows))
    assertEquals(ContrastDiagnostics.preflight(design, Vector(ScanIndex.unsafeOneBased(4))), Left(DesignDiagnosticsError.RowOutOfBounds(ScanIndex.unsafeOneBased(4), 3)))
    assertEquals(ContrastDiagnostics.preflight(design, Vector(ScanIndex.unsafeOneBased(0))), Left(DesignDiagnosticsError.RowOutOfBounds(ScanIndex.unsafeOneBased(0), 3)))

  private def run(index: Int): RunIndex = RunIndex.unsafeOneBased(index)

  private def fRows(design: DesignSchema, rows: Vector[Vector[Double]]): FColumnContrast =
    FColumnContrast(rows.map(row => ColumnContrast(row.zipWithIndex.map((weight, index) => design.columnIds(index) -> weight))))

  private def threeColumnDesign: DesignSchema =
    schema(Array(
      1, 1, 0, 1, 0, 1,
      1, 0, 1, 1, 1, 0,
      1, 1, 1, 1, -1, 0,
      1, 2, 1, 0, 1, 1,
      1, 1, 2, -1, 0, 1,
      1, 2, 2, 0, -1, -1
    ).map(_.toDouble), Vector("i", "a", "b"))

  test("rank policies are values, and separately constructed equal policies combine"):
    val first = RankPolicy.relative(1e-8).toOption.getOrElse(fail("policy"))
    val second = RankPolicy.relative(1e-8).toOption.getOrElse(fail("policy"))
    assertEquals(first, second)
    val design = threeColumnDesign
    val contrast = ColumnContrast(Vector(design.columnIds(1) -> 1.0))
    val runs = Vector(first, second).map(policy => ContrastDiagnostics.analyze(design, Vector(contrast), Vector.empty, rankPolicy = policy).toOption.getOrElse(fail("run")))
    val single = runs.head.t.head.varianceOverSigmaSquared.getOrElse(fail("variance"))
    val combined = ContrastDiagnostics.fixedEffectsT(runs, contrast).toOption.getOrElse(fail("equal policies must combine"))
    assertEqualsDouble(combined.varianceOverSigmaSquared.getOrElse(fail("combined")), single / 2.0, 1e-14)
    val weighted = WeightedContrastDiagnostics.t(runs, contrast, Vector(1.0, 1.0), RunContrastCombination.FixedEffects)
    assert(weighted.isRight, weighted)
    val other = ContrastDiagnostics.analyze(design, Vector(contrast), Vector.empty).toOption.getOrElse(fail("default run"))
    assertEquals(
      ContrastDiagnostics.fixedEffectsT(Vector(runs.head, other), contrast),
      Left(DesignDiagnosticsError.RankPolicyMismatch(Vector(first.label, RankPolicy.Default.label)))
    )
    assert(ContrastDiagnostics.concatenatedTaskT(Vector(runs.head, other), Vector(design.columnIds(1)), contrast).isLeft)

  test("a non-default rank policy changes rank and estimability decisions"):
    // b is a + 1e-7 * (orthogonal direction): estimable by default, aliased
    // once the policy treats singular values below 1e-5 * sigmaMax as zero.
    val design = schema(Array(
      1.0, 1.0 + 1e-7,
      -1.0, -1.0 - 1e-7,
      1.0, 1.0 - 1e-7,
      -1.0, -1.0 + 1e-7
    ), Vector("a", "b"))
    val difference = ColumnContrast(Vector(design.columnIds(0) -> 1.0, design.columnIds(1) -> -1.0))
    val sum = ColumnContrast(Vector(design.columnIds(0) -> 1.0, design.columnIds(1) -> 1.0))
    val coarse = RankPolicy.relative(1e-5).toOption.getOrElse(fail("policy"))
    val strict = ContrastDiagnostics.analyze(design, Vector(difference, sum), Vector.empty).toOption.getOrElse(fail("default"))
    val loose = ContrastDiagnostics.analyze(design, Vector(difference, sum), Vector.empty, rankPolicy = coarse).toOption.getOrElse(fail("coarse"))
    assertEquals(strict.rank, 2)
    assertEquals(strict.t.map(_.estimability), Vector(ContrastEstimability.Estimable, ContrastEstimability.Estimable))
    assertEquals(loose.rankPolicy, coarse)
    assertEquals(loose.rank, 1)
    assertEquals(loose.aliases.length, 1)
    assertAliased(loose.t.head.estimability, 1.0)
    assertEquals(loose.t(1).estimability, ContrastEstimability.Estimable)
    assertEqualsDouble(loose.t(1).varianceOverSigmaSquared.getOrElse(fail("sum variance")), 0.25, 1e-12)

  test("a zero column is reported as dropped before aliasing, also across runs"):
    val design = schema(Array(
      1.0, 0.0, 1.0,
      1.0, 0.0, -1.0,
      1.0, 0.0, 1.0,
      1.0, 0.0, -1.0
    ), Vector("i", "zero", "x"))
    val zero = design.columnIds(1)
    val tContrast = ColumnContrast(Vector(zero -> 1.0, design.columnIds(2) -> 1.0))
    val fContrast = FColumnContrast(Vector(ColumnContrast(Vector(design.columnIds(2) -> 1.0)), ColumnContrast(Vector(zero -> 1.0))))
    val result = ContrastDiagnostics.analyze(design, Vector(tContrast), Vector(fContrast)).toOption.getOrElse(fail("dropped"))
    assertEquals(result.t.head.estimability, ContrastEstimability.Lost(LostEstimability.DroppedColumns(Vector(zero))))
    assertEquals(result.f.head.estimability, ContrastEstimability.Lost(LostEstimability.DroppedColumns(Vector(zero))))
    assertEquals(result.aliases.map(_.coefficients), Vector(Vector(zero -> 1.0)))

    // A second run supports the same columns; a third lacks them entirely
    // (its two columns carry different structural ids).
    val supported = schema(Array(
      1.0, 1.0, 1.0,
      1.0, 0.0, -1.0,
      1.0, 0.0, 1.0,
      1.0, 1.0, -1.0
    ), Vector("i", "zero", "x"))
    val absent = schema(Array(1.0, 1.0, 1.0, -1.0, 1.0, 1.0, 1.0, -1.0), Vector("i", "x"))
    val runs = Vector(result) ++ Vector(supported, absent).map(value => ContrastDiagnostics.analyze(value, Vector.empty, Vector.empty).toOption.getOrElse(fail("run")))
    val combined = ContrastDiagnostics.fixedEffectsT(runs, tContrast).toOption.getOrElse(fail("fixed effects"))
    assertEquals(combined.contributingRuns, Vector(run(2)))
    assertEquals(
      combined.excludedRuns,
      Vector(
        run(1) -> ContrastEstimability.Lost(LostEstimability.DroppedColumns(Vector(zero))),
        run(3) -> ContrastEstimability.Lost(LostEstimability.DroppedColumns(Vector(zero, design.columnIds(2))))
      )
    )
    val fixedF = ContrastDiagnostics.fixedEffectsF(runs, fContrast).toOption.getOrElse(fail("fixed effects F"))
    assertEquals(fixedF.contributingRuns, Vector(run(2)))
    assertEquals(fixedF.effectiveDf, 2)
    assertEquals(fixedF.excludedRuns.map(_._1), Vector(run(1), run(3)))
    assertEquals(fixedF.excludedRuns.head._2, ContrastEstimability.Lost(LostEstimability.DroppedColumns(Vector(zero))))

  test("fixed effects recomputes each run, so equivalent contrast spellings combine identically"):
    val design = threeColumnDesign
    val a = design.columnIds(1)
    val b = design.columnIds(2)
    val base = ColumnContrast(Vector(a -> 1.0, b -> -1.0))
    val respelled = ColumnContrast(Vector(b -> -1.0, design.columnIds(0) -> 0.0, a -> 1.0))
    val runs = Vector.fill(2)(ContrastDiagnostics.analyze(design, Vector(base), Vector.empty).toOption.getOrElse(fail("run")))
    val first = ContrastDiagnostics.fixedEffectsT(runs, base).toOption.getOrElse(fail("base"))
    val second = ContrastDiagnostics.fixedEffectsT(runs, respelled).toOption.getOrElse(fail("respelled"))
    assertEquals(second.contributingRuns, Vector(run(1), run(2)))
    assertEqualsDouble(second.varianceOverSigmaSquared.getOrElse(fail("respelled variance")), first.varianceOverSigmaSquared.getOrElse(fail("base variance")), 1e-15)

  test("an F contrast partly outside the row space is aliased along its worst direction"):
    val design = schema(ContrastDiagnosticsFixture.duplicateDesign, Vector("i", "u", "u2", "w"))
    val u = design.columnIds(1)
    val u2 = design.columnIds(2)
    val partly = FColumnContrast(Vector(ColumnContrast(Vector(u -> 1.0, u2 -> 1.0)), ColumnContrast(Vector(u -> 1.0, u2 -> -1.0))))
    val inside = FColumnContrast(Vector(ColumnContrast(Vector(u -> 1.0, u2 -> 1.0)), ColumnContrast(Vector(design.columnIds(3) -> 1.0))))
    val result = ContrastDiagnostics.analyze(design, Vector.empty, Vector(partly, inside)).toOption.getOrElse(fail("partly"))
    assertAliased(result.f.head.estimability, 1.0)
    assertEquals(result.f.head.effectiveDf, 0)
    assertEquals(result.f(1).estimability, ContrastEstimability.Estimable)
    assertEquals(result.f(1).effectiveDf, 2)

  test("nearly dependent F rows keep their span by default and collapse under a coarse policy"):
    val design = threeColumnDesign
    val nearly = fRows(design, Vector(Vector(0.0, 1.0, 0.0), Vector(0.0, 1.0, 1e-9)))
    val plain = fRows(design, Vector(Vector(0.0, 1.0, 0.0), Vector(0.0, 0.0, 1.0)))
    val single = ColumnContrast(Vector(design.columnIds(1) -> 1.0))
    val default = ContrastDiagnostics.analyze(design, Vector(single), Vector(nearly, plain)).toOption.getOrElse(fail("default"))
    assertEquals(default.f.map(_.effectiveDf), Vector(2, 2))
    val nearlySe = default.f.head.worstDirectionSeOverSigma.getOrElse(fail("nearly"))
    val plainSe = default.f(1).worstDirectionSeOverSigma.getOrElse(fail("plain"))
    // The SVD row basis is accurate to about eps / 7e-10 in angle.
    assertEqualsDouble(nearlySe, plainSe, 1e-6 * plainSe)
    val coarse = RankPolicy.relative(1e-6).toOption.getOrElse(fail("policy"))
    val collapsed = ContrastDiagnostics.analyze(design, Vector(single), Vector(nearly), rankPolicy = coarse).toOption.getOrElse(fail("coarse"))
    assertEquals(collapsed.f.head.effectiveDf, 1)
    val tSe = math.sqrt(collapsed.t.head.varianceOverSigmaSquared.getOrElse(fail("t")))
    assertEqualsDouble(collapsed.f.head.worstDirectionSeOverSigma.getOrElse(fail("collapsed")), tSe, 1e-8 * tSe)

  test("concatenated task F matches the independent residualized-information oracle"):
    val names = Vector("task1", "task2", "intercept", "drift")
    val one = schema(ContrastDiagnosticsFixture.concatenatedRunOne, names)
    val two = schema(ContrastDiagnosticsFixture.concatenatedRunTwo, names)
    val runs = Vector(one, two).map(value => ContrastDiagnostics.analyze(value, Vector.empty, Vector.empty).toOption.getOrElse(fail("run")))
    val task = one.columnIds.take(2)
    val contrast = FColumnContrast(ContrastDiagnosticsFixture.concatenatedFRows.map(row => ColumnContrast(task.zip(row))))
    val result = ContrastDiagnostics.concatenatedTaskF(runs, task, contrast).toOption.getOrElse(fail("concatenated F"))
    val expected = ContrastDiagnosticsFixture.concatenatedFWorstSe
    assertEquals(result.effectiveDf, 2)
    assertEquals(result.contributingRuns, Vector(run(1), run(2)))
    assertEquals(result.runNuisance.map(_.nuisanceRank), Vector(2, 2))
    assertEqualsDouble(result.worstDirectionSeOverSigma.getOrElse(fail("concatenated F value")), expected, OracleTolerance * expected)
    val weighted = WeightedContrastDiagnostics.f(runs, contrast, Vector(0.5, 2.0), RunContrastCombination.Concatenated(task)).toOption.getOrElse(fail("weighted"))
    val weightedExpected = ContrastDiagnosticsFixture.concatenatedWeightedFWorstSe
    assertEqualsDouble(weighted.worstDirectionSe.getOrElse(fail("weighted value")), weightedExpected, OracleTolerance * weightedExpected)
    assertEquals(ContrastDiagnostics.concatenatedTaskF(runs, Vector.empty, contrast), Left(DesignDiagnosticsError.EmptyTaskColumns))
    assertEquals(ContrastDiagnostics.concatenatedTaskF(runs, task :+ task.head, contrast), Left(DesignDiagnosticsError.DuplicateTaskColumn(task.head)))

  test("high-pass F variance ratio is pinned to the without-cosines worst direction"):
    val withSchema = schema(ContrastDiagnosticsFixture.highPassWith, Vector("task1", "task2", "intercept", "cosine"))
    val withoutSchema = schema(ContrastDiagnosticsFixture.highPassWithout, Vector("task1", "task2", "intercept"))
    val task = withoutSchema.columnIds.take(2)
    val contrast = FColumnContrast(Vector(ColumnContrast(Vector(task(0) -> 1.0)), ColumnContrast(Vector(task(1) -> 1.0))))
    val withResult = ContrastDiagnostics.analyze(withSchema, Vector.empty, Vector(contrast)).toOption.getOrElse(fail("with"))
    val withoutResult = ContrastDiagnostics.analyze(withoutSchema, Vector.empty, Vector(contrast)).toOption.getOrElse(fail("without"))
    val cost = ContrastDiagnostics.highPassFVarianceRatio(withResult, withoutResult, contrast, Vector(withSchema.columnIds(3))).toOption.getOrElse(fail("cost"))
    val expectedWith = ContrastDiagnosticsFixture.highPassWithVariance
    val expectedWithout = ContrastDiagnosticsFixture.highPassWithoutVariance
    assertEqualsDouble(cost.withCosinesVarianceOverSigmaSquared, expectedWith, OracleTolerance * expectedWith)
    assertEqualsDouble(cost.withoutCosinesVarianceOverSigmaSquared, expectedWithout, OracleTolerance * expectedWithout)
    assertEqualsDouble(cost.varianceRatio, expectedWith / expectedWithout, OracleTolerance * expectedWith / expectedWithout)
    assertEqualsDouble(cost.excessVarianceRatio, cost.varianceRatio - 1.0, 1e-15)
    assertEquals(
      ContrastDiagnostics.highPassFVarianceRatio(withResult, withoutResult, contrast, Vector.empty),
      Left(DesignDiagnosticsError.InvalidCosineSelection)
    )

  test("heterogeneous run sigmas match the independent fixed-effects oracle for t and F"):
    val sigmas = ContrastDiagnosticsFixture.DurableAudit.weightedSigmas
    val dms = DurableContrastMatrixFixture.dms
    val dmsSchemas = dms.runs.map(auditSchema)
    val tContrastValue = tContrast(dmsSchemas.head, dms.contrastRows.head)
    val dmsRuns = dmsSchemas.map(value => ContrastDiagnostics.analyze(value, Vector.empty, Vector.empty).toOption.getOrElse(fail("dms run")))
    val t = WeightedContrastDiagnostics.t(dmsRuns, tContrastValue, sigmas, RunContrastCombination.FixedEffects).toOption.getOrElse(fail("weighted t"))
    val expectedT = ContrastDiagnosticsFixture.DurableAudit.weightedDmsFixedEffectsVariance
    assertEquals(t.contributingRuns, Vector(run(1), run(2)))
    assertEqualsDouble(t.variance.getOrElse(fail("weighted t value")), expectedT, OracleTolerance * expectedT)

    val block = DurableContrastMatrixFixture.block
    val blockSchemas = block.runs.map(auditSchema)
    val fContrastValue = fContrast(blockSchemas.head, block.contrastRows)
    val blockRuns = blockSchemas.map(value => ContrastDiagnostics.analyze(value, Vector.empty, Vector.empty).toOption.getOrElse(fail("block run")))
    val f = WeightedContrastDiagnostics.f(blockRuns, fContrastValue, sigmas, RunContrastCombination.FixedEffects).toOption.getOrElse(fail("weighted F"))
    val expectedF = ContrastDiagnosticsFixture.DurableAudit.weightedBlockFixedEffectsFWorstSe
    assertEqualsDouble(f.worstDirectionSe.getOrElse(fail("weighted F value")), expectedF, OracleTolerance * expectedF)
