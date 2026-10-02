package scalafim.fmri.design

import scalafim.fmri.design.fixtures.{ContrastDiagnosticsFixture, DurableContrastMatrixFixture}
import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.linalg.Mat

class ContrastDiagnosticsSuite extends munit.FunSuite:
  test("frozen gamble design loses eight residual degrees of freedom for eight independent spikes"):
    // Independent numpy.linalg.matrix_rank: rank 23 before, 31 after adding
    // unit spikes at source scans 0..7, with 240 rows (df 217 -> 209).
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
    assertEquals(original.rank, 23)
    assertEquals(censored.rank, 31)
    assertEquals(run.rows - original.rank, 217)
    assertEquals(run.rows - censored.rank, 209)

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
    assert(result.t.head.estimability.isInstanceOf[ContrastEstimability.Lost])
    assert(result.t(1).estimability.isInstanceOf[ContrastEstimability.Lost])
    assertEquals(result.t(2).estimability, ContrastEstimability.Estimable)
    assertEqualsDouble(result.t(2).varianceOverSigmaSquared.get, 1.0, 1e-10)
    assert(result.t(3).estimability.isInstanceOf[ContrastEstimability.Lost])
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
    assertEquals(result.contributingRuns, Vector(1))
    assertEqualsDouble(result.varianceOverSigmaSquared.get, 1.0, 1e-10)
  }

  test("preflight rejects a design with no residual degrees of freedom") {
    val saturated = schema(Array(1, 0, 0, 1).map(_.toDouble), Vector("a", "b"))
    assert(ContrastDiagnostics.preflight(saturated).isLeft)
  }

  test("preflight uses numerical rank while the column budget remains an explicit guard") {
    val duplicate = schema(Array(
      1, 1, 0,
      0, 0, 1,
      1, 1, 1
    ).map(_.toDouble), Vector("a", "a-copy", "b"))
    assertEquals(ContrastDiagnostics.preflight(duplicate), Right(()))
    assert(ContrastDiagnostics.checkColumnBudget(duplicate, 2).isLeft)
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
    assert(ContrastDiagnostics.highPassVarianceRatio(withResult, changedResult, contrast, Vector(withCosine.columnIds(2))).isLeft)
  }

  test("durable methods-audit receipt preserves the independent reference values") {
    val receipt = ContrastDiagnosticsFixture.DurableAudit
    assertEqualsDouble(receipt.dmsFixedEffectsSeOverSigma.head, 0.16045351706916153, 0.0)
    assertEqualsDouble(receipt.fWorstDirectionSeOverSigma(0), 0.053641418674500875, 0.0)
    assertEquals(receipt.dmsResidualDf, Vector(183, 183))
    assert(receipt.duplicateAliasResidualUpperBound <= 8e-13)
  }

  test("exact m2b raw matrices reproduce the independently recomputed row-orthonormalized F efficiencies") {
    val cases = Vector(
      DurableContrastMatrixFixture.block -> 0.03141900097827853,
      DurableContrastMatrixFixture.stop -> 0.17320232254158896,
      DurableContrastMatrixFixture.dense -> 0.25771995171414197
    )
    cases.foreach { (audit, expected) =>
      val runSchemas = audit.runs.map(auditSchema)
      val contrast = fContrast(runSchemas.head, audit.contrastRows)
      val diagnostics = runSchemas.map(value => ContrastDiagnostics.analyze(value, Vector.empty, Vector(contrast)).toOption.getOrElse(fail("raw audit F diagnostic")))
      val combined = ContrastDiagnostics.fixedEffectsF(diagnostics, contrast).toOption.getOrElse(fail("raw audit F fixed effects"))
      assertEqualsDouble(combined.worstDirectionSeOverSigma.getOrElse(fail("raw audit F value")), expected, 2e-10)
    }
  }

  test("exact m2b DMS matrices reproduce fixed-effects probe-minus-sample efficiency") {
    val audit = DurableContrastMatrixFixture.dms
    val runSchemas = audit.runs.map(auditSchema)
    val contrast = tContrast(runSchemas.head, audit.contrastRows.head)
    val diagnostics = runSchemas.map(value => ContrastDiagnostics.analyze(value, Vector(contrast), Vector.empty).toOption.getOrElse(fail("raw audit DMS diagnostic")))
    val combined = ContrastDiagnostics.fixedEffectsT(diagnostics, contrast)
    assertEqualsDouble(combined.varianceOverSigmaSquared.map(math.sqrt).getOrElse(fail("raw audit DMS value")), 0.16045351706916153, 2e-10)
  }

  test("exact m3 and m4 raw duplicate matrices retain the documented rank-deficient receipts") {
    val m3 = DurableContrastMatrixFixture.dupState.map(auditSchema)
    m3.zip(Vector(4, 5)).foreach { (value, nullity) =>
      val result = ContrastDiagnostics.analyze(value, Vector.empty, Vector.empty).toOption.getOrElse(fail("m3 duplicate diagnostic"))
      assertEquals(result.rank, 27)
      assertEquals(value.matrix.rows - result.rank, 183)
      assertEquals(result.aliases.length, nullity)
      assert(result.aliases.forall(_.residual <= ContrastDiagnosticsFixture.DurableAudit.duplicateAliasResidualUpperBound))
    }
    DurableContrastMatrixFixture.dup2State.map(auditSchema).zip(Vector(5, 6)).foreach { (value, nullity) =>
      val result = ContrastDiagnostics.analyze(value, Vector.empty, Vector.empty).toOption.getOrElse(fail("m4 duplicate diagnostic"))
      assertEquals(result.aliases.length, nullity)
      assert(result.aliases.forall(_.residual <= ContrastDiagnosticsFixture.DurableAudit.duplicateAliasResidualUpperBound))
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
