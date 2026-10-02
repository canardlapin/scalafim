package scalafim.fmri.design

import scalafim.fmri.design.fixtures.DesignDiagnosticsFixture
import scalafim.fmri.hrf.{Seconds}
import scalafim.fmri.hrf.linalg.Mat

class DesignDiagnosticsSuite extends munit.FunSuite:

  test("VIF, separate block R2, and joint coefficients match the independent orthonormal fixture") {
    val schema = schemaFor(4, 3, DesignDiagnosticsFixture.fullRank, Vector("u", "x", "w"))
    val blocks = Vector(
      DiagnosticBlock("task", Vector(schema.columnIds(1))),
      DiagnosticBlock("noise", Vector(schema.columnIds(2)))
    )
    val result = DesignDiagnostics.analyze(schema, blocks).toOption.getOrElse(fail("diagnostics should succeed"))

    assertEquals(result.fingerprint, schema.fingerprint)
    assertEquals(result.selectedRows, schema.rows.selectedRows)
    assertEquals(result.rankPolicy.relativeMultiplier, None)
    assert(result.projections.exists(value => value.target == schema.columnIds.head && value.kind == ProjectionKind.Vif && value.effectiveRank == 2 && value.cutoff > 0.0))
    result.vif.zip(DesignDiagnosticsFixture.expectedVif).foreach { (actual, expected) =>
      actual.outcome match
        case VifOutcome.Finite(value) => assertEqualsDouble(value, expected, 1e-10)
        case other => fail(s"expected finite VIF, got $other")
    }
    val taskR2 = result.blockR2.find(value => value.target == schema.columnIds.head && value.block == "task").getOrElse(fail("task R2"))
    taskR2.outcome match
      case BlockR2Outcome.Value(value) => assertEqualsDouble(value, DesignDiagnosticsFixture.expectedBlockR2, 1e-10)
      case other => fail(s"expected block R2, got $other")
    val contribution = result.jointContributors.find(value => value.target == schema.columnIds.head && value.predictor == schema.columnIds(1)).getOrElse(fail("joint coefficient"))
    assertEquals(contribution.identifiability, CoefficientIdentifiability.Unique)
    assertEqualsDouble(contribution.standardizedCoefficient, DesignDiagnosticsFixture.expectedJointCoefficient, 1e-10)
    assertEquals(result.correlations.regressors, schema.columnNames)
  }

  test("duplicate predictors make only the dependent target aliased") {
    val schema = schemaFor(4, 4, DesignDiagnosticsFixture.deficient, Vector("intercept", "u", "u-copy", "w"))
    val result = DesignDiagnostics.analyze(schema, Vector.empty).toOption.getOrElse(fail("diagnostics should succeed"))

    assertEquals(result.vif.head.outcome, VifOutcome.NotApplicable("constant or zero-support column"))
    assertEquals(result.vif(1).outcome, VifOutcome.Aliased)
    assertEquals(result.vif(2).outcome, VifOutcome.Aliased)
    result.vif(3).outcome match
      case VifOutcome.Finite(value) => assertEqualsDouble(value, 1.0, 1e-10)
      case other => fail(s"independent w should retain a finite VIF, got $other")
    assert(result.jointContributors.exists(_.identifiability == CoefficientIdentifiability.MinNormNonUnique))
  }

  test("a finite near-collinear VIF is formed from the projection residual") {
    val schema = schemaFor(4, 2, DesignDiagnosticsFixture.nearCollinear, Vector("u", "almost-u"))
    val result = DesignDiagnostics.analyze(schema, Vector.empty).toOption.getOrElse(fail("near-collinear diagnostics should succeed"))
    result.vif.foreach { diagnostic =>
      diagnostic.outcome match
        case VifOutcome.Finite(value) =>
          assert(value > 1e17)
          // The independent NumPy value is based on a residual of order 1e-9.
          // An SVD projection on a condition-1e9 system may perturb that
          // residual by a few 1e-8 relatively; VIF squares the residual, so
          // 2e-7 is the resulting cross-backend comparison tolerance.
          val relativeDifference = math.abs(value - DesignDiagnosticsFixture.expectedNearCollinearVif) / DesignDiagnosticsFixture.expectedNearCollinearVif
          assert(relativeDifference <= 2e-7, s"near-collinear VIF relative difference $relativeDifference exceeds the SVD conditioning tolerance")
        case other => fail(s"near-collinear predictor must remain finite, got $other")
    }
  }

  test("wide selected designs use a minimum-norm, nonunique joint fit") {
    val schema = schemaFor(
      3,
      5,
      Array(1.0, 0.0, 1.0, 2.0, -1.0, 1.0, 1.0, 0.0, 2.0, 1.0, 2.0, 1.0, 2.0, 1.0, 0.0),
      Vector("a", "b", "c", "d", "e")
    )
    val selected = Vector(ScanIndex.unsafeOneBased(1), ScanIndex.unsafeOneBased(2), ScanIndex.unsafeOneBased(3))
    val result = DesignDiagnostics.analyze(schema, Vector.empty, selected).toOption.getOrElse(fail("wide diagnostics should succeed"))
    assertEquals(result.selectedRows, selected)
    assert(result.jointContributors.exists(_.identifiability == CoefficientIdentifiability.MinNormNonUnique))
  }

  test("blocks and row selections are validated against schema identities") {
    val schema = schemaFor(4, 3, DesignDiagnosticsFixture.fullRank, Vector("u", "x", "w"))
    val duplicatedRows = Vector(ScanIndex.unsafeOneBased(1), ScanIndex.unsafeOneBased(1))
    assertEquals(DesignDiagnostics.analyze(schema, Vector.empty, duplicatedRows), Left(DesignDiagnosticsError.DuplicateRows))
    val overlap = Vector(
      DiagnosticBlock("one", Vector(schema.columnIds.head)),
      DiagnosticBlock("two", Vector(schema.columnIds.head))
    )
    assertEquals(DesignDiagnostics.analyze(schema, overlap).left.toOption, Some(DesignDiagnosticsError.OverlappingBlocks(schema.columnIds.head)))
    assertEquals(RankPolicy.relative(0.0), Left(DesignDiagnosticsError.InvalidRankMultiplier(0.0)))
    val explicitPolicy = RankPolicy.relative(1e-8).toOption.getOrElse(fail("valid policy"))
    val explicit = DesignDiagnostics.analyze(schema, Vector.empty, explicitPolicy).toOption.getOrElse(fail("explicit policy"))
    assertEquals(explicit.rankPolicy.relativeMultiplier, Some(1e-8))
    assert(explicit.projections.forall(_.cutoff >= 0.0))
  }

  private def schemaFor(rows: Int, cols: Int, values: Array[Double], names: Vector[String]): DesignSchema =
    val layout = RowLayout(
      blockIds = Vector.fill(rows)(RunIndex.unsafeOneBased(1)),
      acquisitionTimes = (0 until rows).toVector.map(index => Seconds(index.toDouble)),
      selectedRows = (1 to rows).toVector.map(ScanIndex.unsafeOneBased)
    )
    val columns = names.zipWithIndex.map { (name, index) =>
      StructuralColumn.fromOrigin(
        index + 1,
        StructuralColumnOrigin.Nuisance(TermId.unsafe("fixture"), ModulatorId.unsafe(name), RunScope.Global),
        name
      ).toOption.getOrElse(fail("fixture column"))
    }
    DesignSchema.validated(Mat.unsafe(rows, cols, values), layout, columns).toOption.getOrElse(fail("fixture schema"))
