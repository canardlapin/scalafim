package scalafim.fmri.design

import scalafim.fmri.design.basis.*

class BasisSuite extends munit.FunSuite:

  private def fitOrFail[A <: ParametricBasis](fit: Either[BasisFitError, BasisFit[A]]): BasisFit[A] =
    fit.fold(err => fail(err.message), identity)

  test("Scale reports zero variance while legacy fit remains compatible") {
    val reported = fitOrFail(ParametricBasis.Scale.fitWithDiagnostics(Vector(2.0, 2.0, 2.0), "rt"))
    val compatible = fitOrFail(
      ParametricBasis.Scale.fitWithDiagnostics(
        Vector(2.0, 2.0, 2.0),
        "rt",
        BasisDegeneracyPolicy.Compatible
      )
    )
    val legacy = ParametricBasis.Scale.fit(Vector(2.0, 2.0, 2.0), "rt")

    assertEqualsDouble(reported.basis.sd, 1e-6, 0.0)
    assertEquals(reported.basis.y.data.toVector, Vector(0.0, 0.0, 0.0))
    assert(reported.diagnostics.exists(_.kind == BasisDegeneracyKind.ZeroVariance))
    assertEquals(compatible.diagnostics, Vector.empty)
    assertEquals(legacy.y.data.toVector, reported.basis.y.data.toVector)
  }

  test("Standardized reports all non-finite values as a typed diagnostic") {
    val reported = fitOrFail(
      ParametricBasis.Standardized.fitWithDiagnostics(Vector(Double.NaN, Double.PositiveInfinity), "rt")
    )

    assertEqualsDouble(reported.basis.sd, 1e-6, 0.0)
    assertEquals(reported.basis.y.data.toVector, Vector(0.0, 0.0))
    assert(reported.diagnostics.exists(_.kind == BasisDegeneracyKind.AllNonFinite))
    assert(reported.diagnostics.exists(_.message.contains("all non-finite")))
  }

  test("ScaleWithin reports group-scoped repairs") {
    val reported = fitOrFail(
      ParametricBasis.ScaleWithin.fitWithDiagnostics(
        x = Vector(1.0, 1.0, Double.NaN, 1.0, 2.0, 3.0),
        group = Vector("A", "A", "A", "B", "B", "B"),
        argName = "rt",
        groupName = "cond"
      )
    )

    assert(reported.diagnostics.exists(d => d.kind == BasisDegeneracyKind.RepairedNonFinite && d.group.contains("A")))
    assert(reported.diagnostics.exists(d => d.kind == BasisDegeneracyKind.ZeroVariance && d.group.contains("A")))
    assert(!reported.diagnostics.exists(_.group.contains("B")))
  }

  test("RobustScale strict policy rejects repaired fits") {
    val strict = ParametricBasis.RobustScale.fitWithDiagnostics(
      Vector(4.0, 4.0, 4.0),
      "rt",
      BasisDegeneracyPolicy.Strict
    )

    strict match
      case Left(BasisFitError.Degenerate(diagnostics)) =>
        assert(diagnostics.exists(_.kind == BasisDegeneracyKind.ZeroVariance))
      case Right(_) =>
        fail("expected strict robust scale fit to reject zero-variance input")
  }
