package scalafim.fmri.fit

import gale.backend.Backend.given
import gale.linalg.DMat
import scalafim.fmri.model.FitConfig

class PreparedManovaGeometrySuite extends munit.FunSuite:

  private val rows = Vector(
    Vector(1.0, -1.0, 0.0, -1.0),
    Vector(1.0, 0.0, -1.0, -0.8),
    Vector(1.0, 1.0, 0.0, -0.6),
    Vector(1.0, 0.0, 1.0, -0.3),
    Vector(1.0, -1.0, 0.0, -0.1),
    Vector(1.0, 0.0, -1.0, 0.1),
    Vector(1.0, 1.0, 0.0, 0.3),
    Vector(1.0, 0.0, 1.0, 0.6),
    Vector(1.0, -1.0, 0.0, 0.8),
    Vector(1.0, 1.0, 0.0, 1.0)
  )
  private val names = Vector("intercept", "a", "b", "drift")

  test("multi-contrast preparation yields a design-normalized effect basis"):
    val geometry = prepare(
      FContrast("a-and-b", Vector(Map("a" -> 1.0), Map("b" -> 1.0)))
    ).toOption.get
    val normalization = geometry.effectBasis.t * geometry.designCrossproduct * geometry.effectBasis

    assertEquals(geometry.contrastRank, 2)
    assertEquals(geometry.receipt.contrastRank, 2)
    assertEqualsDouble(normalization(0, 0), 1.0, 1e-10)
    assertEqualsDouble(normalization(1, 1), 1.0, 1e-10)
    assertEqualsDouble(normalization(0, 1), 0.0, 1e-10)
    assertEqualsDouble(normalization(1, 0), 0.0, 1e-10)

  test("invertible changes of contrast basis preserve the hypothesis projector"):
    val ordinary = prepare(
      FContrast("ordinary", Vector(Map("a" -> 1.0), Map("b" -> 1.0)))
    ).toOption.get
    val changed = prepare(
      FContrast(
        "changed",
        Vector(
          Map("a" -> 1.0, "b" -> 1.0),
          Map("a" -> 2.0, "b" -> -1.0)
        )
      )
    ).toOption.get
    val design = ordinary.preparedDesign.value
    val first = design * ordinary.effectBasis * ordinary.effectBasis.t * design.t
    val second = design * changed.effectBasis * changed.effectBasis.t * design.t

    assertMatrixClose(first, second, 1e-10)

  test("rank-deficient contrast rows are rejected as non-estimable"):
    val result = prepare(
      FContrast(
        "dependent",
        Vector(Map("a" -> 1.0), Map("a" -> 2.0))
      )
    )

    assert(result.left.toOption.exists:
      case FitError.NonEstimableContrast("dependent", _) => true
      case _                                               => false
    )

  private def prepare(contrast: FContrast): Either[FitError, PreparedManovaGeometry] =
    val design = DesignMatrix.unsafe(GaleTestMatrix.fromRows(rows))
    ResponsePreparationPlan
      .fromConfig(FitConfig())
      .prepareManova(
        design,
        names,
        contrast,
        SelectedTimepointIndices.unsafe(rows.indices.toVector),
        Vector(RunPartition(0, rows.indices.toVector, rows.indices.toVector)),
        TemporalNuisanceRank.unsafe(2)
      )

  private def assertMatrixClose(actual: DMat, expected: DMat, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1
