package scalafim.fmri.ar

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.ar.fixtures.FmriArBiasRFixture

class CorrectedNoiseSummarySuite extends munit.FunSuite:
  private val fixture = FmriArBiasRFixture.fitCases.find(_.runLengths.length == 2).get
  private def matrix(rows: Vector[Vector[Double]]): DMat =
    Matrix.tabulate(rows.length, rows.head.length)((row, col) => rows(row)(col))
  private val design = matrix(fixture.design)
  private val residuals = matrix(fixture.residuals)
  private val layout = NoiseEstimationLayout.excludingRows(TimeSegments.fromRunLengths(fixture.runLengths),
    residuals.rows, fixture.censorOneBased.map(_ - 1).toSet).toOption.get
  private def checked[A](result: Either[ArError, A]): A = result.fold(error => fail(error.message), identity)
  private def bits(values: Vector[Vector[Double]]): Vector[Vector[Long]] =
    values.map(_.map(java.lang.Double.doubleToLongBits))

  test("corrected summaries equal dense estimation bitwise for every voxel partition and merge order") {
    Vector(NoisePooling.Global, NoisePooling.Run).foreach { pooling =>
      Vector(ArOrder.Fixed(2), ArOrder.Auto(4)).foreach { order =>
        val options = ArFitOptions(order, pooling, exactFirstAr1 = false)
        val prepared = checked(AcvfBias.prepare(design, layout, CorrectionBudget.Fixed(fixture.correctionMaxLag), order.maxRequested))
        val dense = checked(NoiseFit.estimate(residuals, layout, options, design, prepared))
        (1 to residuals.cols).foreach { blockSize =>
          val blocks = (0 until residuals.cols).grouped(blockSize).map { columns =>
            checked(ArEstimation.summarizeNoise(residuals.slice(0, residuals.rows, columns.head, columns.last + 1),
              layout, order.maxRequestedOrder, design, prepared))
          }.toVector
          Vector(blocks, blocks.reverse).foreach { sequence =>
            val merged = sequence.tail.foldLeft(sequence.head)((sum, next) => checked(sum.merge(next)))
            val actual = checked(NoiseFit.estimate(merged, options))
            assertEquals(bits(actual.plan.coefficients.map(_.phi)), bits(dense.plan.coefficients.map(_.phi)))
            assertEquals(bits(actual.acvf), bits(dense.acvf))
            assertEquals(actual.innovationVariance, dense.innovationVariance)
            assertEquals(actual.corrections, dense.corrections)
            assertEquals(actual.biasMatrices, dense.biasMatrices)
          }
        }
      }
    }
  }

  test("corrected reductions refuse mixed policies, mismatched orders and non-OLS residual blocks") {
    val order = ArOrderValue.unsafe(2)
    val prepared = checked(AcvfBias.prepare(design, layout, CorrectionBudget.Fixed(fixture.correctionMaxLag), order.value))
    val corrected = checked(ArEstimation.summarizeNoise(residuals, layout, order, design, prepared))
    val raw = checked(ArEstimation.summarizeNoise(residuals, layout, corrected.maxOrder))
    assert(corrected.merge(raw).left.toOption.exists {
      case ArError.IncompatibleNoiseSummaries(_) => true
      case _ => false
    })
    assert(ArEstimation.fitNoise(corrected, ArFitOptions(ArOrder.Fixed(1))).left.toOption.exists {
      case ArError.PreparedCorrectionOrderMismatch(2, 1) => true
      case _ => false
    })
    val incompatible = Matrix.tabulate(residuals.rows, residuals.cols)((row, col) => residuals(row, col) + design(row, 0))
    assert(ArEstimation.summarizeNoise(incompatible, layout, order, design, prepared).left.toOption.exists {
      case ArError.DesignResidualMismatch(_, _) => true
      case _ => false
    })
  }

  test("summary finalization reports a zero-variance run as a solve fallback") {
    val localLayout = NoiseEstimationLayout.allRows(TimeSegments.fromRunLengths(Vector(10, 10)), 20).toOption.get
    val localDesign = Matrix.tabulate(20, 2)((row, col) => if row / 10 == col then 1.0 else 0.0)
    val wave = Vector.tabulate(10)(i => math.sin(i.toDouble * 1.3))
    val mean = wave.sum / wave.length
    val localResiduals = Matrix.tabulate(20, 1)((row, _) => if row < 10 then 0.0 else wave(row - 10) - mean)
    val options = ArFitOptions(ArOrder.Fixed(1), NoisePooling.Run)
    val prepared = checked(AcvfBias.prepare(localDesign, localLayout, CorrectionBudget.Fixed(2), 1))
    val summary = checked(ArEstimation.summarizeNoise(localResiduals, localLayout,
      ArOrderValue.unsafe(1), localDesign, prepared))
    val fit = checked(NoiseFit.estimate(summary, options))
    assertEquals(fit.corrections.head, RunCorrection.SolveFallback(CorrectionFallback.NonPositiveRawVariance(0.0)))
    assert(fit.corrections(1).isInstanceOf[RunCorrection.Applied])
    assertEquals(fit.innovationVariance.head, None)
    assertEquals(fit.corrections,
      checked(NoiseFit.estimate(localResiduals, localLayout, options, localDesign, prepared)).corrections)
  }
