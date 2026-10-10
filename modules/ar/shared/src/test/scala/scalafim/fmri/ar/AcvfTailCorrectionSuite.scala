package scalafim.fmri.ar

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.ar.fixtures.FmriAr041RFixture as R

class AcvfTailCorrectionSuite extends munit.FunSuite:
  private def value[A](v: Either[ArError, A]): A = v.fold(e => fail(e.message), identity)
  private def matrix(rows: Vector[Vector[Double]]): DMat = Matrix.tabulate(rows.length, rows.head.length)((r, c) => rows(r)(c))
  private def close(a: Vector[Double], b: Vector[Double]): Unit =
    assertEquals(a.length, b.length)
    a.zip(b).foreach((x,y) => assertEqualsDouble(x,y,1e-8))

  test("censored drift-design tail correction matches fmriAR 0.4.1 and reports actual anchors"):
    val design = matrix(R.tailDesign)
    val residuals = matrix(R.tailResiduals)
    val layout = value(NoiseEstimationLayout.excludingRows(TimeSegments.fromRunLengths(Vector(150,150)), 300, R.tailCensor.toSet))
    val policy = EstimationPolicy.DesignCorrected(design, CorrectionBudget.Fixed(25), AcvfCorrectionSolve.ShortMemoryTail)
    val options = ArFitOptions(order=ArOrder.Fixed(1))
    val fit = value(NoiseFit.estimate(residuals,layout,options,policy))
    close(fit.plan.coefficients.head.phi,R.tailPhi)
    assertEquals(fit.acvf.head.length,2)
    close(fit.acvf.head,R.tailGamma.take(2))
    assertEqualsDouble(fit.innovationVariance.head.get,R.tailSigma2,1e-8)
    assert(fit.corrections.forall {
      case RunCorrection.AppliedWithTailAnchor(_,1) => true
      case _ => false
    },fit.corrections.toString)
    val prepared = value(AcvfBias.prepare(design,layout,CorrectionBudget.Fixed(25),1,AcvfCorrectionSolve.ShortMemoryTail))
    val cached = value(NoiseFit.estimate(residuals,layout,options,design,prepared))
    assertEquals(cached.plan.coefficients,fit.plan.coefficients)
    val left = value(ArEstimation.summarizeNoise(residuals.slice(0,300,0,1),layout,ArOrderValue.unsafe(1),design,prepared))
    val right = value(ArEstimation.summarizeNoise(residuals.slice(0,300,1,3),layout,ArOrderValue.unsafe(1),design,prepared))
    val merged = value(NoiseFit.estimate(value(ArNoiseSummary.merge(left, right)),options))
    assertEquals(merged.plan.coefficients,fit.plan.coefficients)
    assertEquals(merged.corrections,fit.corrections)
    val exact = value(AcvfBias.prepare(design,layout,CorrectionBudget.Fixed(25),1))
    val incompatible = value(ArEstimation.summarizeNoise(residuals.slice(0,300,0,1),layout,ArOrderValue.unsafe(1),design,exact))
    assert(ArNoiseSummary.merge(left, incompatible).isLeft)

  test("median-relative threshold preserves a uniformly small but well-determined spectrum"):
    val map = Matrix.tabulate(20,20)((r,c) => if r != c then 0.0 else if r == 0 then 1.0 else 0.04)
    val layout = value(NoiseEstimationLayout.allRows(TimeSegments.continuous(40),40))
    val correction = value(AcvfCorrection.prepare(AcvfBiasMatrices(19,19,20,Vector(map)),layout,AcvfCorrectionSolve.ShortMemoryTail)).head.get
    assertEquals(correction.anchoredDirections,0)
    assertEquals(correction.operator,None)
