package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import gale.backend.{Backend, BackendConfig, BackendThresholds, Capability, DenseDoubleKernel, PureDenseDoubleKernel}
import gale.platform.DoubleArray

class OlsEstimatesSuite extends munit.FunSuite:
  private def right[A](value: Either[FitError, A]): A =
    value.fold(error => fail(error.message), identity)

  private val design = Matrix(7, 3)(
    1, 0, 2, 1, 1, -1, 1, 2, 3, 1, 3, 0, 1, 4, 1, 1, 5, -2, 1, 6, 2
  )
  private val response = Matrix(7, 2)(3, 1, 4, 0, 9, 2, 7, -1, 10, 3, 9, 1, 15, 4)
  private val readout = Matrix(2, 3)(0, 1, 0, 1, -1, 2)

  private def prepare(
      d: DMat = design,
      l: DMat = readout,
      uncertainty: EstimateUncertaintyRequest = EstimateUncertaintyRequest.None
  ): OlsEstimatePlan =
    right(Ols.prepareEstimates(DesignMatrix.unsafe(d), OlsEstimateRequest(right(CoefficientReadout.fromMatrix(l)), uncertainty)))

  private def close(actual: DMat, expected: DMat, tolerance: Double = 1e-11): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    for row <- 0 until actual.rows; col <- 0 until actual.cols do
      assertEqualsDouble(actual(row, col), expected(row, col), tolerance)

  test("compiled estimator preserves selected functionals and removes nuisance contributions") {
    val plan = prepare()
    close(plan.operator * design, readout)
    val betas = Matrix(3, 2)(3, -2, 0.7, 1.4, 50, -90)
    val fit = right(plan.estimate(ResponseBlock.unsafe(design * betas)))
    close(fit.estimates, readout * betas)
    assertEquals(fit.uncertainty, OlsEstimateUncertainty.NotRequested)
    assertEquals(plan.operator.rows, 2)
    assertEquals(plan.operator.cols, 7)
    val selected = right(CoefficientReadout.coefficients(3, Vector(1)))
    val selectedPlan = right(Ols.prepareEstimates(DesignMatrix.unsafe(design), OlsEstimateRequest(selected)))
    close(selectedPlan.operator * design, Matrix(1, 3)(0, 1, 0))
  }

  test("selected estimates and joint uncertainty match an independent exact-rational oracle") {
    // Generated with Python Fraction Gaussian elimination on integer input.
    // The fixture does not use the QR/operator implementation under test.
    val fit = right(prepare(uncertainty = EstimateUncertaintyRequest.Joint).estimate(ResponseBlock.unsafe(response)))
    close(fit.estimates, Matrix(2, 2)(835.0 / 462, 159.0 / 308, 1915.0 / 924, 47.0 / 308))
    val full = Ols.unsafeFit(DesignMatrix.unsafe(design), ResponseBlock.unsafe(response))
    close(fit.estimates, readout * full.coefficients.value)
    fit.uncertainty match
      case OlsEstimateUncertainty.Joint(covariance, errors, variance, df) =>
        close(covariance, Matrix(2, 2)(17.0 / 462, -127.0 / 924, -127.0 / 924, 685.0 / 924))
        close(covariance, readout * full.normalizedCovariance * readout.t)
        assertEqualsDouble(variance(0), 1219.0 / 1848, 1e-12)
        assertEqualsDouble(variance(1), 1685.0 / 1232, 1e-12)
        assertEquals(df.value, 4)
        for voxel <- 0 until 2 do
          assertEqualsDouble(variance(voxel), full.residualVariance(voxel), 1e-12)
          for output <- 0 until 2 do
            assertEqualsDouble(errors(output, voxel), math.sqrt(covariance(output, output) * variance(voxel)), 1e-12)
      case other => fail(s"Expected joint uncertainty, got $other")
  }

  test("marginal uncertainty agrees with joint readout without a joint covariance product") {
    val marginal = right(prepare(uncertainty = EstimateUncertaintyRequest.Marginal).estimate(ResponseBlock.unsafe(response)))
    val joint = right(prepare(uncertainty = EstimateUncertaintyRequest.Joint).estimate(ResponseBlock.unsafe(response)))
    (marginal.uncertainty, joint.uncertainty) match
      case (OlsEstimateUncertainty.Marginal(a, _, _), OlsEstimateUncertainty.Joint(_, b, _, _)) => close(a.value, b.value)
      case other => fail(s"Unexpected products: $other")
  }

  test("uncertainty execution reuses residual coordinates for its only selected product") {
    var products = Vector.empty[(Int, Int, Int)]
    val kernels = new DenseDoubleKernel:
      export PureDenseDoubleKernel.{dot, nrm2, copy, axpy, scal, gemv, syrk}
      def gemm(
          rows: Int, cols: Int, shared: Int, alpha: Double,
          a: DoubleArray, aOffset: Int, aRowStride: Int, aColStride: Int,
          b: DoubleArray, bOffset: Int, bRowStride: Int, bColStride: Int,
          beta: Double, c: DoubleArray, cOffset: Int, cRowStride: Int, cColStride: Int
      ): Unit =
        assertEquals(aColStride, 1, "prepared readout must admit the row-major vector kernel")
        products :+= ((rows, shared, cols))
        PureDenseDoubleKernel.gemm(rows, cols, shared, alpha, a, aOffset, aRowStride, aColStride,
          b, bOffset, bRowStride, bColStride, beta, c, cOffset, cRowStride, cColStride)
    given Backend = new Backend:
      val name = "observed-portable-kernel"
      val capabilities = Set(Capability.Vectorized)
      val config = BackendConfig.singleThreaded
      val thresholds = new BackendThresholds:
        def nativeGemmMinFlops: Long = 0L
        def nativeGemvMinWork: Long = 0L
        def nativeFactorizationMinSize: Int = Int.MaxValue
      val denseDouble = kernels
    val responseBlock = ResponseBlock.unsafe(response)
    val estimatesOnly = prepare()
    val joint = prepare(uncertainty = EstimateUncertaintyRequest.Joint)
    val expected = right(estimatesOnly.estimate(responseBlock)).estimates
    assertEquals(products, Vector((2, 7, 2)))
    products = Vector.empty
    val actual = right(joint.estimate(responseBlock)).estimates
    assertEquals(products, Vector((2, 3, 2)))
    close(actual, expected)
  }

  test("column reordering and scaling preserve the same estimand") {
    val permutation = Vector(2, 0, 1)
    val scales = Vector(0.1, 7.0, 3.0)
    val changedDesign = Matrix.tabulate(7, 3)((row, col) => design(row, permutation(col)) * scales(col))
    val changedReadout = Matrix.tabulate(2, 3)((row, col) => readout(row, permutation(col)) * scales(col))
    val original = right(prepare().estimate(ResponseBlock.unsafe(response)))
    val changed = right(prepare(changedDesign, changedReadout).estimate(ResponseBlock.unsafe(response)))
    close(changed.estimates, original.estimates)
  }

  test("strided response blocks and repeated execution preserve input and output ownership") {
    val plan = prepare()
    val first = right(plan.estimate(ResponseBlock.unsafe(response.slice(0, 7, 0, 1))))
    val second = right(plan.estimate(ResponseBlock.unsafe(response.slice(0, 7, 1, 2))))
    val whole = right(plan.estimate(ResponseBlock.unsafe(response)))
    close(first.estimates, whole.estimates.slice(0, 2, 0, 1))
    close(second.estimates, whole.estimates.slice(0, 2, 1, 2))
    close(right(plan.estimate(ResponseBlock.unsafe(response))).estimates, whole.estimates)
    close(plan.operator * design, readout)
  }

  test("fixed row selection and temporal transform fuse through the readout adjoint") {
    val rows = Vector(0, 1, 3, 5, 6)
    // A non-square fixed transform both selects scans and applies a lag term.
    val transform = Matrix.tabulate(5, 7) { (row, col) =>
      (if col == rows(row) then 1.0 else 0.0) -
        (if row > 0 && col == rows(row - 1) then 0.4 else 0.0)
    }
    val plan = prepare(transform * design)
    val direct = right(plan.estimate(ResponseBlock.unsafe(transform * response)))
    val fused = (plan.operator * transform) * response
    close(fused, direct.estimates)
    close((plan.operator * transform) * design, readout)
  }

  test("exactly determined coefficients do not require residual degrees of freedom") {
    val d = Matrix(2, 2)(1, 0, 1, 2)
    val request = OlsEstimateRequest(right(CoefficientReadout.coefficients(2, Vector(1))))
    val plan = right(Ols.prepareEstimates(DesignMatrix.unsafe(d), request))
    close(right(plan.estimate(ResponseBlock.unsafe(Matrix(2, 1)(3, 7)))).estimates, Matrix(1, 1)(2))
    assertEquals(Ols.prepareEstimates(DesignMatrix.unsafe(d), request.copy(uncertainty = EstimateUncertaintyRequest.Joint)),
      Left(FitError.NonPositiveResidualDegreesOfFreedom(0)))
  }

  test("invalid readouts, incompatible rows, rank defects and unsupported solve policies fail explicitly") {
    assert(CoefficientReadout.coefficients(3, Vector.empty).isLeft)
    assert(CoefficientReadout.coefficients(3, Vector(1, 1)).isLeft)
    assert(CoefficientReadout.coefficients(3, Vector(3)).isLeft)
    assert(CoefficientReadout.fromMatrix(Matrix(1, 1)(Double.NaN)).isLeft)
    assert(Ols.prepareEstimates(DesignMatrix.unsafe(design), OlsEstimateRequest(right(CoefficientReadout.coefficients(2, Vector(1))))).isLeft)
    assertEquals(prepare().estimate(ResponseBlock.unsafe(Matrix(1, 1)(1))), Left(FitError.RowMismatch(7, 1)))
    val aliased = Matrix.tabulate(7, 3)((row, col) => if col == 2 then design(row, 1) else design(row, col))
    assert(Ols.prepareEstimates(DesignMatrix.unsafe(aliased), OlsEstimateRequest(right(CoefficientReadout.fromMatrix(readout)))).isLeft)
    assert(Ols.prepareEstimates(DesignMatrix.unsafe(design), OlsEstimateRequest(right(CoefficientReadout.fromMatrix(readout))), OlsSolvePolicy.NormalEquations).isLeft)
  }
