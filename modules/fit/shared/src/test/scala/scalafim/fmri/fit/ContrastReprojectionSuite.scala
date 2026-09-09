package scalafim.fmri.fit

import gale.linalg.{DMat, DVec, Vec}

class ContrastReprojectionSuite extends munit.FunSuite:

  private val timepoints = 60
  private val conditions = 2
  private val basisSize = 3
  private val voxels = 4
  private val taskColumns = conditions * basisSize
  private val collapsedDf = (timepoints - conditions).toDouble

  /** Deterministic, well-conditioned basis-expanded design and responses. */
  private lazy val fixture: (Array[Array[Double]], Array[Array[Double]]) =
    val rng = new scala.util.Random(42)
    val design = Array.tabulate(timepoints, taskColumns) { (t, column) =>
      math.sin(0.13 * (t + 1) * (column + 1)) +
        math.cos(0.07 * (t + 2) * (column + 2)) +
        0.05 * rng.nextGaussian()
    }
    val truth = Array.tabulate(taskColumns, voxels)((column, voxel) => 0.3 * (column + 1) - 0.2 * voxel)
    val responses = Array.tabulate(voxels, timepoints) { (voxel, t) =>
      var value = 0.4 * rng.nextGaussian()
      var column = 0
      while column < taskColumns do
        value += design(t)(column) * truth(column)(voxel)
        column += 1
      value
    }
    (design, responses)

  private lazy val product: BasisExpandedFitProduct =
    val (design, responses) = fixture
    val gram = DMat.tabulate(taskColumns, taskColumns) { (a, b) =>
      var sum = 0.0
      var t = 0
      while t < timepoints do
        sum += design(t)(a) * design(t)(b)
        t += 1
      sum
    }
    val crossProducts = DMat.tabulate(taskColumns, voxels) { (column, voxel) =>
      var sum = 0.0
      var t = 0
      while t < timepoints do
        sum += design(t)(column) * responses(voxel)(t)
        t += 1
      sum
    }
    val responseSquares = DVec.tabulate(voxels) { voxel =>
      var sum = 0.0
      var t = 0
      while t < timepoints do
        sum += responses(voxel)(t) * responses(voxel)(t)
        t += 1
      sum
    }
    BasisExpandedFitProduct
      .make(conditions, basisSize, gram, crossProducts, responseSquares, rows = timepoints, nonTaskRank = 0)
      .fold(error => fail(error.message), identity)

  /** Independent ground truth: explicit OLS refit of each response on the
    * collapsed design, with the residual formed literally in the time domain.
    */
  private def directRefit(
      weights: Array[Double],
      contrast: Array[Double]
  ): (Array[Double], Array[Double], Array[Double], Double) =
    val (design, responses) = fixture
    val collapsed = Array.tabulate(timepoints, conditions) { (t, a) =>
      var sum = 0.0
      var i = 0
      while i < basisSize do
        sum += weights(i) * design(t)(a * basisSize + i)
        i += 1
      sum
    }
    var m00 = 0.0
    var m01 = 0.0
    var m11 = 0.0
    var t = 0
    while t < timepoints do
      m00 += collapsed(t)(0) * collapsed(t)(0)
      m01 += collapsed(t)(0) * collapsed(t)(1)
      m11 += collapsed(t)(1) * collapsed(t)(1)
      t += 1
    val determinant = m00 * m11 - m01 * m01
    val inverse = Array(m11 / determinant, -m01 / determinant, -m01 / determinant, m00 / determinant)
    val varianceScale =
      contrast(0) * (inverse(0) * contrast(0) + inverse(1) * contrast(1)) +
        contrast(1) * (inverse(2) * contrast(0) + inverse(3) * contrast(1))
    val maps = new Array[Double](voxels)
    val ses = new Array[Double](voxels)
    val ts = new Array[Double](voxels)
    var voxel = 0
    while voxel < voxels do
      var u0 = 0.0
      var u1 = 0.0
      t = 0
      while t < timepoints do
        u0 += collapsed(t)(0) * responses(voxel)(t)
        u1 += collapsed(t)(1) * responses(voxel)(t)
        t += 1
      val beta0 = inverse(0) * u0 + inverse(1) * u1
      val beta1 = inverse(2) * u0 + inverse(3) * u1
      var rss = 0.0
      t = 0
      while t < timepoints do
        val residual = responses(voxel)(t) - collapsed(t)(0) * beta0 - collapsed(t)(1) * beta1
        rss += residual * residual
        t += 1
      val map = contrast(0) * beta0 + contrast(1) * beta1
      val se = math.sqrt(rss / collapsedDf * varianceScale)
      maps(voxel) = map
      ses(voxel) = se
      ts(voxel) = map / se
      voxel += 1
    (maps, ses, ts, varianceScale)

  test("reprojection equals an independent direct OLS refit on the collapsed design") {
    val weights = Array(0.9, 0.4, -0.2)
    val contrast = Array(1.0, -1.0)
    val result = ContrastReprojection
      .reproject(product, Vec(weights*), Vec(contrast*))
      .fold(error => fail(error.message), identity)
    val (maps, ses, ts, varianceScale) = directRefit(weights, contrast)
    assertEqualsDouble(result.contrastVarianceScale, varianceScale, 1e-12)
    assertEqualsDouble(result.residualDf, collapsedDf, 1e-12)
    var voxel = 0
    while voxel < voxels do
      assertEqualsDouble(result.contrast(voxel), maps(voxel), 1e-9, s"map voxel $voxel")
      assertEqualsDouble(result.standardError(voxel), ses(voxel), 1e-9, s"se voxel $voxel")
      assertEqualsDouble(result.tStatistic(voxel), ts(voxel), 1e-7, s"t voxel $voxel")
      voxel += 1
  }

  test("a second kernel from the same product also matches its refit") {
    val weights = Array(0.2, -0.7, 1.1)
    val contrast = Array(0.5, 0.5)
    val result = ContrastReprojection
      .reproject(product, Vec(weights*), Vec(contrast*))
      .fold(error => fail(error.message), identity)
    val (maps, ses, _, _) = directRefit(weights, contrast)
    var voxel = 0
    while voxel < voxels do
      assertEqualsDouble(result.contrast(voxel), maps(voxel), 1e-9, s"map voxel $voxel")
      assertEqualsDouble(result.standardError(voxel), ses(voxel), 1e-9, s"se voxel $voxel")
      voxel += 1
  }

  test("the product constructor refuses malformed retention") {
    BasisExpandedFitProduct.make(
      conditions, basisSize,
      DMat.zeros(taskColumns, taskColumns + 1),
      product.crossProducts, product.responseSquares, timepoints, 0
    ) match
      case Left(ReprojectionError.ShapeMismatch(_)) => ()
      case other => fail(s"expected ShapeMismatch, got $other")
    BasisExpandedFitProduct.make(
      conditions, basisSize, product.gram, product.crossProducts,
      DVec.zeros(voxels + 1), timepoints, 0
    ) match
      case Left(ReprojectionError.ShapeMismatch(_)) => ()
      case other => fail(s"expected ShapeMismatch, got $other")
    BasisExpandedFitProduct.make(
      conditions, basisSize, product.gram, product.crossProducts,
      product.responseSquares, rows = 2, nonTaskRank = 1
    ) match
      case Left(ReprojectionError.InvalidProduct(_)) => ()
      case other => fail(s"expected InvalidProduct, got $other")
    BasisExpandedFitProduct.make(
      0, basisSize, product.gram, product.crossProducts,
      product.responseSquares, timepoints, 0
    ) match
      case Left(ReprojectionError.InvalidProduct(_)) => ()
      case other => fail(s"expected InvalidProduct, got $other")
  }

  test("reprojection refuses malformed and degenerate requests") {
    ContrastReprojection.reproject(product, Vec(1.0, 0.0), Vec(1.0, -1.0)) match
      case Left(ReprojectionError.ShapeMismatch(_)) => ()
      case other => fail(s"expected ShapeMismatch, got $other")
    ContrastReprojection.reproject(product, Vec(1.0, 0.0, 0.0), Vec(1.0, -1.0, 0.0)) match
      case Left(ReprojectionError.ShapeMismatch(_)) => ()
      case other => fail(s"expected ShapeMismatch, got $other")
    ContrastReprojection.reproject(product, Vec(0.0, 0.0, 0.0), Vec(1.0, -1.0)) match
      case Left(ReprojectionError.SingularReducedGram(_)) => ()
      case other => fail(s"expected SingularReducedGram, got $other")
    ContrastReprojection.reproject(product, Vec(Double.NaN, 0.0, 1.0), Vec(1.0, -1.0)) match
      case Left(ReprojectionError.NonFiniteRequest(_)) => ()
      case other => fail(s"expected NonFiniteRequest, got $other")
  }
