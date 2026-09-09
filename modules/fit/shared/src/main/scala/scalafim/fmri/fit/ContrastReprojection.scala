package scalafim.fmri.fit

import gale.linalg.{CholeskyOptions, DMat, DVec, Matrix}

/** Why a basis-expanded product cannot be formed, or a reprojection request
  * cannot be answered.
  */
enum ReprojectionError:
  case InvalidProduct(reason: String)
  case ShapeMismatch(reason: String)
  case NonFiniteRequest(reason: String)
  case SingularReducedGram(detail: String)

  def message: String = this match
    case InvalidProduct(reason)   => reason
    case ShapeMismatch(reason)    => reason
    case NonFiniteRequest(reason) => reason
    case SingularReducedGram(detail) =>
      s"The reduced task Gram W'GW is not positive definite: $detail"

/** The per-fit retention needed for exact response-model reprojection.
  *
  * The task design is basis-expanded: `conditions` (k) event regressors each
  * carried by `basisSize` (J) response-basis columns, giving `k * J` task
  * columns ordered condition-major (`column = condition * basisSize + basis`).
  * All quantities are after nuisance partialling (Frisch–Waugh–Lovell) and any
  * whitening applied by the original fit:
  *
  *   - `gram` — the `kJ x kJ` cross-product of the partialled task design;
  *   - `crossProducts` — the `kJ x voxels` response cross-product `X'y`
  *     (equivalently `gram * bhat` for the basis-expanded OLS coefficients
  *     `bhat`; reprojection needs only this right-hand side, so it is the
  *     retained per-voxel panel);
  *   - `responseSquares` — per-voxel squared norm of the partialled response;
  *   - `residualDf` — residual degrees of freedom of the '''full''' original
  *     design (task + nuisance), owned by the fit that produced the product.
  *
  * With these retained, the OLS fit of the response on any task design whose
  * columns lie in the span of the expanded design is available in closed form
  * without rereading response data.
  */
final case class BasisExpandedFitProduct private (
    conditions: Int,
    basisSize: Int,
    gram: DMat,
    crossProducts: DMat,
    responseSquares: DVec,
    residualDf: Double):
  def taskColumns: Int = conditions * basisSize
  def voxels: Int = crossProducts.cols

object BasisExpandedFitProduct:

  def make(
      conditions: Int,
      basisSize: Int,
      gram: DMat,
      crossProducts: DMat,
      responseSquares: DVec,
      residualDf: Double
  ): Either[ReprojectionError, BasisExpandedFitProduct] =
    val taskColumns = conditions * basisSize
    if conditions < 1 then
      Left(ReprojectionError.InvalidProduct(s"A product needs at least one condition; got $conditions."))
    else if basisSize < 1 then
      Left(ReprojectionError.InvalidProduct(s"A product needs at least one basis column; got $basisSize."))
    else if gram.rows != taskColumns || gram.cols != taskColumns then
      Left(
        ReprojectionError.ShapeMismatch(
          s"The task Gram must be $taskColumns x $taskColumns; got ${gram.rows} x ${gram.cols}."
        )
      )
    else if crossProducts.rows != taskColumns then
      Left(
        ReprojectionError.ShapeMismatch(
          s"The cross-product matrix must have $taskColumns rows; got ${crossProducts.rows}."
        )
      )
    else if responseSquares.length != crossProducts.cols then
      Left(
        ReprojectionError.ShapeMismatch(
          s"responseSquares has ${responseSquares.length} entries for ${crossProducts.cols} voxels."
        )
      )
    else if !java.lang.Double.isFinite(residualDf) || residualDf <= 0.0 then
      Left(ReprojectionError.InvalidProduct(s"residualDf must be finite and positive; got $residualDf."))
    else
      Right(BasisExpandedFitProduct(conditions, basisSize, gram, crossProducts, responseSquares, residualDf))

/** Per-voxel maps produced by one reprojection request.
  *
  * `contrastVarianceScale` is `c' (W'GW)^-1 c`, the design-side factor of the
  * squared standard error; `standardError(v)` is
  * `sqrt(rss_v / residualDf * contrastVarianceScale)`. A voxel with zero
  * residual gets a zero standard error and a `NaN` t statistic rather than an
  * invented value.
  */
final case class ReprojectedContrastMaps private[fit] (
    contrast: Array[Double],
    standardError: Array[Double],
    tStatistic: Array[Double],
    contrastVarianceScale: Double,
    residualDf: Double)

/** Exact OLS reprojection of a basis-expanded fit onto a single response
  * kernel expressed in basis coordinates.
  *
  * For shared basis weights `w` (each condition keeps the same kernel) the
  * collapsed design is `X_B (I_k (x) w)`, and the closed form gives, per
  * voxel: coefficients `beta = A^-1 u` with `A = W'GW` and `u = W' X'y`,
  * contrast map `c' beta`, and residual sum of squares `y'y - u' beta`. This
  * is the OLS refit identity, not an approximation; the only approximation in
  * the explorer contract lives upstream in the basis truncation receipt.
  */
object ContrastReprojection:

  def reproject(
      product: BasisExpandedFitProduct,
      basisWeights: DVec,
      conditionContrast: DVec
  ): Either[ReprojectionError, ReprojectedContrastMaps] =
    val k = product.conditions
    val j = product.basisSize
    if basisWeights.length != j then
      Left(
        ReprojectionError.ShapeMismatch(
          s"basisWeights has ${basisWeights.length} entries for a basis of size $j."
        )
      )
    else if conditionContrast.length != k then
      Left(
        ReprojectionError.ShapeMismatch(
          s"conditionContrast has ${conditionContrast.length} entries for $k conditions."
        )
      )
    else
      val weights = new Array[Double](j)
      var finite = true
      var index = 0
      while index < j do
        val value = basisWeights(index)
        if !java.lang.Double.isFinite(value) then finite = false
        weights(index) = value
        index += 1
      val contrastVector = new Array[Double](k)
      index = 0
      while index < k do
        val value = conditionContrast(index)
        if !java.lang.Double.isFinite(value) then finite = false
        contrastVector(index) = value
        index += 1
      if !finite then
        Left(ReprojectionError.NonFiniteRequest("basisWeights and conditionContrast must be finite."))
      else
        // A = W' G W with W = I_k (x) w: A(a, b) = sum_{i, l} w_i w_l G(aJ+i, bJ+l).
        val gram = product.gram
        val reduced = DMat.tabulate(k, k) { (a, b) =>
          var sum = 0.0
          var i = 0
          while i < j do
            val wi = weights(i)
            if wi != 0.0 then
              val rowBase = a * j + i
              var l = 0
              while l < j do
                val wl = weights(l)
                if wl != 0.0 then sum += wi * wl * gram(rowBase, b * j + l)
                l += 1
            i += 1
          sum
        }
        reduced
          .cholesky(CholeskyOptions())
          .left
          .map(error => ReprojectionError.SingularReducedGram(error.toString))
          .flatMap { factor =>
            factor
              .solve(Matrix.eye(k))
              .left
              .map(error => ReprojectionError.SingularReducedGram(error.toString))
              .map { inverseMat =>
                val inverse = new Array[Double](k * k)
                var a = 0
                while a < k do
                  var b = 0
                  while b < k do
                    inverse(a * k + b) = inverseMat(a, b)
                    b += 1
                  a += 1
                var varianceScale = 0.0
                a = 0
                while a < k do
                  var b = 0
                  while b < k do
                    varianceScale += contrastVector(a) * inverse(a * k + b) * contrastVector(b)
                    b += 1
                  a += 1
                stream(product, weights, contrastVector, inverse, math.max(varianceScale, 0.0))
              }
          }

  private def stream(
      product: BasisExpandedFitProduct,
      weights: Array[Double],
      contrastVector: Array[Double],
      inverse: Array[Double],
      varianceScale: Double
  ): ReprojectedContrastMaps =
    val k = product.conditions
    val j = product.basisSize
    val voxels = product.voxels
    val crossProducts = product.crossProducts
    val responseSquares = product.responseSquares
    val df = product.residualDf
    val contrastMap = new Array[Double](voxels)
    val standardError = new Array[Double](voxels)
    val tStatistic = new Array[Double](voxels)
    val projected = new Array[Double](k)
    val solved = new Array[Double](k)
    var voxel = 0
    while voxel < voxels do
      // u = W' (X'y): u_a = sum_i w_i crossProducts(aJ + i, voxel).
      var a = 0
      while a < k do
        var sum = 0.0
        var i = 0
        val rowBase = a * j
        while i < j do
          sum += weights(i) * crossProducts(rowBase + i, voxel)
          i += 1
        projected(a) = sum
        a += 1
      a = 0
      while a < k do
        var sum = 0.0
        var b = 0
        while b < k do
          sum += inverse(a * k + b) * projected(b)
          b += 1
        solved(a) = sum
        a += 1
      var map = 0.0
      var fitted = 0.0
      a = 0
      while a < k do
        map += contrastVector(a) * solved(a)
        fitted += projected(a) * solved(a)
        a += 1
      val rss = math.max(responseSquares(voxel) - fitted, 0.0)
      val sigmaSquared = rss / df
      val se = math.sqrt(sigmaSquared * varianceScale)
      contrastMap(voxel) = map
      standardError(voxel) = se
      tStatistic(voxel) = if se > 0.0 then map / se else Double.NaN
      voxel += 1
    ReprojectedContrastMaps(contrastMap, standardError, tStatistic, varianceScale, df)
