package scalafim.fmri.fit

import gale.linalg.{CholeskyOptions, DMat, Matrix, TriangularSolve, Vec}

/** Response-independent temporal geometry for a full-rank contrast subspace.
  *
  * If `W` is the p-by-q aligned contrast matrix and `K = (X'X)^-1`, then
  * `effectBasis = K W L^-T`, where `L L' = W' K W`. Consequently the basis is
  * normalized in the design metric and any nonsingular change of contrast
  * basis induces only an orthogonal rotation of the resulting effect scores.
  */
final class PreparedManovaGeometry private[fit] (
    val preparedDesign: DesignMatrix,
    val alignedContrast: AlignedFContrast,
    val designCrossproduct: DMat,
    val inverseXtX: DMat,
    val effectBasis: DMat,
    val contrastCovariance: DMat,
    val receipt: TemporalPreparationReceipt,
    private[fit] val whitening: CanonicalTemporalWhitening
) extends PreparedCanonicalGeometry:
  require(effectBasis.rows == preparedDesign.predictors, "MANOVA effect basis must match design predictors")
  require(effectBasis.cols == alignedContrast.rank, "MANOVA effect basis must match contrast rank")
  require(contrastCovariance.rows == alignedContrast.rank, "MANOVA contrast covariance must match contrast rank")
  require(contrastCovariance.rows == contrastCovariance.cols, "MANOVA contrast covariance must be square")

  def contrastRank: Int = alignedContrast.rank

object PreparedManovaGeometry:
  private[fit] def compile(
      preparation: ResponsePreparationPlan,
      design: DesignMatrix,
      columnNames: Vector[String],
      contrast: FContrast,
      selectedTimepoints: SelectedTimepointIndices,
      partitions: Vector[RunPartition],
      nuisanceRank: TemporalNuisanceRank,
      scope: TemporalPreparationScope,
      whitening: CanonicalTemporalWhitening,
      solvePolicy: OlsSolvePolicy
  ): Either[FitError, PreparedManovaGeometry] =
    for
      _ <- PreparedContrastGeometry.validateAxis(design, columnNames, selectedTimepoints, partitions, nuisanceRank)
      _ <- PreparedContrastGeometry.validatePreparation(preparation, design, partitions, scope, whitening)
      aligned <- contrast.align(columnNames)
      preparedDesign <- PreparedContrastGeometry.prepareDesign(design, whitening)
      prepared <- Ols.prepare(preparedDesign, solvePolicy)
      residualDf <- ResidualDegreesOfFreedom(preparedDesign.timepoints - prepared.diagnostics.rank)
      normalized <- normalizedEffectBasis(prepared.normalizedCovariance, aligned)
      (basis, covariance) = normalized
      provenance = PreparedContrastGeometry.compiledProvenance(preparation, whitening)
    yield
      new PreparedManovaGeometry(
        preparedDesign = preparedDesign,
        alignedContrast = aligned,
        designCrossproduct = prepared.crossproduct,
        inverseXtX = prepared.normalizedCovariance,
        effectBasis = basis,
        contrastCovariance = covariance,
        receipt = TemporalPreparationReceipt(
          selectedTimepoints = selectedTimepoints,
          scope = scope,
          provenance = provenance,
          nuisanceRank = nuisanceRank,
          designRank = prepared.diagnostics.rank,
          contrastRank = aligned.rank,
          contrastName = aligned.name,
          residualDegreesOfFreedom = residualDf,
          whitening = PreparedContrastGeometry.whiteningReceipt(whitening)
        ),
        whitening = whitening
      )

  private def normalizedEffectBasis(
      inverseXtX: DMat,
      contrast: AlignedFContrast
  ): Either[FitError, (DMat, DMat)] =
    val unscaled = multiply(inverseXtX, contrast.weights)
    val covariance = crossproduct(contrast.weights, unscaled)
    covariance
      .cholesky(CholeskyOptions(choleskyTolerance(covariance)))
      .left
      .map(error => FitError.NonEstimableContrast(contrast.name, error.getMessage))
      .flatMap: factor =>
        normalizeRows(unscaled, factor.lower, contrast.name).map(_ -> covariance)

  private def normalizeRows(
      unscaled: DMat,
      lower: DMat,
      contrastName: String
  ): Either[FitError, DMat] =
    val basis = Matrix.newBuilder(unscaled.rows, unscaled.cols)
    var predictor = 0
    while predictor < unscaled.rows do
      val rhs = Vec.newBuilder(unscaled.cols)
      var contrastIndex = 0
      while contrastIndex < unscaled.cols do
        rhs(contrastIndex) = unscaled(predictor, contrastIndex)
        contrastIndex += 1
      TriangularSolve.lower(lower, rhs.result()) match
        case Left(error) =>
          return Left(FitError.NonEstimableContrast(contrastName, error.getMessage))
        case Right(normalized) =>
          contrastIndex = 0
          while contrastIndex < unscaled.cols do
            basis(predictor, contrastIndex) = normalized(contrastIndex)
            contrastIndex += 1
      predictor += 1
    Right(basis.result())

  private def multiply(left: DMat, right: DMat): DMat =
    require(left.cols == right.rows, "MANOVA basis product dimensions must agree")
    val out = Matrix.newBuilder(left.rows, right.cols)
    var row = 0
    while row < left.rows do
      var col = 0
      while col < right.cols do
        var value = 0.0
        var inner = 0
        while inner < left.cols do
          value += left(row, inner) * right(inner, col)
          inner += 1
        out(row, col) = value
        col += 1
      row += 1
    out.result()

  private def crossproduct(left: DMat, right: DMat): DMat =
    require(left.rows == right.rows, "MANOVA crossproduct row dimensions must agree")
    val out = Matrix.newBuilder(left.cols, right.cols)
    var row = 0
    while row < left.cols do
      var col = 0
      while col < right.cols do
        var value = 0.0
        var inner = 0
        while inner < left.rows do
          value += left(inner, row) * right(inner, col)
          inner += 1
        out(row, col) = value
        col += 1
      row += 1
    out.result()

  private def choleskyTolerance(matrix: DMat): Double =
    var diagonalMax = 0.0
    var index = 0
    while index < matrix.rows do
      diagonalMax = math.max(diagonalMax, math.abs(matrix(index, index)))
      index += 1
    diagonalMax * 1e-12
