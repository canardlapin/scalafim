package scalafim.fmri.fit

import scalafim.fmri.model.{LatentSketchConfig, LatentSketchMethod, LowRankComponentSpec}
import gale.linalg.{DMat, DVec, Matrix, Vec}
import gale.spectral.{SingularOrder, SingularSelection, Svds}

final class LatentSketchPrepared private[fit] (
    val design: DesignMatrix,
    private val basis: LatentSketchBasis,
    private val latentFit: OlsFit
):
  def fitBlock(input: FitBlockInput): Either[FitError, DenseFitBlockResult] =
    for
      decoded <- basis.decodeCoefficients(latentFit.coefficients.value, input.voxelIndices)
      residualVariance = Ols.residualVariance(
        input.design.value,
        input.response.value,
        decoded.value,
        latentFit.residualDegreesOfFreedom
      )
      covariance <- basis.coefficientCovariance(latentFit.normalizedCovariance, input.voxelIndices)
      standardErrors <- basis.standardErrors(
        covariance,
        residualVariance,
        input.voxelIndices
      )
    yield
      DenseFitBlockResult(
        coefficients = decoded,
        inference = CoefficientInference.unsafeFromExisting(
          CoefficientInferenceScope.All,
          standardErrors,
          covariance,
          residualVariance,
          latentFit.residualDegreesOfFreedom
        ),
        residualVariance = residualVariance,
        residualDegreesOfFreedom = latentFit.residualDegreesOfFreedom,
        voxelIndices = input.voxelIndices,
        timepoints = input.timepoints,
        engine = scalafim.fmri.model.FitEngine.LatentSketch,
        olsDiagnostics = Some(latentFit.diagnostics)
      )

object LatentSketchPrepared:
  def prepare(
      design: DesignMatrix,
      response: ResponseBlock,
      voxelIndices: Vector[Int],
      config: LatentSketchConfig
  ): Either[FitError, LatentSketchPrepared] =
    for
      basis <- LatentSketchBasis.from(config, voxelIndices, response)
      latentResponse <- basis.project(response)
      fit <- Ols.fit(design, latentResponse)
    yield new LatentSketchPrepared(design, basis, fit)

private final class LatentSketchBasis private (
    val voxelIndices: Vector[Int],
    val loadings: DMat,
    private val rowNormSquares: Vector[Double],
    private val positionByVoxelIndex: Map[Int, Int]
):
  val components: Int =
    loadings.cols

  def project(response: ResponseBlock): Either[FitError, ResponseBlock] =
    if response.voxels != voxelIndices.length then
      Left(FitError.InvalidFitAxis("latent sketch response", s"expected ${voxelIndices.length} voxels, got ${response.voxels}"))
    else
      val out = Matrix.newBuilder(response.timepoints, components)
      var time = 0
      while time < response.timepoints do
        var component = 0
        while component < components do
          var sum = 0.0
          var voxel = 0
          while voxel < response.voxels do
            sum += response.value(time, voxel) * loadings(voxel, component)
            voxel += 1
          out(time, component) = sum
          component += 1
        time += 1
      Right(ResponseBlock.unsafe(out.result()))

  def decodeCoefficients(
      latentCoefficients: DMat,
      selectedVoxels: Vector[Int]
  ): Either[FitError, CoefficientBlock] =
    if latentCoefficients.cols != components then
      Left(FitError.InvalidFitAxis("latent coefficient columns", s"expected $components, got ${latentCoefficients.cols}"))
    else
      localPositions(selectedVoxels).map { positions =>
        val out = Matrix.newBuilder(latentCoefficients.rows, positions.length)
        var predictor = 0
        while predictor < latentCoefficients.rows do
          var local = 0
          while local < positions.length do
            val position = positions(local)
            var sum = 0.0
            var component = 0
            while component < components do
              sum += latentCoefficients(predictor, component) * loadings(position, component)
              component += 1
            out(predictor, local) = sum
            local += 1
          predictor += 1
        CoefficientBlock(out.result())
      }

  def coefficientCovariance(
      normalizedCovariance: DMat,
      selectedVoxels: Vector[Int]
  ): Either[FitError, CoefficientCovariance] =
    localPositions(selectedVoxels).flatMap { positions =>
      val matrices = positions.map { position =>
        val scale = rowNormSquares(position)
        scaleMatrix(normalizedCovariance, scale)
      }
      CoefficientCovariance.voxelwise(matrices)
    }

  def standardErrors(
      covariance: CoefficientCovariance,
      residualVariance: DVec,
      selectedVoxels: Vector[Int]
  ): Either[FitError, StandardErrorBlock] =
    if residualVariance.length != selectedVoxels.length then
      Left(FitError.InvalidFitAxis("latent sketch residual variance", s"expected ${selectedVoxels.length} voxels, got ${residualVariance.length}"))
    else
      val out = Matrix.newBuilder(covariance.predictors, selectedVoxels.length)
      var voxel = 0
      while voxel < selectedVoxels.length do
        covariance.matrixForVoxelPosition(voxel) match
          case Left(error) =>
            return Left(error)
          case Right(matrix) =>
            var predictor = 0
            while predictor < covariance.predictors do
              val variance = matrix(predictor, predictor) * residualVariance(voxel)
              out(predictor, voxel) =
                if variance < 0.0 && variance > -1e-12 then 0.0 else math.sqrt(variance)
              predictor += 1
        voxel += 1
      Right(StandardErrorBlock(out.result()))

  private def localPositions(selectedVoxels: Vector[Int]): Either[FitError, Vector[Int]] =
    val out = Vector.newBuilder[Int]
    out.sizeHint(selectedVoxels.length)
    var i = 0
    while i < selectedVoxels.length do
      positionByVoxelIndex.get(selectedVoxels(i)) match
        case Some(position) =>
          out += position
        case None =>
          return Left(FitError.InvalidFitAxis("latent sketch voxel", s"voxel ${selectedVoxels(i)} was not present during sketch preparation"))
      i += 1
    Right(out.result())

  private def scaleMatrix(matrix: DMat, scale: Double): DMat =
    Matrix.tabulate(matrix.rows, matrix.cols) { (row, col) =>
      matrix(row, col) * scale
    }

private object LatentSketchBasis:
  def from(
      config: LatentSketchConfig,
      voxelIndices: Vector[Int],
      response: ResponseBlock
  ): Either[FitError, LatentSketchBasis] =
    for
      selected <- SelectedVoxelIndices.fromInts(voxelIndices)
      _ <- validateResponseWidth(response, selected.length)
      basis <- config.method match
        case LatentSketchMethod.IdentityResponse =>
          identity(config, selected)
        case LatentSketchMethod.ContiguousVoxelAveraging =>
          contiguousAveraging(config, selected)
        case LatentSketchMethod.PrincipalComponents =>
          principalComponents(config, selected, response)
    yield basis

  private def identity(
      config: LatentSketchConfig,
      selected: SelectedVoxelIndices
  ): Either[FitError, LatentSketchBasis] =
    for
      components <- componentCount(config, selected.length, selected.length)
      _ <-
        if components == selected.length then Right(())
        else Left(FitError.InvalidFitAxis(
          "latent sketch method",
          "identity response requires one component per selected voxel"
        ))
      basis <- make(selected, DMat.eye(selected.length))
    yield basis

  private def contiguousAveraging(
      config: LatentSketchConfig,
      selected: SelectedVoxelIndices
  ): Either[FitError, LatentSketchBasis] =
    for
      components <- componentCount(config, selected.length, selected.length)
      basis <- make(selected, contiguousLoadings(selected.length, components))
    yield basis

  private def principalComponents(
      config: LatentSketchConfig,
      selected: SelectedVoxelIndices,
      response: ResponseBlock
  ): Either[FitError, LatentSketchBasis] =
    config.components match
      case LowRankComponentSpec.Full =>
        Left(FitError.InvalidFitAxis(
          "latent sketch method",
          "principal components require an explicit fixed component count"
        ))
      case LowRankComponentSpec.Fixed(count) =>
        val limit = math.min(response.timepoints, response.voxels)
        if count.value <= 0 || count.value > limit then
          Left(FitError.InvalidFitAxis("latent sketch principal components", s"rank ${count.value} must be in [1, $limit]"))
        else
          for
            svd <- Svds
              .svd(response.value, SingularSelection.Count(count.value, SingularOrder.Largest))
              .left
              .map(error => FitError.InvalidFitAxis("latent sketch principal components", error.getMessage))
            converged <- svd
              .requireConverged
              .left
              .map(error => FitError.InvalidFitAxis("latent sketch principal components", error.getMessage))
            basis <- make(selected, converged.vt.t)
          yield basis

  private def componentCount(
      config: LatentSketchConfig,
      voxels: Int,
      maximum: Int
  ): Either[FitError, Int] =
    config.components match
      case LowRankComponentSpec.Full =>
        Right(maximum)
      case LowRankComponentSpec.Fixed(count) =>
        if count.value <= maximum then Right(count.value)
        else Left(FitError.InvalidFitAxis("latent sketch components", s"requested ${count.value} components for $voxels selected voxels"))

  private def validateResponseWidth(response: ResponseBlock, voxels: Int): Either[FitError, Unit] =
    if response.voxels == voxels then Right(())
    else Left(FitError.InvalidFitAxis("latent sketch response", s"expected $voxels voxels, got ${response.voxels}"))

  private def make(
      selected: SelectedVoxelIndices,
      loadings: DMat
  ): Either[FitError, LatentSketchBasis] =
    if loadings.rows != selected.length then
      Left(FitError.InvalidFitAxis("latent sketch loadings", s"expected ${selected.length} rows, got ${loadings.rows}"))
    else if loadings.cols < 1 then
      Left(FitError.InvalidFitAxis("latent sketch loadings", "must have at least one component"))
    else
      val rowNorms = rowNormSquares(loadings)
      if rowNorms.exists(value => !value.isFinite || value < 0.0) then
        Left(FitError.InvalidFitAxis("latent sketch loadings", "row norms must be finite and non-negative"))
      else
        Right(
          new LatentSketchBasis(
            voxelIndices = selected.toVector,
            loadings = loadings,
            rowNormSquares = rowNorms,
            positionByVoxelIndex = selected.toVector.zipWithIndex.toMap
          )
        )

  private def contiguousLoadings(voxels: Int, components: Int): DMat =
    val componentByVoxel = assignContiguous(voxels, components)
    val componentSizes = sizes(componentByVoxel, components)
    val data = Matrix.newBuilder(voxels, components)
    var voxel = 0
    while voxel < voxels do
      val component = componentByVoxel(voxel)
      data(voxel, component) = 1.0 / math.sqrt(componentSizes(component).toDouble)
      voxel += 1
    data.result()

  private def assignContiguous(voxels: Int, components: Int): Vector[Int] =
    val out = Vector.newBuilder[Int]
    out.sizeHint(voxels)
    var voxel = 0
    while voxel < voxels do
      out += ((voxel.toLong * components.toLong) / voxels.toLong).toInt
      voxel += 1
    out.result()

  private def sizes(componentByVoxel: Vector[Int], components: Int): Vector[Int] =
    val out = Array.fill(components)(0)
    var voxel = 0
    while voxel < componentByVoxel.length do
      out(componentByVoxel(voxel)) += 1
      voxel += 1
    out.toVector

  private def rowNormSquares(loadings: DMat): Vector[Double] =
    val out = Array.ofDim[Double](loadings.rows)
    var row = 0
    while row < loadings.rows do
      var sum = 0.0
      var component = 0
      while component < loadings.cols do
        val value = loadings(row, component)
        sum += value * value
        component += 1
      out(row) = sum
      row += 1
    out.toVector
