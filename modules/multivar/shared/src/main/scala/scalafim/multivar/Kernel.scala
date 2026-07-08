package scalafim.multivar

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

final case class KernelSpec(name: String, parameters: Map[String, Double] = Map.empty):
  require(name.nonEmpty, "kernel name must be non-empty")

trait Kernel:
  def spec: KernelSpec

  def compute(left: MatrixView, right: MatrixView): Either[MultivarError, DoubleMatrix]

final case class LinearKernel() extends Kernel:
  override def spec: KernelSpec =
    KernelSpec("linear")

  override def compute(left: MatrixView, right: MatrixView): Either[MultivarError, DoubleMatrix] =
    if left.cols != right.cols then
      Left(MultivarError.MatrixShapeMismatch(s"linear kernel expected equal feature counts, got ${left.cols} and ${right.cols}"))
    else
      for
        rightDense <- right.toDense(StoragePolicy.AllowDense)
        out <- left.rightMultiply(rightDense.transpose)
        _ <- MatrixOps.checkFinite("linear kernel matrix", out)
      yield out

final case class RbfKernel(gamma: Double) extends Kernel:
  require(gamma.isFinite && gamma > 0.0, "RBF gamma must be finite and positive")

  override def spec: KernelSpec =
    KernelSpec("rbf", Map("gamma" -> gamma))

  override def compute(left: MatrixView, right: MatrixView): Either[MultivarError, DoubleMatrix] =
    if left.cols != right.cols then
      Left(MultivarError.MatrixShapeMismatch(s"RBF kernel expected equal feature counts, got ${left.cols} and ${right.cols}"))
    else
      for
        leftDense <- left.toDense(StoragePolicy.AllowDense)
        rightDense <- right.toDense(StoragePolicy.AllowDense)
        _ <- MatrixOps.checkFinite("RBF left input", leftDense)
        _ <- MatrixOps.checkFinite("RBF right input", rightDense)
      yield
        val out = new Array[Double](leftDense.rows * rightDense.rows)
        var row = 0
        while row < leftDense.rows do
          var other = 0
          while other < rightDense.rows do
            var col = 0
            var d2 = 0.0
            while col < leftDense.cols do
              val diff = leftDense(row, col) - rightDense(other, col)
              d2 += diff * diff
              col += 1
            out(row * rightDense.rows + other) = Math.exp(-gamma * Math.max(d2, 0.0))
            other += 1
          row += 1
        DoubleMatrix.unsafe(leftDense.rows, rightDense.rows, out)

object Kernel:
  val linear: Kernel =
    LinearKernel()

enum KernelCentering:
  case Uncentered
  case InputPreprocessed

enum KernelNormalization:
  case None
  case StandardNystromScaling
  case DoubleNystromScaling

enum NystromMethod:
  case Standard
  case DoubleNystrom(intermediateRank: ComponentCount)

  def label: String =
    this match
      case Standard                 => "standard"
      case DoubleNystrom(_)         => "double"

final case class LandmarkSet private (indices: Vector[Int]):
  require(indices.nonEmpty, "landmarks must be non-empty")

  def length: Int =
    indices.length

object LandmarkSet:
  def from(indices: Iterable[Int], rows: Int): Either[MultivarError, LandmarkSet] =
    val canonical = indices.toVector.distinct.sorted
    if rows <= 0 then Left(MultivarError.InvalidDimension("landmark row count", rows))
    else if canonical.isEmpty then Left(MultivarError.EmptyIndexSet(IndexAxis.Row))
    else
      var i = 0
      var error = Option.empty[MultivarError]
      while i < canonical.length && error.isEmpty do
        val index = canonical(i)
        if index < 0 || index >= rows then error = Some(MultivarError.IndexOutOfBounds(IndexAxis.Row, index, rows))
        i += 1
      error match
        case Some(value) => Left(value)
        case None        => Right(new LandmarkSet(canonical))

final case class KernelEigenArtifact(
    eigenvectors: DoubleMatrix,
    eigenvalues: DoubleVector,
    standardDeviations: DoubleVector,
    scores: DoubleMatrix
):
  require(eigenvectors.cols == eigenvalues.length, "kernel eigenvectors must match eigenvalue count")
  require(standardDeviations.length == eigenvalues.length, "kernel standard deviations must match eigenvalue count")
  require(scores.rows == eigenvectors.rows, "kernel scores and eigenvectors must have equal rows")
  require(scores.cols == eigenvectors.cols, "kernel scores and eigenvectors must have equal columns")

  def components: Int =
    eigenvalues.length

final case class NystromDiagnostics(
    method: NystromMethod,
    requestedComponents: ComponentCount,
    effectiveComponents: Int,
    centering: KernelCentering,
    normalization: KernelNormalization
)

sealed trait NystromState:
  def scoreWeights: DoubleMatrix

final case class StandardNystromState(
    lambdaLandmark: DoubleVector,
    landmarkEigenvectors: DoubleMatrix,
    scoreWeights: DoubleMatrix
) extends NystromState:
  require(lambdaLandmark.length == landmarkEigenvectors.cols, "standard Nyström eigenvalue/eigenvector mismatch")
  require(scoreWeights.rows == landmarkEigenvectors.rows, "standard Nyström score weights must be landmark x component")

final case class DoubleNystromState(
    firstStageEigenvectors: DoubleMatrix,
    firstStageInvSqrtEigenvalues: DoubleMatrix,
    secondStageEigenvectors: DoubleMatrix,
    secondStageInvSqrtEigenvalues: DoubleMatrix,
    scoreWeights: DoubleMatrix
) extends NystromState:
  require(scoreWeights.rows == firstStageEigenvectors.rows, "double Nyström score weights must be landmark x component")

final case class NystromFit(
    kernel: KernelSpec,
    method: NystromMethod,
    landmarks: LandmarkSet,
    landmarkData: DoubleMatrix,
    preprocessor: FittedPreprocessor,
    originalCols: Int,
    centering: KernelCentering,
    normalization: KernelNormalization,
    eigen: KernelEigenArtifact,
    diagnostics: NystromDiagnostics,
    state: NystromState,
    kernelFunction: Kernel
):
  require(landmarkData.rows == landmarks.length, "landmark data rows must match landmarks")
  require(originalCols > 0, "Nyström original feature count must be positive")

  def transform(newData: MatrixView): Either[MultivarError, DoubleMatrix] =
    if newData.cols != originalCols then
      Left(MultivarError.MatrixShapeMismatch(s"Nyström transform expected $originalCols columns, got ${newData.cols}"))
    else
      for
        processed <- preprocessor.transform(newData)
        kNew <- Nystrom.computeKernel(kernelFunction, processed, MatrixView.dense(landmarkData), "Nyström out-of-sample kernel")
      yield DoubleMatrix.multiply(kNew, state.scoreWeights)

object Nystrom:
  def fit(
      input: MatrixView,
      components: ComponentCount,
      landmarks: Iterable[Int],
      kernel: Kernel = Kernel.linear,
      preproc: PreprocessSpec = PreprocessSpec.Pass,
      method: NystromMethod = NystromMethod.Standard,
      centering: KernelCentering = KernelCentering.Uncentered,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      tolerance: Double = 1e-12
  ): Either[MultivarError, NystromFit] =
    if input.rows <= 0 then Left(MultivarError.InvalidDimension("Nyström input rows", input.rows))
    else if input.cols <= 0 then Left(MultivarError.InvalidDimension("Nyström input columns", input.cols))
    else
      for
        landmarkSet <- LandmarkSet.from(landmarks, input.rows)
        _ <-
          if components.value <= landmarkSet.length then Right(())
          else Left(MultivarError.InvalidComponentRequest(components.value, landmarkSet.length))
        fitted <- preproc.fit(input)
        processedView <- fitted.transform(input)
        processed <- processedView.toDense(StoragePolicy.AllowDense)
        _ <- MatrixOps.checkFinite("Nyström input", processed)
        landmarkData = RowGeometryOps.selectRows(processed, landmarkSet.indices)
        landmarkView = MatrixView.dense(landmarkData)
        kMm <- computeKernel(kernel, landmarkView, landmarkView, "Nyström landmark kernel")
        fit <- method match
          case NystromMethod.Standard =>
            fitStandard(
              inputRows = input.rows,
              components = components,
              landmarkSet = landmarkSet,
              landmarkData = landmarkData,
              processed = processed,
              fitted = fitted,
              kernel = kernel,
              kMm = kMm,
              centering = centering,
              eigenSolver = eigenSolver,
              tolerance = tolerance
            )
          case NystromMethod.DoubleNystrom(intermediateRank) =>
            fitDouble(
              inputRows = input.rows,
              components = components,
              intermediateRank = intermediateRank,
              landmarkSet = landmarkSet,
              landmarkData = landmarkData,
              processed = processed,
              fitted = fitted,
              kernel = kernel,
              kMm = kMm,
              centering = centering,
              eigenSolver = eigenSolver,
              tolerance = tolerance
            )
      yield fit

  private def fitStandard(
      inputRows: Int,
      components: ComponentCount,
      landmarkSet: LandmarkSet,
      landmarkData: DoubleMatrix,
      processed: DoubleMatrix,
      fitted: FittedPreprocessor,
      kernel: Kernel,
      kMm: DoubleMatrix,
      centering: KernelCentering,
      eigenSolver: SymmetricEigenSolver,
      tolerance: Double
  ): Either[MultivarError, NystromFit] =
    for
      eigen <- eigenSolver.decompose(kMm)
      keep = positiveEigenCount(eigen.values, components.value, tolerance)
      fit <-
        if keep == 0 then Left(MultivarError.InvalidKernelFit("Nyström landmark kernel has no positive eigenvalues"))
        else
          val lambdaMm = MatrixOps.takeVector(eigen.values, keep)
          val uMm = MatrixOps.takeColumns(eigen.vectors, keep)
          val scale = Math.sqrt(landmarkSet.length.toDouble / inputRows.toDouble)
          val eigenWeights = scaleColumns(uMm, reciprocal(lambdaMm, scale))
          val eigenvalues = scaleVector(lambdaMm, inputRows.toDouble / landmarkSet.length.toDouble)
          val sdev = sqrtVector(eigenvalues)
          val scoreWeights = scaleColumns(eigenWeights, sdev)
          buildFit(
            inputRows,
            components,
            landmarkSet,
            landmarkData,
            processed,
            fitted,
            kernel,
            NystromMethod.Standard,
            centering,
            KernelNormalization.StandardNystromScaling,
            eigenvalues,
            sdev,
            eigenWeights,
            StandardNystromState(lambdaMm, uMm, scoreWeights),
            scoreWeights
          )
    yield fit

  private def fitDouble(
      inputRows: Int,
      components: ComponentCount,
      intermediateRank: ComponentCount,
      landmarkSet: LandmarkSet,
      landmarkData: DoubleMatrix,
      processed: DoubleMatrix,
      fitted: FittedPreprocessor,
      kernel: Kernel,
      kMm: DoubleMatrix,
      centering: KernelCentering,
      eigenSolver: SymmetricEigenSolver,
      tolerance: Double
  ): Either[MultivarError, NystromFit] =
    if intermediateRank.value > landmarkSet.length then
      Left(MultivarError.InvalidComponentRequest(intermediateRank.value, landmarkSet.length))
    else
      for
        first <- eigenSolver.decompose(kMm)
        firstKeep = positiveEigenCount(first.values, intermediateRank.value, tolerance)
        fit <-
          if firstKeep == 0 then Left(MultivarError.InvalidKernelFit("double Nyström first stage has no positive eigenvalues"))
          else
            val vSL = MatrixOps.takeColumns(first.vectors, firstKeep)
            val lambdaL = MatrixOps.takeVector(first.values, firstKeep)
            val invSqrtLambdaL = MatrixOps.diagonal(inverseSqrt(lambdaL))
            val firstWeights = DoubleMatrix.multiply(vSL, invSqrtLambdaL)
            for
              cAll <- computeKernel(kernel, MatrixView.dense(processed), MatrixView.dense(landmarkData), "double Nyström all-landmark kernel")
              w = DoubleMatrix.multiply(cAll, firstWeights)
              kW = DoubleMatrix.crossProduct(w)
              second <- eigenSolver.decompose(kW)
              finalRequest = Math.min(components.value, firstKeep)
              secondKeep = positiveEigenCount(second.values, finalRequest, tolerance)
              out <-
                if secondKeep == 0 then Left(MultivarError.InvalidKernelFit("double Nyström second stage has no positive eigenvalues"))
                else
                  val lambdaK = MatrixOps.takeVector(second.values, secondKeep)
                  val vK = MatrixOps.takeColumns(second.vectors, secondKeep)
                  val invSqrtLambdaK = MatrixOps.diagonal(inverseSqrt(lambdaK))
                  val eigenWeights = DoubleMatrix.multiply(firstWeights, DoubleMatrix.multiply(vK, invSqrtLambdaK))
                  val sdev = sqrtVector(lambdaK)
                  val scoreWeights = scaleColumns(eigenWeights, sdev)
                  buildFit(
                    inputRows,
                    components,
                    landmarkSet,
                    landmarkData,
                    processed,
                    fitted,
                    kernel,
                    NystromMethod.DoubleNystrom(intermediateRank),
                    centering,
                    KernelNormalization.DoubleNystromScaling,
                    lambdaK,
                    sdev,
                    eigenWeights,
                    DoubleNystromState(vSL, invSqrtLambdaL, vK, invSqrtLambdaK, scoreWeights),
                    scoreWeights
                  )
            yield out
      yield fit

  private def buildFit(
      inputRows: Int,
      requestedComponents: ComponentCount,
      landmarkSet: LandmarkSet,
      landmarkData: DoubleMatrix,
      processed: DoubleMatrix,
      fitted: FittedPreprocessor,
      kernel: Kernel,
      method: NystromMethod,
      centering: KernelCentering,
      normalization: KernelNormalization,
      eigenvalues: DoubleVector,
      sdev: DoubleVector,
      eigenWeights: DoubleMatrix,
      state: NystromState,
      scoreWeights: DoubleMatrix
  ): Either[MultivarError, NystromFit] =
    for
      cAll <- computeKernel(kernel, MatrixView.dense(processed), MatrixView.dense(landmarkData), "Nyström all-landmark kernel")
      eigenvectors = DoubleMatrix.multiply(cAll, eigenWeights)
      scores = DoubleMatrix.multiply(cAll, scoreWeights)
      _ <- MatrixOps.checkFinite("Nyström eigenvectors", eigenvectors)
      _ <- MatrixOps.checkFinite("Nyström scores", scores)
    yield
      val artifact = KernelEigenArtifact(eigenvectors, eigenvalues, sdev, scores)
      val diagnostics = NystromDiagnostics(
        method = method,
        requestedComponents = requestedComponents,
        effectiveComponents = artifact.components,
        centering = centering,
        normalization = normalization
      )
      NystromFit(
        kernel = kernel.spec,
        method = method,
        landmarks = landmarkSet,
        landmarkData = landmarkData,
        preprocessor = fitted,
        originalCols = processed.cols,
        centering = centering,
        normalization = normalization,
        eigen = artifact,
        diagnostics = diagnostics,
        state = state,
        kernelFunction = kernel
      )

  private def positiveEigenCount(values: DoubleVector, requested: Int, tolerance: Double): Int =
    val maxValue =
      if values.length == 0 then 0.0
      else Math.max(1.0, Math.abs(values(0)))
    var keep = 0
    val limit = Math.min(values.length, requested)
    while keep < limit && values(keep) > tolerance * maxValue do
      keep += 1
    keep

  private def reciprocal(values: DoubleVector, factor: Double): DoubleVector =
    val out = new Array[Double](values.length)
    var i = 0
    while i < values.length do
      out(i) = factor / values(i)
      i += 1
    DoubleVector.unsafe(out)

  private def inverseSqrt(values: DoubleVector): DoubleVector =
    val out = new Array[Double](values.length)
    var i = 0
    while i < values.length do
      out(i) = 1.0 / Math.sqrt(values(i))
      i += 1
    DoubleVector.unsafe(out)

  private def sqrtVector(values: DoubleVector): DoubleVector =
    val out = new Array[Double](values.length)
    var i = 0
    while i < values.length do
      out(i) = Math.sqrt(Math.max(values(i), 0.0))
      i += 1
    DoubleVector.unsafe(out)

  private def scaleVector(values: DoubleVector, factor: Double): DoubleVector =
    val out = new Array[Double](values.length)
    var i = 0
    while i < values.length do
      out(i) = values(i) * factor
      i += 1
    DoubleVector.unsafe(out)

  private def scaleColumns(matrix: DoubleMatrix, scale: DoubleVector): DoubleMatrix =
    require(matrix.cols == scale.length, "scale length must match matrix columns")
    val out = matrix.copyData
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) *= scale(col)
        col += 1
      row += 1
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, out)

  private[multivar] def computeKernel(
      kernel: Kernel,
      left: MatrixView,
      right: MatrixView,
      role: String
  ): Either[MultivarError, DoubleMatrix] =
    kernel.compute(left, right).flatMap { out =>
      if out.rows != left.rows || out.cols != right.rows then
        Left(
          MultivarError.InvalidKernelFit(
            s"$role returned ${out.rows}x${out.cols}, expected ${left.rows}x${right.rows}"
          )
        )
      else MatrixOps.checkFinite(role, out).map(_ => out)
    }
