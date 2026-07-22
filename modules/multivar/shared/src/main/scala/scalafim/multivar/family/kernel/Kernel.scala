package scalafim.multivar
package family.kernel

import scalafim.multivar.core.*
import scalafim.multivar.optimization.*

import gale.linalg.DMat
import gale.linalg.DVec

final case class KernelSpec(name: String, parameters: Map[String, Double] = Map.empty):
  require(name.nonEmpty, "kernel name must be non-empty")

trait Kernel:
  def spec: KernelSpec

  def compute(left: MatrixView, right: MatrixView): Either[MultivarError, DMat]

final case class LinearKernel() extends Kernel:
  override def spec: KernelSpec =
    KernelSpec("linear")

  override def compute(left: MatrixView, right: MatrixView): Either[MultivarError, DMat] =
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

  override def compute(left: MatrixView, right: MatrixView): Either[MultivarError, DMat] =
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
        GaleNumerics.matrixFromRowMajor(leftDense.rows, rightDense.rows, out)

object Kernel:
  val linear: Kernel =
    LinearKernel()

/** A kernel input with nominal row and feature identities.
  *
  * The numeric view is retained for kernel evaluation while `table` carries the
  * same value across the typed operator boundary. Equal feature counts alone do
  * not make two kernel inputs compatible.
  */
final class KernelInput[Rows <: SemanticSpace, Features <: SemanticSpace] private (
    val values: MatrixView,
    val rowSpace: SpaceEvidence[Rows],
    val featureSpace: SpaceEvidence[Features],
    val table: OpTable[Rows, Features, UncheckedEvidence],
    val provenance: SemanticProvenance
)

object KernelInput:
  def from[Rows <: SemanticSpace, Features <: SemanticSpace](
      values: MatrixView,
      rowSpace: SpaceEvidence[Rows],
      featureSpace: SpaceEvidence[Features],
      valueIdentity: ValueIdentity,
      provenance: SemanticProvenance = SemanticProvenance.source("kernel-input")
  ): Either[MultivarError, KernelInput[Rows, Features]] =
    if values.rows != rowSpace.dimension then
      Left(MultivarError.MatrixShapeMismatch(
        s"kernel input has ${values.rows} rows but row space '${rowSpace.id.value}' has ${rowSpace.dimension}"
      ))
    else if values.cols != featureSpace.dimension then
      Left(MultivarError.MatrixShapeMismatch(
        s"kernel input has ${values.cols} columns but feature space '${featureSpace.id.value}' has ${featureSpace.dimension}"
      ))
    else
      OperatorFitAdapters
        .semantic(
          Op.fromMatrixView(
            values,
            CoordinateEvidence.dual(featureSpace),
            CoordinateEvidence.primal(rowSpace),
            OperatorRoleWitness.table,
            valueIdentity,
            provenance
          )
        )
        .map(table => new KernelInput(values, rowSpace, featureSpace, table, provenance))

/** Diagnostic tag derived from the fitted preprocessor, never supplied by callers.
  *
  * `InputPreprocessed` records that the inputs were transformed (centered, scaled,
  * or standardized) BEFORE the kernel was evaluated. Kernel-space (double) centering
  * of the kernel matrix itself is not implemented, and for nonlinear kernels input
  * preprocessing is not equivalent to it.
  */
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
    eigenvectors: DMat,
    eigenvalues: DVec,
    standardDeviations: DVec,
    scores: DMat
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
  def scoreWeights: DMat

final case class StandardNystromState(
    lambdaLandmark: DVec,
    landmarkEigenvectors: DMat,
    scoreWeights: DMat
) extends NystromState:
  require(lambdaLandmark.length == landmarkEigenvectors.cols, "standard Nyström eigenvalue/eigenvector mismatch")
  require(scoreWeights.rows == landmarkEigenvectors.rows, "standard Nyström score weights must be landmark x component")

final case class DoubleNystromState(
    firstStageEigenvectors: DMat,
    firstStageInvSqrtEigenvalues: DMat,
    secondStageEigenvectors: DMat,
    secondStageInvSqrtEigenvalues: DMat,
    scoreWeights: DMat
) extends NystromState:
  require(scoreWeights.rows == firstStageEigenvectors.rows, "double Nyström score weights must be landmark x component")

final case class KernelScoreTransform[
    Rows <: SemanticSpace,
    Landmarks <: SemanticSpace,
    Components <: SemanticSpace
](
    extensionKernel: Op[Dual[Landmarks], Primal[Rows], KernelOperatorRole, UncheckedEvidence],
    scores: Op[Primal[Components], Primal[Rows], ScoreOperatorRole, UncheckedEvidence],
    values: DMat
)

/** Typed operator view of a fitted Nyström system.
  *
  * The landmark Gram and low-rank training approximation are certified PSD.
  * Rectangular extension kernels deliberately remain unchecked because
  * definiteness is not a meaningful claim for maps between distinct spaces.
  */
final class NystromOperatorFit private[multivar] (
    val trainingRows: SpaceRef,
    val featureSpace: SpaceRef,
    val landmarkSpace: SpaceRef,
    val componentSpace: SpaceRef,
    val processedTraining: OpTable[trainingRows.Id, featureSpace.Id, UncheckedEvidence],
    val processedLandmarks: OpTable[landmarkSpace.Id, featureSpace.Id, UncheckedEvidence],
    val landmarkKernel: Op[Dual[landmarkSpace.Id], Primal[landmarkSpace.Id], KernelOperatorRole, CertifiedPsd],
    val extensionKernel: Op[Dual[landmarkSpace.Id], Primal[trainingRows.Id], KernelOperatorRole, UncheckedEvidence],
    val approximateKernel: Op[Dual[trainingRows.Id], Primal[trainingRows.Id], KernelOperatorRole, CertifiedPsd],
    val extensionFrame: FunctionalFrame[landmarkSpace.Id, componentSpace.Id, UncheckedEvidence],
    val trainingScores: Op[Primal[componentSpace.Id], Primal[trainingRows.Id], ScoreOperatorRole, UncheckedEvidence],
    val provenance: SemanticProvenance
):
  def transform[Rows <: SemanticSpace, Features <: SemanticSpace](
      input: KernelInput[Rows, Features],
      preprocessor: FittedPreprocessor,
      kernel: Kernel
  ): Either[MultivarError, KernelScoreTransform[Rows, landmarkSpace.Id, componentSpace.Id]] =
    if input.featureSpace.descriptor != featureSpace.descriptor then
      Left(
        MultivarError.InvalidMap(
          s"Nyström transform feature space '${input.featureSpace.id.value}' does not match fitted space '${featureSpace.descriptor.id.value}'"
        )
      )
    else
      for
        processed <- preprocessor.transform(input.values)
        landmarkValues <- OperatorFitAdapters.semantic(processedLandmarks.toDense)
        values <- Nystrom.computeKernel(
          kernel,
          processed,
          MatrixView.dense(landmarkValues),
          "Nyström typed out-of-sample kernel"
        )
        identity = ValueIdentity.derived(
          "nystrom-out-of-sample-extension",
          input.table.valueIdentity,
          processedLandmarks.valueIdentity
        )
        extension <- OperatorFitAdapters.semantic(
          Op.fromDense(
            values,
            CoordinateEvidence.dual(landmarkSpace.evidence),
            CoordinateEvidence.primal(input.rowSpace),
            OperatorRoleWitness.kernel,
            identity,
            (provenance ++ input.provenance).append(
              SemanticProvenanceEvent.Derived(
                "kernel-extension",
                Vector(input.table.valueIdentity, processedLandmarks.valueIdentity)
              )
            )
          )
        )
        scoreOperator = extensionFrame.weights
          .andThen(extension)
          .retag(OperatorRoleWitness.score, "nystrom-out-of-sample-scores")
        scoreValues <- OperatorFitAdapters.semantic(scoreOperator.toDense)
      yield KernelScoreTransform(extension, scoreOperator, scoreValues)

final case class NystromFit(
    kernel: KernelSpec,
    method: NystromMethod,
    landmarks: LandmarkSet,
    landmarkData: DMat,
    preprocessor: FittedPreprocessor,
    originalCols: Int,
    centering: KernelCentering,
    normalization: KernelNormalization,
    eigen: KernelEigenArtifact,
    diagnostics: NystromDiagnostics,
    state: NystromState,
    kernelFunction: Kernel,
    operatorFit: NystromOperatorFit
):
  require(landmarkData.rows == landmarks.length, "landmark data rows must match landmarks")
  require(originalCols > 0, "Nyström original feature count must be positive")

  def transform(newData: MatrixView): Either[MultivarError, DMat] =
    if newData.cols != originalCols then
      Left(MultivarError.MatrixShapeMismatch(s"Nyström transform expected $originalCols columns, got ${newData.cols}"))
    else
      for
        processed <- preprocessor.transform(newData)
        kNew <- Nystrom.computeKernel(kernelFunction, processed, MatrixView.dense(landmarkData), "Nyström out-of-sample kernel")
      yield GaleNumerics.multiply(kNew, state.scoreWeights)

  def transformTyped[Rows <: SemanticSpace, Features <: SemanticSpace](
      newData: KernelInput[Rows, Features]
  ): Either[MultivarError, KernelScoreTransform[Rows, operatorFit.landmarkSpace.Id, operatorFit.componentSpace.Id]] =
    operatorFit.transform(newData, preprocessor, kernelFunction)

object Nystrom:
  /** Fit a Nyström kernel eigensystem over the selected landmarks.
    *
    * Kernel-space (double) centering of the kernel matrix is NOT implemented. The
    * `KernelCentering` tag in the fit and its diagnostics is derived from the fitted
    * preprocessor: an identity preprocessor yields `Uncentered`, anything else yields
    * `InputPreprocessed`. Preprocessing the inputs is not equivalent to centering the
    * kernel matrix for nonlinear kernels such as RBF.
    */
  def fit(
      input: MatrixView,
      components: ComponentCount,
      landmarks: Iterable[Int],
      kernel: Kernel = Kernel.linear,
      preproc: PreprocessSpec = PreprocessSpec.Pass,
      method: NystromMethod = NystromMethod.Standard,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      tolerance: Double = 1e-12
  ): Either[MultivarError, NystromFit] =
    if input.rows <= 0 then Left(MultivarError.InvalidDimension("Nyström input rows", input.rows))
    else if input.cols <= 0 then Left(MultivarError.InvalidDimension("Nyström input columns", input.cols))
    else
      for
        rows <- SpaceRef.of("nystrom.raw.rows", SpaceRole.Samples, input.rows)
        features <- SpaceRef.of("nystrom.raw.features", SpaceRole.Observed, input.cols)
        typed <- KernelInput.from(
          input,
          rows.evidence,
          features.evidence,
          ValueIdentity.source(ValueId.unsafe("nystrom.raw.input")),
          SemanticProvenance.source("raw-nystrom-compatibility-input")
        )
        fit <- fitTyped(typed, components, landmarks, kernel, preproc, method, eigenSolver, tolerance)
      yield fit

  def fitTyped[Rows <: SemanticSpace, Features <: SemanticSpace](
      input: KernelInput[Rows, Features],
      components: ComponentCount,
      landmarks: Iterable[Int],
      kernel: Kernel = Kernel.linear,
      preproc: PreprocessSpec = PreprocessSpec.Pass,
      method: NystromMethod = NystromMethod.Standard,
      eigenSolver: SymmetricEigenSolver = DenseSolvers.symmetricEigen,
      tolerance: Double = 1e-12
  ): Either[MultivarError, NystromFit] =
    if !tolerance.isFinite || tolerance < 0.0 then
      Left(MultivarError.InvalidTolerance("Nyström spectral tolerance", tolerance))
    else
      for
        landmarkSet <- LandmarkSet.from(landmarks, input.values.rows)
        _ <-
          if components.value <= landmarkSet.length then Right(())
          else Left(MultivarError.InvalidComponentRequest(components.value, landmarkSet.length))
        fitted <- preproc.fit(input.values)
        processedView <- fitted.transform(input.values)
        processed <- processedView.toDense(StoragePolicy.AllowDense)
        _ <- MatrixOps.checkFinite("Nyström input", processed)
        landmarkData = RowGeometryOps.selectRows(processed, landmarkSet.indices)
        landmarkView = MatrixView.dense(landmarkData)
        // Kernel is an open trait; symmetrize the square landmark kernel so a user
        // kernel with roundoff asymmetry does not fail the symmetric eigensolver.
        kMmRaw <- computeKernel(kernel, landmarkView, landmarkView, "Nyström landmark kernel")
        kMm = MatrixOps.symmetrize(kMmRaw)
        // The n x m kernel is computed exactly once and shared by every stage.
        cAll <- computeKernel(kernel, MatrixView.dense(processed), landmarkView, "Nyström all-landmark kernel")
        fit <- method match
          case NystromMethod.Standard =>
            fitStandard(
              input = input,
              components = components,
              landmarkSet = landmarkSet,
              landmarkData = landmarkData,
              processed = processed,
              fitted = fitted,
              kernel = kernel,
              kMm = kMm,
              cAll = cAll,
              originalCols = processed.cols,
              eigenSolver = eigenSolver,
              tolerance = tolerance
            )
          case NystromMethod.DoubleNystrom(intermediateRank) =>
            fitDouble(
              input = input,
              components = components,
              intermediateRank = intermediateRank,
              landmarkSet = landmarkSet,
              landmarkData = landmarkData,
              processed = processed,
              fitted = fitted,
              kernel = kernel,
              kMm = kMm,
              cAll = cAll,
              originalCols = processed.cols,
              eigenSolver = eigenSolver,
              tolerance = tolerance
            )
      yield fit

  private def fitStandard[Rows <: SemanticSpace, Features <: SemanticSpace](
      input: KernelInput[Rows, Features],
      components: ComponentCount,
      landmarkSet: LandmarkSet,
      landmarkData: DMat,
      processed: DMat,
      fitted: FittedPreprocessor,
      kernel: Kernel,
      kMm: DMat,
      cAll: DMat,
      originalCols: Int,
      eigenSolver: SymmetricEigenSolver,
      tolerance: Double
  ): Either[MultivarError, NystromFit] =
    // The n x m all-landmark kernel carries the input row count.
    val inputRows = cAll.rows
    for
      eigen <- LinalgErrorAdapter.adapt(eigenSolver.decompose(kMm))
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
            input,
            components,
            landmarkSet,
            landmarkData,
            processed,
            kMm,
            cAll,
            originalCols,
            fitted,
            kernel,
            NystromMethod.Standard,
            KernelNormalization.StandardNystromScaling,
            eigenvalues,
            sdev,
            eigenWeights,
            StandardNystromState(lambdaMm, uMm, scoreWeights),
            scoreWeights
          )
    yield fit

  private def fitDouble[Rows <: SemanticSpace, Features <: SemanticSpace](
      input: KernelInput[Rows, Features],
      components: ComponentCount,
      intermediateRank: ComponentCount,
      landmarkSet: LandmarkSet,
      landmarkData: DMat,
      processed: DMat,
      fitted: FittedPreprocessor,
      kernel: Kernel,
      kMm: DMat,
      cAll: DMat,
      originalCols: Int,
      eigenSolver: SymmetricEigenSolver,
      tolerance: Double
  ): Either[MultivarError, NystromFit] =
    if intermediateRank.value > landmarkSet.length then
      Left(MultivarError.InvalidComponentRequest(intermediateRank.value, landmarkSet.length))
    else
      for
        first <- LinalgErrorAdapter.adapt(eigenSolver.decompose(kMm))
        firstKeep = positiveEigenCount(first.values, intermediateRank.value, tolerance)
        fit <-
          if firstKeep == 0 then Left(MultivarError.InvalidKernelFit("double Nyström first stage has no positive eigenvalues"))
          else
            val vSL = MatrixOps.takeColumns(first.vectors, firstKeep)
            val lambdaL = MatrixOps.takeVector(first.values, firstKeep)
            val invSqrtLambdaL = MatrixOps.diagonal(inverseSqrt(lambdaL))
            val firstWeights = GaleNumerics.multiply(vSL, invSqrtLambdaL)
            val w = GaleNumerics.multiply(cAll, firstWeights)
            val kW = MatrixOps.symmetrize(GaleNumerics.crossProduct(w))
            for
              second <- LinalgErrorAdapter.adapt(eigenSolver.decompose(kW))
              finalRequest = Math.min(components.value, firstKeep)
              secondKeep = positiveEigenCount(second.values, finalRequest, tolerance)
              out <-
                if secondKeep == 0 then Left(MultivarError.InvalidKernelFit("double Nyström second stage has no positive eigenvalues"))
                else
                  val lambdaK = MatrixOps.takeVector(second.values, secondKeep)
                  val vK = MatrixOps.takeColumns(second.vectors, secondKeep)
                  val invSqrtLambdaK = MatrixOps.diagonal(inverseSqrt(lambdaK))
                  val eigenWeights = GaleNumerics.multiply(firstWeights, GaleNumerics.multiply(vK, invSqrtLambdaK))
                  val sdev = sqrtVector(lambdaK)
                  val scoreWeights = scaleColumns(eigenWeights, sdev)
                  buildFit(
                    input,
                    components,
                    landmarkSet,
                    landmarkData,
                    processed,
                    kMm,
                    cAll,
                    originalCols,
                    fitted,
                    kernel,
                    NystromMethod.DoubleNystrom(intermediateRank),
                    KernelNormalization.DoubleNystromScaling,
                    lambdaK,
                    sdev,
                    eigenWeights,
                    DoubleNystromState(vSL, invSqrtLambdaL, vK, invSqrtLambdaK, scoreWeights),
                    scoreWeights
                  )
            yield out
      yield fit

  private def buildFit[Rows <: SemanticSpace, Features <: SemanticSpace](
      input: KernelInput[Rows, Features],
      requestedComponents: ComponentCount,
      landmarkSet: LandmarkSet,
      landmarkData: DMat,
      processed: DMat,
      kMm: DMat,
      cAll: DMat,
      originalCols: Int,
      fitted: FittedPreprocessor,
      kernel: Kernel,
      method: NystromMethod,
      normalization: KernelNormalization,
      eigenvalues: DVec,
      sdev: DVec,
      eigenWeights: DMat,
      state: NystromState,
      scoreWeights: DMat
  ): Either[MultivarError, NystromFit] =
    val eigenvectors = GaleNumerics.multiply(cAll, eigenWeights)
    val scores = GaleNumerics.multiply(cAll, scoreWeights)
    for
      _ <- MatrixOps.checkFinite("Nyström eigenvectors", eigenvectors)
      _ <- MatrixOps.checkFinite("Nyström scores", scores)
      operators <- buildOperatorFit(
        input,
        landmarkSet,
        processed,
        landmarkData,
        kMm,
        cAll,
        scoreWeights,
        scores,
        kernel,
        method
      )
    yield
      val centering = derivedCentering(fitted)
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
        originalCols = originalCols,
        centering = centering,
        normalization = normalization,
        eigen = artifact,
        diagnostics = diagnostics,
        state = state,
        kernelFunction = kernel,
        operatorFit = operators
      )

  private def buildOperatorFit[Rows <: SemanticSpace, Features <: SemanticSpace](
      input: KernelInput[Rows, Features],
      landmarkSet: LandmarkSet,
      processed: DMat,
      landmarkData: DMat,
      landmarkGram: DMat,
      extensionValues: DMat,
      scoreWeights: DMat,
      scores: DMat,
      kernel: Kernel,
      method: NystromMethod
  ): Either[MultivarError, NystromOperatorFit] =
    val landmarkSuffix = landmarkSet.indices.mkString("-")
    val base = input.rowSpace.id.value
    val provenance = input.provenance.append(
      SemanticProvenanceEvent.Derived("nystrom-fit", Vector(input.table.valueIdentity))
    )
    for
      trainingRows <- SpaceRef.of(input.rowSpace.id.value, input.rowSpace.descriptor.role, input.rowSpace.dimension)
      featureSpace <- SpaceRef.of(input.featureSpace.id.value, input.featureSpace.descriptor.role, input.featureSpace.dimension)
      landmarkSpace <- SpaceRef.of(
        s"$base.nystrom-landmarks-$landmarkSuffix",
        SpaceRole.Kernel,
        landmarkSet.length
      )
      componentSpace <- SpaceRef.of(
        s"$base.nystrom-${method.label}-components",
        SpaceRole.Latent,
        scoreWeights.cols
      )
      processedTable <- OperatorFitAdapters.semantic(
        Op.fromDense(
          processed,
          CoordinateEvidence.dual(featureSpace.evidence),
          CoordinateEvidence.primal(trainingRows.evidence),
          OperatorRoleWitness.table,
          ValueIdentity.derived("nystrom-processed-training", input.table.valueIdentity),
          provenance.append(
            SemanticProvenanceEvent.Derived("preprocess-kernel-input", Vector(input.table.valueIdentity))
          )
        )
      )
      landmarkTable <- OperatorFitAdapters.semantic(
        Op.fromDense(
          landmarkData,
          CoordinateEvidence.dual(featureSpace.evidence),
          CoordinateEvidence.primal(landmarkSpace.evidence),
          OperatorRoleWitness.table,
          ValueIdentity.derived("nystrom-landmark-table", input.table.valueIdentity),
          provenance.append(
            SemanticProvenanceEvent.Derived("select-landmarks", Vector(input.table.valueIdentity))
          )
        )
      )
      landmarkIdentity = ValueIdentity.derived("nystrom-landmark-kernel", landmarkTable.valueIdentity)
      landmarkLinear <- OperatorFitAdapters.semantic(
        Lin.fromDenseMatrix(
          landmarkGram,
          CoordinateEvidence.dual(landmarkSpace.evidence),
          CoordinateEvidence.primal(landmarkSpace.evidence),
          landmarkIdentity,
          provenance.append(
            SemanticProvenanceEvent.Derived("kernel-gram", Vector(landmarkTable.valueIdentity))
          )
        )
      )
      landmarkCertificate <- FormCertificates
        .psd(landmarkLinear)
        .left
        .map(error => MultivarError.InvalidKernelFit(s"landmark kernel is not certified PSD: ${error.message}"))
      landmarkUnchecked = Op.fromLin(landmarkLinear, OperatorRoleWitness.kernel)
      landmarkKernel <- OperatorFitAdapters.semantic(Op.certifiedPsd(landmarkUnchecked, landmarkCertificate))
      extension <- OperatorFitAdapters.semantic(
        Op.fromDense(
          extensionValues,
          CoordinateEvidence.dual(landmarkSpace.evidence),
          CoordinateEvidence.primal(trainingRows.evidence),
          OperatorRoleWitness.kernel,
          ValueIdentity.derived("nystrom-training-extension", input.table.valueIdentity, landmarkTable.valueIdentity),
          provenance.append(
            SemanticProvenanceEvent.Derived(
              "kernel-extension",
              Vector(input.table.valueIdentity, landmarkTable.valueIdentity)
            )
          )
        )
      )
      frame <- OperatorFitAdapters.semantic(
        Op.fromDense(
          scoreWeights,
          CoordinateEvidence.primal(componentSpace.evidence),
          CoordinateEvidence.dual(landmarkSpace.evidence),
          OperatorRoleWitness.frame,
          ValueIdentity.derived("nystrom-extension-frame", landmarkKernel.valueIdentity),
          provenance.append(
            SemanticProvenanceEvent.Derived("nystrom-extension-frame", Vector(landmarkKernel.valueIdentity))
          )
        )
      )
      scoreOperator = frame.andThen(extension).retag(OperatorRoleWitness.score, "nystrom-training-scores")
      approximateUnchecked <- OperatorFitAdapters.semantic(
        Op.lowRank(
          scores,
          scores,
          CoordinateEvidence.dual(trainingRows.evidence),
          CoordinateEvidence.primal(trainingRows.evidence),
          OperatorRoleWitness.kernel,
          ValueIdentity.derived("nystrom-low-rank-kernel", scoreOperator.valueIdentity)
        )
      )
      approximateCertificate <- algebraicPsd(
        approximateUnchecked.valueIdentity,
        "low-rank-factor-product"
      )
      approximate <- OperatorFitAdapters.semantic(Op.certifiedPsd(approximateUnchecked, approximateCertificate))
    yield
      new NystromOperatorFit(
        trainingRows,
        featureSpace,
        landmarkSpace,
        componentSpace,
        processedTable,
        landmarkTable,
        landmarkKernel,
        extension,
        approximate,
        FunctionalFrame(frame),
        scoreOperator,
        provenance
      )

  private def algebraicPsd(
      identity: ValueIdentity,
      method: String
  ): Either[MultivarError, Certificate[PsdProperty]] =
    for
      context <- CertificateContext
        .from(
          CertificateTolerance.strict,
          CertificateNorm.Frobenius,
          method,
          "operator-algebra",
          NumericalPrecision.Float64
        )
        .left
        .map(error => MultivarError.InvalidMap(error.message))
    yield
      Certificate.unsafe[PsdProperty](
        identity,
        CertificateClaim.PositiveSemidefinite(0.0, 0.0, 1.0),
        context
      )

  /** The centering tag is a derived diagnostic: an identity preprocessor means the
    * kernel saw the raw inputs; anything else means the inputs were preprocessed.
    * No code path centers the kernel matrix itself (see `fit`).
    */
  private def derivedCentering(fitted: FittedPreprocessor): KernelCentering =
    fitted match
      case affine: FittedColumnAffine if isIdentityAffine(affine) =>
        KernelCentering.Uncentered
      case _ =>
        KernelCentering.InputPreprocessed

  private def isIdentityAffine(affine: FittedColumnAffine): Boolean =
    var identity = true
    var i = 0
    while identity && i < affine.inputCols do
      identity = affine.scale(i) == 1.0 && affine.shift(i) == 0.0
      i += 1
    identity

  private def positiveEigenCount(values: DVec, requested: Int, tolerance: Double): Int =
    val maxValue =
      if values.length == 0 then 0.0
      else Math.max(1.0, Math.abs(values(0)))
    var keep = 0
    val limit = Math.min(values.length, requested)
    while keep < limit && values(keep) > tolerance * maxValue do
      keep += 1
    keep

  private def reciprocal(values: DVec, factor: Double): DVec =
    val out = new Array[Double](values.length)
    var i = 0
    while i < values.length do
      out(i) = factor / values(i)
      i += 1
    GaleNumerics.vectorFromArray(out)

  private def inverseSqrt(values: DVec): DVec =
    val out = new Array[Double](values.length)
    var i = 0
    while i < values.length do
      out(i) = 1.0 / Math.sqrt(values(i))
      i += 1
    GaleNumerics.vectorFromArray(out)

  private def sqrtVector(values: DVec): DVec =
    val out = new Array[Double](values.length)
    var i = 0
    while i < values.length do
      out(i) = Math.sqrt(Math.max(values(i), 0.0))
      i += 1
    GaleNumerics.vectorFromArray(out)

  private def scaleVector(values: DVec, factor: Double): DVec =
    val out = new Array[Double](values.length)
    var i = 0
    while i < values.length do
      out(i) = values(i) * factor
      i += 1
    GaleNumerics.vectorFromArray(out)

  private def scaleColumns(matrix: DMat, scale: DVec): DMat =
    require(matrix.cols == scale.length, "scale length must match matrix columns")
    val out = matrix.copyData
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) *= scale(col)
        col += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(matrix.rows, matrix.cols, out)

  private[multivar] def computeKernel(
      kernel: Kernel,
      left: MatrixView,
      right: MatrixView,
      role: String
  ): Either[MultivarError, DMat] =
    kernel.compute(left, right).flatMap { out =>
      if out.rows != left.rows || out.cols != right.rows then
        Left(
          MultivarError.InvalidKernelFit(
            s"$role returned ${out.rows}x${out.cols}, expected ${left.rows}x${right.rows}"
          )
        )
      else MatrixOps.checkFinite(role, out).map(_ => out)
    }
