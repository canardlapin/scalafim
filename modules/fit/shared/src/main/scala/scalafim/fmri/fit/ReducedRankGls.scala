package scalafim.fmri.fit

import scalafim.fmri.ar.{WhiteningPlan, WhiteningTransform}
import scalafim.fmri.model.{
  FitEngine,
  ReducedRankBootstrapConfig,
  ReducedRankComponentSpec,
  ReducedRankGlsConfig,
  ReducedRankInferencePolicy
}
import gale.linalg.{DMat, DVec, LinAlgError, Matrix, QR, QROptions, QRPivoting, Vec}
import gale.spectral.{SingularSelection, Svds}

final case class ReducedRankDesignPartition private (
    predictors: Int,
    targetColumns: Vector[Int],
    nuisanceColumns: Vector[Int]
):
  require(predictors > 0, "reduced-rank design partition requires at least one predictor")
  require(targetColumns.nonEmpty, "reduced-rank design partition requires at least one target column")
  require((targetColumns ++ nuisanceColumns).length == predictors, "reduced-rank design partition must cover every predictor")

  def targetPredictors: Int =
    targetColumns.length

  def nuisancePredictors: Int =
    nuisanceColumns.length

object ReducedRankDesignPartition:
  def all(predictors: Int): ReducedRankDesignPartition =
    fromColumns(predictors, targetColumns = (0 until predictors).toVector, nuisanceColumns = Vector.empty)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def fromColumns(
      predictors: Int,
      targetColumns: Vector[Int],
      nuisanceColumns: Vector[Int]
  ): Either[FitError, ReducedRankDesignPartition] =
    if predictors <= 0 then Left(FitError.EmptyDesign)
    else if targetColumns.isEmpty then Left(FitError.InvalidFitAxis("reduced-rank target columns", "must contain at least one event/task predictor"))
    else
      val allColumns = targetColumns ++ nuisanceColumns
      if allColumns.exists(col => col < 0 || col >= predictors) then
        Left(FitError.InvalidFitAxis("reduced-rank design columns", s"indices must be in [0, ${predictors - 1}]"))
      else if allColumns.distinct.length != allColumns.length then
        Left(FitError.InvalidFitAxis("reduced-rank design columns", "target and nuisance columns must be unique and disjoint"))
      else if allColumns.toSet.size != predictors then
        Left(FitError.InvalidFitAxis("reduced-rank design columns", "target and nuisance columns must cover every predictor"))
      else Right(new ReducedRankDesignPartition(predictors, targetColumns, nuisanceColumns))

final class ReducedRankGlsPrepared private[fit] (
    private val gls: GlsPrepared,
    private val projection: Option[ReducedRankGlsProjection],
    private val fallbackScope: CoefficientInferenceScope
):
  val design: DesignMatrix =
    gls.design

  val partitions: Vector[RunPartition] =
    gls.partitions

  def fitBlock(input: FitBlockInput): Either[FitError, DenseFitBlockResult] =
    projection match
      case None =>
        for
          fit <- gls.fit(input.response, input.voxelIndices)
          block = DenseFitBlockResult.fromGls(input, fit, FitEngine.ReducedRankGls)
          inference <- block.inference.restrict(
            fallbackScope,
            CoefficientInferenceMethod.ReducedRankFullRankVoxelwiseFallback
          )
        yield block.copy(inference = inference)
      case Some(value) =>
        value.fitBlock(input)

object ReducedRankGlsPrepared:
  def prepare(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: Vector[RunPartition],
      config: ReducedRankGlsConfig,
      selectedVoxelIndices: Vector[Int]
  ): Either[FitError, ReducedRankGlsPrepared] =
    prepare(
      design = design,
      response = response,
      partitions = partitions,
      config = config,
      selectedVoxelIndices = selectedVoxelIndices,
      designPartition = ReducedRankDesignPartition.all(design.predictors)
    )

  def prepare(
      design: DesignMatrix,
      response: ResponseBlock,
      partitions: Vector[RunPartition],
      config: ReducedRankGlsConfig,
      selectedVoxelIndices: Vector[Int],
      designPartition: ReducedRankDesignPartition
  ): Either[FitError, ReducedRankGlsPrepared] =
    for
      _ <- validatePartition(design, designPartition)
      prepared <- Gls.prepare(design, response, partitions, config.autocorrelation.toLegacy, selectedVoxelIndices)
      rankRequest <- rankRequest(config, designPartition.targetPredictors, response.voxels)
      inferenceScope = CoefficientInferenceScope.unsafeOnly(
        designPartition.targetColumns,
        "reduced-rank GLS target/event coefficient"
      )
      projection <- prepared.whitening match
        case GlsWhitening.Shared(plan) =>
          for
            fullFit <- prepared.fit(response, selectedVoxelIndices)
            reduced <- ReducedRankGlsProjection.from(
              fullFit,
              design,
              response,
              selectedVoxelIndices,
              plan,
              rankRequest,
              designPartition,
              config.inference
            )
          yield Some(reduced)
        case GlsWhitening.Voxelwise(_) =>
          if isFullRankRequest(config, designPartition.targetPredictors, response.voxels) &&
              config.inference == ReducedRankInferencePolicy.Conditional
          then Right(None)
          else Left(FitError.UnsupportedEngine(
            "compressed or bootstrap ReducedRankGls requires shared GLS whitening; voxelwise-AR reduced-rank geometry is tracked separately"
          ))
    yield new ReducedRankGlsPrepared(prepared, projection, inferenceScope)

  private def validatePartition(
      design: DesignMatrix,
      partition: ReducedRankDesignPartition
  ): Either[FitError, Unit] =
    if partition.predictors != design.predictors then
      Left(FitError.InvalidFitAxis("reduced-rank design partition", s"expected ${design.predictors} predictors, got ${partition.predictors}"))
    else Right(())

  private def rankRequest(
      config: ReducedRankGlsConfig,
      targetPredictors: Int,
      voxels: Int
  ): Either[FitError, ReducedRankRequest] =
    val limit = math.min(targetPredictors, voxels)
    config.components match
      case ReducedRankComponentSpec.Full =>
        Right(ReducedRankRequest.Full)
      case ReducedRankComponentSpec.Fixed(count) =>
        if count.value > limit then
          Left(FitError.InvalidFitAxis(
            "reduced-rank GLS components",
            s"requested ${count.value} components but selected data allow at most $limit"
          ))
        else Right(ReducedRankRequest.Fixed(count.value))
      case ReducedRankComponentSpec.EnergyRetained(keep) =>
        Right(ReducedRankRequest.EnergyRetained(keep.value))
      case ReducedRankComponentSpec.ResidualSumsOfSquaresBudget(budget) =>
        Right(ReducedRankRequest.ResidualSumsOfSquaresBudget(budget.value))

  private def isFullRankRequest(
      config: ReducedRankGlsConfig,
      targetPredictors: Int,
      voxels: Int
  ): Boolean =
    config.components.isFull(math.min(targetPredictors, voxels))

private enum ReducedRankRequest:
  case Full
  case Fixed(rank: Int)
  case EnergyRetained(keep: Double)
  case ResidualSumsOfSquaresBudget(budget: Double)

private final class ReducedRankGlsProjection private (
    selectedVoxelIndices: Vector[Int],
    designPartition: ReducedRankDesignPartition,
    basis: DMat,
    targetLatentCoefficients: DMat,
    nuisanceCoefficients: DMat,
    fullFit: GlsFit,
    whiteningPlan: WhiteningPlan,
    fullInference: CoefficientInference,
    positionByVoxelIndex: Map[Int, Int]
):
  private val components: Int =
    basis.cols

  def fitBlock(input: FitBlockInput): Either[FitError, DenseFitBlockResult] =
    for
      positions <- localPositions(input.voxelIndices)
      coefficients <- decodeCoefficients(positions)
      residualVariance <- reducedResidualVariance(input, coefficients.value)
      inference <- fullInference.selectVoxelPositions(positions)
    yield
      DenseFitBlockResult(
        coefficients = coefficients,
        inference = inference,
        residualVariance = residualVariance,
        residualDegreesOfFreedom = fullFit.residualDegreesOfFreedom,
        voxelIndices = input.voxelIndices,
        timepoints = input.timepoints,
        engine = FitEngine.ReducedRankGls,
        olsDiagnostics = Some(fullFit.finalOlsDiagnostics),
        autocorrelation = Some(fullFit.diagnostics),
        coefficientAxis = input.coefficientAxis
      )

  private def decodeCoefficients(positions: Vector[Int]): Either[FitError, CoefficientBlock] =
    if targetLatentCoefficients.cols != components then
      Left(FitError.InvalidFitAxis("reduced-rank GLS latent coefficients", s"expected $components columns, got ${targetLatentCoefficients.cols}"))
    else if nuisanceCoefficients.cols != selectedVoxelIndices.length then
      Left(FitError.InvalidFitAxis("reduced-rank GLS nuisance coefficients", s"expected ${selectedVoxelIndices.length} voxels, got ${nuisanceCoefficients.cols}"))
    else
      val out = Matrix.newBuilder(designPartition.predictors, positions.length)
      var target = 0
      while target < designPartition.targetPredictors do
        val predictor = designPartition.targetColumns(target)
        var local = 0
        while local < positions.length do
          val position = positions(local)
          var sum = 0.0
          var component = 0
          while component < components do
            sum += targetLatentCoefficients(target, component) * basis(position, component)
            component += 1
          out(predictor, local) = sum
          local += 1
        target += 1

      var nuisance = 0
      while nuisance < designPartition.nuisancePredictors do
        val predictor = designPartition.nuisanceColumns(nuisance)
        var local = 0
        while local < positions.length do
          val position = positions(local)
          out(predictor, local) = nuisanceCoefficients(nuisance, position)
          local += 1
        nuisance += 1

      Right(CoefficientBlock(out.result()))

  private def reducedResidualVariance(
      input: FitBlockInput,
      coefficients: DMat
  ): Either[FitError, DVec] =
    for
      whitened <- WhiteningTransform(
        whiteningPlan,
        input.design.value,
        input.response.value
      ).left.map(Gls.arToFitError)
    yield Ols.residualVariance(
      whitened.design,
      whitened.response,
      coefficients,
      fullFit.residualDegreesOfFreedom
    )

  private def localPositions(voxelIndices: Vector[Int]): Either[FitError, Vector[Int]] =
    val out = Vector.newBuilder[Int]
    out.sizeHint(voxelIndices.length)
    var i = 0
    while i < voxelIndices.length do
      positionByVoxelIndex.get(voxelIndices(i)) match
        case Some(position) =>
          out += position
        case None =>
          return Left(FitError.InvalidFitAxis("reduced-rank GLS voxel", s"voxel ${voxelIndices(i)} was not present during preparation"))
      i += 1
    Right(out.result())

private object ReducedRankGlsProjection:
  def from(
      fullFit: GlsFit,
      design: DesignMatrix,
      response: ResponseBlock,
      selectedVoxelIndices: Vector[Int],
      whiteningPlan: WhiteningPlan,
      rankRequest: ReducedRankRequest,
      designPartition: ReducedRankDesignPartition,
      inferencePolicy: ReducedRankInferencePolicy
  ): Either[FitError, ReducedRankGlsProjection] =
    for
      _ <- validateSelectedResponse(response, selectedVoxelIndices)
      factors <- taskSubspaceFactors(design, response, whiteningPlan, rankRequest, designPartition)
      inference <- buildInference(
        factors,
        rankRequest,
        designPartition,
        fullFit.residualDegreesOfFreedom,
        inferencePolicy
      )
    yield
      new ReducedRankGlsProjection(
        selectedVoxelIndices = selectedVoxelIndices,
        designPartition = designPartition,
        basis = factors.basis,
        targetLatentCoefficients = factors.targetCoefficients,
        nuisanceCoefficients = factors.nuisanceCoefficients,
        fullFit = fullFit,
        whiteningPlan = whiteningPlan,
        fullInference = inference,
        positionByVoxelIndex = selectedVoxelIndices.zipWithIndex.toMap
      )

  private final case class ReducedRankFactors(
      taskFit: ReducedTaskFit,
      nuisanceCoefficients: DMat,
      residualizedTargetDesign: DMat,
      residualizedResponse: DMat
  ):
    def basis: DMat = taskFit.basis
    def targetCoefficients: DMat = taskFit.latentCoefficients

  private final case class ReducedTaskFit(
      basis: DMat,
      latentCoefficients: DMat,
      coefficients: DMat,
      normalizedCovariance: DMat
  )

  private def taskSubspaceFactors(
      design: DesignMatrix,
      response: ResponseBlock,
      whiteningPlan: WhiteningPlan,
      rankRequest: ReducedRankRequest,
      partition: ReducedRankDesignPartition
  ): Either[FitError, ReducedRankFactors] =
    for
      whitened <- WhiteningTransform(
        whiteningPlan,
        design.value,
        response.value
      ).left.map(Gls.arToFitError)
      whitenedDesign = whitened.design
      whitenedResponse = whitened.response
      targetDesign = selectColumns(whitenedDesign, partition.targetColumns)
      nuisanceDesign = selectColumns(whitenedDesign, partition.nuisanceColumns)
      residualized <- residualizeAgainstNuisance(targetDesign, whitenedResponse, nuisanceDesign)
      taskFit <- fitReducedTask(residualized.targetDesign, residualized.response, rankRequest)
      taskFitted = targetDesign * taskFit.coefficients
      nuisanceResponse = subtract(whitenedResponse, taskFitted)
      nuisanceCoefficients <- fitNuisance(nuisanceDesign, nuisanceResponse)
    yield ReducedRankFactors(
      taskFit = taskFit,
      nuisanceCoefficients = nuisanceCoefficients,
      residualizedTargetDesign = residualized.targetDesign,
      residualizedResponse = residualized.response
    )

  private def fitReducedTask(
      targetDesign: DMat,
      response: DMat,
      rankRequest: ReducedRankRequest
  ): Either[FitError, ReducedTaskFit] =
    for
      qr <- fullRankQr(targetDesign)
      qty <- leadingQtRows(qr, response, targetDesign.cols)
      svd <- Svds
        .svd(qty, SingularSelection.All)
        .left
        .map(error => FitError.InvalidFitAxis("reduced-rank GLS task basis", error.getMessage))
      converged <- svd
        .requireConverged
        .left
        .map(error => FitError.InvalidFitAxis("reduced-rank GLS task basis", error.getMessage))
      rank <- resolveRank(rankRequest, converged.singularValues)
      u = takeColumns(converged.u, rank)
      singularValues = takeValues(converged.singularValues, rank)
      basis = takeColumns(converged.vt.t, rank)
      scoreTargets <- qScores(qr, scaleColumns(u, singularValues), rank)
      targetCoefficients <- qr
        .solveLeastSquares(scoreTargets)
        .left
        .map(FitError.SingularDesign.apply)
      covariance <- qr.normalizedCovariance.left.map(FitError.SingularDesign.apply)
    yield ReducedTaskFit(
      basis = basis,
      latentCoefficients = targetCoefficients,
      coefficients = targetCoefficients * basis.t,
      normalizedCovariance = covariance
    )

  private def buildInference(
      factors: ReducedRankFactors,
      rankRequest: ReducedRankRequest,
      partition: ReducedRankDesignPartition,
      residualDegreesOfFreedom: ResidualDegreesOfFreedom,
      policy: ReducedRankInferencePolicy
  ): Either[FitError, CoefficientInference] =
    val scope = CoefficientInferenceScope.unsafeOnly(
      partition.targetColumns,
      "reduced-rank GLS target/event coefficient"
    )
    policy match
      case ReducedRankInferencePolicy.Conditional =>
        for
          varianceScale <- conditionalVariance(factors, residualDegreesOfFreedom)
          covariance <- CoefficientCovariance.shared(
            embedTargetCovariance(
              factors.taskFit.normalizedCovariance,
              partition.predictors,
              partition.targetColumns
            )
          )
          inference <- CoefficientInference.fromCovariance(
            scope,
            covariance,
            varianceScale,
            residualDegreesOfFreedom,
            CoefficientInferenceMethod.ReducedRankConditional
          )
        yield inference
      case ReducedRankInferencePolicy.Bootstrap(config) =>
        for
          covariance <- bootstrapCovariance(factors, rankRequest, partition, config)
          varianceScale = DVec.fromSeq(Vector.fill(factors.residualizedResponse.cols)(1.0))
          inference <- CoefficientInference.fromCovariance(
            scope,
            covariance,
            varianceScale,
            residualDegreesOfFreedom,
            CoefficientInferenceMethod.ReducedRankBootstrap(
              config.replicates.value,
              config.blockSize.value,
              config.seed.value
            )
          )
        yield inference

  private def conditionalVariance(
      factors: ReducedRankFactors,
      residualDegreesOfFreedom: ResidualDegreesOfFreedom
  ): Either[FitError, DVec] =
    val latentResponse = factors.residualizedResponse * factors.basis
    val latentFitted = factors.residualizedTargetDesign * factors.targetCoefficients
    val residual = subtract(latentResponse, latentFitted)
    val sigma = scaleMatrix(
      residual.t * residual,
      1.0 / residualDegreesOfFreedom.value.toDouble
    )
    val out = Vec.newBuilder(factors.basis.rows)
    var voxel = 0
    while voxel < factors.basis.rows do
      var value = 0.0
      var left = 0
      while left < factors.basis.cols do
        var right = 0
        while right < factors.basis.cols do
          value += factors.basis(voxel, left) * sigma(left, right) * factors.basis(voxel, right)
          right += 1
        left += 1
      if !value.isFinite || value < -1e-12 then
        return Left(FitError.InvalidFitAxis("reduced-rank GLS conditional variance", s"voxel $voxel has value $value"))
      out(voxel) = math.max(value, 2.220446049250313e-16)
      voxel += 1
    Right(out.result())

  private def bootstrapCovariance(
      factors: ReducedRankFactors,
      rankRequest: ReducedRankRequest,
      partition: ReducedRankDesignPartition,
      config: ReducedRankBootstrapConfig
  ): Either[FitError, CoefficientCovariance] =
    val targetDesign = factors.residualizedTargetDesign
    val response = factors.residualizedResponse
    val targetPredictors = targetDesign.cols
    val voxels = response.cols
    val fitted = targetDesign * factors.taskFit.coefficients
    val residual = subtract(response, fitted)
    val mean = new Array[Double](targetPredictors * voxels)
    val m2 = new Array[Double](voxels * targetPredictors * targetPredictors)
    val delta = new Array[Double](targetPredictors)
    val rng = ParkMillerRng(config.seed.value)
    var failure: FitError | Null = null
    var replicate = 1
    while replicate <= config.replicates.value && failure == null do
      val indices = sampleIndices(response.rows, config.blockSize.value, rng)
      val sampled = resampledResponse(fitted, residual, indices)
      fitReducedTask(targetDesign, sampled, rankRequest) match
        case Left(error) =>
          failure = error
        case Right(fit) =>
          var voxel = 0
          while voxel < voxels do
            var predictor = 0
            while predictor < targetPredictors do
              val index = predictor * voxels + voxel
              val value = fit.coefficients(predictor, voxel)
              delta(predictor) = value - mean(index)
              mean(index) += delta(predictor) / replicate.toDouble
              predictor += 1

            var left = 0
            while left < targetPredictors do
              var right = 0
              while right < targetPredictors do
                val rightIndex = right * voxels + voxel
                val covarianceIndex = (voxel * targetPredictors + left) * targetPredictors + right
                m2(covarianceIndex) += delta(left) * (fit.coefficients(right, voxel) - mean(rightIndex))
                right += 1
              left += 1
            voxel += 1
      replicate += 1

    failure match
      case error: FitError => Left(error)
      case null =>
        val divisor = (config.replicates.value - 1).toDouble
        val matrices = Vector.tabulate(voxels) { voxel =>
          val task = Matrix.newBuilder(targetPredictors, targetPredictors)
          var row = 0
          while row < targetPredictors do
            var col = 0
            while col < targetPredictors do
              task(row, col) = m2((voxel * targetPredictors + row) * targetPredictors + col) / divisor
              col += 1
            row += 1
          embedTargetCovariance(
            task.result(),
            partition.predictors,
            partition.targetColumns
          )
        }
        CoefficientCovariance.voxelwise(matrices)

  private def embedTargetCovariance(
      target: DMat,
      predictors: Int,
      targetColumns: Vector[Int]
  ): DMat =
    require(target.rows == targetColumns.length && target.cols == targetColumns.length, "target covariance shape must match target columns")
    val out = Matrix.newBuilder(predictors, predictors)
    var row = 0
    while row < target.rows do
      var col = 0
      while col < target.cols do
        out(targetColumns(row), targetColumns(col)) = target(row, col)
        col += 1
      row += 1
    out.result()

  private def sampleIndices(
      rows: Int,
      blockSize: Int,
      rng: ParkMillerRng
  ): Array[Int] =
    val blockStarts = (0 until rows by blockSize).toVector
    val out = new Array[Int](rows)
    var written = 0
    while written < rows do
      val start = blockStarts(rng.nextInt(blockStarts.length))
      var offset = 0
      while offset < blockSize && start + offset < rows && written < rows do
        out(written) = start + offset
        written += 1
        offset += 1
    out

  private def resampledResponse(
      fitted: DMat,
      residual: DMat,
      indices: Array[Int]
  ): DMat =
    val out = Matrix.newBuilder(fitted.rows, fitted.cols)
    var row = 0
    while row < fitted.rows do
      var voxel = 0
      while voxel < fitted.cols do
        out(row, voxel) = fitted(row, voxel) + residual(indices(row), voxel)
        voxel += 1
      row += 1
    out.result()

  private final class ParkMillerRng private (private var state: Long):
    def nextInt(bound: Int): Int =
      require(bound > 0, "random bound must be positive")
      state = (state * 48271L) % 2147483647L
      ((state - 1L) % bound.toLong).toInt

  private object ParkMillerRng:
    def apply(seed: Int): ParkMillerRng =
      new ParkMillerRng(if seed == 0 then 1L else seed.toLong)

  private def scaleMatrix(matrix: DMat, scale: Double): DMat =
    Matrix.tabulate(matrix.rows, matrix.cols) { (row, col) =>
      matrix(row, col) * scale
    }

  private def resolveRank(
      request: ReducedRankRequest,
      singularValues: DVec
  ): Either[FitError, Int] =
    val positive = positiveSingularValues(singularValues)
    val available =
      if positive.nonEmpty then positive.length
      else singularValues.length
    request match
      case ReducedRankRequest.Full =>
        Right(math.max(1, available))
      case ReducedRankRequest.Fixed(rank) =>
        if rank >= 1 && rank <= singularValues.length then Right(rank)
        else Left(FitError.InvalidFitAxis("reduced-rank GLS components", s"requested $rank components but selected data allow at most ${singularValues.length}"))
      case ReducedRankRequest.EnergyRetained(keep) =>
        if positive.isEmpty then Right(1)
        else
          val total = positive.iterator.map(value => value * value).sum
          var cumulative = 0.0
          var rank = 0
          while rank < positive.length && cumulative / total < keep do
            val value = positive(rank)
            cumulative += value * value
            rank += 1
          Right(math.max(1, math.min(rank, available)))
      case ReducedRankRequest.ResidualSumsOfSquaresBudget(budget) =>
        if positive.isEmpty then Right(1)
        else
          val ss = positive.map(value => value * value)
          var rank = 1
          var found = false
          while rank <= ss.length && !found do
            var tail = 0.0
            var i = rank
            while i < ss.length do
              tail += ss(i)
              i += 1
            if tail <= budget then found = true
            else rank += 1
          Right(math.max(1, math.min(rank, available)))

  private def positiveSingularValues(values: DVec): Vector[Double] =
    val tolerance =
      var maxValue = 1.0
      var i = 0
      while i < values.length do
        val value = math.abs(values(i))
        if value.isFinite && value > maxValue then maxValue = value
        i += 1
      values.length.toDouble * 2.220446049250313e-16 * maxValue

    val out = Vector.newBuilder[Double]
    var i = 0
    while i < values.length do
      val value = values(i)
      if value.isFinite && value > tolerance then out += value
      i += 1
    out.result()

  private def takeColumns(matrix: DMat, count: Int): DMat =
    require(count >= 1 && count <= matrix.cols, "invalid column count")
    val out = Matrix.newBuilder(matrix.rows, count)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < count do
        out(row, col) = matrix(row, col)
        col += 1
      row += 1
    out.result()

  private def takeValues(values: DVec, count: Int): DVec =
    require(count >= 1 && count <= values.length, "invalid value count")
    val out = Vec.newBuilder(count)
    var i = 0
    while i < count do
      out(i) = values(i)
      i += 1
    out.result()

  private final case class ResidualizedTask(targetDesign: DMat, response: DMat)

  private def residualizeAgainstNuisance(
      targetDesign: DMat,
      response: DMat,
      nuisanceDesign: DMat
  ): Either[FitError, ResidualizedTask] =
    if nuisanceDesign.cols == 0 then
      Right(ResidualizedTask(
        Matrix.tabulate(targetDesign.rows, targetDesign.cols)(targetDesign.apply),
        Matrix.tabulate(response.rows, response.cols)(response.apply)
      ))
    else
      val nuisanceQr = nuisanceDesign.qr(QROptions(QRPivoting.Column, Some(1e-7)))
      for
        residualizedTarget <- nuisanceQr.residualize(targetDesign).left.map(FitError.SingularDesign.apply)
        residualizedResponse <- nuisanceQr.residualize(response).left.map(FitError.SingularDesign.apply)
      yield ResidualizedTask(
        targetDesign = residualizedTarget,
        response = residualizedResponse
      )

  private def fitNuisance(
      nuisanceDesign: DMat,
      response: DMat
  ): Either[FitError, DMat] =
    if nuisanceDesign.cols == 0 then Right(Matrix.zeros(0, response.cols))
    else
      for
        qr <- fullRankQr(nuisanceDesign)
        coefficients <- qr
          .solveLeastSquares(response)
          .left
          .map(FitError.SingularDesign.apply)
      yield coefficients

  private def selectColumns(matrix: DMat, columns: Vector[Int]): DMat =
    if columns.isEmpty then Matrix.zeros(matrix.rows, 0)
    else
      val out = Matrix.newBuilder(matrix.rows, columns.length)
      var row = 0
      while row < matrix.rows do
        var col = 0
        while col < columns.length do
          out(row, col) = matrix(row, columns(col))
          col += 1
        row += 1
      out.result()

  private def subtract(left: DMat, right: DMat): DMat =
    require(left.rows == right.rows && left.cols == right.cols, "matrix subtraction requires equal shapes")
    Matrix.tabulate(left.rows, left.cols) { (row, col) =>
      left(row, col) - right(row, col)
    }

  private def fullRankQr(design: DMat): Either[FitError, QR] =
    val qr = design.qr(QROptions(QRPivoting.Column, Some(1e-7)))
    val rank = qr.diagnostics.rank.getOrElse(design.cols)
    if rank < design.cols then Left(FitError.SingularDesign(LinAlgError.RankDeficient(rank, design.cols)))
    else Right(qr)

  private def leadingQtRows(qr: QR, response: DMat, rowCount: Int): Either[FitError, DMat] =
    qr.applyQT(response)
      .left
      .map(FitError.SingularDesign.apply)
      .map { qTy =>
        Matrix.tabulate(rowCount, response.cols) { (row, col) =>
          qTy(row, col)
        }
      }

  private def scaleColumns(matrix: DMat, scale: DVec): DMat =
    require(matrix.cols == scale.length, "scale length must match matrix columns")
    Matrix.tabulate(matrix.rows, matrix.cols) { (row, col) =>
      matrix(row, col) * scale(col)
    }

  private def qScores(qr: QR, leadingRows: DMat, scoreCols: Int): Either[FitError, DMat] =
    require(leadingRows.rows <= qr.reflectors.rows, "score row count must not exceed QR rows")
    require(leadingRows.cols == scoreCols, "score column count mismatch")
    val out = Matrix.newBuilder(qr.reflectors.rows, scoreCols)
    var row = 0
    while row < leadingRows.rows do
      var col = 0
      while col < scoreCols do
        out(row, col) = leadingRows(row, col)
        col += 1
      row += 1
    qr.applyQ(out.result()).left.map(FitError.SingularDesign.apply)

  private def validateSelectedResponse(
      response: ResponseBlock,
      selectedVoxelIndices: Vector[Int]
  ): Either[FitError, Unit] =
    if response.voxels != selectedVoxelIndices.length then
      Left(FitError.InvalidFitAxis("reduced-rank GLS response", s"expected ${selectedVoxelIndices.length} voxels, got ${response.voxels}"))
    else SelectedVoxelIndices.fromInts(selectedVoxelIndices).map(_ => ())
