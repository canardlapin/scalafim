package scalafim.fmri.fit

import gale.linalg.{DMat, DVec, Matrix, Vec}

opaque type RunIndex = Int

object RunIndex:
  def apply(value: Int): Either[FitError, RunIndex] =
    if value >= 0 then Right(value)
    else Left(FitError.InvalidFitAxis("run index", s"value $value must be non-negative"))

  def unsafe(value: Int): RunIndex =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (index: RunIndex)
    inline def value: Int = index

opaque type SelectedRowIndex = Int

object SelectedRowIndex:
  def apply(value: Int): Either[FitError, SelectedRowIndex] =
    if value >= 0 then Right(value)
    else Left(FitError.InvalidFitAxis("selected row index", s"value $value must be non-negative"))

  def unsafe(value: Int): SelectedRowIndex =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (index: SelectedRowIndex)
    inline def value: Int = index

opaque type SelectedTimepointIndex = Int

object SelectedTimepointIndex:
  def apply(value: Int): Either[FitError, SelectedTimepointIndex] =
    if value >= 0 then Right(value)
    else Left(FitError.InvalidFitAxis("selected timepoint index", s"value $value must be non-negative"))

  def unsafe(value: Int): SelectedTimepointIndex =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (index: SelectedTimepointIndex)
    inline def value: Int = index

opaque type SelectedVoxelIndex = Int

object SelectedVoxelIndex:
  def apply(value: Int): Either[FitError, SelectedVoxelIndex] =
    if value >= 0 then Right(value)
    else Left(FitError.InvalidFitAxis("selected voxel index", s"value $value must be non-negative"))

  def unsafe(value: Int): SelectedVoxelIndex =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (index: SelectedVoxelIndex)
    inline def value: Int = index

opaque type ResidualDegreesOfFreedom = Int

object ResidualDegreesOfFreedom:
  def apply(value: Int): Either[FitError, ResidualDegreesOfFreedom] =
    if value > 0 then Right(value)
    else Left(FitError.NonPositiveResidualDegreesOfFreedom(value))

  def unsafe(value: Int): ResidualDegreesOfFreedom =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (df: ResidualDegreesOfFreedom)
    inline def value: Int = df

final case class SelectedVoxelIndices private (values: Vector[SelectedVoxelIndex]):
  require(values.nonEmpty, "selected voxel indices must be non-empty")
  def length: Int = values.length
  def toVector: Vector[Int] = values.map(_.value)

object SelectedVoxelIndices:
  def fromInts(values: Vector[Int]): Either[FitError, SelectedVoxelIndices] =
    if values.isEmpty then Left(FitError.InvalidFitAxis("selected voxel indices", "must be non-empty"))
    else if values.distinct.length != values.length then Left(FitError.InvalidFitAxis("selected voxel indices", "must be unique"))
    else
      val out = Vector.newBuilder[SelectedVoxelIndex]
      var i = 0
      while i < values.length do
        SelectedVoxelIndex(values(i)) match
          case Right(index) =>
            out += index
          case Left(error) =>
            return Left(error)
        i += 1
      Right(new SelectedVoxelIndices(out.result()))

  def unsafe(values: Vector[Int]): SelectedVoxelIndices =
    fromInts(values).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class SelectedTimepointIndices private (values: Vector[SelectedTimepointIndex]):
  require(values.nonEmpty, "selected timepoint indices must be non-empty")
  def length: Int = values.length
  def toVector: Vector[Int] = values.map(_.value)

object SelectedTimepointIndices:
  def fromInts(values: Vector[Int]): Either[FitError, SelectedTimepointIndices] =
    if values.isEmpty then Left(FitError.InvalidFitAxis("selected timepoint indices", "must be non-empty"))
    else
      val out = Vector.newBuilder[SelectedTimepointIndex]
      var i = 0
      while i < values.length do
        SelectedTimepointIndex(values(i)) match
          case Right(index) =>
            out += index
          case Left(error) =>
            return Left(error)
        i += 1
      Right(new SelectedTimepointIndices(out.result()))

  def unsafe(values: Vector[Int]): SelectedTimepointIndices =
    fromInts(values).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class InferenceReadyDenseFit private[fit] (
    result: DenseFmriFitResult,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom
):
  def columnNames: Vector[String] = result.columnNames
  def coefficients: CoefficientBlock = result.coefficients
  def normalizedCovariance: DMat = result.inference.normalizedCovariance
  def coefficientCovariance: CoefficientCovariance = result.inference.covariance
  def inferenceScope: CoefficientInferenceScope = result.inference.scope
  def varianceScale: DVec = result.inference.varianceScale
  def voxelIndices: Vector[Int] = result.voxelIndices
  def voxels: Int = result.voxels
  def voxelStatuses: Vector[VoxelFitStatus] = result.resolvedVoxelStatuses
  def fitExclusions: Vector[VoxelInferenceExclusion] = result.fitExclusions

sealed trait CoefficientInferenceScope:
  def validateFor(predictors: Int): Either[FitError, Unit]
  def allowedIndices(predictors: Int): Vector[Int]
  def label: String
  def validateTContrast(
      contrastName: String,
      weights: Array[Double],
      columnNames: Vector[String]
  ): Either[FitError, Unit]
  def validateFContrast(
      contrastName: String,
      weights: DMat,
      columnNames: Vector[String]
  ): Either[FitError, Unit]

object CoefficientInferenceScope:
  case object All extends CoefficientInferenceScope:
    def allowedIndices(predictors: Int): Vector[Int] =
      (0 until math.max(0, predictors)).toVector

    val label: String = "all coefficients"

    def validateFor(predictors: Int): Either[FitError, Unit] =
      if predictors > 0 then Right(())
      else Left(FitError.InvalidFitAxis("coefficient inference scope", "requires at least one predictor"))

    def validateTContrast(
        contrastName: String,
        weights: Array[Double],
        columnNames: Vector[String]
    ): Either[FitError, Unit] =
      if weights.length == columnNames.length then Right(())
      else Left(FitError.InvalidFitAxis("contrast weights", s"expected ${columnNames.length} coefficients, got ${weights.length}"))

    def validateFContrast(
        contrastName: String,
        weights: DMat,
        columnNames: Vector[String]
    ): Either[FitError, Unit] =
      if weights.rows == columnNames.length then Right(())
      else Left(FitError.InvalidFitAxis("contrast weights", s"expected ${columnNames.length} coefficient rows, got ${weights.rows}"))

  final case class Only private[fit] (
      indices: Vector[Int],
      label: String
  ) extends CoefficientInferenceScope:
    require(indices.nonEmpty, "limited coefficient inference scope must contain at least one index")
    require(indices.distinct.length == indices.length, "limited coefficient inference scope indices must be unique")
    require(indices.forall(_ >= 0), "limited coefficient inference scope indices must be non-negative")
    require(label.nonEmpty, "limited coefficient inference scope label must be non-empty")

    def allowedIndices(predictors: Int): Vector[Int] =
      indices.filter(_ < predictors)

    def validateFor(predictors: Int): Either[FitError, Unit] =
      if predictors <= 0 then Left(FitError.InvalidFitAxis("coefficient inference scope", "requires at least one predictor"))
      else if indices.forall(_ < predictors) then Right(())
      else Left(FitError.InvalidFitAxis("coefficient inference scope", s"indices must be in [0, ${predictors - 1}]"))

    def validateTContrast(
        contrastName: String,
        weights: Array[Double],
        columnNames: Vector[String]
    ): Either[FitError, Unit] =
      if weights.length != columnNames.length then
        Left(FitError.InvalidFitAxis("contrast weights", s"expected ${columnNames.length} coefficients, got ${weights.length}"))
      else rejectDisallowed(contrastName, columnNames) { index =>
        weights(index) != 0.0
      }

    def validateFContrast(
        contrastName: String,
        weights: DMat,
        columnNames: Vector[String]
    ): Either[FitError, Unit] =
      if weights.rows != columnNames.length then
        Left(FitError.InvalidFitAxis("contrast weights", s"expected ${columnNames.length} coefficient rows, got ${weights.rows}"))
      else rejectDisallowed(contrastName, columnNames) { index =>
        var nonZero = false
        var col = 0
        while col < weights.cols && !nonZero do
          if weights(index, col) != 0.0 then nonZero = true
          col += 1
        nonZero
      }

    private def rejectDisallowed(
        contrastName: String,
        columnNames: Vector[String]
    )(
        isNonZero: Int => Boolean
    ): Either[FitError, Unit] =
      val allowed = indices.toSet
      var index = 0
      while index < columnNames.length do
        if isNonZero(index) && !allowed.contains(index) then
          return Left(FitError.NonEstimableContrast(
            contrastName,
            s"column '${columnNames(index)}' is outside the $label inference scope"
          ))
        index += 1
      Right(())

  def only(
      indices: Vector[Int],
      label: String
  ): Either[FitError, CoefficientInferenceScope] =
    if indices.isEmpty then Left(FitError.InvalidFitAxis("coefficient inference scope", "must contain at least one index"))
    else if indices.distinct.length != indices.length then Left(FitError.InvalidFitAxis("coefficient inference scope", "indices must be unique"))
    else if indices.exists(_ < 0) then Left(FitError.InvalidFitAxis("coefficient inference scope", "indices must be non-negative"))
    else if label.isEmpty then Left(FitError.InvalidFitAxis("coefficient inference scope", "label must be non-empty"))
    else Right(Only(indices, label))

  def unsafeOnly(indices: Vector[Int], label: String): CoefficientInferenceScope =
    only(indices, label).fold(error => throw new IllegalArgumentException(error.message), identity)

enum CoefficientInferenceMethod:
  case Classical
  case ReducedRankConditional
  case ReducedRankBootstrap(replicates: Int, blockSize: Int, seed: Int)
  case ReducedRankFullRankVoxelwiseFallback

  def label: String =
    this match
      case Classical                                => "classical"
      case ReducedRankConditional                   => "reduced_rank_conditional"
      case ReducedRankBootstrap(_, _, _)            => "reduced_rank_bootstrap"
      case ReducedRankFullRankVoxelwiseFallback     => "reduced_rank_full_rank_voxelwise_fallback"

final case class CoefficientInference private (
    scope: CoefficientInferenceScope,
    standardErrors: StandardErrorBlock,
    covariance: CoefficientCovariance,
    varianceScale: DVec,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    method: CoefficientInferenceMethod
):
  require(standardErrors.predictors == covariance.predictors, "inference standard errors must match covariance predictors")
  require(standardErrors.voxels == varianceScale.length, "inference standard errors must match variance scale voxels")
  require(covariance.validateVoxelCount(varianceScale.length).isRight, "inference covariance must be shared or match voxel count")
  require(scope.validateFor(covariance.predictors).isRight, "inference scope must be valid for covariance predictors")
  require(varianceScale.toSeq.forall(value => value >= 0.0 && value.isFinite), "inference variance scale must be non-negative and finite")

  def predictors: Int = covariance.predictors
  def voxels: Int = varianceScale.length
  def normalizedCovariance: DMat = covariance.canonicalMatrix

  def standardError(predictor: Int, voxel: Int): Option[Double] =
    if predictor < 0 || predictor >= predictors || voxel < 0 || voxel >= voxels then None
    else if scope.allowedIndices(predictors).contains(predictor) then Some(standardErrors(predictor, voxel))
    else None

  def restrict(
      restrictedScope: CoefficientInferenceScope,
      restrictedMethod: CoefficientInferenceMethod
  ): Either[FitError, CoefficientInference] =
    CoefficientInference.fromCovariance(
      restrictedScope,
      covariance,
      varianceScale,
      residualDegreesOfFreedom,
      restrictedMethod
    )

  def selectVoxelPositions(positions: Vector[Int]): Either[FitError, CoefficientInference] =
    if positions.isEmpty then Left(FitError.InvalidFitAxis("coefficient inference voxel positions", "must be non-empty"))
    else if positions.exists(position => position < 0 || position >= voxels) then
      Left(FitError.InvalidFitAxis("coefficient inference voxel positions", s"must be in [0, ${voxels - 1}]"))
    else
      val selectedCovariance =
        covariance.scope match
          case CoefficientCovarianceScope.Shared =>
            Right(covariance)
          case CoefficientCovarianceScope.Voxelwise =>
            CoefficientCovariance.voxelwise(positions.map(covariance.matrices))
      selectedCovariance.flatMap { selected =>
        val selectedVariance = DVec.fromSeq(positions.map(varianceScale.apply))
        val selectedStandardErrors =
          val out = Matrix.newBuilder(predictors, positions.length)
          var predictor = 0
          while predictor < predictors do
            var local = 0
            while local < positions.length do
              out(predictor, local) = standardErrors(predictor, positions(local))
              local += 1
            predictor += 1
          StandardErrorBlock(out.result())
        CoefficientInference.fromExisting(
          scope,
          selectedStandardErrors,
          selected,
          selectedVariance,
          residualDegreesOfFreedom,
          method
        )
      }

object CoefficientInference:
  def fromExisting(
      scope: CoefficientInferenceScope,
      standardErrors: StandardErrorBlock,
      covariance: CoefficientCovariance,
      varianceScale: DVec,
      residualDegreesOfFreedom: ResidualDegreesOfFreedom,
      method: CoefficientInferenceMethod = CoefficientInferenceMethod.Classical
  ): Either[FitError, CoefficientInference] =
    for
      _ <- scope.validateFor(covariance.predictors)
      _ <- covariance.validateVoxelCount(varianceScale.length)
      _ <- validateStandardErrors(standardErrors, covariance.predictors, varianceScale.length)
      _ <- validateVarianceScale(varianceScale)
    yield new CoefficientInference(scope, standardErrors, covariance, varianceScale, residualDegreesOfFreedom, method)

  def unsafeFromExisting(
      scope: CoefficientInferenceScope,
      standardErrors: StandardErrorBlock,
      covariance: CoefficientCovariance,
      varianceScale: DVec,
      residualDegreesOfFreedom: ResidualDegreesOfFreedom,
      method: CoefficientInferenceMethod = CoefficientInferenceMethod.Classical
  ): CoefficientInference =
    fromExisting(scope, standardErrors, covariance, varianceScale, residualDegreesOfFreedom, method)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def fromCovariance(
      scope: CoefficientInferenceScope,
      covariance: CoefficientCovariance,
      varianceScale: DVec,
      residualDegreesOfFreedom: ResidualDegreesOfFreedom,
      method: CoefficientInferenceMethod
  ): Either[FitError, CoefficientInference] =
    for
      _ <- scope.validateFor(covariance.predictors)
      _ <- covariance.validateVoxelCount(varianceScale.length)
      _ <- validateVarianceScale(varianceScale)
      standardErrors <- standardErrors(scope, covariance, varianceScale)
    yield new CoefficientInference(scope, standardErrors, covariance, varianceScale, residualDegreesOfFreedom, method)

  def mergeByVoxel(blocks: IndexedSeq[CoefficientInference]): Either[FitError, CoefficientInference] =
    if blocks.isEmpty then Left(FitError.IncompatibleFitBlocks("at least one coefficient inference block is required"))
    else
      val first = blocks.head
      var i = 0
      while i < blocks.length do
        val block = blocks(i)
        if block.scope != first.scope then
          return Left(FitError.IncompatibleFitBlocks("all dense blocks must have identical inference scopes"))
        if block.method != first.method then
          return Left(FitError.IncompatibleFitBlocks("all dense blocks must have identical inference methods"))
        if block.residualDegreesOfFreedom != first.residualDegreesOfFreedom then
          return Left(FitError.IncompatibleFitBlocks("all dense blocks must have identical inference degrees of freedom"))
        if block.predictors != first.predictors then
          return Left(FitError.IncompatibleFitBlocks("all dense blocks must have identical inference predictor counts"))
        i += 1

      for
        covariance <- CoefficientCovariance.mergeByVoxel(blocks.map(_.covariance))
        varianceScale = bindVectors(blocks.map(_.varianceScale))
        standardErrors = StandardErrorBlock(bindMatrixColumns(blocks.map(_.standardErrors.value)))
        merged <- fromExisting(
          first.scope,
          standardErrors,
          covariance,
          varianceScale,
          first.residualDegreesOfFreedom,
          first.method
        )
      yield merged

  private def standardErrors(
      scope: CoefficientInferenceScope,
      covariance: CoefficientCovariance,
      varianceScale: DVec
  ): Either[FitError, StandardErrorBlock] =
    val predictors = covariance.predictors
    val voxels = varianceScale.length
    val allowed = scope.allowedIndices(predictors).toSet
    val out = Matrix.newBuilder(predictors, voxels)
    var voxel = 0
    while voxel < voxels do
      covariance.matrixForVoxelPosition(voxel) match
        case Left(error) =>
          return Left(error)
        case Right(matrix) =>
          var predictor = 0
          while predictor < predictors do
            if allowed.contains(predictor) then
              val variance = matrix(predictor, predictor) * varianceScale(voxel)
              if variance < -1e-12 || !variance.isFinite then
                return Left(FitError.InvalidFitAxis("coefficient inference variance", s"predictor $predictor voxel $voxel has value $variance"))
              out(predictor, voxel) = math.sqrt(math.max(0.0, variance))
            predictor += 1
      voxel += 1
    Right(StandardErrorBlock(out.result()))

  private def validateStandardErrors(
      standardErrors: StandardErrorBlock,
      predictors: Int,
      voxels: Int
  ): Either[FitError, Unit] =
    if standardErrors.predictors != predictors then
      Left(FitError.InvalidFitAxis("coefficient inference standard errors", s"expected $predictors predictor rows, got ${standardErrors.predictors}"))
    else if standardErrors.voxels != voxels then
      Left(FitError.InvalidFitAxis("coefficient inference standard errors", s"expected $voxels voxel columns, got ${standardErrors.voxels}"))
    else
      var row = 0
      while row < standardErrors.value.rows do
        var col = 0
        while col < standardErrors.value.cols do
          val value = standardErrors.value(row, col)
          if value < 0.0 || !value.isFinite then
            return Left(FitError.InvalidFitAxis("coefficient inference standard errors", "must be non-negative and finite"))
          col += 1
        row += 1
      Right(())

  private def validateVarianceScale(varianceScale: DVec): Either[FitError, Unit] =
    if varianceScale.length <= 0 then Left(FitError.InvalidFitAxis("coefficient inference variance scale", "must be non-empty"))
    else if varianceScale.toSeq.exists(value => value < 0.0 || !value.isFinite) then
      Left(FitError.InvalidFitAxis("coefficient inference variance scale", "must be non-negative and finite"))
    else Right(())

  private def bindMatrixColumns(matrices: IndexedSeq[DMat]): DMat =
    val rows = matrices.head.rows
    val totalCols = matrices.iterator.map(_.cols).sum
    val out = Matrix.newBuilder(rows, totalCols)
    var row = 0
    while row < rows do
      var offset = 0
      var block = 0
      while block < matrices.length do
        val matrix = matrices(block)
        var col = 0
        while col < matrix.cols do
          out(row, offset + col) = matrix(row, col)
          col += 1
        offset += matrix.cols
        block += 1
      row += 1
    out.result()

  private def bindVectors(vectors: IndexedSeq[DVec]): DVec =
    val out = Vec.newBuilder(vectors.iterator.map(_.length).sum)
    var offset = 0
    var vectorIndex = 0
    while vectorIndex < vectors.length do
      val vector = vectors(vectorIndex)
      var index = 0
      while index < vector.length do
        out(offset + index) = vector(index)
        index += 1
      offset += vector.length
      vectorIndex += 1
    out.result()

enum FitImageMapKind:
  case Coefficients
  case StandardErrors
  case TStatistic(contrastName: String)
  case TContrastBundle(contrastName: String)
  case FStatistic(contrastName: String)
  case FContrastBundle(contrastName: String)
  case Custom(customLabel: String)

  def label: String =
    this match
      case Coefficients                 => "coefficients"
      case StandardErrors               => "standard_errors"
      case TStatistic(contrastName)     => s"t_$contrastName"
      case TContrastBundle(contrastName) => s"t_$contrastName"
      case FStatistic(contrastName)     => s"f_$contrastName"
      case FContrastBundle(contrastName) => s"f_$contrastName"
      case Custom(value)                => value
