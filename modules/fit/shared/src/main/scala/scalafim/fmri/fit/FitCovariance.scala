package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}

enum CoefficientCovarianceScope:
  case Shared
  case Voxelwise

  def label: String =
    this match
      case Shared    => "shared"
      case Voxelwise => "voxelwise"

final case class CoefficientCovariance private (
    scope: CoefficientCovarianceScope,
    private val values: IndexedSeq[DMat]
):
  require(values.nonEmpty, "coefficient covariance must contain at least one matrix")
  require(CoefficientMatrixStorage.valid(values, predictors), "coefficient covariance matrices must be finite and share one positive square predictor shape")
  require(scope == CoefficientCovarianceScope.Voxelwise || values.length == 1, "shared coefficient covariance must contain exactly one matrix")

  def predictors: Int =
    values.head.rows

  def matrixCount: Int =
    values.length

  def canonicalMatrix: DMat =
    values.head

  /** Legacy full materialization; prefer indexed access or materialize with a limit. */
  def matrices: Vector[DMat] = values.toVector

  /** Retained numeric doubles, excluding temporary matrices, object overhead and integer axes.
    * Shared component references may be counted more than once: this is a conservative sum.
    */
  def retainedDoubleCount: Long = CoefficientMatrixStorage.retained(values)

  def materialize(maximumMatrices: Int): Either[FitError, Vector[DMat]] =
    if maximumMatrices < matrixCount then Left(FitError.InvalidFitAxis("coefficient covariance materialization", s"$matrixCount matrices exceed limit $maximumMatrices"))
    else Right(matrices)

  def selectVoxelPositions(positions: Vector[Int]): Either[FitError, CoefficientCovariance] =
    if positions.isEmpty || positions.exists(_ < 0) then Left(FitError.InvalidFitAxis("covariance selection", "nonempty non-negative positions required"))
    else if isShared then Right(this)
    else if positions.exists(_ >= matrixCount) then Left(FitError.InvalidFitAxis("covariance selection", "position out of bounds"))
    else Right(CoefficientCovariance(scope, CoefficientMatrixStorage.selected(values, positions)))

  def isShared: Boolean =
    scope == CoefficientCovarianceScope.Shared

  def isVoxelwise: Boolean =
    scope == CoefficientCovarianceScope.Voxelwise

  def validateVoxelCount(voxels: Int): Either[FitError, Unit] =
    scope match
      case CoefficientCovarianceScope.Shared =>
        Right(())
      case CoefficientCovarianceScope.Voxelwise =>
        if values.length == voxels then Right(())
        else Left(FitError.InvalidFitAxis("voxelwise coefficient covariance", s"expected $voxels matrices, got ${values.length}"))

  def matrixForVoxelPosition(position: Int): Either[FitError, DMat] =
    if position < 0 then Left(FitError.InvalidFitAxis("voxel covariance position", s"value $position must be non-negative"))
    else
      scope match
        case CoefficientCovarianceScope.Shared =>
          Right(values.head)
        case CoefficientCovarianceScope.Voxelwise =>
          if position < values.length then Right(values(position))
          else Left(FitError.InvalidFitAxis("voxel covariance position", s"value $position is out of bounds for ${values.length} matrices"))

  def unsafeMatrixForVoxelPosition(position: Int): DMat =
    matrixForVoxelPosition(position).fold(error => throw new IllegalArgumentException(error.message), identity)

object CoefficientCovariance:
  /** Exact voxelwise inverses of summed precisions; validates every inverse using
    * bounded working storage and retains the structured inputs, not all inverses.
    */
  private[fit] def fromPrecisionSum(components: Vector[IndexedSeq[DMat]]): Either[FitError, CoefficientCovariance] =
    CoefficientMatrixStorage.inverseSum(components).map(values => CoefficientCovariance(CoefficientCovarianceScope.Voxelwise, values))

  def shared(matrix: DMat): Either[FitError, CoefficientCovariance] =
    validateMatrix(matrix).map(_ => CoefficientCovariance(CoefficientCovarianceScope.Shared, Vector(copyMatrix(matrix))))

  def unsafeShared(matrix: DMat): CoefficientCovariance =
    shared(matrix).fold(error => throw new IllegalArgumentException(error.message), identity)

  def voxelwise(matrices: Vector[DMat]): Either[FitError, CoefficientCovariance] =
    if matrices.isEmpty then Left(FitError.InvalidFitAxis("voxelwise coefficient covariance", "must contain at least one matrix"))
    else
      val first = matrices.head
      validateMatrix(first).flatMap { _ =>
        var error: FitError | Null = null
        var i = 1
        while i < matrices.length && error == null do
          validateMatrix(matrices(i)) match
            case Left(err) =>
              error = err
            case Right(_) =>
              if matrices(i).rows != first.rows || matrices(i).cols != first.cols then
                error = FitError.InvalidFitAxis("voxelwise coefficient covariance", "matrices must share one predictor shape")
          i += 1
        error match
          case null => Right(CoefficientCovariance(CoefficientCovarianceScope.Voxelwise, matrices.map(copyMatrix)))
          case err  => Left(err)
      }

  def unsafeVoxelwise(matrices: Vector[DMat]): CoefficientCovariance =
    voxelwise(matrices).fold(error => throw new IllegalArgumentException(error.message), identity)

  def mergeByVoxel(blocks: IndexedSeq[CoefficientCovariance]): Either[FitError, CoefficientCovariance] =
    if blocks.isEmpty then Left(FitError.IncompatibleFitBlocks("at least one coefficient covariance block is required"))
    else if blocks.forall(_.isShared) then
      val first = blocks.head
      if blocks.forall(block => sameMatrix(block.canonicalMatrix, first.canonicalMatrix)) then Right(first)
      else Left(FitError.IncompatibleFitBlocks("all shared coefficient covariance blocks must be identical"))
    else if blocks.forall(_.isVoxelwise) then
      val totalMatrices = blocks.foldLeft(0L)((total, block) => total + block.matrixCount.toLong)
      if totalMatrices > Int.MaxValue then
        return Left(FitError.IncompatibleFitBlocks(s"combined voxel covariance count $totalMatrices exceeds Int capacity"))
      val predictors = blocks.head.predictors
      var i = 0
      while i < blocks.length do
        if blocks(i).predictors != predictors then
          return Left(FitError.IncompatibleFitBlocks("voxelwise coefficient covariance blocks must share predictor count"))
        i += 1
      Right(CoefficientCovariance(CoefficientCovarianceScope.Voxelwise, CoefficientMatrixStorage.concatenated(blocks.map(_.values).toVector)))
    else Left(FitError.IncompatibleFitBlocks("cannot merge shared and voxelwise coefficient covariance blocks"))

  private def validateMatrix(matrix: DMat): Either[FitError, Unit] =
    if matrix.rows <= 0 then Left(FitError.InvalidFitAxis("coefficient covariance", "must contain at least one predictor"))
    else if matrix.rows != matrix.cols then
      Left(FitError.InvalidFitAxis("coefficient covariance", s"matrix must be square, got ${matrix.rows}x${matrix.cols}"))
    else if !allFinite(matrix) then
      Left(FitError.NonFiniteInput("coefficient covariance"))
    else Right(())

  private def copyMatrix(matrix: DMat): DMat =
    Matrix.tabulate(matrix.rows, matrix.cols)(matrix.apply)

  private def sameMatrix(left: DMat, right: DMat): Boolean =
    if left.rows != right.rows || left.cols != right.cols then false
    else
      var row = 0
      while row < left.rows do
        var col = 0
        while col < left.cols do
          if left(row, col) != right(row, col) then return false
          col += 1
        row += 1
      true

  private def allFinite(matrix: DMat): Boolean =
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        if !matrix(row, col).isFinite then return false
        col += 1
      row += 1
    true
