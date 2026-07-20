package scalafim.fmri.fit

import scalafim.linalg.DoubleMatrix

enum CoefficientCovarianceScope:
  case Shared
  case Voxelwise

  def label: String =
    this match
      case Shared    => "shared"
      case Voxelwise => "voxelwise"

final case class CoefficientCovariance private (
    scope: CoefficientCovarianceScope,
    private val values: Vector[DoubleMatrix]
):
  require(values.nonEmpty, "coefficient covariance must contain at least one matrix")
  require(values.forall(matrix => matrix.rows == predictors && matrix.cols == predictors), "coefficient covariance matrices must share one square predictor shape")
  require(values.forall(matrix => matrix.copyData.forall(_.isFinite)), "coefficient covariance matrices must be finite")
  require(scope == CoefficientCovarianceScope.Voxelwise || values.length == 1, "shared coefficient covariance must contain exactly one matrix")

  def predictors: Int =
    values.head.rows

  def matrixCount: Int =
    values.length

  def canonicalMatrix: DoubleMatrix =
    values.head

  def matrices: Vector[DoubleMatrix] =
    values

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

  def matrixForVoxelPosition(position: Int): Either[FitError, DoubleMatrix] =
    if position < 0 then Left(FitError.InvalidFitAxis("voxel covariance position", s"value $position must be non-negative"))
    else
      scope match
        case CoefficientCovarianceScope.Shared =>
          Right(values.head)
        case CoefficientCovarianceScope.Voxelwise =>
          if position < values.length then Right(values(position))
          else Left(FitError.InvalidFitAxis("voxel covariance position", s"value $position is out of bounds for ${values.length} matrices"))

  def unsafeMatrixForVoxelPosition(position: Int): DoubleMatrix =
    matrixForVoxelPosition(position).fold(error => throw new IllegalArgumentException(error.message), identity)

object CoefficientCovariance:
  def shared(matrix: DoubleMatrix): Either[FitError, CoefficientCovariance] =
    validateMatrix(matrix).map(_ => CoefficientCovariance(CoefficientCovarianceScope.Shared, Vector(copyMatrix(matrix))))

  def unsafeShared(matrix: DoubleMatrix): CoefficientCovariance =
    shared(matrix).fold(error => throw new IllegalArgumentException(error.message), identity)

  def voxelwise(matrices: Vector[DoubleMatrix]): Either[FitError, CoefficientCovariance] =
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

  def unsafeVoxelwise(matrices: Vector[DoubleMatrix]): CoefficientCovariance =
    voxelwise(matrices).fold(error => throw new IllegalArgumentException(error.message), identity)

  def mergeByVoxel(blocks: IndexedSeq[CoefficientCovariance]): Either[FitError, CoefficientCovariance] =
    if blocks.isEmpty then Left(FitError.IncompatibleFitBlocks("at least one coefficient covariance block is required"))
    else if blocks.forall(_.isShared) then
      val first = blocks.head
      if blocks.forall(block => sameMatrix(block.canonicalMatrix, first.canonicalMatrix)) then Right(first)
      else Left(FitError.IncompatibleFitBlocks("all shared coefficient covariance blocks must be identical"))
    else if blocks.forall(_.isVoxelwise) then
      val predictors = blocks.head.predictors
      var i = 0
      while i < blocks.length do
        if blocks(i).predictors != predictors then
          return Left(FitError.IncompatibleFitBlocks("voxelwise coefficient covariance blocks must share predictor count"))
        i += 1
      voxelwise(blocks.iterator.flatMap(_.matrices).toVector)
    else Left(FitError.IncompatibleFitBlocks("cannot merge shared and voxelwise coefficient covariance blocks"))

  private def validateMatrix(matrix: DoubleMatrix): Either[FitError, Unit] =
    if matrix.rows <= 0 then Left(FitError.InvalidFitAxis("coefficient covariance", "must contain at least one predictor"))
    else if matrix.rows != matrix.cols then
      Left(FitError.InvalidFitAxis("coefficient covariance", s"matrix must be square, got ${matrix.rows}x${matrix.cols}"))
    else if matrix.copyData.exists(value => !value.isFinite) then
      Left(FitError.NonFiniteInput("coefficient covariance"))
    else Right(())

  private def copyMatrix(matrix: DoubleMatrix): DoubleMatrix =
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, matrix.copyData)

  private def sameMatrix(left: DoubleMatrix, right: DoubleMatrix): Boolean =
    left.rows == right.rows &&
      left.cols == right.cols &&
      left.copyData.sameElements(right.copyData)
