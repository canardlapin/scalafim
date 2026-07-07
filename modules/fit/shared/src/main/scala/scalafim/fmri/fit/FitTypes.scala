package scalafim.fmri.fit

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
  def normalizedCovariance = result.normalizedCovariance
  def residualVariance = result.residualVariance
  def voxelIndices: Vector[Int] = result.voxelIndices
  def voxels: Int = result.voxels

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
