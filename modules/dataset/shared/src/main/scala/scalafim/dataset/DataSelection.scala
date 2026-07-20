package scalafim.dataset

import scalafim.image.Mask

opaque type TimepointIndex = Int

object TimepointIndex:
  def make(value: Int): Either[DatasetError, TimepointIndex] =
    if value < 0 then Left(DatasetError.NegativeIndex(DatasetAxis.Timepoint, value))
    else Right(value)

  def unsafe(value: Int): TimepointIndex =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (index: TimepointIndex)
    inline def value: Int = index

  private[dataset] inline def raw(index: TimepointIndex): Int = index

opaque type VoxelIndex = Int

object VoxelIndex:
  def make(value: Int): Either[DatasetError, VoxelIndex] =
    if value < 0 then Left(DatasetError.NegativeIndex(DatasetAxis.Voxel, value))
    else Right(value)

  def unsafe(value: Int): VoxelIndex =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (index: VoxelIndex)
    inline def value: Int = index

  private[dataset] inline def raw(index: VoxelIndex): Int = index

enum TimepointSelection:
  case All
  case Indices(values: Vector[TimepointIndex])

  def resolve(size: Int): Either[DatasetError, Vector[TimepointIndex]] =
    this match
      case TimepointSelection.All =>
        resolveAllTimepoints(size)
      case TimepointSelection.Indices(values) =>
        validateTimepoints(values, size)

object TimepointSelection:
  def fromInts(values: Int*): Either[DatasetError, TimepointSelection] =
    buildTypedSelection(DatasetAxis.Timepoint, values.toVector, TimepointIndex.make)
      .map(TimepointSelection.Indices.apply)

  def indices(values: Int*): TimepointSelection =
    fromInts(values*).fold(error => throw new IllegalArgumentException(error.message), identity)

enum VoxelSelection:
  case All
  case AllSpatial
  case Indices(values: Vector[VoxelIndex])

  def resolve(size: Int): Either[DatasetError, Vector[VoxelIndex]] =
    this match
      case VoxelSelection.All | VoxelSelection.AllSpatial =>
        resolveAllVoxels(size)
      case VoxelSelection.Indices(values) =>
        validateVoxels(values, size)

object VoxelSelection:
  def fromInts(values: Int*): Either[DatasetError, VoxelSelection] =
    buildTypedSelection(DatasetAxis.Voxel, values.toVector, VoxelIndex.make)
      .map(VoxelSelection.Indices.apply)

  def indices(values: Int*): VoxelSelection =
    fromInts(values*).fold(error => throw new IllegalArgumentException(error.message), identity)

enum VoxelDomainKind:
  case FullSpatial
  case ActiveMask

final class VoxelDomain private (
    val kind: VoxelDomainKind,
    val spatialSize: Int,
    private val readableVoxelValues: Vector[VoxelIndex],
    private val readableLookup: Array[Boolean]
):
  require(spatialSize > 0, "voxel domain spatial size must be positive")
  require(readableVoxelValues.nonEmpty, "voxel domain must contain at least one readable voxel")
  require(readableLookup.length == spatialSize, "voxel domain lookup length must match spatial size")

  def voxels: Vector[VoxelIndex] =
    readableVoxelValues

  def indices: Vector[Int] =
    readableVoxelValues.map(VoxelIndex.raw)

  def nVoxels: Int =
    readableVoxelValues.length

  def isFullSpatial: Boolean =
    kind == VoxelDomainKind.FullSpatial

  def contains(voxel: VoxelIndex): Boolean =
    val value = VoxelIndex.raw(voxel)
    value >= 0 && value < spatialSize && readableLookup(value)

  def resolve(selection: VoxelSelection): Either[DatasetError, Vector[VoxelIndex]] =
    selection match
      case VoxelSelection.All =>
        Right(readableVoxelValues)
      case VoxelSelection.AllSpatial =>
        resolveAllVoxels(spatialSize)
      case VoxelSelection.Indices(values) =>
        validateVoxels(values, spatialSize).flatMap(validateReadable)

  private def validateReadable(values: Vector[VoxelIndex]): Either[DatasetError, Vector[VoxelIndex]] =
    var i = 0
    while i < values.length do
      val voxel = values(i)
      val value = VoxelIndex.raw(voxel)
      if !readableLookup(value) then return Left(DatasetError.VoxelOutsideMask(value))
      i += 1
    Right(values)

object VoxelDomain:
  def full(shape: DatasetShape): Either[DatasetError, VoxelDomain] =
    fromVoxels(
      kind = VoxelDomainKind.FullSpatial,
      spatialSize = shape.spatialSize,
      voxels = Vector.tabulate(shape.spatialSize)(VoxelIndex.unsafe)
    )

  def fullUnsafe(shape: DatasetShape): VoxelDomain =
    full(shape).fold(error => throw new IllegalArgumentException(error.message), identity)

  def active(
      spatialSize: Int,
      voxels: Vector[VoxelIndex]
  ): Either[DatasetError, VoxelDomain] =
    fromVoxels(VoxelDomainKind.ActiveMask, spatialSize, voxels)

  def activeUnsafe(
      spatialSize: Int,
      voxels: Vector[VoxelIndex]
  ): VoxelDomain =
    active(spatialSize, voxels).fold(error => throw new IllegalArgumentException(error.message), identity)

  def fromMask(mask: Mask.MaskVol, shape: DatasetShape): Either[DatasetError, VoxelDomain] =
    if mask.space.spatialDims != shape.spatialDims then
      Left(DatasetError.ShapeMismatch("mask/space dimension mismatch"))
    else if mask.space.spacing != shape.space.spacing || mask.space.origin != shape.space.origin then
      Left(DatasetError.ShapeMismatch("mask/space mismatch"))
    else
      val maskIndices = Mask.indices(mask)
      val voxels = Vector.newBuilder[VoxelIndex]
      voxels.sizeHint(maskIndices.length)
      var i = 0
      while i < maskIndices.length do
        VoxelIndex.make(maskIndices(i)) match
          case Left(error) => return Left(error)
          case Right(voxel) => voxels += voxel
        i += 1
      fromVoxels(VoxelDomainKind.ActiveMask, shape.spatialSize, voxels.result())

  private def fromVoxels(
      kind: VoxelDomainKind,
      spatialSize: Int,
      voxels: Vector[VoxelIndex]
  ): Either[DatasetError, VoxelDomain] =
    if spatialSize <= 0 then Left(DatasetError.NonPositiveAxisSize(DatasetAxis.Voxel, spatialSize))
    else
      validateVoxels(voxels, spatialSize).map { valid =>
        val lookup = Array.fill(spatialSize)(false)
        var i = 0
        while i < valid.length do
          lookup(VoxelIndex.raw(valid(i))) = true
          i += 1
        new VoxelDomain(kind, spatialSize, valid, lookup)
      }

enum IndexSelection:
  case All
  case Indices(values: Vector[Int])

object IndexSelection:
  def indices(values: Int*): IndexSelection =
    IndexSelection.Indices(values.toVector)

final class DataSelection private (
    val time: TimepointSelection,
    val voxels: VoxelSelection
):
  def resolveEither(shape: DatasetShape): Either[DatasetError, ResolvedDataSelection] =
    VoxelDomain.full(shape).flatMap(resolveEither(shape, _))

  def resolveEither(
      shape: DatasetShape,
      voxelDomain: VoxelDomain
  ): Either[DatasetError, ResolvedDataSelection] =
    for
      timepoints <- time.resolve(shape.timepoints)
      voxelIndices <- voxelDomain.resolve(voxels)
      resolved <- ResolvedDataSelection.make(timepoints, voxelIndices)
    yield resolved

  def resolve(shape: DatasetShape): ResolvedDataSelection =
    resolveEither(shape).fold(error => throw new IllegalArgumentException(error.message), identity)

  override def equals(other: Any): Boolean =
    other match
      case that: DataSelection => time == that.time && voxels == that.voxels
      case _ => false

  override def hashCode(): Int =
    31 * time.hashCode() + voxels.hashCode()

  override def toString: String =
    s"DataSelection(time=$time, voxels=$voxels)"

object DataSelection:
  type TimeInput = TimepointSelection | IndexSelection
  type VoxelInput = VoxelSelection | IndexSelection

  val All: DataSelection =
    new DataSelection(TimepointSelection.All, VoxelSelection.All)

  def apply(
      time: TimeInput = TimepointSelection.All,
      voxels: VoxelInput = VoxelSelection.All
  ): DataSelection =
    new DataSelection(toTimepointSelection(time), toVoxelSelection(voxels))

  private def toTimepointSelection(selection: TimeInput): TimepointSelection =
    selection match
      case typed: TimepointSelection => typed
      case legacy: IndexSelection =>
        legacy match
          case IndexSelection.All => TimepointSelection.All
          case IndexSelection.Indices(values) =>
            TimepointSelection.fromInts(values*).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def toVoxelSelection(selection: VoxelInput): VoxelSelection =
    selection match
      case typed: VoxelSelection => typed
      case legacy: IndexSelection =>
        legacy match
          case IndexSelection.All => VoxelSelection.All
          case IndexSelection.Indices(values) =>
            VoxelSelection.fromInts(values*).fold(error => throw new IllegalArgumentException(error.message), identity)

final class ResolvedDataSelection private (
    val timepointIndices: Vector[TimepointIndex],
    val voxelIndexValues: Vector[VoxelIndex]
):
  def timepoints: Vector[Int] =
    timepointIndices.map(TimepointIndex.raw)

  def voxels: Vector[Int] =
    voxelIndexValues.map(VoxelIndex.raw)

  def nTimepoints: Int =
    timepointIndices.length

  def nVoxels: Int =
    voxelIndexValues.length

object ResolvedDataSelection:
  def make(
      timepoints: Vector[TimepointIndex],
      voxels: Vector[VoxelIndex]
  ): Either[DatasetError, ResolvedDataSelection] =
    if timepoints.isEmpty then Left(DatasetError.EmptySelection(DatasetAxis.Timepoint))
    else if voxels.isEmpty then Left(DatasetError.EmptySelection(DatasetAxis.Voxel))
    else Right(new ResolvedDataSelection(timepoints, voxels))

  private[dataset] def unsafe(
      timepoints: Vector[TimepointIndex],
      voxels: Vector[VoxelIndex]
  ): ResolvedDataSelection =
    make(timepoints, voxels).fold(error => throw new IllegalArgumentException(error.message), identity)

private def resolveAllTimepoints(size: Int): Either[DatasetError, Vector[TimepointIndex]] =
  if size <= 0 then Left(DatasetError.NonPositiveAxisSize(DatasetAxis.Timepoint, size))
  else Right(Vector.tabulate(size)(TimepointIndex.unsafe))

private def resolveAllVoxels(size: Int): Either[DatasetError, Vector[VoxelIndex]] =
  if size <= 0 then Left(DatasetError.NonPositiveAxisSize(DatasetAxis.Voxel, size))
  else Right(Vector.tabulate(size)(VoxelIndex.unsafe))

private def validateTimepoints(
    values: Vector[TimepointIndex],
    size: Int
): Either[DatasetError, Vector[TimepointIndex]] =
  validateResolvedValues(DatasetAxis.Timepoint, values.map(TimepointIndex.raw), size).map(values => values.map(TimepointIndex.unsafe))

private def validateVoxels(
    values: Vector[VoxelIndex],
    size: Int
): Either[DatasetError, Vector[VoxelIndex]] =
  validateResolvedValues(DatasetAxis.Voxel, values.map(VoxelIndex.raw), size).map(values => values.map(VoxelIndex.unsafe))

private def buildTypedSelection[A](
    axis: DatasetAxis,
    values: Vector[Int],
    makeIndex: Int => Either[DatasetError, A]
): Either[DatasetError, Vector[A]] =
  if values.isEmpty then Left(DatasetError.EmptySelection(axis))
  else
    val out = Vector.newBuilder[A]
    out.sizeHint(values.length)
    var i = 0
    var error = Option.empty[DatasetError]
    while i < values.length && error.isEmpty do
      makeIndex(values(i)) match
        case Left(err) => error = Some(err)
        case Right(index) => out += index
      i += 1

    error match
      case Some(err) => Left(err)
      case None => Right(out.result())

private def validateResolvedValues(
    axis: DatasetAxis,
    values: Vector[Int],
    size: Int
): Either[DatasetError, Vector[Int]] =
  if size <= 0 then Left(DatasetError.NonPositiveAxisSize(axis, size))
  else if values.isEmpty then Left(DatasetError.EmptySelection(axis))
  else
    val seen = scala.collection.mutable.HashSet.empty[Int]
    var i = 0
    var error = Option.empty[DatasetError]
    while i < values.length && error.isEmpty do
      val value = values(i)
      if value < 0 then error = Some(DatasetError.NegativeIndex(axis, value))
      else if value >= size then error = Some(DatasetError.IndexOutOfBounds(axis, value, size))
      else if seen.contains(value) then error = Some(DatasetError.DuplicateSelection(axis, value))
      else seen += value
      i += 1

    error match
      case Some(err) => Left(err)
      case None => Right(values)
