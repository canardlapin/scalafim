package scalafim.dataset

import scalafim.image.{
  GridCompatibility,
  Indexing,
  Mask,
  SampleSpaces,
  SomeSampleSpace,
  VoxelCoord
}
import scalafim.image.GridDomainOps.*
import scalafim.image.SampleSpaces.*
import scalafim.image.space
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.locus.GridDomain
import locus4s.Selection
import locus4s.SpaceMismatch
import scalafim.locus.Selection as LocusSelection

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
  case Window(start: TimepointIndex, length: Int)
  /** Keep the acquisition axis in order while omitting explicitly censored scans. */
  case Excluding(values: Vector[TimepointIndex])

  def resolve(size: Int): Either[DatasetError, Vector[TimepointIndex]] =
    this match
      case TimepointSelection.All =>
        resolveAllTimepoints(size)
      case TimepointSelection.Indices(values) =>
        validateTimepoints(values, size)
      case TimepointSelection.Window(start, length) =>
        resolveWindow(start, length, size)
      case TimepointSelection.Excluding(values) =>
        resolveExcluding(values, size)

object TimepointSelection:
  def fromInts(values: Int*): Either[DatasetError, TimepointSelection] =
    buildTypedSelection(DatasetAxis.Timepoint, values.toVector, TimepointIndex.make)
      .map(TimepointSelection.Indices.apply)

  def indices(values: Int*): TimepointSelection =
    fromInts(values*).fold(error => throw new IllegalArgumentException(error.message), identity)

  def window(start: Int, length: Int): Either[DatasetError, TimepointSelection] =
    for
      startIndex <- TimepointIndex.make(start)
      _ <-
        if length > 0 then Right(())
        else Left(DatasetError.InvalidTimeAxis(
          s"timepoint window length must be positive; got $length"
        ))
    yield TimepointSelection.Window(startIndex, length)

  def unsafeWindow(start: Int, length: Int): TimepointSelection =
    window(start, length).fold(error => throw new IllegalArgumentException(error.message), identity)

  def fromExcludedInts(values: Int*): Either[DatasetError, TimepointSelection] =
    buildTypedSelection(DatasetAxis.Timepoint, values.toVector, TimepointIndex.make)
      .map(TimepointSelection.Excluding.apply)

  /** Select every scan except the declared zero-based acquisition indices. */
  def excluding(values: Int*): TimepointSelection =
    fromExcludedInts(values*)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

enum VoxelSelection:
  case All
  case AllSpatial
  case Indices(values: Vector[VoxelIndex])
  case Coords(values: Vector[VoxelCoord])

  def resolve(size: Int): Either[DatasetError, Vector[VoxelIndex]] =
    this match
      case VoxelSelection.All | VoxelSelection.AllSpatial =>
        resolveAllVoxels(size)
      case VoxelSelection.Indices(values) =>
        validateVoxels(values, size)
      case VoxelSelection.Coords(_) =>
        Left(DatasetError.ShapeMismatch(
          "voxel coordinates require dataset geometry for resolution"
        ))

object VoxelSelection:
  def fromInts(values: Int*): Either[DatasetError, VoxelSelection] =
    buildTypedSelection(DatasetAxis.Voxel, values.toVector, VoxelIndex.make)
      .map(VoxelSelection.Indices.apply)

  def indices(values: Int*): VoxelSelection =
    fromInts(values*).fold(error => throw new IllegalArgumentException(error.message), identity)

  def coords(values: VoxelCoord*): VoxelSelection =
    VoxelSelection.Coords(values.toVector)

  def fromImage[F <: Frame[D3], S, T](
      domain: GridDomain[F, D3, S],
      selection: Selection[T],
      shape: DatasetShape
  ): Either[DatasetError, VoxelSelection] =
    val ownerCheck =
      if domain.space.sameRuntimeOwnerAs(selection.space) then Right(())
      else Left(SpaceMismatch.between(domain.space, selection.space))
    ownerCheck
      .left
      .map(error => DatasetError.ShapeMismatch(error.message))
      .flatMap: _ =>
        GridCompatibility
          .volume(shape.volumeSpace, domain.volumeSpace)
          .left
          .map(error => DatasetError.ShapeMismatch(error.message))
      .flatMap: _ =>
        fromInts(selection.ordinals.toVector*)

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

  def resolve(
      selection: VoxelSelection,
      space: SomeSampleSpace
  ): Either[DatasetError, Vector[VoxelIndex]] =
    selection match
      case VoxelSelection.All =>
        Right(readableVoxelValues)
      case VoxelSelection.AllSpatial =>
        resolveAllVoxels(spatialSize)
      case VoxelSelection.Indices(values) =>
        validateVoxels(values, spatialSize).flatMap(validateReadable)
      case VoxelSelection.Coords(values) =>
        resolveCoordinates(values, space).flatMap(validateReadable)

  def resolve(selection: VoxelSelection): Either[DatasetError, Vector[VoxelIndex]] =
    selection match
      case VoxelSelection.Coords(_) =>
        Left(DatasetError.ShapeMismatch(
          "voxel coordinates require dataset geometry for resolution"
        ))
      case other =>
        resolveWithoutCoordinates(other)

  private def resolveWithoutCoordinates(
      selection: VoxelSelection
  ): Either[DatasetError, Vector[VoxelIndex]] =
    selection match
      case VoxelSelection.All =>
        Right(readableVoxelValues)
      case VoxelSelection.AllSpatial =>
        resolveAllVoxels(spatialSize)
      case VoxelSelection.Indices(values) =>
        validateVoxels(values, spatialSize).flatMap(validateReadable)
      case VoxelSelection.Coords(_) =>
        Left(DatasetError.ShapeMismatch(
          "voxel coordinates require dataset geometry for resolution"
        ))

  private def resolveCoordinates(
      values: Vector[VoxelCoord],
      space: SomeSampleSpace
  ): Either[DatasetError, Vector[VoxelIndex]] =
    if values.isEmpty then Left(DatasetError.EmptySelection(DatasetAxis.Voxel))
    else
      val resolved = Vector.newBuilder[VoxelIndex]
      resolved.sizeHint(values.length)
      var index = 0
      var failure = Option.empty[DatasetError]
      while index < values.length && failure.isEmpty do
        val coordinate = values(index)
        Indexing.gridToIndexChecked(space.spatialShape, coordinate) match
          case Left(error) =>
            failure = Some(DatasetError.InvalidVoxelCoordinate(coordinate, error.message))
          case Right(linear) =>
            VoxelIndex.make(linear) match
              case Left(error) => failure = Some(error)
              case Right(voxel) => resolved += voxel
        index += 1
      failure match
        case Some(error) => Left(error)
        case None => validateVoxels(resolved.result(), spatialSize)

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
    GridCompatibility.spatial(shape.space, mask.space)
      .left
      .map(error => DatasetError.ShapeMismatch(error.message))
      .flatMap(_ => fromAlignedMask(mask, shape))

  def fromMask(
      mask: Mask.MaskVol,
      shape: DatasetShape,
      congruence: scalafim.image.CertifiedGridCongruence
  ): Either[DatasetError, VoxelDomain] =
    GridCompatibility
      .acceptCertifiedSpatial(congruence, shape.space, mask.space)
      .left
      .map(error => DatasetError.ShapeMismatch(error.message))
      .flatMap(_ => fromAlignedMask(mask, shape))

  private def fromAlignedMask(
      mask: Mask.MaskVol,
      shape: DatasetShape
  ): Either[DatasetError, VoxelDomain] =
    val maskIndices = Mask.indices(mask)
    val voxels = Vector.newBuilder[VoxelIndex]
    voxels.sizeHint(maskIndices.size)
    var i = 0
    var failure = Option.empty[DatasetError]
    while i < maskIndices.size && failure.isEmpty do
      VoxelIndex.make(maskIndices(i)) match
        case Left(error) => failure = Some(error)
        case Right(voxel) => voxels += voxel
      i += 1
    failure match
      case Some(error) => Left(error)
      case None => fromVoxels(VoxelDomainKind.ActiveMask, shape.spatialSize, voxels.result())

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
    DatasetAcquisitionDomain
      .structuralCompatibility(shape, voxelDomain)
      .flatMap(resolveEither)

  def resolveEither(
      domain: DatasetAcquisitionDomain
  ): Either[DatasetError, ResolvedDataSelection] =
    domain.resolve(time, voxels)

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
      case untyped: IndexSelection =>
        untyped match
          case IndexSelection.All => TimepointSelection.All
          case IndexSelection.Indices(values) =>
            TimepointSelection.fromInts(values*).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def toVoxelSelection(selection: VoxelInput): VoxelSelection =
    selection match
      case typed: VoxelSelection => typed
      case untyped: IndexSelection =>
        untyped match
          case IndexSelection.All => VoxelSelection.All
          case IndexSelection.Indices(values) =>
            VoxelSelection.fromInts(values*).fold(error => throw new IllegalArgumentException(error.message), identity)

final class ResolvedDataSelection private (
    val timepointIndices: Vector[TimepointIndex],
    val voxelIndexValues: Vector[VoxelIndex],
    val locus: ResolvedLocusSelection
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
    else
      val timeSize = timepoints.map(TimepointIndex.raw).max + 1
      val voxelSize = voxels.map(VoxelIndex.raw).max + 1
      for
        validTimepoints <- validateTimepoints(timepoints, timeSize)
        validVoxels <- validateVoxels(voxels, voxelSize)
        shape <- DatasetShape.make(
          SampleSpaces(Vector(voxelSize, 1, 1)),
          timeSize
        )
        voxelDomain <- VoxelDomain.full(shape)
        domain <- DatasetAcquisitionDomain.structuralCompatibility(shape, voxelDomain)
      yield
          val times =
            LocusSelection
              .fromOrdinals(
                domain.timeSpace,
                validTimepoints.map(TimepointIndex.raw)
              )
              .toOption
              .get
          val selectedVoxels =
            LocusSelection
              .fromOrdinals(
                domain.fullVoxelSpace,
                validVoxels.map(VoxelIndex.raw)
              )
              .toOption
              .get
          fromLocus(domain, times, selectedVoxels)

  private[dataset] def fromLocus(
      domain: DatasetAcquisitionDomain,
      timepoints: LocusSelection[domain.T],
      voxels: LocusSelection[domain.X]
  ): ResolvedDataSelection =
    val locusSelection =
      new ResolvedLocusSelection:
        type T = domain.T
        type X = domain.X
        val timepoints: LocusSelection[T] = timepoints
        val voxels: LocusSelection[X] = voxels
    new ResolvedDataSelection(
      timepoints.indices.map(index => TimepointIndex.unsafe(index.value)).toVector,
      voxels.indices.map(index => VoxelIndex.unsafe(index.value)).toVector,
      locusSelection
    )

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

private def resolveWindow(
    start: TimepointIndex,
    length: Int,
    size: Int
): Either[DatasetError, Vector[TimepointIndex]] =
  val startValue = TimepointIndex.raw(start)
  val endExclusive = startValue.toLong + length.toLong
  if size <= 0 then Left(DatasetError.NonPositiveAxisSize(DatasetAxis.Timepoint, size))
  else if length <= 0 then
    Left(DatasetError.InvalidTimeAxis(
      s"timepoint window length must be positive; got $length"
    ))
  else if endExclusive > size.toLong then
    Left(DatasetError.InvalidTimeAxis(
      s"timepoint window [$startValue, $endExclusive) exceeds size $size"
    ))
  else
    Right(Vector.tabulate(length)(offset => TimepointIndex.unsafe(startValue + offset)))

private def resolveExcluding(
    values: Vector[TimepointIndex],
    size: Int
): Either[DatasetError, Vector[TimepointIndex]] =
  if values.isEmpty then Left(DatasetError.EmptySelection(DatasetAxis.Timepoint))
  else
    validateTimepoints(values, size).flatMap { excluded =>
      val excludedValues = excluded.iterator.map(TimepointIndex.raw).toSet
      val retained =
        Vector.tabulate(size)(identity).iterator
          .filterNot(excludedValues)
          .map(TimepointIndex.unsafe)
          .toVector
      if retained.nonEmpty then Right(retained)
      else Left(DatasetError.EmptySelection(DatasetAxis.Timepoint))
    }

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
