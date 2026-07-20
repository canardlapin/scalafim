package scalafim.latent

import scalafim.linalg.DoubleVector

opaque type RadialActiveVoxelIndex = Int

object RadialActiveVoxelIndex:
  def apply(value: Int): Either[LatentError, RadialActiveVoxelIndex] =
    if value < 0 then Left(LatentError.NegativeIndex("active voxel", value))
    else Right(value)

  def unsafe(value: Int): RadialActiveVoxelIndex =
    apply(value).fold(error => throw IllegalArgumentException(error.message), identity)

  extension (index: RadialActiveVoxelIndex)
    inline def value: Int = index

opaque type RadialFullGridVoxelIndex = Int

object RadialFullGridVoxelIndex:
  def apply(value: Int): Either[LatentError, RadialFullGridVoxelIndex] =
    if value < 0 then Left(LatentError.NegativeIndex("full-grid voxel", value))
    else Right(value)

  def unsafe(value: Int): RadialFullGridVoxelIndex =
    apply(value).fold(error => throw IllegalArgumentException(error.message), identity)

  extension (index: RadialFullGridVoxelIndex)
    inline def value: Int = index

final case class RadialMaskOrder private (
    activeToFullGrid: Vector[Int],
    activeRowsInMaskOrder: Vector[Int]
):
  require(activeToFullGrid.nonEmpty, "radial mask order must be non-empty")
  require(activeRowsInMaskOrder.length == activeToFullGrid.length, "mask-order row count must match active rows")

  def activeCount: Int = activeToFullGrid.length

  def validateMaskSize(maskSize: Int): Either[RadialBasisError, Unit] =
    if maskSize <= 0 then Left(RadialBasisError.InvalidMaskDimensions("mask size must be positive"))
    else
      var i = 0
      var error = Option.empty[RadialBasisError]
      while i < activeToFullGrid.length && error.isEmpty do
        val index = activeToFullGrid(i)
        if index >= maskSize then error = Some(RadialBasisError.ActiveIndexOutOfBounds(index, maskSize))
        i += 1
      error.toLeft(())

  def maskValues(maskSize: Int): Either[RadialBasisError, Vector[Boolean]] =
    validateMaskSize(maskSize).map { _ =>
      val values = Array.fill(maskSize)(false)
      var i = 0
      while i < activeToFullGrid.length do
        values(activeToFullGrid(i)) = true
        i += 1
      values.toVector
    }

  def activeRowsForFullGrid(
      requested: IndexedSeq[Int]
  ): Either[LatentError, IndexedSeq[Int]] =
    if requested.isEmpty then Left(LatentError.EmptySelection("full-grid voxel"))
    else
      val fullToActive = activeToFullGrid.zipWithIndex.toMap
      val seen = scala.collection.mutable.HashSet.empty[Int]
      val out = Vector.newBuilder[Int]
      out.sizeHint(requested.length)
      var i = 0
      var error = Option.empty[LatentError]
      while i < requested.length && error.isEmpty do
        val index = requested(i)
        if index < 0 then error = Some(LatentError.NegativeIndex("full-grid voxel", index))
        else if seen.contains(index) then error = Some(LatentError.DuplicateSelection("full-grid voxel", index))
        else
          fullToActive.get(index) match
            case Some(activeRow) =>
              seen += index
              out += activeRow
            case None =>
              error = Some(LatentError.MissingComponent(s"full-grid voxel $index is not active in radial basis"))
        i += 1
      error match
        case Some(err) => Left(err)
        case None      => Right(out.result())

  def vectorInActiveOrderFromMaskOrder(values: DoubleVector): Either[LatentError, DoubleVector] =
    if values.length != activeCount then Left(LatentError.DimensionMismatch("radial basis vector length", activeCount, values.length))
    else
      val out = new Array[Double](activeCount)
      var maskRow = 0
      while maskRow < activeRowsInMaskOrder.length do
        out(activeRowsInMaskOrder(maskRow)) = values(maskRow)
        maskRow += 1
      Right(DoubleVector.unsafe(out))

object RadialMaskOrder:
  def fromActiveIndices(indices: Vector[Int]): Either[RadialBasisError, RadialMaskOrder] =
    if indices.isEmpty then Left(RadialBasisError.EmptyActiveCoordinates)
    else
      val seen = scala.collection.mutable.HashSet.empty[Int]
      var i = 0
      var error = Option.empty[RadialBasisError]
      while i < indices.length && error.isEmpty do
        val index = indices(i)
        if index < 0 then error = Some(RadialBasisError.InvalidActiveVoxelIndices(s"negative index $index"))
        else if seen.contains(index) then error = Some(RadialBasisError.DuplicateActiveIndex(index))
        else seen += index
        i += 1

      error match
        case Some(err) =>
          Left(err)
        case None =>
          Right(RadialMaskOrder(indices, indices.zipWithIndex.sortBy(_._1).map(_._2)))

  def checked(
      indices: Vector[Int],
      maskSize: Int
  ): Either[RadialBasisError, RadialMaskOrder] =
    for
      order <- fromActiveIndices(indices)
      _ <- order.validateMaskSize(maskSize)
    yield order

enum RadialVoxelSelection:
  case AllActive
  case Active(indices: IndexedSeq[RadialActiveVoxelIndex])
  case FullGrid(indices: IndexedSeq[RadialFullGridVoxelIndex])

object RadialVoxelSelection:
  val All: RadialVoxelSelection =
    AllActive

  def active(indices: IndexedSeq[Int]): Either[LatentError, RadialVoxelSelection] =
    checked(indices, RadialActiveVoxelIndex.apply).map(Active.apply)

  def fullGrid(indices: IndexedSeq[Int]): Either[LatentError, RadialVoxelSelection] =
    checked(indices, RadialFullGridVoxelIndex.apply).map(FullGrid.apply)

  private def checked[A](
      indices: IndexedSeq[Int],
      constructor: Int => Either[LatentError, A]
  ): Either[LatentError, IndexedSeq[A]] =
    val out = Vector.newBuilder[A]
    out.sizeHint(indices.length)
    var i = 0
    var error = Option.empty[LatentError]
    while i < indices.length && error.isEmpty do
      constructor(indices(i)) match
        case Right(value) => out += value
        case Left(err)    => error = Some(err)
      i += 1
    error match
      case Some(err) => Left(err)
      case None      => Right(out.result())

final case class RadialDecodeSelection(
    timepoints: Option[IndexedSeq[TimepointIndex]] = None,
    voxels: RadialVoxelSelection = RadialVoxelSelection.All
)

object RadialDecodeSelection:
  val All: RadialDecodeSelection =
    RadialDecodeSelection()

  def checked(
      timepoints: Option[IndexedSeq[Int]] = None,
      activeVoxels: Option[IndexedSeq[Int]] = None,
      fullGridVoxels: Option[IndexedSeq[Int]] = None
  ): Either[LatentError, RadialDecodeSelection] =
    if activeVoxels.nonEmpty && fullGridVoxels.nonEmpty then
      Left(LatentError.ProjectionFailed("choose active or full-grid radial voxels, not both"))
    else
      for
        checkedTime <- checkedTimepoints(timepoints)
        checkedVoxels <- checkedVoxelSelection(activeVoxels, fullGridVoxels)
      yield RadialDecodeSelection(checkedTime, checkedVoxels)

  private def checkedTimepoints(
      timepoints: Option[IndexedSeq[Int]]
  ): Either[LatentError, Option[IndexedSeq[TimepointIndex]]] =
    timepoints match
      case None =>
        Right(None)
      case Some(values) =>
        val out = Vector.newBuilder[TimepointIndex]
        out.sizeHint(values.length)
        var i = 0
        var error = Option.empty[LatentError]
        while i < values.length && error.isEmpty do
          TimepointIndex(values(i)) match
            case Right(value) => out += value
            case Left(err)    => error = Some(err)
          i += 1
        error match
          case Some(err) => Left(err)
          case None      => Right(Some(out.result()))

  private def checkedVoxelSelection(
      activeVoxels: Option[IndexedSeq[Int]],
      fullGridVoxels: Option[IndexedSeq[Int]]
  ): Either[LatentError, RadialVoxelSelection] =
    activeVoxels match
      case Some(values) =>
        RadialVoxelSelection.active(values)
      case None =>
        fullGridVoxels match
          case Some(values) => RadialVoxelSelection.fullGrid(values)
          case None         => Right(RadialVoxelSelection.All)
