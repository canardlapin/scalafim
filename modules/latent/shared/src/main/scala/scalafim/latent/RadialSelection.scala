package scalafim.latent

import gale.linalg.DVec
import scalafim.locus.{
  FiniteSpace,
  Injection,
  Point,
  Selection,
  SpaceKey,
  TotalMap
}

sealed trait RadialActivePoint
sealed trait RadialFullGridPoint

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

  lazy val activeSpace: FiniteSpace[RadialActivePoint] =
    FiniteSpace
      .make[RadialActivePoint](
        SpaceKey.unsafe(
          s"scalafim:latent:radial-active:${activeToFullGrid.mkString(",")}"
        ),
        activeCount
      )
      .toOption
      .get

  lazy val maskOrderSelection: Selection[RadialActivePoint] =
    Selection
      .fromOrdinals(activeSpace, activeRowsInMaskOrder)
      .toOption
      .get

  def locus(
      maskSize: Int
  ): Either[RadialBasisError, RadialLocusOrder] =
    validateMaskSize(maskSize).map: _ =>
      RadialLocusOrder.make(this, maskSize)

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
    locus(maskSize).map { domain =>
      val values = Array.fill(maskSize)(false)
      val points = domain.activeSelection.points
      while points.hasNext do
        values(points.next().ordinal) = true
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

  def vectorInActiveOrderFromMaskOrder(values: DVec): Either[LatentError, DVec] =
    if values.length != activeCount then Left(LatentError.DimensionMismatch("radial basis vector length", activeCount, values.length))
    else
      val out = new Array[Double](activeCount)
      var maskRow = 0
      val rows = maskOrderSelection.ordinals
      while maskRow < rows.length do
        out(rows(maskRow)) = values(maskRow)
        maskRow += 1
      Right(LatentNumerics.vectorFromArray(out))

final class RadialLocusOrder private (
    val order: RadialMaskOrder,
    val fullGridSpace: FiniteSpace[RadialFullGridPoint],
    val activeToFull: Injection[RadialActivePoint, RadialFullGridPoint],
    val activeSelection: Selection[RadialFullGridPoint],
    private val fullToActive: Array[Int]
):
  def activePointFor(
      full: Point[RadialFullGridPoint]
  ): Either[LatentError, Point[RadialActivePoint]] =
    val ordinal = fullToActive(full.ordinal)
    if ordinal < 0 then
      Left(
        LatentError.MissingComponent(
          s"full-grid voxel ${full.ordinal} is not active in radial basis"
        )
      )
    else
      Right(order.activeSpace.point(ordinal).get)

  def fullPointFor(
      active: Point[RadialActivePoint]
  ): Point[RadialFullGridPoint] =
    activeToFull.mapping(active)

object RadialLocusOrder:
  private[latent] def make(
      order: RadialMaskOrder,
      maskSize: Int
  ): RadialLocusOrder =
    val full =
      FiniteSpace
        .make[RadialFullGridPoint](
          SpaceKey.unsafe(s"scalafim:latent:radial-full:$maskSize"),
          maskSize
        )
        .toOption
        .get
    val mapping =
      TotalMap
        .fromTargetOrdinals(
          order.activeSpace,
          full,
          order.activeToFullGrid.toArray
        )
        .toOption
        .get
    val injection = Injection.validate(mapping).toOption.get
    val selection =
      Selection
        .fromOrdinals(full, order.activeToFullGrid)
        .toOption
        .get
    val reverse = Array.fill(maskSize)(-1)
    var active = 0
    while active < order.activeToFullGrid.length do
      reverse(order.activeToFullGrid(active)) = active
      active += 1
    new RadialLocusOrder(order, full, injection, selection, reverse)

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
