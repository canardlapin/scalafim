package scalafim.image

import image4s.Axis as ImageAxis
import image4s.AxisKind
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.SomeSampleSpace
import image4s.geometry.Affine as GeometryAffine
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.CoordinateConvention
import image4s.geometry.Frame
import image4s.geometry.FrameId
import image4s.geometry.FrameMetadata
import image4s.geometry.Grid

enum NeuroSpaceError:
  case EmptyDimensions
  case NonPositiveDimension(index: Int, value: Int)
  case SpatialVectorLengthMismatch(label: String, expected: Int, actual: Int)
  case NonFiniteSpatialValue(label: String, axis: SpatialAxis, value: Double)
  case NonPositiveSpacing(axis: SpatialAxis, value: Double)
  case InvalidTransformShape(rows: Int, cols: Int)
  case NonFiniteTransformValue(index: Int)
  case InvalidTransformBottomRow(actual: Vector[Double])
  case SingularTransform(reason: String)
  case AxisCountMismatch(expected: Int, actual: Int)
  case ExpectedDimensionality(label: String, expected: Int, actual: Int)
  case CanonicalGeometry(reason: String)

  def message: String =
    this match
      case EmptyDimensions =>
        "'dims' must contain at least one dimension"
      case NonPositiveDimension(index, value) =>
        s"dimension $index must be positive; got $value"
      case SpatialVectorLengthMismatch(label, expected, actual) =>
        s"'$label' must contain $expected spatial values; got $actual"
      case NonFiniteSpatialValue(label, axis, value) =>
        s"'$label' ${axis.label} value must be finite; got $value"
      case NonPositiveSpacing(axis, value) =>
        s"'spacing' ${axis.label} value must be positive; got $value"
      case InvalidTransformShape(rows, cols) =>
        s"spatial transform must be 4x4; got ${rows}x${cols}"
      case NonFiniteTransformValue(index) =>
        s"spatial transform value at linear index $index is not finite"
      case InvalidTransformBottomRow(actual) =>
        s"spatial transform bottom row must be [0, 0, 0, 1]; got $actual"
      case SingularTransform(reason) =>
        s"transformation matrix not invertible: $reason"
      case AxisCountMismatch(expected, actual) =>
        s"axis count must match dimensionality: expected $expected, got $actual"
      case ExpectedDimensionality(label, expected, actual) =>
        s"$label requires exactly $expected dimensions; got $actual"
      case CanonicalGeometry(reason) =>
        s"canonical sampling geometry is invalid: $reason"

/** Zero-wrapper compatibility name for image4s' canonical sampling geometry. */
opaque type NeuroSpace = SomeSampleSpace

object NeuroSpace:
  private val rasD2FrameId =
    FrameId
      .parse("scalafim-ras-d2")
      .fold(error => throw new IllegalStateException(error.message), identity)

  private val rasD3FrameId =
    FrameId
      .parse("scalafim-ras-d3")
      .fold(error => throw new IllegalStateException(error.message), identity)

  private[image] def fromCanonical(space: SomeSampleSpace): NeuroSpace =
    space

  private[image] def canonical(space: NeuroSpace): SomeSampleSpace =
    space

  private[image] def logicalDims(space: NeuroSpace): Vector[Int] =
    space.logicalShape

  private[image] def spatialShapeOf(
      space: NeuroSpace
  ): SpatialDims =
    SpatialDims.unsafeFromVector(space.grid.shape)

  private[image] def affineOf(space: NeuroSpace): Affine3D =
    val transform = canonicalTransform(space)
    Affine3D.unsafe(
      transform,
      DMat
        .invert(transform)
        .fold(reason => throw new IllegalStateException(reason), identity)
    )

  private[image] def spatialPart(space: NeuroSpace): NeuroSpace =
    fromCanonical(space.typed.spatialOnly)

  private[image] def withTime(
      space: NeuroSpace,
      extent: Int
  ): NeuroSpace =
    space.addDim(extent, Some(Axis.Time))

  extension (space: NeuroSpace)
    def dims: Vector[Int] =
      space.logicalShape

    def ndim: Int =
      dims.length

    def spatialDims: Vector[Int] =
      space.grid.shape

    def spatialShape: SpatialDims =
      SpatialDims.unsafeFromVector(spatialDims)

    def spacing: Vector[Double] =
      Affine.voxelSizes(trans).take(space.spatialRank)

    def origin: Vector[Double] =
      Vector.tabulate(space.spatialRank)(axis => trans(axis, 3))

    def axes: AxisSet =
      val spatial = defaultAxes(space.spatialRank, trans).spatialAxes
      val additional =
        space.nonSpatialAxes.values.map: axis =>
          legacyAxis(axis)
      AxisSet((spatial ++ additional)*)

    def trans: DMat =
      canonicalTransform(space)

    def inverse: DMat =
      DMat
        .invert(trans)
        .fold(reason => throw new IllegalStateException(reason), identity)

    def affine3D: Affine3D =
      Affine3D.unsafe(trans, inverse)

    def asVolumeSpace: Either[NeuroSpaceError, VolumeSpace] =
      VolumeSpace.make(space)

    def asSeriesSpace: Either[NeuroSpaceError, SeriesSpace] =
      SeriesSpace.make(space)

    def gridToIndex(coords: Vector[Int]): Int =
      Indexing.gridToIndex(dims, coords)

    def indexToGrid(idx: Int): Vector[Int] =
      Indexing.indexToGrid(dims, idx)

    def gridToIndex3D(x: Int, y: Int, z: Int): Int =
      gridToIndex3D(VoxelCoord(x, y, z))

    def gridToIndex3D(coord: VoxelCoord): Int =
      Indexing.gridToIndex3D(spatialShape, coord)

    def indexToGrid3D(idx: Int): Vector[Int] =
      indexToVoxel3D(idx).toVector

    def indexToVoxel3D(idx: Int): VoxelCoord =
      Indexing.indexToGrid3D(spatialShape, idx)

    def spatialSpace: NeuroSpace =
      fromCanonical(space.typed.spatialOnly)

    def addDim(n: Int, axis: Option[Axis] = None): NeuroSpace =
      require(n > 0, "added dimension must be positive")
      val newDims = dims :+ n
      val newAxis =
        axis.getOrElse {
          if newDims.length == 4 then Axis.Time else Axis.NoneAxis
        }
      if space.spatialRank < 3 &&
          space.nonSpatialAxes.size == 0 &&
          axis != Some(Axis.Time)
      then
        NeuroSpace(
          dims = newDims,
          spacing = Some(spacing :+ 1.0),
          origin = Some(origin :+ 0.0),
          axes = Some(AxisSet((axes.axes :+ newAxis)*)),
          trans = None
        )
      else
        val axisIndex = space.spatialRank + space.nonSpatialAxes.size
        val appended =
          for
            canonical <- imageAxis(newAxis, n, axisIndex)
            refined <- space.typed
              .appendNonSpatial(canonical)
              .left
              .map(error =>
                NeuroSpaceError.CanonicalGeometry(error.message)
              )
          yield fromCanonical(refined)
        appended.fold(
          error => throw new IllegalArgumentException(error.message),
          identity
        )

    def dropDim(dimnum: Int = space.ndim - 1): NeuroSpace =
      require(ndim >= 2, "cannot drop from <2D space")
      require(dimnum >= 0 && dimnum < ndim, "dimnum out of range")
      if dimnum >= space.spatialRank then
        space.typed
          .removeNonSpatial(dimnum - space.spatialRank)
          .map(fromCanonical)
          .fold(
            error => throw new IllegalArgumentException(error.message),
            identity
          )
      else
        val keepIdx = dims.indices.filter(_ != dimnum).toVector
        val newDims = keepIdx.map(dims)
        val newAxes = AxisSet(keepIdx.map(axes.axes(_))*)
        val newSpacing = spacing.patch(dimnum, Nil, 1)
        val newOrigin = origin.patch(dimnum, Nil, 1)
        NeuroSpace(
          dims = newDims,
          spacing = Some(newSpacing),
          origin = Some(newOrigin),
          axes = Some(newAxes),
          trans = None
        )

    def indexToCoord(index: Vector[Double]): Vector[Double] =
      transformCoordinates(trans, index)

    def indexToPoint(index: SpatialPoint): SpatialPoint =
      SpatialPoint.unsafeFromVector(
        indexToCoord(index.toVector),
        "world coordinate"
      )

    def voxelToWorld(voxel: VoxelPoint): WorldPoint =
      affine3D.voxelToWorld(voxel)

    def coordToIndex(coord: Vector[Double]): Vector[Double] =
      transformCoordinates(inverse, coord)

    def coordToIndexPoint(coord: SpatialPoint): SpatialPoint =
      SpatialPoint.unsafeFromVector(
        coordToIndex(coord.toVector),
        "voxel coordinate"
      )

    def worldToVoxel(world: WorldPoint): VoxelPoint =
      affine3D.worldToVoxel(world)

  def fromSpatialDims(
      dims: SpatialDims,
      spacing: Option[Vector[Double]] = None,
      origin: Option[Vector[Double]] = None,
      axes: Option[AxisSet] = None,
      trans: Option[DMat] = None
  ): NeuroSpace =
    NeuroSpace(dims.toVector, spacing, origin, axes, trans)

  def make(
      dims: Vector[Int],
      spacing: Option[Vector[Double]] = None,
      origin: Option[Vector[Double]] = None,
      axes: Option[AxisSet] = None,
      trans: Option[DMat] = None
  ): Either[NeuroSpaceError, NeuroSpace] =
    validateDims(dims).flatMap: checkedDims =>
      val spatialDimCount = math.min(checkedDims.length, 3)
      val defaultSpacing = Vector.fill(spatialDimCount)(1.0)
      val defaultOrigin = Vector.fill(spatialDimCount)(0.0)
      for
        _ <-
          if spatialDimCount >= 2 then Right(())
          else
            Left(
              NeuroSpaceError.ExpectedDimensionality(
                "NeuroSpace spatial part",
                2,
                spatialDimCount
              )
            )
        sp <- spatialVector(
          "spacing",
          spacing,
          defaultSpacing,
          requirePositive = true
        )
        org <- spatialVector(
          "origin",
          origin,
          defaultOrigin,
          requirePositive = false
        )
        matrix <- validateTransform(
          trans.getOrElse(defaultTransform(sp, org, spatialDimCount))
        )
        _ <- DMat
          .invert(matrix)
          .left
          .map(NeuroSpaceError.SingularTransform.apply)
        checkedAxes <- axes match
          case Some(value) =>
            validateAxes(checkedDims.length, value).map(_ => value)
          case None =>
            Right(defaultAxes(checkedDims.length, matrix))
        canonical <-
          canonicalSpace(checkedDims, matrix, checkedAxes)
      yield canonical

  def apply(
      dims: Vector[Int],
      spacing: Option[Vector[Double]] = None,
      origin: Option[Vector[Double]] = None,
      axes: Option[AxisSet] = None,
      trans: Option[DMat] = None
  ): NeuroSpace =
    make(dims, spacing, origin, axes, trans)
      .fold(err => throw new IllegalArgumentException(err.message), identity)

  private def canonicalSpace(
      dims: Vector[Int],
      transform: DMat,
      axes: AxisSet
  ): Either[NeuroSpaceError, NeuroSpace] =
    if dims.length == 2 then
      for
        metadata <- FrameMetadata
          .create("scalafim-space")
          .left
          .map(error => NeuroSpaceError.CanonicalGeometry(error.message))
        frame = Frame.createPersistent[D2](
          rasD2FrameId,
          metadata,
          convention = CoordinateConvention.RAS
        )
        affine <- GeometryAffine
          .fromRowMajor[D2](
            Vector(
              transform(0, 0),
              transform(0, 1),
              transform(0, 3),
              transform(1, 0),
              transform(1, 1),
              transform(1, 3),
              0.0,
              0.0,
              1.0
            )
          )
          .left
          .map(error => NeuroSpaceError.CanonicalGeometry(error.message))
        grid <- Grid
          .in(frame)(dims, affine)
          .left
          .map(error => NeuroSpaceError.CanonicalGeometry(error.message))
      yield fromCanonical(
        SampleSpace.create(grid, NonSpatialAxes.empty)
      )
    else
      for
        metadata <- FrameMetadata
          .create("scalafim-space")
          .left
          .map(error => NeuroSpaceError.CanonicalGeometry(error.message))
        frame = Frame.createPersistent[D3](
          rasD3FrameId,
          metadata,
          convention = CoordinateConvention.RAS
        )
        affine <- GeometryAffine
          .fromRowMajor[D3](
            Vector.tabulate(16)(transform.data.apply)
          )
          .left
          .map(error => NeuroSpaceError.CanonicalGeometry(error.message))
        grid <- Grid
          .in(frame)(dims.take(3), affine)
          .left
          .map(error => NeuroSpaceError.CanonicalGeometry(error.message))
        nonSpatial <- canonicalAxes(dims.drop(3), axes.axes.drop(3))
      yield fromCanonical(SampleSpace.create(grid, nonSpatial))

  private def canonicalAxes(
      extents: Vector[Int],
      axes: Vector[Axis]
  ): Either[NeuroSpaceError, NonSpatialAxes] =
    val built =
      extents.zipWithIndex.foldLeft[
        Either[NeuroSpaceError, Vector[ImageAxis]]
      ](Right(Vector.empty)):
        case (acc, (extent, index)) =>
          val legacy = axes.lift(index).getOrElse(Axis.NoneAxis)
          val kind =
            if legacy == Axis.Time then AxisKind.Time
            else AxisKind.Other
          val name =
            if kind == AxisKind.Time then "time"
            else s"axis-${index + 3}"
          for
            values <- acc
            axis <- ImageAxis
              .create(name, extent, kind)
              .left
              .map(error =>
                NeuroSpaceError.CanonicalGeometry(error.message)
              )
          yield values :+ axis
    built.flatMap: values =>
      NonSpatialAxes
        .from(values)
        .left
        .map(error => NeuroSpaceError.CanonicalGeometry(error.message))

  private def legacyAxis(axis: ImageAxis): Axis =
    axis.kind match
      case AxisKind.Time => Axis.Time
      case _             => Axis(axis.name.value)

  private def imageAxis(
      axis: Axis,
      extent: Int,
      index: Int
  ): Either[NeuroSpaceError, ImageAxis] =
    val kind =
      if axis == Axis.Time then AxisKind.Time
      else
        axis.label.toLowerCase match
          case "channel"   => AxisKind.Channel
          case "echo"      => AxisKind.Echo
          case "coil"      => AxisKind.Coil
          case "direction" => AxisKind.Direction
          case "batch"     => AxisKind.Batch
          case _           => AxisKind.Other
    val name =
      if axis == Axis.NoneAxis then s"axis-$index"
      else axis.label.toLowerCase
    ImageAxis
      .create(name, extent, kind)
      .left
      .map(error => NeuroSpaceError.CanonicalGeometry(error.message))

  private def canonicalTransform(space: SomeSampleSpace): DMat =
    val matrix = space.grid.indexToFrame.matrix
    if space.spatialRank == 3 then
      DMat.fromRows(
        Vector.tabulate(4)(row =>
          Vector.tabulate(4)(column => matrix(row, column))
        )
      )
    else
      DMat.fromRows(
        Vector(
          Vector(matrix(0, 0), matrix(0, 1), 0.0, matrix(0, 2)),
          Vector(matrix(1, 0), matrix(1, 1), 0.0, matrix(1, 2)),
          Vector(0.0, 0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )

  private def transformCoordinates(
      matrix: DMat,
      coordinates: Vector[Double]
  ): Vector[Double] =
    val count = math.min(coordinates.length, 3)
    val hom = Array.ofDim[Double](4)
    var axis = 0
    while axis < count do
      hom(axis) = coordinates(axis)
      axis += 1
    hom(3) = 1.0
    Vector.tabulate(count): row =>
      var column = 0
      var sum = 0.0
      while column < 4 do
        sum += matrix(row, column) * hom(column)
        column += 1
      sum

  private def validateDims(
      dims: Vector[Int]
  ): Either[NeuroSpaceError, Vector[Int]] =
    if dims.isEmpty then Left(NeuroSpaceError.EmptyDimensions)
    else
      dims.zipWithIndex.collectFirst {
        case (value, index) if value <= 0 =>
          NeuroSpaceError.NonPositiveDimension(index, value)
      } match
        case Some(error) => Left(error)
        case None => Right(dims)

  private def spatialVector(
      label: String,
      candidate: Option[Vector[Double]],
      default: Vector[Double],
      requirePositive: Boolean
  ): Either[NeuroSpaceError, Vector[Double]] =
    val values = candidate.getOrElse(default)
    if values.length != default.length then
      Left(
        NeuroSpaceError.SpatialVectorLengthMismatch(
          label,
          default.length,
          values.length
        )
      )
    else
      values.zipWithIndex.collectFirst {
        case (value, index) if !value.isFinite =>
          NeuroSpaceError.NonFiniteSpatialValue(
            label,
            SpatialAxis.all(index),
            value
          )
        case (value, index) if requirePositive && value <= 0.0 =>
          NeuroSpaceError.NonPositiveSpacing(
            SpatialAxis.all(index),
            value
          )
      } match
        case Some(error) => Left(error)
        case None => Right(values)

  private def defaultTransform(
      spacing: Vector[Double],
      origin: Vector[Double],
      spatialDimCount: Int
  ): DMat =
    DMat.fromRows(
      Vector.tabulate(4)(row =>
        Vector.tabulate(4)(column =>
          if row == column && row < spatialDimCount then spacing(row)
          else if column == 3 && row < spatialDimCount then origin(row)
          else if row == column then 1.0
          else 0.0
        )
      )
    )

  private def validateTransform(
      transform: DMat
  ): Either[NeuroSpaceError, DMat] =
    if transform.rows != 4 || transform.cols != 4 then
      Left(
        NeuroSpaceError.InvalidTransformShape(
          transform.rows,
          transform.cols
        )
      )
    else
      val nonFinite =
        Vector
          .tabulate(transform.data.length)(identity)
          .find(index => !transform.data(index).isFinite)
      nonFinite match
        case Some(index) =>
          Left(NeuroSpaceError.NonFiniteTransformValue(index))
        case None =>
          val bottom =
            Vector(
              transform(3, 0),
              transform(3, 1),
              transform(3, 2),
              transform(3, 3)
            )
          if bottom == Vector(0.0, 0.0, 0.0, 1.0) then
            Right(transform)
          else
            Left(NeuroSpaceError.InvalidTransformBottomRow(bottom))

  private def validateAxes(
      expected: Int,
      axes: AxisSet
  ): Either[NeuroSpaceError, Unit] =
    if axes.ndim == expected then Right(())
    else
      Left(NeuroSpaceError.AxisCountMismatch(expected, axes.ndim))

  private def defaultAxes(
      dimsLength: Int,
      transform: DMat
  ): AxisSet =
    val base = AxisSet.standard(dimsLength)
    if dimsLength >= 3 then
      val inferred = Orientation.findAnatomy(transform)
      AxisSet((inferred.axes ++ base.additionalAxes)*)
    else base

opaque type VolumeSpace = NeuroSpace

object VolumeSpace:
  extension (space: VolumeSpace)
    def shape: SpatialDims =
      NeuroSpace.spatialShapeOf(space)

    def dims: Vector[Int] =
      NeuroSpace.logicalDims(space)

    def affine: Affine3D =
      NeuroSpace.affineOf(space)

    def nVoxels: Int =
      shape.product

    def toNeuroSpace: NeuroSpace =
      space

    def voxelToWorld(voxel: VoxelPoint): WorldPoint =
      affine.voxelToWorld(voxel)

    def worldToVoxel(world: WorldPoint): VoxelPoint =
      affine.worldToVoxel(world)

    def addTime(n: Int): SeriesSpace =
      SeriesSpace.unsafe(NeuroSpace.withTime(space, n))

  def make(
      space: NeuroSpace
  ): Either[NeuroSpaceError, VolumeSpace] =
    if NeuroSpace.canonical(space).spatialRank == 3 &&
        NeuroSpace.canonical(space).nonSpatialAxes.size == 0
    then Right(space)
    else
      Left(
        NeuroSpaceError.ExpectedDimensionality(
          "VolumeSpace",
          3,
          NeuroSpace.logicalDims(space).length
        )
      )

  def fromSpatialPart(
      space: NeuroSpace
  ): Either[NeuroSpaceError, VolumeSpace] =
    if NeuroSpace.canonical(space).spatialRank == 3 then
      Right(NeuroSpace.spatialPart(space))
    else
      Left(
        NeuroSpaceError.ExpectedDimensionality(
          "VolumeSpace spatial part",
          3,
          NeuroSpace.logicalDims(space).length
        )
      )

  def apply(space: NeuroSpace): VolumeSpace =
    make(space)
      .fold(err => throw new IllegalArgumentException(err.message), identity)

  def unsafe(space: NeuroSpace): VolumeSpace =
    space

opaque type SeriesSpace = NeuroSpace

object SeriesSpace:
  import VolumeSpace.*

  extension (space: SeriesSpace)
    def volumeSpace: VolumeSpace =
      VolumeSpace.unsafe(NeuroSpace.spatialPart(space))

    def spatialShape: SpatialDims =
      volumeSpace.shape

    def dims: Vector[Int] =
      NeuroSpace.logicalDims(space).take(4)

    def nVolumes: Int =
      space.dims(3)

    def affine: Affine3D =
      NeuroSpace.affineOf(space)

    def toNeuroSpace: NeuroSpace =
      space

  def make(
      space: NeuroSpace
  ): Either[NeuroSpaceError, SeriesSpace] =
    if NeuroSpace.canonical(space).spatialRank == 3 &&
        NeuroSpace.canonical(space).nonSpatialAxes.size == 1 &&
        NeuroSpace
          .canonical(space)
          .nonSpatialAxes(0)
          .exists(_.kind == AxisKind.Time)
    then Right(space)
    else
      Left(
        NeuroSpaceError.ExpectedDimensionality(
          "SeriesSpace",
          4,
          NeuroSpace.logicalDims(space).length
        )
      )

  def apply(space: NeuroSpace): SeriesSpace =
    make(space)
      .fold(err => throw new IllegalArgumentException(err.message), identity)

  def unsafe(space: NeuroSpace): SeriesSpace =
    space
