package scalafim.image

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

final class NeuroSpace private (
    val dims: Vector[Int],
    val spacing: Vector[Double],
    val origin: Vector[Double],
    val axes: AxisSet,
    val trans: DMat,
    val inverse: DMat
):
  val affine3D: Affine3D =
    Affine3D.unsafe(trans, inverse)

  def ndim: Int = dims.length
  def spatialDims: Vector[Int] = dims.take(3)
  def spatialShape: SpatialDims =
    SpatialDims.unsafeFromVector(spatialDims)

  def asVolumeSpace: Either[NeuroSpaceError, VolumeSpace] =
    VolumeSpace.make(this)

  def asSeriesSpace: Either[NeuroSpaceError, SeriesSpace] =
    SeriesSpace.make(this)

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
    NeuroSpace(
      dims = spatialDims,
      spacing = Some(spacing),
      origin = Some(origin),
      axes = Some(AxisSet(axes.spatialAxes*)),
      trans = Some(trans)
    )

  def addDim(n: Int, axis: Option[Axis] = None): NeuroSpace =
    require(n > 0, "added dimension must be positive")
    val newDims = dims :+ n
    val newAxis =
      axis.getOrElse {
        if newDims.length == 4 then Axis.Time else Axis.NoneAxis
      }
    val newAxes = AxisSet((axes.axes :+ newAxis)*)
    NeuroSpace(
      dims = newDims,
      spacing = Some(spacing),
      origin = Some(origin),
      axes = Some(newAxes),
      trans = Some(trans)
    )

  def dropDim(dimnum: Int = ndim - 1): NeuroSpace =
    require(ndim >= 2, "cannot drop from <2D space")
    require(dimnum >= 0 && dimnum < ndim, "dimnum out of range")
    val keepIdx = dims.indices.filter(_ != dimnum).toVector
    val newDims = keepIdx.map(dims)
    val newAxes = AxisSet(keepIdx.map(axes.axes(_))*)

    val (newSpacing, newOrigin) =
      if dimnum < 3 then
        val sp = spacing.patch(dimnum, Nil, 1)
        val org = origin.patch(dimnum, Nil, 1)
        (sp, org)
      else (spacing, origin)

    NeuroSpace(
      dims = newDims,
      spacing = Some(newSpacing),
      origin = Some(newOrigin),
      axes = Some(newAxes),
      trans = None
    )

  def indexToCoord(index: Vector[Double]): Vector[Double] =
    val d = math.min(index.length, 3)
    val hom = Array.ofDim[Double](4)
    var i = 0
    while i < d do
      hom(i) = index(i)
      i += 1
    hom(3) = 1.0

    val out = Array.ofDim[Double](4)
    var r = 0
    while r < 4 do
      var c = 0
      var sum = 0.0
      while c < 4 do
        sum += trans(r, c) * hom(c)
        c += 1
      out(r) = sum
      r += 1
    out.take(d).toVector

  def indexToPoint(index: SpatialPoint): SpatialPoint =
    SpatialPoint.unsafeFromVector(indexToCoord(index.toVector), "world coordinate")

  def voxelToWorld(voxel: VoxelPoint): WorldPoint =
    affine3D.voxelToWorld(voxel)

  def coordToIndex(coord: Vector[Double]): Vector[Double] =
    val d = math.min(coord.length, 3)
    val hom = Array.ofDim[Double](4)
    var i = 0
    while i < d do
      hom(i) = coord(i)
      i += 1
    hom(3) = 1.0

    val out = Array.ofDim[Double](4)
    var r = 0
    while r < 4 do
      var c = 0
      var sum = 0.0
      while c < 4 do
        sum += inverse(r, c) * hom(c)
        c += 1
      out(r) = sum
      r += 1
    out.take(d).toVector

  def coordToIndexPoint(coord: SpatialPoint): SpatialPoint =
    SpatialPoint.unsafeFromVector(coordToIndex(coord.toVector), "voxel coordinate")

  def worldToVoxel(world: WorldPoint): VoxelPoint =
    affine3D.worldToVoxel(world)

  override def equals(other: Any): Boolean =
    other match
      case that: NeuroSpace =>
        dims == that.dims &&
          spacing == that.spacing &&
          origin == that.origin &&
          axes == that.axes &&
          trans == that.trans &&
          inverse == that.inverse
      case _ => false

  override def hashCode(): Int =
    var h = dims.hashCode()
    h = 31 * h + spacing.hashCode()
    h = 31 * h + origin.hashCode()
    h = 31 * h + axes.hashCode()
    h = 31 * h + trans.hashCode()
    h = 31 * h + inverse.hashCode()
    h

  override def toString: String =
    s"NeuroSpace(dims=$dims, spacing=$spacing, origin=$origin, axes=$axes)"

object NeuroSpace:
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
    validateDims(dims).flatMap { checkedDims =>
      val spatialDimCount = math.min(checkedDims.length, 3)
      val defaultSpacing = Vector.fill(spatialDimCount)(1.0)
      val defaultOrigin = Vector.fill(spatialDimCount)(0.0)

      for
        sp <- spatialVector("spacing", spacing, defaultSpacing, requirePositive = true)
        org <- spatialVector("origin", origin, defaultOrigin, requirePositive = false)
        t <- validateTransform(trans.getOrElse(defaultTransform(sp, org, spatialDimCount)))
        inv <- DMat.invert(t).left.map(NeuroSpaceError.SingularTransform.apply)
        ax <- axes match
          case Some(value) => validateAxes(checkedDims.length, value).map(_ => value)
          case None => Right(defaultAxes(checkedDims.length, t))
      yield new NeuroSpace(checkedDims, sp, org, ax, t, inv)
    }

  def apply(
      dims: Vector[Int],
      spacing: Option[Vector[Double]] = None,
      origin: Option[Vector[Double]] = None,
      axes: Option[AxisSet] = None,
      trans: Option[DMat] = None
  ): NeuroSpace =
    make(dims, spacing, origin, axes, trans)
      .fold(err => throw new IllegalArgumentException(err.message), identity)

  private def validateDims(dims: Vector[Int]): Either[NeuroSpaceError, Vector[Int]] =
    if dims.isEmpty then Left(NeuroSpaceError.EmptyDimensions)
    else
      var i = 0
      var error = Option.empty[NeuroSpaceError]
      while i < dims.length && error.isEmpty do
        if dims(i) <= 0 then error = Some(NeuroSpaceError.NonPositiveDimension(i, dims(i)))
        i += 1

      error match
        case Some(err) => Left(err)
        case None => Right(dims)

  private def spatialVector(
      label: String,
      candidate: Option[Vector[Double]],
      default: Vector[Double],
      requirePositive: Boolean
  ): Either[NeuroSpaceError, Vector[Double]] =
    val values = candidate.getOrElse(default)
    if values.length != default.length then
      Left(NeuroSpaceError.SpatialVectorLengthMismatch(label, default.length, values.length))
    else
      var i = 0
      var error = Option.empty[NeuroSpaceError]
      while i < values.length && error.isEmpty do
        val value = values(i)
        val axis = SpatialAxis.all(i)
        if !value.isFinite then error = Some(NeuroSpaceError.NonFiniteSpatialValue(label, axis, value))
        else if requirePositive && value <= 0.0 then error = Some(NeuroSpaceError.NonPositiveSpacing(axis, value))
        i += 1

      error match
        case Some(err) => Left(err)
        case None => Right(values)

  private def defaultTransform(spacing: Vector[Double], origin: Vector[Double], spatialDimCount: Int): DMat =
    val rows = Vector.tabulate(4)(r =>
      Vector.tabulate(4)(c =>
        if r == c && r < spatialDimCount then spacing(r)
        else if c == 3 && r < spatialDimCount then origin(r)
        else if r == c then 1.0
        else 0.0
      )
    )
    DMat.fromRows(rows)

  private def validateTransform(transform: DMat): Either[NeuroSpaceError, DMat] =
    if transform.rows != 4 || transform.cols != 4 then
      Left(NeuroSpaceError.InvalidTransformShape(transform.rows, transform.cols))
    else
      var i = 0
      var error = Option.empty[NeuroSpaceError]
      while i < transform.data.length && error.isEmpty do
        if !transform.data(i).isFinite then error = Some(NeuroSpaceError.NonFiniteTransformValue(i))
        i += 1

      error match
        case Some(err) => Left(err)
        case None =>
          val bottom = Vector(transform(3, 0), transform(3, 1), transform(3, 2), transform(3, 3))
          if bottom == Vector(0.0, 0.0, 0.0, 1.0) then Right(transform)
          else Left(NeuroSpaceError.InvalidTransformBottomRow(bottom))

  private def validateAxes(expected: Int, axes: AxisSet): Either[NeuroSpaceError, Unit] =
    if axes.ndim == expected then Right(())
    else Left(NeuroSpaceError.AxisCountMismatch(expected, axes.ndim))

  private def defaultAxes(dimsLength: Int, transform: DMat): AxisSet =
    val base = AxisSet.standard(dimsLength)
    if dimsLength >= 3 then
      val inferred = Orientation.findAnatomy(transform)
      AxisSet((inferred.axes ++ base.additionalAxes)*)
    else base

final case class VolumeSpace private (space: NeuroSpace):
  def shape: SpatialDims =
    space.spatialShape

  def dims: Vector[Int] =
    space.dims

  def affine: Affine3D =
    space.affine3D

  def nVoxels: Int =
    shape.product

  def toNeuroSpace: NeuroSpace =
    space

  def voxelToWorld(voxel: VoxelPoint): WorldPoint =
    affine.voxelToWorld(voxel)

  def worldToVoxel(world: WorldPoint): VoxelPoint =
    affine.worldToVoxel(world)

  def addTime(n: Int): SeriesSpace =
    SeriesSpace.unsafe(space.addDim(n, Some(Axis.Time)))

object VolumeSpace:
  def make(space: NeuroSpace): Either[NeuroSpaceError, VolumeSpace] =
    if space.ndim == 3 then Right(new VolumeSpace(space))
    else Left(NeuroSpaceError.ExpectedDimensionality("VolumeSpace", 3, space.ndim))

  def fromSpatialPart(space: NeuroSpace): Either[NeuroSpaceError, VolumeSpace] =
    if space.ndim >= 3 then Right(new VolumeSpace(space.spatialSpace))
    else Left(NeuroSpaceError.ExpectedDimensionality("VolumeSpace spatial part", 3, space.ndim))

  def apply(space: NeuroSpace): VolumeSpace =
    make(space).fold(err => throw new IllegalArgumentException(err.message), identity)

  def unsafe(space: NeuroSpace): VolumeSpace =
    new VolumeSpace(space)

final case class SeriesSpace private (space: NeuroSpace):
  def volumeSpace: VolumeSpace =
    VolumeSpace.unsafe(space.spatialSpace)

  def spatialShape: SpatialDims =
    volumeSpace.shape

  def dims: Vector[Int] =
    space.dims.take(4)

  def nVolumes: Int =
    space.dims(3)

  def affine: Affine3D =
    space.affine3D

  def toNeuroSpace: NeuroSpace =
    space

object SeriesSpace:
  def make(space: NeuroSpace): Either[NeuroSpaceError, SeriesSpace] =
    if space.ndim == 4 then Right(new SeriesSpace(space))
    else Left(NeuroSpaceError.ExpectedDimensionality("SeriesSpace", 4, space.ndim))

  def apply(space: NeuroSpace): SeriesSpace =
    make(space).fold(err => throw new IllegalArgumentException(err.message), identity)

  def unsafe(space: NeuroSpace): SeriesSpace =
    new SeriesSpace(space)
