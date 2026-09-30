package scalafim.image

import SampleSpaces.*

import gale.linalg.DMat
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.Point
import scalafim.image.world.{FrameCatalog, WorldSpace}

object SpaceUtils:

  /** An axis-aligned output grid covering a source grid, with the source's world bounds in the source frame. */
  final case class AlignedSpace[F <: Frame[D3]](
      shape: Vector[Int],
      affine: Affine[D3],
      bounds: WorldBox[F]
  )

  enum IndexBase:
    case R, Zero

  def outputAlignedSpace(space: SomeSampleSpace): AlignedSpace[?] =
    outputAlignedSpace(space, None)

  def outputAlignedSpace(space: SomeSampleSpace, voxelSizes: Vector[Double]): AlignedSpace[?] =
    outputAlignedSpace(space, Some(voxelSizes))

  def outputAlignedSpace(space: SomeSampleSpace, voxelSize: Double): AlignedSpace[?] =
    outputAlignedSpace(space, Some(Vector(voxelSize)))

  def outputAlignedSpace(space: SomeSampleSpace, voxelSizes: Option[Vector[Double]]): AlignedSpace[?] =
    alignedSpace(space, voxelSizes).fold(error => throw new IllegalArgumentException(error.message), identity)

  /** The aligned output space of a sample space, in the space's own frame, or a typed error. */
  def alignedSpace(space: SomeSampleSpace, voxelSizes: Option[Vector[Double]]): Either[SampleSpaceError, AlignedSpace[?]] =
    space.affineD3.flatMap(affine => aligned(GridSpec.fromSpace(space).frame, space.dims, affine, voxelSizes))

  /** The aligned output space of a frame-typed grid; its bounds stay in the grid's frame. */
  def outputAlignedSpace[F <: Frame[D3]](grid: GridSpec[F], voxelSizes: Option[Vector[Double]]): AlignedSpace[F] =
    aligned(grid.frame, grid.dims, grid.affine, voxelSizes).fold(error => throw new IllegalArgumentException(error.message), identity)

  def outputAlignedSpace[A, Sem](vol: SomeNeuroVolume[A, Sem]): AlignedSpace[?] =
    outputAlignedSpace(vol.space, None)

  def outputAlignedSpace[A, Sem](vol: SomeNeuroVolume[A, Sem], voxelSizes: Option[Vector[Double]]): AlignedSpace[?] =
    outputAlignedSpace(vol.space, voxelSizes)

  @scala.annotation.targetName("outputAlignedNeuroSeries")
  def outputAlignedSpace[A, Sem](vec: SomeNeuroSeries[A, Sem]): AlignedSpace[?] =
    outputAlignedSpace(vec.space, None)

  @scala.annotation.targetName("outputAlignedNeuroSeriesWithVoxelSizes")
  def outputAlignedSpace[A, Sem](vec: SomeNeuroSeries[A, Sem], voxelSizes: Option[Vector[Double]]): AlignedSpace[?] =
    outputAlignedSpace(vec.space, voxelSizes)

  def outputAlignedSpace(shape: Vector[Int], affine: Affine[D3]): AlignedSpace[?] =
    outputAlignedSpace(shape, affine, None)

  def outputAlignedSpace(shape: Vector[Int], affine: Affine[D3], voxelSizes: Vector[Double]): AlignedSpace[?] =
    outputAlignedSpace(shape, affine, Some(voxelSizes))

  def outputAlignedSpace(shape: Vector[Int], affine: Affine[D3], voxelSize: Double): AlignedSpace[?] =
    outputAlignedSpace(shape, affine, Some(Vector(voxelSize)))

  /** A bare shape and affine name no world, so the bounds are placed in a fresh unresolved RAS-mm world frame. */
  def outputAlignedSpace(
      shape: Vector[Int],
      affine: Affine[D3],
      voxelSizes: Option[Vector[Double]]
  ): AlignedSpace[?] =
    alignedSpace(shape, affine, voxelSizes).fold(error => throw new IllegalArgumentException(error.message), identity)

  /** The axis-aligned grid enclosing a grid's corners, or a typed error for invalid shapes, voxel sizes or affines. */
  def alignedSpace(
      shape: Vector[Int],
      affine: Affine[D3],
      voxelSizes: Option[Vector[Double]]
  ): Either[SampleSpaceError, AlignedSpace[?]] =
    aligned(FrameCatalog.frame(WorldSpace.freshUnresolved()), shape, affine, voxelSizes)

  private def aligned[F <: Frame[D3]](
      frame: F,
      shape: Vector[Int],
      affine: Affine[D3],
      voxelSizes: Option[Vector[Double]]
  ): Either[SampleSpaceError, AlignedSpace[F]] =
    def invalid(reason: String) = SampleSpaceError.InvalidArgument(reason)
    for
      _ <- Either.cond(shape.nonEmpty, (), invalid("shape must have at least one dimension"))
      _ <- Either.cond(shape.forall(_ > 0), (), invalid("shape must contain positive dimensions"))
      nAxes = math.min(3, shape.length)
      spatialShape = shape.take(nAxes)
      outVox <- checkedVoxelSizes(voxelSizes, nAxes)
      corners = Vector.tabulate(1 << nAxes) { mask =>
        Vector.tabulate(3)(axis => if axis >= nAxes || ((mask >>> axis) & 1) == 0 then 0.0 else (spatialShape(axis) - 1).toDouble)
      }
      worldCorners <- corners.foldLeft[Either[SampleSpaceError, Vector[Point[F, D3]]]](Right(Vector.empty)): (acc, corner) =>
        acc.flatMap(done => affine(corner).flatMap(world => GridSpec.pointIn(frame, world)).left.map(SampleSpaceError.Geometry(_)).map(done :+ _))
      bounds <- WorldBox.enclosing(worldCorners).toRight(invalid("no grid corners"))
      mins = bounds.min.coordinates
      maxs = bounds.max.coordinates
      outShape = Vector.tabulate(3)(axis => math.ceil((maxs(axis) - mins(axis)) / outVox(axis)).toInt + 1).take(nAxes)
      outAffine <- Affine
        .fromRowMajor[D3](
          Vector.tabulate(16) { flat =>
            val (r, c) = (flat / 4, flat % 4)
            if r < 3 && c < 3 && r == c then outVox(r)
            else if r < 3 && c == 3 then mins(r)
            else if r == 3 && c == 3 then 1.0
            else 0.0
          }
        )
        .left
        .map(SampleSpaceError.Geometry(_))
    yield AlignedSpace(outShape, outAffine, bounds)

  def vox2outVox(space: SomeSampleSpace): AlignedSpace[?] =
    outputAlignedSpace(space)

  def vox2outVox(space: SomeSampleSpace, voxelSizes: Vector[Double]): AlignedSpace[?] =
    outputAlignedSpace(space, voxelSizes)

  def sliceToVolumeAffine(
    index: Int,
    axis: Int,
    shape: Option[Vector[Int]] = None,
    indexBase: IndexBase = IndexBase.Zero,
    axisBase: IndexBase = IndexBase.Zero
  ): DMat =
    val axis0 =
      axisBase match
        case IndexBase.Zero =>
          require(axis >= 0 && axis < 3, "zero-based axis must be in 0..2")
          axis
        case IndexBase.R =>
          require(axis >= 1 && axis <= 3, "R-style axis must be in 1..3")
          axis - 1

    val index0 =
      indexBase match
        case IndexBase.Zero =>
          require(index >= 0, "zero-based index must be non-negative")
          index
        case IndexBase.R =>
          require(index >= 1, "R-style index must be positive")
          index - 1

    shape.foreach { s =>
      require(s.length >= 3 && s.take(3).forall(_ > 0), "shape must provide three positive spatial dimensions")
      require(index0 < s(axis0), "index exceeds shape along axis")
    }

    val keptAxes = Vector(0, 1, 2, 3).filterNot(_ == axis0)
    DMat.dense(
      4,
      3,
      Vector.tabulate(12) { flat =>
          val r = flat / 3
          val c = flat % 3
          if r == axis0 && c == 2 then index0.toDouble
          else if keptAxes(c) == r then 1.0
          else 0.0
      }
    )

  def slice2volume(
    index: Int,
    axis: Int,
    shape: Option[Vector[Int]] = None,
    indexBase: IndexBase = IndexBase.Zero,
    axisBase: IndexBase = IndexBase.Zero
  ): DMat =
    sliceToVolumeAffine(index, axis, shape, indexBase, axisBase)

  private def checkedVoxelSizes(voxelSizes: Option[Vector[Double]], nAxes: Int): Either[SampleSpaceError, Vector[Double]] =
    voxelSizes match
      case None => Right(Vector(1.0, 1.0, 1.0))
      case Some(vs) =>
        val expanded = if vs.length == 1 then Vector.fill(nAxes)(vs.head) else vs
        if expanded.length != nAxes then Left(SampleSpaceError.InvalidArgument("voxelSizes must have length 1 or match spatial axes"))
        else if !expanded.forall(v => v.isFinite && v > 0.0) then Left(SampleSpaceError.InvalidArgument("voxelSizes must be positive and finite"))
        else Right(Vector.tabulate(3)(axis => if axis < nAxes then expanded(axis) else 1.0))

object Deoblique:

  def target(space: SomeSampleSpace): SomeSampleSpace =
    target(space, gridset = None, newgrid = None)

  def target(space: SomeSampleSpace, newgrid: Double): SomeSampleSpace =
    target(space, gridset = None, newgrid = Some(newgrid))

  def target(space: SomeSampleSpace, gridset: SomeSampleSpace): SomeSampleSpace =
    target(space, gridset = Some(gridset), newgrid = None)

  def target(space: SomeSampleSpace, gridset: Option[SomeSampleSpace], newgrid: Option[Double]): SomeSampleSpace =
    targetEither(space, gridset, newgrid).fold(error => throw new IllegalArgumentException(error.message), identity)

  /** The deobliqued (axis-aligned) grid for `space`, kept in the same world space, or a typed error. */
  def targetEither(space: SomeSampleSpace, gridset: Option[SomeSampleSpace], newgrid: Option[Double]): Either[SampleSpaceError, SomeSampleSpace] =
    def invalid(reason: String) = SampleSpaceError.InvalidArgument(reason)
    if space.ndim != 3 then Left(invalid("deoblique currently supports 3D spaces only"))
    else if gridset.isDefined && newgrid.isDefined then Left(invalid("gridset and newgrid are mutually exclusive"))
    else
      gridset match
        case Some(grid) =>
          if grid.ndim == 3 then Right(grid) else Left(invalid("gridset must define a 3D SomeSampleSpace"))
        case None =>
          val voxelSize = newgrid.getOrElse(space.spacing.take(3).min)
          if !(voxelSize.isFinite && voxelSize > 0.0) then Left(invalid("newgrid must be positive and finite"))
          else
            for
              aligned <- SpaceUtils.alignedSpace(space, Some(Vector(voxelSize)))
              world <- SampleSpaces.worldOf(space)
              deobliqued <- SampleSpaces.make(dims = aligned.shape, affine = Some(aligned.affine))
              placed <- SampleSpaces.inWorld(deobliqued, world)
            yield placed

  def apply(space: SomeSampleSpace): SomeSampleSpace =
    target(space)

  def apply(space: SomeSampleSpace, newgrid: Double): SomeSampleSpace =
    target(space, newgrid)

  def apply(space: SomeSampleSpace, gridset: SomeSampleSpace): SomeSampleSpace =
    target(space, gridset)

  def apply(vol: SomeScalarVolume[Double], method: Resample.Method = Resample.Method.Linear): SomeScalarVolume[Double] =
    Resample.resampleTo(vol, target(vol.space), method)

  def apply(vol: SomeScalarVolume[Double], newgrid: Double, method: Resample.Method): SomeScalarVolume[Double] =
    Resample.resampleTo(vol, target(vol.space, newgrid), method)

  def apply(vol: SomeScalarVolume[Double], gridset: SomeSampleSpace, method: Resample.Method): SomeScalarVolume[Double] =
    Resample.resampleTo(vol, target(vol.space, gridset), method)
