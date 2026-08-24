package scalafim.image

import image4s.Axis as ImageAxis
import image4s.AxisKind
import image4s.ImageError
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
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.GridId
import image4s.geometry.LengthUnit
import image4s.locus.GridDomain
import gale.linalg.DMat
import scalafim.image.NeuroAffineSyntax.*

enum SampleSpaceError:
  case EmptyDimensions
  case NonPositiveDimension(index: Int, value: Int)
  case SpatialVectorLengthMismatch(label: String, expected: Int, actual: Int)
  case NonFiniteSpatialValue(label: String, axis: SpatialAxis, value: Double)
  case NonPositiveSpacing(axis: SpatialAxis, value: Double)
  case AxisCountMismatch(expected: Int, actual: Int)
  case AxisExtentMismatch(index: Int, expected: Int, actual: Int)
  case ExpectedDimensionality(label: String, expected: Int, actual: Int)
  case UnexpectedNonSpatialAxes(actual: Vector[AxisKind])
  case Geometry(cause: GeometryError)
  case Image(cause: ImageError)

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
      case AxisCountMismatch(expected, actual) =>
        s"non-spatial axis count must match trailing dimensionality: expected $expected, got $actual"
      case AxisExtentMismatch(index, expected, actual) =>
        s"non-spatial axis $index extent must match its dimension: expected $expected, got $actual"
      case ExpectedDimensionality(label, expected, actual) =>
        s"$label requires exactly $expected dimensions; got $actual"
      case UnexpectedNonSpatialAxes(actual) =>
        val kinds = actual.map(_.id).mkString(", ")
        s"spatial-only sample space requires no non-spatial axes; got [$kinds]"
      case Geometry(cause) =>
        cause.message
      case Image(cause) =>
        cause.message

/** Neuroimaging constructors and checked refinements for image4s sampling
  * geometry. Values remain the exact provider-owned `SomeSampleSpace` object.
  */
object SampleSpaces:
  private val rasD2FrameId =
    FrameId
      .parse("scalafim-ras-d2")
      .fold(error => throw new IllegalStateException(error.message), identity)

  private val rasD3FrameId =
    FrameId
      .parse("scalafim-ras-d3")
      .fold(error => throw new IllegalStateException(error.message), identity)

  private[image] def fromCanonical(space: SomeSampleSpace): SomeSampleSpace =
    space

  private[image] def canonical(space: SomeSampleSpace): SomeSampleSpace =
    space

  /** Recover the checked D3 provider type at a dynamic compatibility boundary.
    *
    * `SomeSampleSpace` can contain only the sealed image4s dimensions. The
    * runtime rank check therefore justifies the erased cast; the retained
    * object is still the exact original SampleSpace and grid owner.
    */
  private[scalafim] def requireD3(
      space: SomeSampleSpace
  ): Either[
    SampleSpaceError,
    SampleSpace[? <: Frame[D3], D3]
  ] =
    val canonical = space.typed
    if canonical.spatialRank == 3 then
      Right(
        canonical.asInstanceOf[SampleSpace[Frame[D3], D3]]
      )
    else
      Left(
        SampleSpaceError.ExpectedDimensionality(
          "D3 sample space",
          3,
          canonical.spatialRank
        )
      )

  private[scalafim] def requireSpatialD3(
      space: SomeSampleSpace
  ): Either[
    SampleSpaceError,
    SampleSpace[? <: Frame[D3], D3]
  ] =
    requireD3(space).map(_.spatialOnly)

  /** Admit a volume space without silently discarding non-spatial axes. */
  private[scalafim] def requireVolumeD3(
      space: SomeSampleSpace
  ): Either[
    SampleSpaceError,
    SampleSpace[? <: Frame[D3], D3]
  ] =
    requireD3(space).flatMap: spatial =>
      val nonSpatialKinds = spatial.nonSpatialAxes.values.map(_.kind)
      if nonSpatialKinds.isEmpty then Right(spatial)
      else Left(SampleSpaceError.UnexpectedNonSpatialAxes(nonSpatialKinds))

  /** Assign deterministic persistent identity to exact D3 sampling geometry.
    *
    * External decoders intentionally produce ephemeral frame and grid owners.
    * ScalaFIM admits those values by retaining their exact geometry and axes
    * while constructing the persistent frame/grid keys used by GridDomain.
    * Existing persistent sample spaces pass through unchanged.
    */
  private[scalafim] def persistentD3[F <: Frame[D3]](
      space: SampleSpace[F, D3]
  ): Either[
    GeometryError,
    SampleSpace[? <: Frame[D3], D3]
  ] =
    if space.grid.persistentId.nonEmpty then Right(space)
    else
      for
        frameId <- persistentFrameId(
          3,
          space.grid.frame.unit,
          space.grid.frame.convention
        )
        frame = Frame.createPersistent[D3](
          frameId,
          space.grid.frame.metadata,
          space.grid.frame.unit,
          space.grid.frame.convention
        )
        gridId <- admittedGridId(
          3,
          frameId,
          space.grid.shape,
          space.grid.indexToFrame.rowMajor
        )
        grid <- Grid.createPersistent(gridId, frame)(
          space.grid.shape,
          space.grid.indexToFrame
        )
      yield SampleSpace.create(grid, space.nonSpatialAxes)

  private[image] def logicalDims(space: SomeSampleSpace): Vector[Int] =
    space.logicalShape

  private[image] def spatialShapeOf(
      space: SomeSampleSpace
  ): SpatialDims =
    SpatialDims.unsafeFromVector(space.grid.shape)

  private[image] def spatialPart(space: SomeSampleSpace): SomeSampleSpace =
    fromCanonical(space.typed.spatialOnly)

  extension (space: SomeSampleSpace)
    private[scalafim] def dims: Vector[Int] =
      space.logicalShape

    private[scalafim] def ndim: Int =
      dims.length

    private[scalafim] def spatialDims: Vector[Int] =
      space.grid.shape

    private[scalafim] def spatialShape: SpatialDims =
      SpatialDims.unsafeFromVector(spatialDims)

    private[scalafim] def spacing: Vector[Double] =
      val matrix = space.grid.indexToFrame.matrix
      Vector.tabulate(space.spatialRank): column =>
        var row = 0
        var sum = 0.0
        while row < space.spatialRank do
          val value = matrix(row, column)
          sum += value * value
          row += 1
        math.sqrt(sum)

    private[scalafim] def origin: Vector[Double] =
      val matrix = space.grid.indexToFrame.matrix
      Vector.tabulate(space.spatialRank)(axis =>
        matrix(axis, space.spatialRank)
      )

    private[scalafim] def orientation: Orientation3D =
      val matrix = space.grid.indexToFrame.matrix
      Orientation.findAnatomy(matrix)

    private[scalafim] def affineD3: Either[SampleSpaceError, GeometryAffine[D3]] =
      requireD3(space).map(_.grid.indexToFrame)

    private[scalafim] def inverseAffineD3: Either[SampleSpaceError, GeometryAffine[D3]] =
      affineD3.map(_.inverse)

    private[scalafim] def gridToIndex(coords: Vector[Int]): Int =
      Indexing.gridToIndex(dims, coords)

    private[scalafim] def indexToGrid(idx: Int): Vector[Int] =
      Indexing.indexToGrid(dims, idx)

    private[scalafim] def gridToIndex3D(x: Int, y: Int, z: Int): Int =
      gridToIndex3D(VoxelCoord(x, y, z))

    private[scalafim] def gridToIndex3D(coord: VoxelCoord): Int =
      Indexing.gridToIndex3D(spatialShape, coord)

    private[scalafim] def indexToGrid3D(idx: Int): Vector[Int] =
      indexToVoxel3D(idx).toVector

    private[scalafim] def indexToVoxel3D(idx: Int): VoxelCoord =
      Indexing.indexToGrid3D(spatialShape, idx)

    private[scalafim] def spatialSpace: SomeSampleSpace =
      fromCanonical(space.typed.spatialOnly)

    /** Append one exact provider-owned non-spatial sampling axis. */
    private[scalafim] def addDim(axis: ImageAxis): SomeSampleSpace =
      space.typed
        .appendNonSpatial(axis)
        .map(fromCanonical)
        .fold(
          error => throw new IllegalArgumentException(error.message),
          identity
        )

    private[scalafim] def dropDim(dimnum: Int = space.ndim - 1): SomeSampleSpace =
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
        val newSpacing = spacing.patch(dimnum, Nil, 1)
        val newOrigin = origin.patch(dimnum, Nil, 1)
        SampleSpaces(
          dims = newDims,
          spacing = Some(newSpacing),
          origin = Some(newOrigin),
          axes = Some(space.nonSpatialAxes),
          affine = None
        )

    private[scalafim] def indexToCoord(index: Vector[Double]): Vector[Double] =
      transformCoordinates(space.grid.indexToFrame.matrix, index)

    private[scalafim] def indexToPoint(index: SpatialPoint): SpatialPoint =
      SpatialPoint.unsafeFromVector(
        indexToCoord(index.toVector),
        "world coordinate"
      )

    private[scalafim] def voxelToWorld(
        voxel: VoxelPoint
    ): Either[SampleSpaceError, WorldPoint] =
      affineD3
        .flatMap(
          _.apply(voxel.toVector).left.map(SampleSpaceError.Geometry.apply)
        )
        .map(value => WorldPoint.unsafeFromVector(value, "world point"))

    private[scalafim] def coordToIndex(coord: Vector[Double]): Vector[Double] =
      transformCoordinates(space.grid.indexToFrame.inverse.matrix, coord)

    private[scalafim] def coordToIndexPoint(coord: SpatialPoint): SpatialPoint =
      SpatialPoint.unsafeFromVector(
        coordToIndex(coord.toVector),
        "voxel coordinate"
      )

    private[scalafim] def worldToVoxel(
        world: WorldPoint
    ): Either[SampleSpaceError, VoxelPoint] =
      inverseAffineD3
        .flatMap(
          _.apply(world.toVector).left.map(SampleSpaceError.Geometry.apply)
        )
        .map(value => VoxelPoint.unsafeFromVector(value, "voxel point"))

  extension [F <: Frame[D3]](grid: Grid[F, D3])
    private[scalafim] def spatialShape: SpatialDims =
      SpatialDims.unsafeFromVector(grid.shape)

    private[scalafim] def affine: GeometryAffine[D3] =
      grid.indexToFrame

    private[scalafim] def nVoxels: Int =
      grid.shape.product

    private[scalafim] def voxelToWorld(voxel: VoxelPoint): Either[GeometryError, WorldPoint] =
      affine(voxel.toVector)
        .map(value => WorldPoint.unsafeFromVector(value, "world point"))

    private[scalafim] def worldToVoxel(world: WorldPoint): Either[GeometryError, VoxelPoint] =
      affine.inverse(world.toVector)
        .map(value => VoxelPoint.unsafeFromVector(value, "voxel point"))

  def fromSpatialDims(
      dims: SpatialDims,
      spacing: Option[Vector[Double]] = None,
      origin: Option[Vector[Double]] = None,
      axes: Option[NonSpatialAxes] = None,
      affine: Option[GeometryAffine[D3]] = None
  ): SomeSampleSpace =
    SampleSpaces(dims.toVector, spacing, origin, axes, affine)

  def make(
      dims: Vector[Int],
      spacing: Option[Vector[Double]] = None,
      origin: Option[Vector[Double]] = None,
      axes: Option[NonSpatialAxes] = None,
      affine: Option[GeometryAffine[D3]] = None
  ): Either[SampleSpaceError, SomeSampleSpace] =
    validateDims(dims).flatMap: checkedDims =>
      val spatialDimCount = math.min(checkedDims.length, 3)
      val defaultSpacing = Vector.fill(spatialDimCount)(1.0)
      val defaultOrigin = Vector.fill(spatialDimCount)(0.0)
      for
        _ <-
          if spatialDimCount >= 2 then Right(())
          else
            Left(
              SampleSpaceError.ExpectedDimensionality(
                "SomeSampleSpace spatial part",
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
        transform <- affine match
          case Some(value) => Right(value)
          case None =>
            defaultAffine(sp, org, spatialDimCount)
              .left
              .map(SampleSpaceError.Geometry.apply)
        checkedAxes <- axes match
          case Some(value) =>
            validateAxes(checkedDims.drop(spatialDimCount), value).map(_ => value)
          case None =>
            defaultNonSpatialAxes(checkedDims.drop(spatialDimCount))
        canonical <-
          canonicalSpace(checkedDims, transform, checkedAxes)
      yield canonical

  def apply(
      dims: Vector[Int],
      spacing: Option[Vector[Double]] = None,
      origin: Option[Vector[Double]] = None,
      axes: Option[NonSpatialAxes] = None,
      affine: Option[GeometryAffine[D3]] = None
  ): SomeSampleSpace =
    make(dims, spacing, origin, axes, affine)
      .fold(err => throw new IllegalArgumentException(err.message), identity)

  private def canonicalSpace(
      dims: Vector[Int],
      transform: GeometryAffine[D3],
      axes: NonSpatialAxes
  ): Either[SampleSpaceError, SomeSampleSpace] =
    if dims.length == 2 then
      for
        metadata <- FrameMetadata
          .create("scalafim-space")
          .left
          .map(SampleSpaceError.Geometry.apply)
        frame = Frame.createPersistent[D2](
          rasD2FrameId,
          metadata,
          convention = CoordinateConvention.RAS
        )
        affine <- GeometryAffine
          .fromRowMajor[D2](
            Vector(
              transform.matrix(0, 0),
              transform.matrix(0, 1),
              transform.matrix(0, 3),
              transform.matrix(1, 0),
              transform.matrix(1, 1),
              transform.matrix(1, 3),
              0.0,
              0.0,
              1.0
            )
          )
          .left
          .map(SampleSpaceError.Geometry.apply)
        gridId <- persistentGridId(2, dims, affine.rowMajor)
        grid <- Grid
          .createPersistent(gridId, frame)(dims, affine)
          .left
          .map(SampleSpaceError.Geometry.apply)
      yield fromCanonical(
        SampleSpace.create(grid, axes)
      )
    else
      for
        metadata <- FrameMetadata
          .create("scalafim-space")
          .left
          .map(SampleSpaceError.Geometry.apply)
        frame = Frame.createPersistent[D3](
          rasD3FrameId,
          metadata,
          convention = CoordinateConvention.RAS
        )
        spatialShape = dims.take(3)
        gridId <- persistentGridId(3, spatialShape, transform.rowMajor)
        grid <- Grid
          .createPersistent(gridId, frame)(spatialShape, transform)
          .left
          .map(SampleSpaceError.Geometry.apply)
      yield fromCanonical(SampleSpace.create(grid, axes))

  private def persistentGridId(
      rank: Int,
      shape: Vector[Int],
      affineRowMajor: Vector[Double]
  ): Either[SampleSpaceError, GridId] =
    parseGridId(rank, None, shape, affineRowMajor)
      .left
      .map(SampleSpaceError.Geometry.apply)

  private def admittedGridId(
      rank: Int,
      frameId: FrameId,
      shape: Vector[Int],
      affineRowMajor: Vector[Double]
  ): Either[GeometryError, GridId] =
    val frameComponent =
      if frameId == rasD3FrameId then None else Some(frameId.value)
    parseGridId(rank, frameComponent, shape, affineRowMajor)

  private def parseGridId(
      rank: Int,
      frameComponent: Option[String],
      shape: Vector[Int],
      affineRowMajor: Vector[Double]
  ): Either[GeometryError, GridId] =
    val affineBits =
      affineRowMajor.map: value =>
        java.lang.Long.toUnsignedString(
          java.lang.Double.doubleToRawLongBits(value),
          16
        )
    val framePart = frameComponent.fold("")(value => s"-$value")
    GridId.parse(
      s"scalafim-grid$framePart-d$rank-${shape.mkString("x")}-${affineBits.mkString("-")}"
    )

  private def persistentFrameId(
      rank: Int,
      unit: LengthUnit,
      convention: CoordinateConvention
  ): Either[GeometryError, FrameId] =
    if rank == 3 &&
        unit == LengthUnit.Millimeter &&
        convention == CoordinateConvention.RAS
    then Right(rasD3FrameId)
    else
      FrameId.parse(
        s"scalafim-frame-d$rank-${lengthUnitId(unit)}-${coordinateConventionId(convention)}"
      )

  private def lengthUnitId(unit: LengthUnit): String =
    unit match
      case LengthUnit.Millimeter => "millimeter"
      case LengthUnit.Meter      => "meter"
      case LengthUnit.Micrometer => "micrometer"

  private def coordinateConventionId(
      convention: CoordinateConvention
  ): String =
    convention match
      case CoordinateConvention.Unspecified => "unspecified"
      case CoordinateConvention.RAS         => "ras"
      case CoordinateConvention.LPS         => "lps"

  private def defaultNonSpatialAxes(
      extents: Vector[Int]
  ): Either[SampleSpaceError, NonSpatialAxes] =
    val built =
      extents.zipWithIndex.foldLeft[
        Either[SampleSpaceError, Vector[ImageAxis]]
      ](Right(Vector.empty)):
        case (acc, (extent, index)) =>
          val kind = if index == 0 then AxisKind.Time else AxisKind.Other
          val name = if index == 0 then "time" else s"axis-${index + 3}"
          for
            values <- acc
            axis <- ImageAxis
              .ordinal(name, kind, extent)
              .left
              .map(SampleSpaceError.Image.apply)
          yield values :+ axis
    built.flatMap: values =>
      NonSpatialAxes
        .from(values)
        .left
        .map(SampleSpaceError.Image.apply)

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
  ): Either[SampleSpaceError, Vector[Int]] =
    if dims.isEmpty then Left(SampleSpaceError.EmptyDimensions)
    else
      dims.zipWithIndex.collectFirst {
        case (value, index) if value <= 0 =>
          SampleSpaceError.NonPositiveDimension(index, value)
      } match
        case Some(error) => Left(error)
        case None => Right(dims)

  private def spatialVector(
      label: String,
      candidate: Option[Vector[Double]],
      default: Vector[Double],
      requirePositive: Boolean
  ): Either[SampleSpaceError, Vector[Double]] =
    val values = candidate.getOrElse(default)
    if values.length != default.length then
      Left(
        SampleSpaceError.SpatialVectorLengthMismatch(
          label,
          default.length,
          values.length
        )
      )
    else
      values.zipWithIndex.collectFirst {
        case (value, index) if !value.isFinite =>
          SampleSpaceError.NonFiniteSpatialValue(
            label,
            SpatialAxis.all(index),
            value
          )
        case (value, index) if requirePositive && value <= 0.0 =>
          SampleSpaceError.NonPositiveSpacing(
            SpatialAxis.all(index),
            value
          )
      } match
        case Some(error) => Left(error)
        case None => Right(values)

  private def defaultAffine(
      spacing: Vector[Double],
      origin: Vector[Double],
      spatialDimCount: Int
  ): Either[GeometryError, GeometryAffine[D3]] =
    GeometryAffine.fromRowMajor[D3](
      Vector.tabulate(16): flat =>
        val row = flat / 4
        val column = flat % 4
        if row == column && row < spatialDimCount then spacing(row)
        else if column == 3 && row < spatialDimCount then origin(row)
        else if row == column then 1.0
        else 0.0
    )

  private def validateAxes(
      expectedExtents: Vector[Int],
      axes: NonSpatialAxes
  ): Either[SampleSpaceError, Unit] =
    if axes.size != expectedExtents.size then
      Left(
        SampleSpaceError.AxisCountMismatch(
          expectedExtents.size,
          axes.size
        )
      )
    else
      val mismatch =
        expectedExtents.zip(axes.values).zipWithIndex.collectFirst:
          case ((expected, axis), index) if axis.extent != expected =>
            SampleSpaceError.AxisExtentMismatch(index, expected, axis.extent)
      mismatch match
        case Some(error) => Left(error)
        case None        => Right(())
