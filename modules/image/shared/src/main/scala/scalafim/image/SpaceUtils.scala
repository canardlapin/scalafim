package scalafim.image

import SampleSpaces.*

import gale.linalg.DMat
import image4s.geometry.Affine
import image4s.geometry.D3

object SpaceUtils:

  final case class Bounds(min: Vector[Double], max: Vector[Double])
  final case class AlignedSpace(
      shape: Vector[Int],
      affine: Affine[D3],
      bounds: Bounds
  )

  enum IndexBase:
    case R, Zero

  def outputAlignedSpace(space: SomeSampleSpace): AlignedSpace =
    outputAlignedSpace(space, None)

  def outputAlignedSpace(space: SomeSampleSpace, voxelSizes: Vector[Double]): AlignedSpace =
    outputAlignedSpace(space, Some(voxelSizes))

  def outputAlignedSpace(space: SomeSampleSpace, voxelSize: Double): AlignedSpace =
    outputAlignedSpace(space, Some(Vector(voxelSize)))

  def outputAlignedSpace(space: SomeSampleSpace, voxelSizes: Option[Vector[Double]]): AlignedSpace =
    val affine =
      space.affineD3
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    outputAlignedSpace(space.dims, affine, voxelSizes)

  def outputAlignedSpace[A, Sem](vol: SomeNeuroVolume[A, Sem]): AlignedSpace =
    outputAlignedSpace(vol.space, None)

  def outputAlignedSpace[A, Sem](vol: SomeNeuroVolume[A, Sem], voxelSizes: Option[Vector[Double]]): AlignedSpace =
    outputAlignedSpace(vol.space.dims, vol.grid.indexToFrame, voxelSizes)

  @scala.annotation.targetName("outputAlignedNeuroSeries")
  def outputAlignedSpace[A, Sem](vec: SomeNeuroSeries[A, Sem]): AlignedSpace =
    outputAlignedSpace(vec.space, None)

  @scala.annotation.targetName("outputAlignedNeuroSeriesWithVoxelSizes")
  def outputAlignedSpace[A, Sem](vec: SomeNeuroSeries[A, Sem], voxelSizes: Option[Vector[Double]]): AlignedSpace =
    outputAlignedSpace(vec.space.dims, vec.grid.indexToFrame, voxelSizes)

  def outputAlignedSpace(shape: Vector[Int], affine: Affine[D3]): AlignedSpace =
    outputAlignedSpace(shape, affine, None)

  def outputAlignedSpace(shape: Vector[Int], affine: Affine[D3], voxelSizes: Vector[Double]): AlignedSpace =
    outputAlignedSpace(shape, affine, Some(voxelSizes))

  def outputAlignedSpace(shape: Vector[Int], affine: Affine[D3], voxelSize: Double): AlignedSpace =
    outputAlignedSpace(shape, affine, Some(Vector(voxelSize)))

  def outputAlignedSpace(
      shape: Vector[Int],
      affine: Affine[D3],
      voxelSizes: Option[Vector[Double]]
  ): AlignedSpace =
    require(shape.nonEmpty, "shape must have at least one dimension")
    require(shape.forall(_ > 0), "shape must contain positive dimensions")

    val nAxes = math.min(3, shape.length)
    val spatialShape = shape.take(nAxes)
    val outVox = normalizeVoxelSizes(voxelSizes, nAxes)

    val nCorners = 1 << nAxes
    val corners =
      Vector.tabulate(nCorners) { mask =>
        Vector.tabulate(3) { axis =>
          if axis >= nAxes then 0.0
          else if ((mask >>> axis) & 1) == 0 then 0.0
          else (spatialShape(axis) - 1).toDouble
        }
      }

    val worldCorners = corners.map: corner =>
      affine(corner)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    val mins = Vector.tabulate(3)(axis => worldCorners.map(_(axis)).min)
    val maxs = Vector.tabulate(3)(axis => worldCorners.map(_(axis)).max)
    val fullShape =
      Vector.tabulate(3) { axis =>
        math.ceil((maxs(axis) - mins(axis)) / outVox(axis)).toInt + 1
      }
    val outShape = fullShape.take(nAxes)
    val outAffine =
      Affine
        .fromRowMajor[D3](
          Vector.tabulate(16) { flat =>
            val r = flat / 4
            val c = flat % 4
            if r < 3 && c < 3 && r == c then outVox(r)
            else if r < 3 && c == 3 then mins(r)
            else if r == 3 && c == 3 then 1.0
            else 0.0
          }
        )
        .fold(error => throw new IllegalArgumentException(error.message), identity)

    AlignedSpace(outShape, outAffine, Bounds(mins, maxs))

  def vox2outVox(space: SomeSampleSpace): AlignedSpace =
    outputAlignedSpace(space)

  def vox2outVox(space: SomeSampleSpace, voxelSizes: Vector[Double]): AlignedSpace =
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

  private def normalizeVoxelSizes(voxelSizes: Option[Vector[Double]], nAxes: Int): Vector[Double] =
    voxelSizes match
      case None => Vector(1.0, 1.0, 1.0)
      case Some(vs) =>
        val expanded =
          if vs.length == 1 then Vector.fill(nAxes)(vs.head)
          else vs
        require(expanded.length == nAxes, "voxelSizes must have length 1 or match spatial axes")
        require(expanded.forall(v => v.isFinite && v > 0.0), "voxelSizes must be positive and finite")
        Vector.tabulate(3)(axis => if axis < nAxes then expanded(axis) else 1.0)

object Deoblique:

  def target(space: SomeSampleSpace): SomeSampleSpace =
    target(space, gridset = None, newgrid = None)

  def target(space: SomeSampleSpace, newgrid: Double): SomeSampleSpace =
    target(space, gridset = None, newgrid = Some(newgrid))

  def target(space: SomeSampleSpace, gridset: SomeSampleSpace): SomeSampleSpace =
    target(space, gridset = Some(gridset), newgrid = None)

  def target(space: SomeSampleSpace, gridset: Option[SomeSampleSpace], newgrid: Option[Double]): SomeSampleSpace =
    require(space.ndim == 3, "deoblique currently supports 3D spaces only")
    require(!(gridset.isDefined && newgrid.isDefined), "gridset and newgrid are mutually exclusive")

    gridset match
      case Some(grid) =>
        require(grid.ndim == 3, "gridset must define a 3D SomeSampleSpace")
        grid
      case None =>
        val voxelSize =
          newgrid.getOrElse(space.spacing.take(3).min)
        require(voxelSize.isFinite && voxelSize > 0.0, "newgrid must be positive and finite")
        val aligned = SpaceUtils.outputAlignedSpace(space, voxelSize)
        val spacing = Vector.tabulate(3)(i => aligned.affine.matrix(i, i))
        val origin = Vector.tabulate(3)(i => aligned.affine.matrix(i, 3))
        SampleSpaces(
          dims = aligned.shape,
          spacing = Some(spacing),
          origin = Some(origin),
          affine = Some(aligned.affine)
        )

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
