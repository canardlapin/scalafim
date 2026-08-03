package scalafim.image

import cats.Applicative
import cats.syntax.all.*
import ravel.Array1
import ravel.DType
import ravel.NDArray as RavelArray
import ravel.Rank
import ravel.Shape
import ravel.map

final class RoiValues[A] private[image] (
    val selection: VoxelSelection,
    private val data: Array1[A],
    val label: String
):
  def size: Int =
    selection.size

  def apply(voxel: Int): A =
    data(voxel)

  def voxelCoords: Vector[VoxelCoord] =
    selection.voxelCoords

  /** Immutable Ravel storage in selection order. */
  def values: Array1[A] =
    data

  def mapValues[B](f: A => B)(using DType[B]): RoiValues[B] =
    RoiValues.unsafe(selection, data.map(f), label)

  def mapVoxels[B](f: (VoxelCoord, A) => B)(using DType[B]): RoiValues[B] =
    val coords = selection.voxelCoords
    val out =
      RavelArray.tabulate[B](data.size): i =>
        f(coords(i), data(i))
    RoiValues.unsafe(selection, out, label)

  def traverseValues[F[_], B](
      f: A => F[B]
  )(using Applicative[F], DType[B]): F[RoiValues[B]] =
    Vector
      .tabulate(data.size)(index => data(index))
      .traverse(f)
      .map(values =>
        RoiValues.unsafe(
          selection,
          RavelArray.fromSeq(Shape(values.length), values),
          label
        )
      )

  def toROIVol: ROIVol[A] =
    ROIVol.unsafeOwned(
      selection.space.toNeuroSpace,
      selection.toVoxelRoi,
      data
    )

  private[image] def unsafeArray: Array1[A] =
    data

object RoiValues:
  private[image] def unsafe[A](
      selection: VoxelSelection,
      data: Array1[A],
      label: String
  ): RoiValues[A] =
    require(data.size == selection.size, "ROI value count must match selection")
    new RoiValues(selection, data, label)

final class RoiSeries[A] private[image] (
    val source: SeriesSpace,
    val selection: VoxelSelection,
    private val data: RavelArray[A, Rank[2]],
    val label: String
):
  def nTime: Int =
    data.shape(0)

  def nVoxels: Int =
    selection.size

  def apply(time: Int, voxel: Int): A =
    data(time, voxel)

  def voxelCoords: Vector[VoxelCoord] =
    selection.voxelCoords

  def seriesAt(voxel: Int)(using DType[A]): Array1[A] =
    require(voxel >= 0 && voxel < nVoxels, "voxel index out of bounds")
    RavelArray.tabulate[A](nTime)(time => data(time, voxel))

  def toNDArray: RavelArray[A, Rank[2]] =
    data

  def mapValues[B](f: A => B)(using DType[B]): RoiSeries[B] =
    RoiSeries.unsafe(source, selection, data.map(f), label)

  def mapVoxels[B](f: (VoxelCoord, A) => B)(using
      DType[B]
  ): RoiSeries[B] =
    val coords = selection.voxelCoords
    val out =
      RavelArray.tabulate[B](nTime, nVoxels) { (time, voxel) =>
        f(coords(voxel), data(time, voxel))
      }
    RoiSeries.unsafe(source, selection, out, label)

  def mapSamples[B](
      f: (VoxelCoord, Int, A) => B
  )(using DType[B]): RoiSeries[B] =
    val coords = selection.voxelCoords
    val out =
      RavelArray.tabulate[B](nTime, nVoxels) { (time, voxel) =>
        f(coords(voxel), time, data(time, voxel))
      }
    RoiSeries.unsafe(source, selection, out, label)

  def traverseValues[F[_], B](
      f: A => F[B]
  )(using Applicative[F], DType[B]): F[RoiSeries[B]] =
    Vector
      .tabulate(data.size): index =>
        val time = index / nVoxels
        val voxel = index % nVoxels
        data(time, voxel)
      .traverse(f)
      .map: values =>
        RoiSeries.unsafe(
          source,
          selection,
          RavelArray.fromSeq(Shape(nTime, nVoxels), values),
          label
        )

  def toROIVec: ROIVec[A] =
    ROIVec(source.toNeuroSpace, selection.toVoxelRoi.toROICoords, toNDArray)

object RoiSeries:
  private[image] def unsafe[A](
      source: SeriesSpace,
      selection: VoxelSelection,
      data: RavelArray[A, Rank[2]],
      label: String
  ): RoiSeries[A] =
    GridCompatibility.requireVolume(source.volumeSpace, selection.space)
    require(
      data.shape == Shape(source.nVolumes, selection.size),
      "ROI series shape mismatch"
    )
    new RoiSeries(source, selection, data, label)
