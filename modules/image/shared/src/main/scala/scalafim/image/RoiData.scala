package scalafim.image

import cats.Applicative
import cats.syntax.all.*
import narr.NArray
import scala.reflect.ClassTag

final class RoiValues[A] private[image] (
    val selection: VoxelSelection,
    private val data: NArray[A],
    val label: String
):
  def size: Int =
    selection.size

  def apply(voxel: Int): A =
    data(voxel)

  def voxelCoords: Vector[VoxelCoord] =
    selection.voxelCoords

  def toNArray(using ClassTag[A]): NArray[A] =
    NArray.copy(data)

  def mapValues[B](f: A => B)(using ClassTag[B]): RoiValues[B] =
    val out = NArray.ofSize[B](data.length)
    var i = 0
    while i < data.length do
      out(i) = f(data(i))
      i += 1
    RoiValues.unsafe(selection, out, label)

  def mapVoxels[B](f: (VoxelCoord, A) => B)(using ClassTag[B]): RoiValues[B] =
    val coords = selection.voxelCoords
    val out = NArray.ofSize[B](data.length)
    var i = 0
    while i < data.length do
      out(i) = f(coords(i), data(i))
      i += 1
    RoiValues.unsafe(selection, out, label)

  def traverseValues[F[_], B](
      f: A => F[B]
  )(using Applicative[F], ClassTag[B]): F[RoiValues[B]] =
    Vector
      .tabulate(data.length)(index => data(index))
      .traverse(f)
      .map(values => RoiValues.unsafe(selection, NArrayUtil.fromArray(values.toArray), label))

  def toROIVol(using ClassTag[A]): ROIVol[A] =
    ROIVol(selection.space.toNeuroSpace, selection.toVoxelRoi, toNArray)

  private[image] def unsafeArray: NArray[A] =
    data

object RoiValues:
  private[image] def unsafe[A](
      selection: VoxelSelection,
      data: NArray[A],
      label: String
  ): RoiValues[A] =
    require(data.length == selection.size, "ROI value count must match selection")
    new RoiValues(selection, data, label)

final class RoiSeries[A] private[image] (
    val source: SeriesSpace,
    val selection: VoxelSelection,
    private val data: NDArray[A],
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

  def seriesAt(voxel: Int)(using ClassTag[A]): NArray[A] =
    require(voxel >= 0 && voxel < nVoxels, "voxel index out of bounds")
    val out = NArray.ofSize[A](nTime)
    var time = 0
    while time < nTime do
      out(time) = data(time, voxel)
      time += 1
    out

  def toNDArray(using ClassTag[A]): NDArray[A] =
    NDArray(NArray.copy(data.data), data.shape)

  def mapValues[B](f: A => B)(using ClassTag[B]): RoiSeries[B] =
    RoiSeries.unsafe(source, selection, data.map(f), label)

  def mapVoxels[B](f: (VoxelCoord, A) => B)(using ClassTag[B]): RoiSeries[B] =
    val coords = selection.voxelCoords
    val out = NArray.ofSize[B](data.data.length)
    var voxel = 0
    while voxel < nVoxels do
      var time = 0
      while time < nTime do
        val index = time + voxel * nTime
        out(index) = f(coords(voxel), data.data(index))
        time += 1
      voxel += 1
    RoiSeries.unsafe(source, selection, NDArray(out, data.shape), label)

  def mapSamples[B](
      f: (VoxelCoord, Int, A) => B
  )(using ClassTag[B]): RoiSeries[B] =
    val coords = selection.voxelCoords
    val out = NArray.ofSize[B](data.data.length)
    var voxel = 0
    while voxel < nVoxels do
      var time = 0
      while time < nTime do
        val index = time + voxel * nTime
        out(index) = f(coords(voxel), time, data.data(index))
        time += 1
      voxel += 1
    RoiSeries.unsafe(source, selection, NDArray(out, data.shape), label)

  def traverseValues[F[_], B](
      f: A => F[B]
  )(using Applicative[F], ClassTag[B]): F[RoiSeries[B]] =
    Vector
      .tabulate(data.data.length)(index => data.data(index))
      .traverse(f)
      .map: values =>
        RoiSeries.unsafe(
          source,
          selection,
          NDArray(NArrayUtil.fromArray(values.toArray), data.shape),
          label
        )

  def toROIVec(using ClassTag[A]): ROIVec[A] =
    ROIVec(source.toNeuroSpace, selection.toVoxelRoi.toROICoords, toNDArray)

object RoiSeries:
  private[image] def unsafe[A](
      source: SeriesSpace,
      selection: VoxelSelection,
      data: NDArray[A],
      label: String
  ): RoiSeries[A] =
    GridCompatibility.requireVolume(source.volumeSpace, selection.space)
    require(data.shape == Vector(source.nVolumes, selection.size), "ROI series shape mismatch")
    new RoiSeries(source, selection, data, label)
