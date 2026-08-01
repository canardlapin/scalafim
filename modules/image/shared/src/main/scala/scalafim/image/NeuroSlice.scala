package scalafim.image

import ravel.DType
import ravel.NDArray as RavelArray
import ravel.Rank

final class NeuroSlice[A] private[image] (
    private[image] val packed: Image4sInterop.PackedSlice[A],
    val label: String
):
  inline def values: RavelArray[A, Rank[2]] =
    packed.data

  inline def sampled: image4s.Sampled[
    ? <: image4s.geometry.Frame[image4s.geometry.D2],
    image4s.geometry.D2,
    A,
    image4s.FieldRole,
    Rank[2]
  ] =
    packed

  /** Zero-wrapper ScalaFIM name for the geometry retained by `sampled`. */
  inline def space: NeuroSpace =
    NeuroSpace.fromCanonical(packed.sampleSpace)

  def ndim: Int =
    2

  def typedSpace: ImageSpace[Slice2D] =
    ImageSpace
      .make[Slice2D](space)
      .fold(error => throw new IllegalStateException(error.message), identity)

  inline def apply(i: Int, j: Int): A =
    values(i, j)

  def linear(index: Int): A =
    require(index >= 0 && index < values.size, "linear index out of bounds")
    val i = index % space.dims(0)
    val j = index / space.dims(0)
    apply(i, j)

  def map[B](f: A => B)(using DType[B]): NeuroSlice[B] =
    NeuroSlice(
      RavelArray.tabulate[B](space.dims(0), space.dims(1)) {
        (i, j) => f(apply(i, j))
      },
      space,
      label
    )

  def copy(
      values: RavelArray[A, Rank[2]] = this.values,
      space: NeuroSpace = this.space,
      label: String = this.label
  ): NeuroSlice[A] =
    NeuroSlice(values, space, label)

  override def equals(other: Any): Boolean =
    other match
      case that: NeuroSlice[?] =>
        values == that.values && space == that.space && label == that.label
      case _ => false

  override def hashCode(): Int =
    var hash = values.hashCode()
    hash = 31 * hash + space.hashCode()
    31 * hash + label.hashCode()

  override def toString: String =
    s"NeuroSlice(values=$values, space=$space, label=$label)"

object NeuroSlice:
  def apply[A](
      values: RavelArray[A, Rank[2]],
      space: NeuroSpace,
      label: String = ""
  ): NeuroSlice[A] =
    val packed =
      Image4sInterop
        .sliceFromRavel(values, space, label)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    new NeuroSlice(packed, label)

  def fromLinear[A](
      data: Array[A],
      space: NeuroSpace,
      label: String = ""
  )(using DType[A]): NeuroSlice[A] =
    require(space.ndim == 2, "NeuroSlice requires exactly two dimensions")
    require(data.length == space.dims.product, "NeuroSlice data length mismatch")
    val n0 = space.dims(0)
    val values =
      RavelArray.tabulate[A](n0, space.dims(1)) {
        (i, j) => data(i + n0 * j)
      }
    apply(values, space, label)
