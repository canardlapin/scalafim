package scalafim.image

import narr.NArray
import scala.reflect.ClassTag

final case class NeuroSlice[A](
  values: NDArray[A],
  space: NeuroSpace,
  label: String = ""
):
  require(values.ndim == 2, "NeuroSlice must be 2D")
  require(values.shape == space.dims.take(2), "data/space dimension mismatch")

  inline def apply(i: Int, j: Int): A =
    values(i, j)

  def map[B](f: A => B)(using ClassTag[B]): NeuroSlice[B] =
    NeuroSlice(values.map(f), space, label)

object NeuroSlice:
  def fromLinear[A](data: NArray[A], space: NeuroSpace, label: String = ""): NeuroSlice[A] =
    NeuroSlice(NDArray(data, space.dims.take(2)), space, label)
