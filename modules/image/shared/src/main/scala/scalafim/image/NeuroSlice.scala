package scalafim.image

import narr.NArray
import scala.reflect.ClassTag

final class NeuroSlice[A] private[image] (
  protected val image: NeuroImage[A, Slice2D]
) extends NeuroImageView[A, Slice2D]:

  inline def apply(i: Int, j: Int): A =
    values(i, j)

  def map[B](f: A => B)(using ClassTag[B]): NeuroSlice[B] =
    NeuroSlice(values.map(f), space, label)

  def copy(values: NDArray[A] = this.values, space: NeuroSpace = this.space, label: String = this.label): NeuroSlice[A] =
    NeuroSlice(values, space, label)

  override def equals(other: Any): Boolean =
    other match
      case that: NeuroSlice[?] => sameImage(that)
      case _ => false

  override def hashCode(): Int =
    imageHash

  override def toString: String =
    s"NeuroSlice(values=$values, space=$space, label=$label)"

object NeuroSlice:
  def apply[A](values: NDArray[A], space: NeuroSpace, label: String = ""): NeuroSlice[A] =
    NeuroImage.make[A, Slice2D](values, space, label)
      .fold(err => throw new IllegalArgumentException(err.message), image => new NeuroSlice(image))

  def fromLinear[A](data: NArray[A], space: NeuroSpace, label: String = ""): NeuroSlice[A] =
    new NeuroSlice(NeuroImage.fromLinear[A, Slice2D](data, space, label))
