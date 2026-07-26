package scalafim.image

import narr.NArray
import scala.reflect.ClassTag

sealed trait ImageDim
sealed trait Slice2D extends ImageDim
sealed trait Volume3D extends ImageDim
sealed trait Series4D extends ImageDim

trait ImageDimEvidence[D <: ImageDim]:
  def label: String
  def rank: Int
  def expectedShape(space: NeuroSpace): Either[NeuroImageError, Vector[Int]]
  def canonicalSpace(space: NeuroSpace): Either[NeuroImageError, NeuroSpace]

object ImageDimEvidence:
  given slice2D: ImageDimEvidence[Slice2D] with
    def label: String = "NeuroSlice"
    def rank: Int = 2

    def expectedShape(space: NeuroSpace): Either[NeuroImageError, Vector[Int]] =
      if space.ndim == 2 then Right(space.dims)
      else Left(NeuroImageError.Space(NeuroSpaceError.ExpectedDimensionality(label, 2, space.ndim)))

    def canonicalSpace(space: NeuroSpace): Either[NeuroImageError, NeuroSpace] =
      expectedShape(space).map(_ => space)

  given volume3D: ImageDimEvidence[Volume3D] with
    def label: String = "NeuroVol"
    def rank: Int = 3

    def expectedShape(space: NeuroSpace): Either[NeuroImageError, Vector[Int]] =
      if space.ndim >= 3 then Right(space.spatialDims)
      else Left(NeuroImageError.Space(NeuroSpaceError.ExpectedDimensionality(label, 3, space.ndim)))

    def canonicalSpace(space: NeuroSpace): Either[NeuroImageError, NeuroSpace] =
      VolumeSpace.fromSpatialPart(space)
        .map(_.toNeuroSpace)
        .left.map(NeuroImageError.Space.apply)

  given series4D: ImageDimEvidence[Series4D] with
    def label: String = "NeuroVec"
    def rank: Int = 4

    def expectedShape(space: NeuroSpace): Either[NeuroImageError, Vector[Int]] =
      if space.ndim == 4 then Right(space.dims.take(4))
      else Left(NeuroImageError.Space(NeuroSpaceError.ExpectedDimensionality(label, 4, space.ndim)))

    def canonicalSpace(space: NeuroSpace): Either[NeuroImageError, NeuroSpace] =
      expectedShape(space).map(_ => space)

final case class ImageSpace[D <: ImageDim] private (
  raw: NeuroSpace
):
  def toNeuroSpace: NeuroSpace =
    raw

object ImageSpace:
  def make[D <: ImageDim](space: NeuroSpace)(using dim: ImageDimEvidence[D]): Either[NeuroImageError, ImageSpace[D]] =
    dim.canonicalSpace(space).map(space => new ImageSpace[D](space))

  private[image] def unsafe[D <: ImageDim](space: NeuroSpace): ImageSpace[D] =
    new ImageSpace[D](space)

enum NeuroImageError:
  case InvalidRank(label: String, expected: Int, actual: Int)
  case ShapeMismatch(label: String, expected: Vector[Int], actual: Vector[Int])
  case LinearSizeMismatch(label: String, expected: Int, actual: Int)
  case Space(error: NeuroSpaceError)

  def message: String =
    this match
      case InvalidRank(label, expected, actual) =>
        s"$label requires $expected-dimensional data; got $actual-dimensional data"
      case ShapeMismatch(label, expected, actual) =>
        s"$label data shape mismatch: expected $expected, got $actual"
      case LinearSizeMismatch(label, expected, actual) =>
        s"$label linear data length mismatch: expected $expected, got $actual"
      case Space(error) =>
        error.message

final class NeuroImage[A, D <: ImageDim] private[image] (
  val values: NDArray[A],
  val space: NeuroSpace,
  val label: String = ""
):
  require(values.shape == space.dims.take(values.ndim), "data/space dimension mismatch")

  def ndim: Int =
    values.ndim

  def linear(i: Int): A =
    values.data(i)

  def typedSpace(using dim: ImageDimEvidence[D]): ImageSpace[D] =
    ImageSpace.make[D](space).fold(err => throw new IllegalArgumentException(err.message), identity)

  def map[B](f: A => B)(using ClassTag[B]): NeuroImage[B, D] =
    NeuroImage.unsafe(values.map(f), space, label)

trait NeuroImageView[A, D <: ImageDim]:
  protected def image: NeuroImage[A, D]

  def values: NDArray[A] =
    image.values

  def space: NeuroSpace =
    image.space

  def label: String =
    image.label

  def ndim: Int =
    image.ndim

  def linear(i: Int): A =
    image.linear(i)

  def typedSpace(using dim: ImageDimEvidence[D]): ImageSpace[D] =
    image.typedSpace

  protected def sameImage(that: NeuroImageView[?, ?]): Boolean =
    values == that.values && space == that.space && label == that.label

  protected def imageHash: Int =
    var h = values.hashCode()
    h = 31 * h + space.hashCode()
    h = 31 * h + label.hashCode()
    h

object NeuroImage:
  def make[A, D <: ImageDim](
    values: NDArray[A],
    space: NeuroSpace,
    label: String = ""
  )(using dim: ImageDimEvidence[D]): Either[NeuroImageError, NeuroImage[A, D]] =
    if values.ndim != dim.rank then Left(NeuroImageError.InvalidRank(dim.label, dim.rank, values.ndim))
    else
      dim.expectedShape(space).flatMap { expected =>
        if values.shape == expected then Right(new NeuroImage[A, D](values, space, label))
        else Left(NeuroImageError.ShapeMismatch(dim.label, expected, values.shape))
      }

  private[scalafim] def fromLinear[A, D <: ImageDim](
    data: NArray[A],
    space: NeuroSpace,
    label: String = ""
  )(using dim: ImageDimEvidence[D]): NeuroImage[A, D] =
    val shape =
      dim.expectedShape(space).fold(err => throw new IllegalArgumentException(err.message), identity)
    make[A, D](NDArray(data, shape), space, label)
      .fold(err => throw new IllegalArgumentException(err.message), identity)

  private[image] def unsafe[A, D <: ImageDim](
    values: NDArray[A],
    space: NeuroSpace,
    label: String = ""
  ): NeuroImage[A, D] =
    new NeuroImage[A, D](values, space, label)
