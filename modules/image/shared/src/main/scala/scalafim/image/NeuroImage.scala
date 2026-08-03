package scalafim.image

import image4s.ImageError
import image4s.geometry.GeometryError

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
  case Geometry(error: GeometryError)
  case Image(error: ImageError)

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
      case Geometry(error) =>
        error.message
      case Image(error) =>
        error.message
