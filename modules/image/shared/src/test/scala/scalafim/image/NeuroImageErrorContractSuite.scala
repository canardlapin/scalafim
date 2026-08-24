package scalafim.image

import image4s.AxisKind
import image4s.Continuous
import image4s.ImageError
import image4s.ImageMetadata
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import ravel.CanonicalLayoutError
import ravel.DType.given
import ravel.NDArray
import ravel.Shape
import spire.std.double.given

class NeuroImageErrorContractSuite extends munit.FunSuite:

  private enum ErrorCase derives CanEqual:
    case InvalidRank
    case ShapeMismatch
    case LinearSizeMismatch
    case Space
    case Geometry
    case Image
    case GridOwnerMismatch
    case ConcatenationMetadataMismatch
    case ExpectedSingleTimeAxis
    case CanonicalArraySizeMismatch
    case SpatialAxisOutOfBounds
    case SpatialIndexOutOfBounds

  /** This match is an API contract. The image test settings promote E29 to an
    * error only in this source file, so adding a NeuroImageError case cannot
    * leave the fixture stale.
    */
  private def classify(error: NeuroImageError): ErrorCase =
    error match
      case NeuroImageError.InvalidRank(_, _, _) =>
        ErrorCase.InvalidRank
      case NeuroImageError.ShapeMismatch(_, _, _) =>
        ErrorCase.ShapeMismatch
      case NeuroImageError.LinearSizeMismatch(_, _, _) =>
        ErrorCase.LinearSizeMismatch
      case NeuroImageError.Space(_) =>
        ErrorCase.Space
      case NeuroImageError.Geometry(_) =>
        ErrorCase.Geometry
      case NeuroImageError.Image(_) =>
        ErrorCase.Image
      case NeuroImageError.GridOwnerMismatch(_, _) =>
        ErrorCase.GridOwnerMismatch
      case NeuroImageError.ConcatenationMetadataMismatch(_, _) =>
        ErrorCase.ConcatenationMetadataMismatch
      case NeuroImageError.ExpectedSingleTimeAxis(_) =>
        ErrorCase.ExpectedSingleTimeAxis
      case NeuroImageError.CanonicalArraySizeMismatch(_, _) =>
        ErrorCase.CanonicalArraySizeMismatch
      case NeuroImageError.SpatialAxisOutOfBounds(_) =>
        ErrorCase.SpatialAxisOutOfBounds
      case NeuroImageError.SpatialIndexOutOfBounds(_, _, _) =>
        ErrorCase.SpatialIndexOutOfBounds

  private val volumeSpace = SampleSpaces(Vector(2, 2, 1))

  private val volumeData =
    NDArray.tabulate[Double](2, 2, 1): (x, y, _) =>
      10.0 * x + y

  test("the exhaustive classifier names every current NeuroImageError case"):
    val examples =
      Vector[NeuroImageError](
        NeuroImageError.InvalidRank("rank", 4, 3),
        NeuroImageError.ShapeMismatch("shape", Vector(2), Vector(1)),
        NeuroImageError.LinearSizeMismatch("linear", 2, 1),
        NeuroImageError.Space(SampleSpaceError.EmptyDimensions),
        NeuroImageError.Geometry(GeometryError.InvalidFrameId("")),
        NeuroImageError.Image(ImageError.InvalidAxisName("")),
        NeuroImageError.GridOwnerMismatch(None, None),
        NeuroImageError.ConcatenationMetadataMismatch(
          ImageMetadata.empty,
          ImageMetadata.named("foreign")
        ),
        NeuroImageError.ExpectedSingleTimeAxis(Vector(AxisKind.Channel)),
        NeuroImageError.CanonicalArraySizeMismatch(2, 1),
        NeuroImageError.SpatialAxisOutOfBounds(3),
        NeuroImageError.SpatialIndexOutOfBounds(2, 1, 1)
      )

    assertEquals(examples.map(classify), ErrorCase.values.toVector)

  Vector[(String, () => Unit)](
    "wrong rank is NeuroImageError.InvalidRank" -> (() =>
      SomeNeuroSeries.copyFromCanonicalArray[Double, Continuous](
        Array.empty[Double],
        volumeSpace
      ) match
        case Left(NeuroImageError.InvalidRank("NeuroSeries space", 4, 3)) => ()
        case other => fail(s"expected InvalidRank, found $other")
    ),
    "shape mismatch retains the typed image4s cause" -> (() =>
      SomeScalarVolume.fromRavel(
        NDArray.fill(Shape(1, 2, 1), 0.0),
        volumeSpace
      ) match
        case Left(
              NeuroImageError.Image(
                ImageError.SampledShapeMismatch(
                  Vector(2, 2, 1),
                  Vector(1, 2, 1)
                )
              )
            ) => ()
        case other => fail(s"expected typed SampledShapeMismatch, found $other")
    ),
    "linear mismatch is NeuroImageError.LinearSizeMismatch" -> (() =>
      SomeNeuroVolume.copyFromCanonicalArray[Double, Continuous](
        Array(1.0),
        volumeSpace
      ) match
        case Left(
              NeuroImageError.LinearSizeMismatch(
                "NeuroVolume canonical array",
                4,
                1
              )
            ) => ()
        case other => fail(s"expected LinearSizeMismatch, found $other")
    ),
    "foreign volume sampling retains the typed image4s geometry cause" -> (() =>
      val expected = liveSpace("neuro-error-contract-expected")
      val foreign = liveSpace("neuro-error-contract-foreign")
      val left =
        SomeScalarVolume.unsafeFromRavel(volumeData, expected)
      val rightVolume =
        SomeScalarVolume.unsafeFromRavel(volumeData, foreign)

      left.zipExact(rightVolume)(_ + _) match
        case Left(
              NeuroImageError.Geometry(
                GeometryError.EphemeralFrameMismatch
              )
            ) =>
          ()
        case other => fail(s"expected typed image4s geometry cause, found $other")
    ),
    "noncanonical layout remains the typed Ravel error" -> (() =>
      val volume = right(SomeScalarVolume.fromRavel(volumeData, volumeSpace))
      val flipped =
        SomeNeuroVolume.unsafeFromSampled(
          right(volume.sampled.flipSpatial(0))
        )

      flipped.wholeCanonical match
        case Left(_: CanonicalLayoutError) => ()
        case other => fail(s"expected CanonicalLayoutError, found $other")
    )
  ).foreach: (name, verify) =>
    test(name):
      verify()

  private def liveSpace(label: String): SomeSampleSpace =
    val frame = right(Frame.named[D3](label))
    val grid =
      right(
        Grid.in(frame)(
          Vector(2, 2, 1),
          Affine.identity[D3]
        )
      )
    SampleSpaces.fromCanonical(SampleSpace.create(grid, NonSpatialAxes.empty))

  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)
