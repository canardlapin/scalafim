package scalafim.image

import SampleSpaces.*

import image4s.ApproximateSamplingCongruence
import image4s.Axis
import image4s.AxisKind
import image4s.AxisUnit
import image4s.ImageError
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.SamplingAlignment
import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.FrameId
import image4s.geometry.GeometryError as ImageGeometryError
import image4s.geometry.Grid
import image4s.geometry.GridId
import image4s.geometry.LengthUnit
import Ops.*
import spire.std.double.given

final class ProviderCongruenceContractSuite extends munit.FunSuite:

  private val shape = Vector(2, 3, 2)

  test("provider certificates retain their exact endpoint references"):
    val fixture = persistentFixture("provider-congruence-endpoints")
    val left = SampleSpace.create(fixture.original, NonSpatialAxes.empty)
    val rightSpace = SampleSpace.create(fixture.restored, NonSpatialAxes.empty)

    val gridCertificate = right(
      Grid.exactCongruence(fixture.original, fixture.restored)
    )
    val samplingCertificate = right(
      SamplingAlignment.exact(left, rightSpace)
    )

    assert(gridCertificate.left eq fixture.original)
    assert(gridCertificate.right eq fixture.restored)
    assert(samplingCertificate.left eq left)
    assert(samplingCertificate.right eq rightSpace)
    assert(!fixture.original.eq(fixture.restored))
    assert(fixture.original.sameRuntimeOwnerAs(fixture.restored))

  test("persistent restore is admitted while a third owner remains outside the evidence"):
    val fixture = persistentFixture("provider-congruence-restore")
    val thirdFrame = persistentFrame("provider-congruence-third-frame")
    val third = right(Grid.in(thirdFrame)(shape, Affine.identity[D3]))
    val certificate = right(
      Grid.exactCongruence(fixture.original, fixture.restored)
    )

    assert(certificate.left eq fixture.original)
    assert(certificate.right eq fixture.restored)
    assert(!certificate.right.eq(third))
    Grid.exactCongruence(fixture.original, third) match
      case Left(_: ImageGeometryError.FrameMismatch) => ()
      case other => fail(s"expected a third-owner frame mismatch, found $other")

  test("distinct ephemeral frames are rejected even for identical geometry"):
    val leftFrame = right(Frame.named[D3]("provider-congruence-ephemeral-left"))
    val rightFrame = right(Frame.named[D3]("provider-congruence-ephemeral-right"))
    val left = right(Grid.in(leftFrame)(shape, Affine.identity[D3]))
    val rightGrid = right(Grid.in(rightFrame)(shape, Affine.identity[D3]))

    assertEquals(
      Grid.exactCongruence(left, rightGrid),
      Left(ImageGeometryError.EphemeralFrameMismatch)
    )

  test("full sampling distinguishes axes that spatial congruence deliberately ignores"):
    val fixture = persistentFixture("provider-congruence-axes")
    val three = right(NonSpatialAxes.from(Vector(ProviderAxes.time(3))))
    val five = right(NonSpatialAxes.from(Vector(ProviderAxes.time(5))))
    val left = SampleSpace.create(fixture.original, three)
    val rightSpace = SampleSpace.create(fixture.restored, five)

    assert(Grid.exactCongruence(left.grid, rightSpace.grid).isRight)
    SamplingAlignment.exact(left, rightSpace) match
      case Left(_: ImageError.NonSpatialSamplingMismatch) => ()
      case other => fail(s"expected non-spatial sampling mismatch, found $other")

  test("series zip preserves the complete provider sampling mismatch"):
    val fixture = persistentFixture("provider-series-zip-axes")
    val leftAxis =
      right(
        Axis.regular(
          "time",
          AxisKind.Time,
          extent = 3,
          origin = 0.0,
          step = 1.0,
          unit = AxisUnit.Seconds
        )
      )
    val rightAxis =
      right(
        Axis.regular(
          "time",
          AxisKind.Time,
          extent = 4,
          origin = 10.0,
          step = 2.0,
          unit = AxisUnit.Seconds
        )
      )
    val leftAxes = right(NonSpatialAxes.from(Vector(leftAxis)))
    val rightAxes = right(NonSpatialAxes.from(Vector(rightAxis)))
    val leftSpace = SampleSpace.create(fixture.original, leftAxes)
    val rightSpace = SampleSpace.create(fixture.original, rightAxes)
    val left = scalarSeries(leftSpace)
    val rightSeries = scalarSeries(rightSpace)

    left.zipExact(rightSeries)(_ + _) match
      case Left(
            NeuroImageError.Image(
              ImageError.NonSpatialSamplingMismatch(expected, actual)
            )
          ) =>
        assertEquals(expected, leftAxes.records)
        assertEquals(actual, rightAxes.records)
      case other =>
        fail(s"expected complete sampling mismatch from series zip, found $other")

  test("exact and approximate provider congruence distinguish translation and reflection"):
    val frame = persistentFrame("provider-congruence-transform-frame")
    val original = right(Grid.in(frame)(shape, Affine.identity[D3]))
    val translated = right(Grid.in(frame)(shape, affine(translationX = 1e-6)))
    val reflected = right(Grid.in(frame)(shape, affine(reflectX = true)))

    assertEquals(
      Grid.exactCongruence(original, translated),
      Left(ImageGeometryError.GridsNotCongruent(0.0))
    )
    assertEquals(
      Grid.exactCongruence(original, reflected),
      Left(ImageGeometryError.GridsNotCongruent(0.0))
    )

    val approximate = right(
      Grid.approximateCongruence(original, translated, tolerance = 1e-5)
    )
    assert(approximate.left eq original)
    assert(approximate.right eq translated)
    assertEqualsDouble(approximate.tolerance, 1e-5, 0.0)
    assert(!approximate.exact)
    assert(
      Grid.approximateCongruence(original, translated, tolerance = 1e-7).isLeft
    )
    assert(
      Grid.approximateCongruence(original, reflected, tolerance = 1e-5).isLeft
    )

  test("approximate full sampling retains exact axes and provider endpoints"):
    val frame = persistentFrame("provider-congruence-approximate-sampling")
    val original = right(Grid.in(frame)(shape, Affine.identity[D3]))
    val translated = right(Grid.in(frame)(shape, affine(translationX = 1e-6)))
    val axes = right(NonSpatialAxes.from(Vector(ProviderAxes.time(3))))
    val left = SampleSpace.create(original, axes)
    val rightSpace = SampleSpace.create(translated, axes)
    val certificate = right(
      ApproximateSamplingCongruence.check(left, rightSpace, tolerance = 1e-5)
    )

    assert(certificate.left eq left)
    assert(certificate.right eq rightSpace)
    assertEqualsDouble(certificate.tolerance, 1e-5, 0.0)

    val differentAxes =
      SampleSpace.create(
        translated,
        right(NonSpatialAxes.from(Vector(ProviderAxes.time(4))))
      )
    ApproximateSamplingCongruence.check(left, differentAxes, tolerance = 1e-5) match
      case Left(_: ImageError.NonSpatialSamplingMismatch) => ()
      case other => fail(s"expected exact-axis mismatch, found $other")

  test("invalid congruence tolerances retain the provider error on both surfaces"):
    val frame = persistentFrame("provider-congruence-invalid-tolerance")
    val left = right(Grid.in(frame)(shape, Affine.identity[D3]))
    val rightGrid = right(Grid.in(frame)(shape, Affine.identity[D3]))
    val leftSpace = SampleSpace.create(left, NonSpatialAxes.empty)
    val rightSpace = SampleSpace.create(rightGrid, NonSpatialAxes.empty)

    Vector(-1.0, Double.NaN, Double.PositiveInfinity).foreach: tolerance =>
      Grid.approximateCongruence(left, rightGrid, tolerance) match
        case Left(ImageGeometryError.InvalidCongruenceTolerance(actual)) =>
          assert(sameDouble(actual, tolerance))
        case other => fail(s"expected invalid grid tolerance, found $other")

      ApproximateSamplingCongruence.check(leftSpace, rightSpace, tolerance) match
        case Left(
              ImageError.Geometry(
                ImageGeometryError.InvalidCongruenceTolerance(actual)
              )
            ) =>
          assert(sameDouble(actual, tolerance))
        case other => fail(s"expected invalid sampling tolerance, found $other")

  test("image arithmetic and volume concatenation reject another physical grid"):
    val expected = SampleSpaces(shape)
    val translated =
      SampleSpaces(shape, affine = Some(affine(translationX = 10.0)))
    val left = SomeScalarVolume.unsafeCopyFromCanonicalArray(
      PrimitiveBuffers.fillConst[Double](shape.product, 1.0),
      expected
    )
    val rightVolume = SomeScalarVolume.unsafeCopyFromCanonicalArray(
      PrimitiveBuffers.fillConst[Double](shape.product, 2.0),
      translated
    )

    intercept[IllegalArgumentException](left + rightVolume)
    intercept[IllegalArgumentException](left.concatenate(rightVolume))

  test("series mask extraction preserves same-owner coordinates"):
    val space = SampleSpaces(Vector(2, 3, 2, 4))
    val series = scalarSeries(space)
    val mask =
      Mask.fromIndices(
        space,
        Array(
          space.gridToIndex3D(0, 1, 1),
          space.gridToIndex3D(1, 2, 0)
        )
      )
    val selected = right(series.timeSeries(mask))

    assertEquals(selected.shape, ravel.Shape(2, 4))
    assertEqualsDouble(selected(0, 0), 110.0, 0.0)
    assertEqualsDouble(selected(0, 3), 113.0, 0.0)
    assertEqualsDouble(selected(1, 0), 1200.0, 0.0)
    assertEqualsDouble(selected(1, 3), 1203.0, 0.0)

  test("series mask extraction rejects translated, reflected, and foreign live owners"):
    val ownerFrame = persistentFrame("provider-mask-owner-frame")
    val ownerGridId = right(GridId.parse("provider-mask-owner-grid"))
    val expectedGrid =
      right(
        Grid.createPersistent(ownerGridId, ownerFrame)(
          Vector(2, 3, 2),
          Affine.identity[D3]
        )
      )
    val foreignGrid =
      right(
        Grid.createPersistent(ownerGridId, ownerFrame)(
          Vector(2, 3, 2),
          Affine.identity[D3]
        )
      )
    val expectedSpace = SampleSpace.create(expectedGrid, NonSpatialAxes.empty)
    val series = scalarSeries(expectedSpace.addDim(ProviderAxes.time(4)))
    val translatedGrid =
      right(
        Grid.in(ownerFrame)(
          Vector(2, 3, 2),
          affine(translationX = 10.0)
        )
      )
    val translated =
      Mask.fromIndices(
        SampleSpace.create(translatedGrid, NonSpatialAxes.empty),
        Array(0)
      )
    val reflectedGrid =
      right(
        Grid.in(ownerFrame)(
          Vector(2, 3, 2),
          affine(reflectX = true)
        )
      )
    val reflected =
      Mask.fromIndices(
        SampleSpace.create(reflectedGrid, NonSpatialAxes.empty),
        Array(0)
      )
    Vector(
      "translated" -> translated,
      "reflected" -> reflected
    ).foreach: (label, mask) =>
      series.timeSeries(mask) match
        case Left(
              NeuroImageError.Geometry(
                ImageGeometryError.GridsNotCongruent(0.0)
              )
            ) =>
          ()
        case other =>
          fail(s"expected $label provider geometry mismatch, found $other")

    val foreignSpace = SampleSpace.create(foreignGrid, NonSpatialAxes.empty)
    assert(expectedSpace.grid.samePersistentKeyAs(foreignSpace.grid))
    assert(!expectedSpace.grid.sameRuntimeOwnerAs(foreignSpace.grid))
    val foreignMask = Mask.fromIndices(foreignSpace, Array(0))
    series.timeSeries(foreignMask) match
      case Left(NeuroImageError.GridOwnerMismatch(expected, actual)) =>
        assertEquals(expected, expectedSpace.grid.persistentId)
        assertEquals(actual, foreignSpace.grid.persistentId)
      case other => fail(s"expected typed runtime-owner mismatch, found $other")

  private final case class PersistentFixture(
      original: Grid[? <: Frame[D3], D3],
      restored: Grid[? <: Frame[D3], D3]
  )

  private def persistentFixture(label: String): PersistentFixture =
    val frame = persistentFrame(s"$label-frame")
    val original =
      right(
        Grid.createPersistent(
          right(GridId.parse(s"$label-grid")),
          frame
        )(shape, Affine.identity[D3])
      )
    val registry = right(Grid.Registry.empty.register(original))
    val restored =
      right(Grid.restore(right(original.record), frame, registry)).grid
    PersistentFixture(original, restored)

  private def persistentFrame(label: String): Frame[D3] =
    right(
      Frame.persistentNamed[D3](
        right(FrameId.parse(label)),
        label,
        LengthUnit.Millimeter,
        CoordinateConvention.RAS
      )
    )

  private def affine(
      translationX: Double = 0.0,
      reflectX: Boolean = false
  ): Affine[D3] =
    val xScale = if reflectX then -1.0 else 1.0
    right(
      Affine.fromRowMajor[D3](
        Vector(
          xScale, 0.0, 0.0, translationX,
          0.0, 1.0, 0.0, 0.0,
          0.0, 0.0, 1.0, 0.0,
          0.0, 0.0, 0.0, 1.0
        )
      )
    )


  private def scalarSeries(space: SomeSampleSpace): SomeScalarSeries[Double] =
    val spatialShape = space.spatialDims
    val time = space.dims(3)
    val data =
      ravel.NDArray.tabulate[Double](
        spatialShape(0),
        spatialShape(1),
        spatialShape(2),
        time
      ):
        (x, y, z, t) => 1000.0 * x + 100.0 * y + 10.0 * z + t
    SomeScalarSeries.unsafeFromRavel(data, space, "mask-series-oracle")

  private def sameDouble(left: Double, right: Double): Boolean =
    (left.isNaN && right.isNaN) || left == right

  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)
