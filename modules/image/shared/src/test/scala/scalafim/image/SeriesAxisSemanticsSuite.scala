package scalafim.image

import image4s.Axis
import image4s.AxisConcatenationPolicy
import image4s.AxisCoordinate
import image4s.AxisCoordinatesRecord
import image4s.AxisKind
import image4s.AxisUnit
import image4s.Categorical
import image4s.ImageError
import image4s.ImageMetadata
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.Grid
import ravel.DType.given
import ravel.NDArray
import ravel.Rank
import ravel.Shape

class SeriesAxisSemanticsSuite extends munit.FunSuite:
  private val frame =
    right(Frame.named[D3]("series-axis-semantics-suite"))

  private val grid =
    right(
      Grid.in(frame)(
        Vector(2, 1, 1),
        Affine.identity[D3]
      )
    )

  private def seriesFor(
      axis: Axis,
      offset: Int = 0,
      metadata: ImageMetadata = ImageMetadata.named("source"),
      targetGrid: Grid[? <: Frame[D3], D3] = grid
  ): SomeLabelSeries[Int] =
    val axes = right(NonSpatialAxes.from(Vector(axis)))
    val space = SampleSpace.create(targetGrid, axes)
    val shape = targetGrid.shape
    val data =
      NDArray.tabulate[Int](shape(0), shape(1), shape(2), axis.extent):
        (x, _, _, time) =>
          offset + 100 * x + time
    right(SomeLabelSeries.fromRavel(data, space, metadata))

  test("time selection delegates every coordinate model to Axis.select"):
    val axes =
      Vector(
        right(Axis.ordinal("time", AxisKind.Time, 4)),
        right(
          Axis.regular(
            "time",
            AxisKind.Time,
            4,
            0.5,
            1.5,
            AxisUnit.Seconds
          )
        ),
        right(
          Axis.explicit(
            "time",
            AxisKind.Time,
            Vector(0.0, 3.0, 7.0, 12.0),
            AxisUnit.Milliseconds
          )
        ),
        right(
          Axis.categorical(
            "time",
            AxisKind.Time,
            Vector("rest", "cue", "task", "probe")
          )
        )
      )
    val selections =
      Vector(
        Vector(0, 1, 2, 3),
        Vector(0, 2),
        Vector(3, 2, 1, 0),
        Vector(2, 0, 2)
      )

    axes.foreach: axis =>
      val source = seriesFor(axis)
      selections.foreach: indices =>
        val expectedAxis = right(axis.select(indices))
        val selected = right(source.selectTimes(indices))
        val actualAxis = selected.sampled.nonSpatialAxes.values.head

        assertEquals(actualAxis.record, expectedAxis.record, clue = (axis.record, indices))
        val expectedValues =
          Vector.tabulate(2): x =>
            indices.map(time => 100 * x + time)
        val actualValues =
          Vector.tabulate(2): x =>
            Vector.tabulate(indices.size): time =>
              selected(x, 0, 0, time)
        assertEquals(actualValues, expectedValues, clue = (axis.record, indices))

  test("time selection reports provider empty and bounds errors unchanged"):
    val axis = right(Axis.ordinal("time", AxisKind.Time, 4))
    val source = seriesFor(axis)

    assertEquals(
      source.selectTimes(Vector.empty),
      Left(NeuroImageError.Image(ImageError.EmptyAxisSelection(axis.name)))
    )
    assertEquals(
      source.selectTimes(Vector(0, 4)),
      Left(
        NeuroImageError.Image(
          ImageError.NonSpatialIndexOutOfBounds(axis.name, 4, 4)
        )
      )
    )

  test("continuous regular concatenation preserves provider coordinates and values"):
    val leftAxis =
      right(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          0.0,
          2.0,
          AxisUnit.Seconds
        )
      )
    val rightAxis =
      right(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          4.0,
          2.0,
          AxisUnit.Seconds
        )
      )
    val result =
      right(
        seriesFor(leftAxis, 0)
          .concatenate(seriesFor(rightAxis, 10))(
            AxisConcatenationPolicy.RequireContinuous,
            SeriesMetadataPolicy.RequireEqual
          )
      )

    assertEquals(
      result.sampled.nonSpatialAxes.values.head.record.coordinates,
      AxisCoordinatesRecord.Regular(4, 0.0, 2.0, AxisUnit.Seconds.id)
    )
    assertEquals(
      Vector.tabulate(4)(time => result(0, 0, 0, time)),
      Vector(0, 1, 10, 11)
    )
    assertEquals(result.sampled.metadata, ImageMetadata.named("source"))

  test("series concatenation exposes provider unit, kind, and discontinuity errors"):
    val leftAxis =
      right(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          0.0,
          1.0,
          AxisUnit.Seconds
        )
      )
    val wrongUnit =
      right(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          2.0,
          1.0,
          AxisUnit.Milliseconds
        )
      )
    val wrongKind =
      right(
        Axis.regular(
          "time",
          AxisKind.Echo,
          2,
          2.0,
          1.0,
          AxisUnit.Seconds
        )
      )
    val gap =
      right(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          3.0,
          1.0,
          AxisUnit.Seconds
        )
      )
    val left = seriesFor(leftAxis)

    assertEquals(
      left.concatenate(seriesFor(wrongUnit))(
        AxisConcatenationPolicy.RequireContinuous,
        SeriesMetadataPolicy.RequireEqual
      ),
      Left(
        NeuroImageError.Image(
          ImageError.AxisConcatenationUnitMismatch(
            leftAxis.name,
            AxisUnit.Seconds,
            AxisUnit.Milliseconds
          )
        )
      )
    )
    assertEquals(
      left.concatenate(unsafeSeriesFor(wrongKind))(
        AxisConcatenationPolicy.RequireContinuous,
        SeriesMetadataPolicy.RequireEqual
      ),
      Left(
        NeuroImageError.Image(
          ImageError.AxisConcatenationKindMismatch(
            AxisKind.Time,
            AxisKind.Echo
          )
        )
      )
    )
    val expected = AxisCoordinate.Numeric(2.0, AxisUnit.Seconds)
    val actual = right(gap.coordinateAt(0))
    assertEquals(
      left.concatenate(seriesFor(gap))(
        AxisConcatenationPolicy.RequireContinuous,
        SeriesMetadataPolicy.RequireEqual
      ),
      Left(
        NeuroImageError.Image(
          ImageError.AxisConcatenationDiscontinuity(
            leftAxis.name,
            expected,
            actual
          )
        )
      )
    )

  test("series concatenation rejects grid and metadata mismatches through typed errors"):
    val leftAxis =
      right(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          0.0,
          1.0,
          AxisUnit.Seconds
        )
      )
    val rightAxis =
      right(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          2.0,
          1.0,
          AxisUnit.Seconds
        )
      )
    val leftMetadata = ImageMetadata.named("left")
    val rightMetadata = ImageMetadata.named("right")
    val left = seriesFor(leftAxis, metadata = leftMetadata)
    val mismatchedMetadata =
      seriesFor(rightAxis, metadata = rightMetadata)

    assertEquals(
      left.concatenate(mismatchedMetadata)(
        AxisConcatenationPolicy.RequireContinuous,
        SeriesMetadataPolicy.RequireEqual
      ),
      Left(
        NeuroImageError.ConcatenationMetadataMismatch(
          leftMetadata,
          rightMetadata
        )
      )
    )
    val selectedLeft =
      right(
        left.concatenate(mismatchedMetadata)(
          AxisConcatenationPolicy.RequireContinuous,
          SeriesMetadataPolicy.UseLeft
        )
      )
    assertEquals(selectedLeft.sampled.metadata, leftMetadata)

    val otherGrid =
      right(
        Grid.in(frame)(
          Vector(1, 1, 1),
          Affine.identity[D3]
        )
      )
    val wrongGrid = seriesFor(rightAxis, targetGrid = otherGrid)
    left.concatenate(wrongGrid)(
      AxisConcatenationPolicy.RequireContinuous,
      SeriesMetadataPolicy.UseLeft
    ) match
      case Left(
            NeuroImageError.Geometry(
              image4s.geometry.GeometryError.GridsNotCongruent(0.0)
            )
          ) =>
        ()
      case other =>
        fail(s"expected typed grid mismatch, found $other")

  test("axis concatenation exposes overlap and explicit append semantics"):
    val leftRegular =
      right(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          0.0,
          1.0,
          AxisUnit.Seconds
        )
      )
    val overlap =
      right(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          1.0,
          1.0,
          AxisUnit.Seconds
        )
      )
    assertEquals(
      seriesFor(leftRegular).concatenate(seriesFor(overlap))(
        AxisConcatenationPolicy.RequireContinuous,
        SeriesMetadataPolicy.RequireEqual
      ),
      Left(
        NeuroImageError.Image(
          ImageError.AxisConcatenationOverlap(
            leftRegular.name,
            AxisCoordinate.Numeric(1.0, AxisUnit.Seconds),
            AxisCoordinate.Numeric(1.0, AxisUnit.Seconds)
          )
        )
      )
    )

    val leftExplicit =
      right(
        Axis.explicit(
          "time",
          AxisKind.Time,
          Vector(0.0, 2.0),
          AxisUnit.Seconds
        )
      )
    val rightExplicit =
      right(
        Axis.explicit(
          "time",
          AxisKind.Time,
          Vector(1.0, 5.0),
          AxisUnit.Seconds
        )
      )
    val appended =
      right(
        seriesFor(leftExplicit, 0).concatenate(seriesFor(rightExplicit, 10))(
          AxisConcatenationPolicy.AppendDeclaredCoordinates,
          SeriesMetadataPolicy.RequireEqual
        )
      )
    assertEquals(
      appended.sampled.nonSpatialAxes.values.head.record.coordinates,
      AxisCoordinatesRecord.Explicit(
        Vector(0.0, 2.0, 1.0, 5.0),
        AxisUnit.Seconds.id
      )
    )
    assertEquals(
      Vector.tabulate(4)(time => appended(0, 0, 0, time)),
      Vector(0, 1, 10, 11)
    )

  test("series concatenation is associative for values and full axis records"):
    def regular(origin: Double): Axis =
      right(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          origin,
          1.0,
          AxisUnit.Seconds
        )
      )
    val first = seriesFor(regular(0.0), 0)
    val second = seriesFor(regular(2.0), 10)
    val third = seriesFor(regular(4.0), 20)
    val leftAssociated =
      right(
        right(
          first.concatenate(second)(
            AxisConcatenationPolicy.RequireContinuous,
            SeriesMetadataPolicy.RequireEqual
          )
        ).concatenate(third)(
          AxisConcatenationPolicy.RequireContinuous,
          SeriesMetadataPolicy.RequireEqual
        )
      )
    val direct =
      right(
        first.concatenate(second, third)(
          AxisConcatenationPolicy.RequireContinuous,
          SeriesMetadataPolicy.RequireEqual
        )
      )

    assertEquals(
      leftAssociated.sampled.nonSpatialAxes.records,
      direct.sampled.nonSpatialAxes.records
    )
    assertEquals(
      leftAssociated.sampled.data.iterator.toVector,
      direct.sampled.data.iterator.toVector
    )

  /** Test-only access to the trusted constructor proves provider errors are not
    * hidden even if an internal caller violates the Time-axis admission invariant.
    */
  private def unsafeSeriesFor(axis: Axis): SomeLabelSeries[Int] =
    val space = SampleSpace.create(grid, right(NonSpatialAxes.from(Vector(axis))))
    val sampled =
      right(
        Sampled.create[Int, Categorical, Rank[4]](
          space,
          NDArray.fill(Shape(2, 1, 1, axis.extent), 0),
          ImageMetadata.named("source")
        )
      )
    SomeNeuroSeries.eraseSpace(NeuroSeries.unsafeFromSampled(sampled))

  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)
