package scalafim.image

import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.FrameId
import image4s.geometry.GeometryError as ImageGeometryError
import image4s.geometry.Grid
import image4s.geometry.GridId
import image4s.geometry.LengthUnit
import ravel.NDArray

final class SliceCompatibilityParitySuite extends munit.FunSuite:

  private val shape = Vector(3, 3, 3)

  private final case class PlanRunner(
      name: String,
      sample: SomeScalarVolume[Double] => Either[SlicePlanError, SliceImage[Double]]
  )

  test("ordinary and mapped plans accept one persistent grid restored through its registry"):
    val frame = persistentFrame("slice-parity-restore-frame")
    val original =
      right(
        Grid.createPersistent(
          right(GridId.parse("slice-parity-restore-grid")),
          frame
        )(shape, Affine.identity[D3])
      )
    val registry = right(Grid.Registry.empty.register(original))
    val restored =
      right(Grid.restore(right(original.record), frame, registry)).grid
    val plans = runners(original)
    val originalVolume = volume(original)
    val restoredVolume = volume(restored)

    plans.foreach: plan =>
      val expected = right(plan.sample(originalVolume))
      val actual = right(plan.sample(restoredVolume))
      assertEquals(actual.values.shape, expected.values.shape, clue = plan.name)
      assertEquals(
        Vector.tabulate(actual.values.size)(actual.valueAtCanonicalOrdinal),
        Vector.tabulate(expected.values.size)(expected.valueAtCanonicalOrdinal),
        clue = plan.name
      )

  test("ordinary and mapped plans reject distinct ephemeral frames with the same provider cause"):
    val sourceFrame = right(Frame.named[D3]("slice-parity-ephemeral-source"))
    val foreignFrame = right(Frame.named[D3]("slice-parity-ephemeral-foreign"))
    val source = right(Grid.in(sourceFrame)(shape, Affine.identity[D3]))
    val foreign = right(Grid.in(foreignFrame)(shape, Affine.identity[D3]))

    assertParity(
      runners(source),
      volume(foreign),
      ImageGeometryError.EphemeralFrameMismatch
    )

  test("ordinary and mapped plans reject translated and reflected grids identically"):
    val frame = persistentFrame("slice-parity-transform-frame")
    val source = right(Grid.in(frame)(shape, Affine.identity[D3]))
    val translated = right(Grid.in(frame)(shape, affine(translationX = 2.0)))
    val reflected = right(Grid.in(frame)(shape, affine(reflectX = true)))
    val plans = runners(source)

    Vector(
      "translated" -> volume(translated),
      "reflected" -> volume(reflected)
    ).foreach: (label, candidate) =>
      assertParity(
        plans,
        candidate,
        ImageGeometryError.GridsNotCongruent(0.0),
        label
      )

  test("ordinary and mapped plans reject an unrelated third owner identically"):
    val sourceFrame = persistentFrame("slice-parity-third-source-frame")
    val thirdFrame = persistentFrame("slice-parity-third-foreign-frame")
    val source = right(Grid.in(sourceFrame)(shape, Affine.identity[D3]))
    val third = right(Grid.in(thirdFrame)(shape, Affine.identity[D3]))
    val sourceId = sourceFrame.persistentId.fold(fail("missing source frame id"))(identity)
    val thirdId = thirdFrame.persistentId.fold(fail("missing third frame id"))(identity)
    val expected =
      ImageGeometryError.FrameMismatch(
        sourceId,
        thirdId
      )

    assertParity(runners(source), volume(third), expected)

  private def runners(
      source: Grid[? <: Frame[D3], D3]
  ): Vector[PlanRunner] =
    val grid =
      SliceGrid.covering(
        source,
        SlicePlane.canonical(
          AnatomicalPlane.Axial,
          WorldPoint(0.0, 0.0, 1.0)
        ),
        PixelSpacing(1.0, 1.0)
      )
    val ordinary = SlicePlan.make(source, grid)
    val sourceGrid = GridSpec.fromGrid(source)
    val mapped =
      MappedSlicePlan.make(
        source,
        grid,
        SpatialPullbacks.worldAligned(sourceGrid, sourceGrid)
      ).fold(error => fail(error.message), identity)

    Vector(
      PlanRunner(
        "ordinary",
        candidate => ordinary.sample(candidate, SliceSampling.Linear())
      ),
      PlanRunner(
        "mapped",
        candidate => mapped.sample(candidate, SliceSampling.Linear())
      )
    )

  private def assertParity(
      plans: Vector[PlanRunner],
      candidate: SomeScalarVolume[Double],
      expected: ImageGeometryError,
      label: String = ""
  ): Unit =
    plans.foreach: plan =>
      plan.sample(candidate) match
        case Left(SlicePlanError.Geometry(actual)) =>
          assertEquals(actual, expected, clue = s"$label ${plan.name}")
        case other =>
          fail(s"$label ${plan.name}: expected provider geometry failure, found $other")

  private def volume(
      grid: Grid[? <: Frame[D3], D3]
  ): SomeScalarVolume[Double] =
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val data =
      NDArray.tabulate[Double](shape(0), shape(1), shape(2)):
        (x, y, z) => 100.0 * x + 10.0 * y + z
    SomeScalarVolume.unsafeFromRavel(data, space)

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

  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)
