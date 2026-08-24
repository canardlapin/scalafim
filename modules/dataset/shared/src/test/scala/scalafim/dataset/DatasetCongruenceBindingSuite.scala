package scalafim.dataset

import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.FrameId
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.GridId
import image4s.geometry.LengthUnit
import scalafim.image.Mask

class DatasetCongruenceBindingSuite extends munit.FunSuite:

  test("matching provider congruence admits its exact mask endpoint"):
    val frame = persistentFrame("dataset-binding-matching-frame")
    val left = persistentGrid("dataset-binding-matching-left", frame, Affine.identity[D3])
    val rightGrid = persistentGrid("dataset-binding-matching-right", frame, shifted(5e-7))
    val leftSpace = SampleSpace.create(left, NonSpatialAxes.empty)
    val rightSpace = SampleSpace.create(rightGrid, NonSpatialAxes.empty)
    val shape = DatasetShape.unsafe(leftSpace, timepoints = 2)
    val mask = Mask.fromIndices(rightSpace, Array(0, 3))
    val congruence = right(Grid.approximateCongruence(left, rightGrid, 1e-6))

    val admitted = VoxelDomain.fromMask(mask, shape, congruence)

    assertEquals(admitted.map(_.indices), Right(Vector(0, 3)))
    assert(congruence.left eq left)
    assert(congruence.right eq rightGrid)

  test("a congruence cannot be substituted, reversed, or paired with swapped operands"):
    val frame = persistentFrame("dataset-binding-substitution-frame")
    val left = persistentGrid("dataset-binding-substitution-left", frame, Affine.identity[D3])
    val rightGrid = persistentGrid("dataset-binding-substitution-right", frame, shifted(5e-7))
    val third = persistentGrid("dataset-binding-substitution-third", frame, shifted(5e-7))
    val leftSpace = SampleSpace.create(left, NonSpatialAxes.empty)
    val rightSpace = SampleSpace.create(rightGrid, NonSpatialAxes.empty)
    val thirdSpace = SampleSpace.create(third, NonSpatialAxes.empty)
    val leftShape = DatasetShape.unsafe(leftSpace, timepoints = 2)
    val rightShape = DatasetShape.unsafe(rightSpace, timepoints = 2)
    val rightMask = Mask.fromIndices(rightSpace, Array(0))
    val thirdMask = Mask.fromIndices(thirdSpace, Array(0))
    val leftMask = Mask.fromIndices(leftSpace, Array(0))
    val congruence = right(Grid.approximateCongruence(left, rightGrid, 1e-6))
    val reversed = right(Grid.approximateCongruence(rightGrid, left, 1e-6))

    assertEquals(
      VoxelDomain.fromMask(thirdMask, leftShape, congruence),
      Left(DatasetError.CongruenceEndpointMismatch(CongruenceEndpoint.Right))
    )
    assertEquals(
      VoxelDomain.fromMask(rightMask, leftShape, reversed),
      Left(DatasetError.CongruenceEndpointMismatch(CongruenceEndpoint.Left))
    )
    assertEquals(
      VoxelDomain.fromMask(leftMask, rightShape, congruence),
      Left(DatasetError.CongruenceEndpointMismatch(CongruenceEndpoint.Left))
    )

  test("approximate congruence admits aligned persistent owners and rejects ephemeral substitutes"):
    val leftFrame = persistentFrame("dataset-binding-restored-frame")
    val rightFrame = persistentFrame("dataset-binding-restored-frame")
    val left = persistentGrid("dataset-binding-restored-left", leftFrame, Affine.identity[D3])
    val rightGrid = persistentGrid("dataset-binding-restored-right", rightFrame, shifted(5e-7))
    val leftSpace = SampleSpace.create(left, NonSpatialAxes.empty)
    val rightSpace = SampleSpace.create(rightGrid, NonSpatialAxes.empty)
    val shape = DatasetShape.unsafe(leftSpace, timepoints = 2)
    val mask = Mask.fromIndices(rightSpace, Array(0))
    val persistentCongruence = right(Grid.approximateCongruence(left, rightGrid, 1e-6))

    assert(leftFrame ne rightFrame)
    assert(VoxelDomain.fromMask(mask, shape, persistentCongruence).isRight)

    val ephemeralLeft = right(Frame.named[D3]("dataset-binding-ephemeral"))
    val ephemeralRight = right(Frame.named[D3]("dataset-binding-ephemeral"))
    val ephemeralLeftGrid = right(Grid.in(ephemeralLeft)(Vector(2, 2, 1), Affine.identity[D3]))
    val ephemeralRightGrid = right(Grid.in(ephemeralRight)(Vector(2, 2, 1), shifted(5e-7)))

    Grid.approximateCongruence(ephemeralLeftGrid, ephemeralRightGrid, 1e-6) match
      case Left(GeometryError.EphemeralFrameMismatch) => ()
      case other => fail(s"expected EphemeralFrameMismatch, found $other")

  private def persistentFrame(id: String): Frame[D3] =
    right(
      Frame.persistentNamed[D3](
        right(FrameId.parse(id)),
        id,
        LengthUnit.Millimeter,
        CoordinateConvention.RAS
      )
    )

  private def persistentGrid(
      id: String,
      frame: Frame[D3],
      affine: Affine[D3]
  ): Grid[? <: Frame[D3], D3] =
    right(
      Grid.createPersistent(right(GridId.parse(id)), frame)(
        Vector(2, 2, 1),
        affine
      )
    )

  private def shifted(offset: Double): Affine[D3] =
    right(
      Affine.fromRowMajor[D3](
        Vector(
          1.0, 0.0, 0.0, offset,
          0.0, 1.0, 0.0, 0.0,
          0.0, 0.0, 1.0, 0.0,
          0.0, 0.0, 0.0, 1.0
        )
      )
    )

  private def right[E, A](value: Either[E, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(s"expected Right, found Left($error)")
