package scalafim.image

import cats.instances.option.given
import image4s.Categorical
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.Grid
import ravel.DType.given
import ravel.NDArray
import spire.std.int.given

class OwnerPreservingVolumeSuite extends munit.FunSuite:
  private val frame =
    right(Frame.named[D3]("owner-preserving-volume-suite"))

  private val grid =
    right(
      Grid.in(frame)(
        Vector(2, 2, 1),
        Affine.identity[D3]
      )
    )

  private val space =
    SampleSpace.create(grid, NonSpatialAxes.empty)

  private def volume: LabelVolume[space.type, Int] =
    right(
      NeuroVolume.categorical(
        space,
        NDArray.fromSeq(ravel.Shape(2, 2, 1), Vector(-1, 0, 2, 3))
      )
    )

  test("typed unary operations preserve values and the exact owner"):
    val source = volume
    val mapped: LabelVolume[space.type, Int] =
      source.mapValues[Int, Categorical](_ + 10)
    val sameSemantics: LabelVolume[space.type, Int] =
      source.mapValues(_ + 1)
    val alias: LabelVolume[space.type, Int] =
      source.map[Int, Categorical](_ * 2)
    val located: LabelVolume[space.type, Int] =
      source.mapVoxels[Int, Categorical]: (voxel, value) =>
        value + voxel.x + 10 * voxel.y
    val logical: MaskVolume[space.type] = source.asLogical
    val positive: MaskVolume[space.type] = source.asMask
    val traversed: Option[LabelVolume[space.type, Int]] =
      source.traverseValues[Option, Int, Categorical](value => Some(value - 1))
    val materialized: LabelVolume[space.type, Int] =
      source.materializedCanonical

    val outputs =
      Vector(
        mapped.sampled,
        sameSemantics.sampled,
        alias.sampled,
        located.sampled,
        logical.sampled,
        positive.sampled,
        traversed.get.sampled,
        materialized.sampled
      )
    outputs.foreach: sampled =>
      assert(sampled.sampleSpace.asInstanceOf[AnyRef] eq space)

    assertEquals(mapped.sampled.data.iterator.toVector, Vector(9, 10, 12, 13))
    assertEquals(sameSemantics.sampled.data.iterator.toVector, Vector(0, 1, 3, 4))
    assertEquals(alias.sampled.data.iterator.toVector, Vector(-2, 0, 4, 6))
    assertEquals(located.sampled.data.iterator.toVector, Vector(-1, 10, 3, 14))
    assertEquals(logical.sampled.data.iterator.toVector, Vector(true, false, true, true))
    assertEquals(positive.sampled.data.iterator.toVector, Vector(false, false, true, true))
    assertEquals(traversed.get.sampled.data.iterator.toVector, Vector(-2, -1, 1, 2))

  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)
