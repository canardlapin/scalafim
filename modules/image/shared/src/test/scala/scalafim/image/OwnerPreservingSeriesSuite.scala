package scalafim.image

import image4s.Axis
import image4s.AxisKind
import image4s.Categorical
import image4s.ImageMetadata
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.Grid
import ravel.DType.given
import ravel.NDArray

class OwnerPreservingSeriesSuite extends munit.FunSuite:
  private val frame =
    right(Frame.named[D3]("owner-preserving-series-suite"))

  private val grid =
    right(
      Grid.in(frame)(
        Vector(2, 1, 1),
        Affine.identity[D3]
      )
    )

  private val time =
    right(Axis.ordinal("time", AxisKind.Time, 3))

  private val space =
    SampleSpace.create(grid, right(NonSpatialAxes.from(Vector(time))))

  private def series: LabelSeries[space.type, Int] =
    right(
      NeuroSeries.categorical(
        space,
        NDArray.fromSeq(ravel.Shape(2, 1, 1, 3), Vector(0, 1, 2, 3, 4, 5)),
        ImageMetadata.named("source")
      )
    )

  test("typed shape-preserving operations preserve values and the exact owner"):
    val source = series
    val mapped: LabelSeries[space.type, Int] =
      source.mapValues[Int, Categorical](_ + 10)
    val alias: LabelSeries[space.type, Int] =
      source.map[Int, Categorical](_ * 2)
    val voxels: LabelSeries[space.type, Int] =
      source.mapVoxels[Int, Categorical]: (voxel, value) =>
        value + 100 * voxel.x
    val samples: LabelSeries[space.type, Int] =
      source.mapSamples[Int, Categorical]: (voxel, time, value) =>
        value + 100 * voxel.x + 10 * time
    val materialized: LabelSeries[space.type, Int] =
      source.materializedCanonical
    val metadata = ImageMetadata.named("updated")
    val relabeled: LabelSeries[space.type, Int] =
      source.withImageMetadata(metadata)

    val outputs =
      Vector(
        mapped.sampled,
        alias.sampled,
        voxels.sampled,
        samples.sampled,
        materialized.sampled,
        relabeled.sampled
      )
    outputs.foreach: sampled =>
      assert(sampled.sampleSpace.asInstanceOf[AnyRef] eq space)

    assertEquals(mapped.sampled.data.iterator.toVector, Vector(10, 11, 12, 13, 14, 15))
    assertEquals(alias.sampled.data.iterator.toVector, Vector(0, 2, 4, 6, 8, 10))
    assertEquals(voxels.sampled.data.iterator.toVector, Vector(0, 1, 2, 103, 104, 105))
    assertEquals(samples.sampled.data.iterator.toVector, Vector(0, 11, 22, 103, 114, 125))
    assertEquals(relabeled.sampled.metadata, metadata)

  test("provider and owner-erased crossings are explicit and zero-wrapper"):
    val source = series
    val sampled = source.sampled
    val erased = SomeNeuroSeries.eraseSpace(source)

    assert(sampled.asInstanceOf[AnyRef] eq erased.sampled.asInstanceOf[AnyRef])
    assert(erased.sampled.sampleSpace.asInstanceOf[AnyRef] eq space)

  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)
