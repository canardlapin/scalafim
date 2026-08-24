package scalafim.image

import SampleSpaces.*

import image4s.Axis
import image4s.AxisKind
import image4s.ImageMetadata
import locus4s.DomainRegistry
import locus4s.Selection
import ravel.NDArray
import ravel.Shape
import spire.std.double.given

class SubVectorSyntaxSuite extends munit.FunSuite:

  test("SomeNeuroSeries apply overloads select one volume or a time subset") {
    val sp = SampleSpaces(Vector(2, 1, 1, 4))
    val vec = SomeScalarSeries.unsafeCopyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](8)(_.toDouble), sp)

    val v2 = vec(2)
    assertEquals(v2.space.dims, Vector(2, 1, 1), clue = "")
    assertEquals(Vector.tabulate(v2.copyToCanonicalArray.length)(i => v2.copyToCanonicalArray(i)), Vector(2.0, 6.0), clue = "")

    val sub = right(vec.selectTimes(1 to 3))
    assertEquals(sub.space.dims, Vector(2, 1, 1, 3), clue = "")
    assertEquals(Vector.tabulate(sub.copyToCanonicalArray.length)(i => sub.copyToCanonicalArray(i)), Vector(1.0, 2.0, 3.0, 5.0, 6.0, 7.0), clue = "")
  }

  test("selected series use exact support and contiguous position-time rows") {
    val spatial = SampleSpaces(Vector(3, 1, 1))
    val packed =
      GridDomain
        .register(
          ProviderSpaces.grid(spatial),
          "sub-vector selected series",
          DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain = packed.value
    val selection =
      Selection.fromOrdinals(domain.space, Vector(0, 2)).toOption.get
    val time = Axis.create("time", 4, AxisKind.Time).toOption.get
    val selected =
      SelectedSeries
        .continuous(
          domain,
          selection,
          time,
          NDArray.fromSeq(
            Shape(2, 4),
            Vector(1.0, 2.0, 3.0, 4.0, 10.0, 20.0, 30.0, 40.0)
          ),
          ImageMetadata("selected")
        )
        .toOption
        .get

    assertEquals(selected.selection.ordinals.toVector, Vector(0, 2), clue = "")
    assertEquals(selected.data.shape, Shape(2, 4), clue = "")
    assertEquals(
      selected.data.iterator.toVector,
      Vector(1.0, 2.0, 3.0, 4.0, 10.0, 20.0, 30.0, 40.0),
      clue = ""
    )
    val provider = selected.selected
    val first =
      provider.seriesAt(
        provider.selection.positions.indexAtValidatedOrdinal(0)
      )
    assert(first.isContiguous, clue = "")
    assertEquals(first.iterator.toVector, Vector(1.0, 2.0, 3.0, 4.0), clue = "")
  }

  test("NeuroSeriesSeq supports explicit time selection") {
    val sp = SampleSpaces(Vector(2, 2, 1, 3))
    val vec = SomeScalarSeries.unsafeCopyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](12)(_.toDouble), sp)
    val seq = NeuroSeriesSeq(
      Vector(
        right(vec.selectTimes(Seq(0, 1))),
        right(vec.selectTimes(Seq(2)))
      )
    )
    val seqSub = seq.selectTimes(Seq(1, 2))
    assertEquals(seqSub.frameCount, 2, clue = "")
    assertEquals(Vector.tabulate(seqSub.volumeAt(0).copyToCanonicalArray.length)(i => seqSub.volumeAt(0).copyToCanonicalArray(i)), Vector(1.0, 4.0, 7.0, 10.0), clue = "")
    assertEquals(Vector.tabulate(seqSub.volumeAt(1).copyToCanonicalArray.length)(i => seqSub.volumeAt(1).copyToCanonicalArray(i)), Vector(2.0, 5.0, 8.0, 11.0), clue = "")
  }

  test("NeuroSeriesSeq time selection preserves requested cross-block order and duplicates") {
    val sp = SampleSpaces(Vector(1, 1, 1, 4))
    val vec = SomeScalarSeries.unsafeCopyFromCanonicalArray[Double](
      Array(10.0, 20.0, 30.0, 40.0),
      sp
    )
    val seq = NeuroSeriesSeq(
      Vector(
        right(vec.selectTimes(Seq(0, 1))),
        right(vec.selectTimes(Seq(2, 3)))
      )
    )

    val reverse = seq.selectTimes(Seq(3, 0))
    val interleaved = seq.selectTimes(Seq(0, 3, 1, 2))
    val duplicated = seq.selectTimes(Seq(3, 0, 3))

    assertEquals(
      Vector.tabulate(reverse.frameCount)(time => reverse.volumeAt(time)(0, 0, 0)),
      Vector(40.0, 10.0),
      clue = ""
    )
    assertEquals(
      Vector.tabulate(interleaved.frameCount)(time => interleaved.volumeAt(time)(0, 0, 0)),
      Vector(10.0, 40.0, 20.0, 30.0),
      clue = ""
    )
    assertEquals(
      Vector.tabulate(duplicated.frameCount)(time => duplicated.volumeAt(time)(0, 0, 0)),
      Vector(40.0, 10.0, 40.0),
      clue = ""
    )
  }

  test("NeuroSeriesSeq selection agrees with the materialized-series oracle") {
    val sp = SampleSpaces(Vector(1, 1, 1, 4))
    val materialized = SomeScalarSeries.unsafeCopyFromCanonicalArray[Double](
      Array(10.0, 20.0, 30.0, 40.0),
      sp
    )
    val seq = NeuroSeriesSeq(
      Vector(
        right(materialized.selectTimes(Seq(0, 1))),
        right(materialized.selectTimes(Seq(2, 3)))
      )
    )
    val selections =
      (1 to 4).toVector.flatMap: length =>
        Vector.fill(length)(0 until seq.frameCount).foldLeft(Vector(Vector.empty[Int])):
          (prefixes, choices) =>
            prefixes.flatMap(prefix => choices.map(prefix :+ _))

    selections.foreach: indices =>
      val expected = right(materialized.selectTimes(indices))
      val actual = seq.selectTimes(indices)
      val expectedValues =
        Vector.tabulate(indices.length)(time => expected(time)(0, 0, 0))
      val actualValues =
        Vector.tabulate(indices.length)(time => actual.volumeAt(time)(0, 0, 0))
      assertEquals(actualValues, expectedValues, clue = indices)
  }

  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)
