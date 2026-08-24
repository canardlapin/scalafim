package scalafim.image

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

    val sub = vec.selectTimes(1 to 3)
    assertEquals(sub.space.dims, Vector(2, 1, 1, 3), clue = "")
    assertEquals(Vector.tabulate(sub.copyToCanonicalArray.length)(i => sub.copyToCanonicalArray(i)), Vector(1.0, 2.0, 3.0, 5.0, 6.0, 7.0), clue = "")
  }

  test("selected series use exact support and contiguous position-time rows") {
    val spatial = SampleSpaces(Vector(3, 1, 1))
    val packed =
      GridDomain
        .register(
          VolumeSpace(spatial).sampleSpace.grid,
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
    val seq = NeuroSeriesSeq(Vector(vec.selectTimes(Seq(0, 1)), vec.selectTimes(Seq(2))))
    val seqSub = seq.selectTimes(Seq(1, 2))
    assertEquals(seqSub.frameCount, 2, clue = "")
    assertEquals(Vector.tabulate(seqSub.volumeAt(0).copyToCanonicalArray.length)(i => seqSub.volumeAt(0).copyToCanonicalArray(i)), Vector(1.0, 4.0, 7.0, 10.0), clue = "")
    assertEquals(Vector.tabulate(seqSub.volumeAt(1).copyToCanonicalArray.length)(i => seqSub.volumeAt(1).copyToCanonicalArray(i)), Vector(2.0, 5.0, 8.0, 11.0), clue = "")
  }
