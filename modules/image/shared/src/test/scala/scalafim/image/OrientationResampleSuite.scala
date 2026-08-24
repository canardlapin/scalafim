package scalafim.image

import SampleSpaces.*

import gale.linalg.DMat
import image4s.Axis
import image4s.AxisCoordinatesRecord
import image4s.AxisKind
import image4s.AxisUnit
import image4s.NonSpatialAxes
import Ops.*

class OrientationResampleSuite extends munit.FunSuite:

  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private val signedOrientations: Vector[Orientation3D] =
    val choices =
      for
        x <- Vector(AnatomicalAxis.L, AnatomicalAxis.R)
        y <- Vector(AnatomicalAxis.P, AnatomicalAxis.A)
        z <- Vector(AnatomicalAxis.I, AnatomicalAxis.S)
      yield Vector(x, y, z)
    choices.flatMap: axes =>
      axes.permutations.map(values => Orientation3D.unsafe(values(0), values(1), values(2))).toVector

  test("findAnatomy3D returns expected orientation from axis abbreviations") {
    val orient = Orientation.findAnatomy3D("L", "P", "I")
    assertEquals(orient.axes, Vector(AnatomicalAxis.L, AnatomicalAxis.P, AnatomicalAxis.I), clue = "")
  }

  test("Orientation3D rejects duplicate anatomical axes before reorientation") {
    val typed = Orientation3D.unsafe(AnatomicalAxis.R, AnatomicalAxis.A, AnatomicalAxis.S)

    assertEquals(typed.axes, Vector(AnatomicalAxis.R, AnatomicalAxis.A, AnatomicalAxis.S), clue = "")
    assert(Orientation3D.make(AnatomicalAxis.L, AnatomicalAxis.R, AnatomicalAxis.I).isLeft, clue = "duplicate x axes should be rejected")
    assert(Orientation.findAnatomy3DEither("bogus", "P", "I").isLeft, clue = "unknown abbreviations should be represented as errors")
  }

  test("all 48 signed anatomical permutations round-trip through their matrices") {
    assertEquals(signedOrientations.size, 48)
    assertEquals(signedOrientations.distinct.size, 48)
    signedOrientations.foreach: expected =>
      val matrix = Orientation.permMat3D(expected)
      val actual = Orientation.findAnatomyEither(matrix)
      assertEquals(actual, Right(expected), clue = expected.axes.map(_.abbrev).mkString)
  }

  test("findAnatomy recovers orientation from identity matrix") {
    val pmat = DMat.dense(
      3,
      3,
      Vector(
        Vector(1.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 1.0)
      ).flatten
    )
    val orient = Orientation.findAnatomy(pmat)
    assertEquals(orient, Orientation3D.LPI, clue = "")
  }

  test("findAnatomyEither reports degenerate matrices as typed errors") {
    val singular =
      DMat.dense(
        3,
        3,
        Vector(
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 0.0, 0.0),
          Vector(0.0, 0.0, 1.0)
        ).flatten
      )

    val result = Orientation.findAnatomyEither(singular)
    assert(result.isLeft, clue = "singular orientation matrices should not throw on checked path")
  }

  test("reorient updates SomeSampleSpace transform and axes") {
    val sp = SampleSpaces(
      dims = Vector(2, 2, 2),
      spacing = Some(Vector(2.0, 3.0, 4.0)),
      origin = Some(Vector(10.0, 20.0, 30.0))
    )
    val ras = Orientation.reorient(sp, Seq("R", "A", "S"))
    assertEquals(ras.orientation.axes, Vector(AnatomicalAxis.R, AnatomicalAxis.A, AnatomicalAxis.S), clue = "")
    assertEquals(ras.indexToCoord(Vector(1.0, 1.0, 1.0)), Vector(-12.0, -23.0, -34.0), clue = "")

    val typed = sp.reorient(Orientation3D.unsafe(AnatomicalAxis.R, AnatomicalAxis.A, AnatomicalAxis.S))
    assertEquals(typed.orientation, ras.orientation, clue = "")
    assertEquals(typed.affineD3, ras.affineD3, clue = "")
  }

  test("SomeSampleSpace infers axes from trans when axes are not provided") {
    val tx =
      ProviderSpaces.affine(
        Vector(
          Vector(-2.0, 0.0, 0.0, 10.0),
          Vector(0.0, -3.0, 0.0, 20.0),
          Vector(0.0, 0.0, -4.0, 30.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )

    val sp3 =
      SampleSpaces(
        dims = Vector(2, 2, 2),
        spacing = Some(Vector(2.0, 3.0, 4.0)),
        origin = Some(Vector(10.0, 20.0, 30.0)),
        affine = Some(tx)
      )
    assertEquals(sp3.orientation.axes, Vector(AnatomicalAxis.R, AnatomicalAxis.A, AnatomicalAxis.S), clue = "")

    val sp4 =
      SampleSpaces(
        dims = Vector(2, 2, 2, 5),
        spacing = Some(Vector(2.0, 3.0, 4.0)),
        origin = Some(Vector(10.0, 20.0, 30.0)),
        affine = Some(tx)
      )
    assertEquals(sp4.nonSpatialAxes.values.head.kind, AxisKind.Time, clue = "")
    assertEquals(sp4.orientation.axes, Vector(AnatomicalAxis.R, AnatomicalAxis.A, AnatomicalAxis.S), clue = "")
  }

  test("SampleSpaces retains provider axis records without ordinal reconstruction") {
    val regular = right(Axis.regular("time", AxisKind.Time, 4, 0.25, 1.5, AxisUnit.Seconds))
    val explicit = right(Axis.explicit("echo", AxisKind.Echo, Vector(1.2, 2.5, 4.0), AxisUnit.Milliseconds))
    val categorical = right(Axis.categorical("condition", AxisKind.Other, Vector("rest", "task")))
    val axes = right(NonSpatialAxes.from(Vector(regular, explicit, categorical)))

    val space = SampleSpaces(Vector(2, 2, 2, 4, 3, 2), axes = Some(axes))

    assert(space.nonSpatialAxes.values(0) eq regular)
    assert(space.nonSpatialAxes.values(1) eq explicit)
    assert(space.nonSpatialAxes.values(2) eq categorical)
    assertEquals(
      space.nonSpatialAxes.records.map(_.coordinates),
      Vector(
        AxisCoordinatesRecord.Regular(4, 0.25, 1.5, "s"),
        AxisCoordinatesRecord.Explicit(Vector(1.2, 2.5, 4.0), "ms"),
        AxisCoordinatesRecord.Categorical(Vector("rest", "task"))
      )
    )
  }

  test("dropDim removes only the selected provider axis and retains the survivor") {
    val regular = right(Axis.regular("time", AxisKind.Time, 4, 0.0, 2.0, AxisUnit.Seconds))
    val categorical = right(Axis.categorical("condition", AxisKind.Other, Vector("rest", "task")))
    val axes = right(NonSpatialAxes.from(Vector(regular, categorical)))
    val space = SampleSpaces(Vector(2, 2, 2, 4, 2), axes = Some(axes))

    val dropped = space.dropDim(3)

    assertEquals(dropped.dims, Vector(2, 2, 2, 2))
    assertEquals(dropped.nonSpatialAxes.records, Vector(categorical.record))
    assert(dropped.nonSpatialAxes.values.head eq categorical)
  }

  test("resampleTo maps method strings to internal interpolators") {
    val spSrc = SampleSpaces(Vector(4, 4, 4))
    val vol = SomeScalarVolume.unsafeCopyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](64)(_.toDouble), spSrc)
    val spTarg = SampleSpaces(Vector(3, 3, 3))

    val n = Resample.resampleTo(vol, spTarg, method = "nearest", engine = "internal")
    val l = Resample.resampleTo(vol, spTarg, method = "linear", engine = "internal")
    val c = Resample.resampleTo(vol, spTarg, method = "cubic", engine = "internal")

    assertEquals(n.space.dims, spTarg.dims, clue = "")
    assertEquals(l.space.dims, spTarg.dims, clue = "")
    assertEquals(c.space.dims, spTarg.dims, clue = "")

    val n2 = Resample.resampleTo(vol, spTarg, method = "nearest")
    assertEquals(n2.space.dims, spTarg.dims, clue = "")
  }

  test("resampleTo refuses unknown engine") {
    val spSrc = SampleSpaces(Vector(4, 4, 4))
    val vol = SomeScalarVolume.unsafeCopyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](64)(i => (i + 1).toDouble), spSrc)
    val spTarg = SampleSpaces(Vector(2, 2, 2))

    interceptMessage[IllegalArgumentException]("Only engine = 'internal'") {
      Resample.resampleTo(vol, spTarg, method = "nearest", engine = "RNiftyReg")
    }
  }

  test("resampleToEither reports parser failures without throwing") {
    val spSrc = SampleSpaces(Vector(2, 2, 2))
    val vol = SomeScalarVolume.unsafeCopyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](8)(_.toDouble), spSrc)
    val spTarg = SampleSpaces(Vector(2, 2, 2))

    val ok = Resample.resampleTo(vol, spTarg, method = Resample.Method.Nearest, engine = Resample.Engine.Internal)
    assertEquals(ok.space.dims, spTarg.dims, clue = "")

    val badEngine = Resample.resampleToEither(vol, spTarg, method = "nearest", engine = "RNiftyReg")
    badEngine match
      case Left(Resample.ResampleError.UnsupportedEngine(_)) => ()
      case other => fail(s"expected UnsupportedEngine, got $other")

    val badMethod = Resample.resampleToEither(vol, spTarg, method = "unknown")
    badMethod match
      case Left(Resample.ResampleError.UnknownMethod(_)) => ()
      case other => fail(s"expected UnknownMethod, got $other")
  }

  test("resampleTo accepts SomeNeuroVolume/SomeNeuroSeries targets (plus Ops syntax)") {
    val spSrc = SampleSpaces(Vector(2, 2, 2))
    val vol = SomeScalarVolume.unsafeCopyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](8)(_.toDouble), spSrc)

    val targVol = SomeScalarVolume.unsafeCopyFromCanonicalArray[Double](PrimitiveBuffers.fillConst[Double](27, 0.0), SampleSpaces(Vector(3, 3, 3)))
    val outVol = Resample.resampleTo(vol, targVol, method = Resample.Method.Nearest)
    assertEquals(outVol.space.dims, Vector(3, 3, 3), clue = "")

    val spVec = SampleSpaces(Vector(2, 2, 2, 3))
    val vec = SomeScalarSeries.unsafeCopyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](2 * 2 * 2 * 3)(_.toDouble), spVec)

    val targVec = SomeScalarSeries.unsafeCopyFromCanonicalArray[Double](PrimitiveBuffers.fillConst[Double](3 * 3 * 3 * 2, 0.0), SampleSpaces(Vector(3, 3, 3, 2)))
    val outVec = Resample.resampleTo(vec, targVec, method = Resample.Method.Linear)
    assertEquals(outVec.space.dims, Vector(3, 3, 3, 3), clue = "")

    val outVec2 = vec.resampleTo(targVec, "linear")
    assertEquals(outVec2.space.dims, Vector(3, 3, 3, 3), clue = "")
  }
