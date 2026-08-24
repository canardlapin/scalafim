package scalafim.image

import Ops.*

class OrientationResampleSuite extends munit.FunSuite:

  test("findAnatomy3D returns expected orientation from axis abbreviations") {
    val orient = Orientation.findAnatomy3D("L", "P", "I")
    assertEquals(orient.ndim, 3, clue = "")
    assertEquals(orient(0), Axis.LeftRight, clue = "")
    assertEquals(orient(1), Axis.PosteriorAnterior, clue = "")
    assertEquals(orient(2), Axis.InferiorSuperior, clue = "")
  }

  test("Orientation3D rejects duplicate anatomical axes before reorientation") {
    val typed = Orientation3D.unsafe(AnatomicalAxis.R, AnatomicalAxis.A, AnatomicalAxis.S)
    val orient = Orientation.findAnatomy3D(typed)

    assertEquals(orient.axes, Vector(Axis.RightLeft, Axis.AnteriorPosterior, Axis.SuperiorInferior), clue = "")
    assert(Orientation3D.make(AnatomicalAxis.L, AnatomicalAxis.R, AnatomicalAxis.I).isLeft, clue = "duplicate x axes should be rejected")
    assert(Orientation.findAnatomy3DEither("bogus", "P", "I").isLeft, clue = "unknown abbreviations should be represented as errors")
  }

  test("findAnatomy recovers orientation from identity matrix") {
    val pmat = DMat.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 1.0)
      )
    )
    val orient = Orientation.findAnatomy(pmat)
    assertEquals(orient(0), Axis.LeftRight, clue = "")
    assertEquals(orient(1), Axis.PosteriorAnterior, clue = "")
    assertEquals(orient(2), Axis.InferiorSuperior, clue = "")
  }

  test("findAnatomyEither reports degenerate matrices as typed errors") {
    val singular =
      DMat.fromRows(
        Vector(
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 0.0, 0.0),
          Vector(0.0, 0.0, 1.0)
        )
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
    assertEquals(ras.axes.spatialAxes, Vector(Axis.RightLeft, Axis.AnteriorPosterior, Axis.SuperiorInferior), clue = "")
    assertEquals(ras.indexToCoord(Vector(1.0, 1.0, 1.0)), Vector(-12.0, -23.0, -34.0), clue = "")

    val typed = sp.reorient(Orientation3D.unsafe(AnatomicalAxis.R, AnatomicalAxis.A, AnatomicalAxis.S))
    assertEquals(typed.axes.spatialAxes, ras.axes.spatialAxes, clue = "")
    assertEquals(typed.trans, ras.trans, clue = "")
  }

  test("SomeSampleSpace infers axes from trans when axes are not provided") {
    val tx =
      DMat.fromRows(
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
        trans = Some(tx)
      )
    assertEquals(sp3.axes.spatialAxes, Vector(Axis.RightLeft, Axis.AnteriorPosterior, Axis.SuperiorInferior), clue = "")

    val sp4 =
      SampleSpaces(
        dims = Vector(2, 2, 2, 5),
        spacing = Some(Vector(2.0, 3.0, 4.0)),
        origin = Some(Vector(10.0, 20.0, 30.0)),
        trans = Some(tx)
      )
    assertEquals(sp4.axes(3), Axis.Time, clue = "")
    assertEquals(sp4.axes.spatialAxes, Vector(Axis.RightLeft, Axis.AnteriorPosterior, Axis.SuperiorInferior), clue = "")
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
