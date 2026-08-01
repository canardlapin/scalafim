package scalafim.fmri.hrf

import scalafim.fmri.hrf.design.{Design, SamplingFrame}

/** Two runs that both begin at numerical time zero do not share a time line.
  *
  * `Design.regressorDesign` used to convolve the whole experiment on one
  * concatenated axis, so an HRF tail crossed the join.
  */
class RunBoundarySuite extends munit.FunSuite:

  private val sframe = SamplingFrame(blockLens = Seq(50, 50), tr = Seq(2.0))

  test("an event at the end of a run does not respond in the next run"):
    // Run length is 50 scans x 2 s = 100 s; the event is 2 s before the end.
    val m = Design.regressorDesign(
      onsets = Seq(98.0),
      fac = Seq("A"),
      block = Seq(0),
      sframe = sframe,
      hrf = Hrfs.SPMG1
    )
    assertEquals(m.rows, 100)
    assertEquals(m.cols, 1)
    val run2 = (50 until 100).map(r => math.abs(m(r, 0)))
    assertEqualsDouble(
      run2.max,
      0.0,
      0.0,
      s"run 1's HRF tail leaked into run 2 (peak ${run2.max})"
    )
    // The response is still there, truncated by the end of its own run.
    val run1 = (0 until 50).map(r => math.abs(m(r, 0)))
    assert(run1.max > 0.0, "the event produced no response at all in its own run")

  test("each run is rendered against its own local clock"):
    // The same local onset in each run must produce the same column segment.
    val m = Design.regressorDesign(
      onsets = Seq(10.0, 10.0),
      fac = Seq("A", "A"),
      block = Seq(0, 1),
      sframe = sframe,
      hrf = Hrfs.SPMG1
    )
    var r = 0
    while r < 50 do
      assertEqualsDouble(m(r, 0), m(r + 50, 0), 1e-12, s"run 1 and run 2 differ at within-run scan $r")
      r += 1
    assert((0 until 50).map(r => math.abs(m(r, 0))).max > 0.1)

  test("events are attributed to their own run only"):
    val both = Design.regressorDesign(
      onsets = Seq(10.0, 30.0),
      fac = Seq("A", "A"),
      block = Seq(0, 1),
      sframe = sframe,
      hrf = Hrfs.SPMG1
    )
    val firstOnly = Design.regressorDesign(
      onsets = Seq(10.0),
      fac = Seq("A"),
      block = Seq(0),
      sframe = sframe,
      hrf = Hrfs.SPMG1
    )
    // Run 1's rows must not depend on whether run 2 had events.
    var r = 0
    while r < 50 do
      assertEqualsDouble(both(r, 0), firstOnly(r, 0), 1e-12, s"run 2's events changed run 1 at scan $r")
      r += 1

  test("columns stay aligned when a level is absent from a run"):
    val m = Design.regressorDesign(
      onsets = Seq(10.0, 20.0, 12.0),
      fac = Seq("A", "B", "B"),
      block = Seq(0, 0, 1),
      sframe = sframe,
      hrf = Hrfs.SPMG1
    )
    assertEquals(m.cols, 2)
    // "A" occurs only in run 1, so its run-2 rows must be exactly zero.
    val aInRun2 = (50 until 100).map(r => math.abs(m(r, 0)))
    assertEqualsDouble(aInRun2.max, 0.0, 0.0, "condition A responded in a run where it never occurred")
    val bInRun2 = (50 until 100).map(r => math.abs(m(r, 1)))
    assert(bInRun2.max > 0.1, "condition B should respond in run 2")

  test("multi-basis HRFs keep condition-major column order"):
    val m = Design.regressorDesign(
      onsets = Seq(10.0, 20.0),
      fac = Seq("A", "B"),
      block = Seq(0, 0),
      sframe = sframe,
      hrf = Hrfs.SPMG3
    )
    assertEquals(m.cols, 6, "two conditions x three basis columns")
    assertEquals(m.rows, 100)
