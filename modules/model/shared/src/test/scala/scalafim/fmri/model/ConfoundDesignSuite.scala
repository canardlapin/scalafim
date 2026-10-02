package scalafim.fmri.model

import scalafim.fmri.hrf.linalg.Mat

class ConfoundDesignSuite extends munit.FunSuite:
  private def motion(rows: Int): Mat =
    Mat.unsafe(rows, 6, Array.tabulate(rows * 6) { index =>
      val row = index / 6
      val column = index % 6
      (row * 10 + column).toDouble
    })
  private def acompcor(rows: Int): Mat =
    Mat.unsafe(rows, 3, Array.tabulate(rows * 3)(index => (index + 1).toDouble / 10.0))
  private def input(rows: Int, fd: Option[Vector[Double]] = None): ConfoundRunInput =
    ConfoundRunInput(motion(rows), acompcor(rows), Some(Vector.tabulate(rows)(_.toDouble)), Some(Vector.fill(rows)(2.0)), Some(Vector.fill(rows)(3.0)), fd)

  test("motion derivatives and powers are scan-local and stable across runs"):
    val spec = ConfoundSpec(MotionExpansion.Friston24, acompcorComponents = 2, includeWhiteMatter = true, includeCsf = true, includeGlobalSignal = true)
    val prepared = ConfoundDesign.prepare(spec, Vector(input(4), input(4))).toOption.getOrElse(fail("confounds should prepare"))
    assertEquals(prepared.excluded, Vector.empty)
    assertEquals(prepared.included.map(_.columnNames.length), Vector(29, 29))
    val first = prepared.included.head
    assertEquals(first.columnNames.take(8), Vector("motion_x", "motion_y", "motion_z", "motion_rot_x", "motion_rot_y", "motion_rot_z", "motion_x_derivative1", "motion_y_derivative1"))
    assertEqualsDouble(first.matrix(0, 6), 0.0, 0.0)
    assertEqualsDouble(first.matrix(1, 6), 10.0, 0.0)
    assertEqualsDouble(first.matrix(1, 18), 100.0, 0.0)
    assertEquals(first.columnNames.takeRight(5), Vector("acompcor_01", "acompcor_02", "white_matter", "csf", "global_signal"))

  test("FD neighbour union creates one deduplicated spike per censored scan"):
    val policy = FdCensorPolicy.make(0.5, before = 1, after = 2).toOption.getOrElse(fail("policy"))
    val fd = Vector(0.0, 0.6, 0.0, 0.0, 0.0, 0.7, 0.0, 0.0, 0.0, 0.0)
    val result = ConfoundDesign.prepare(ConfoundSpec(censor = Some(policy)), Vector(input(10, Some(fd)))).toOption.getOrElse(fail("censor preparation"))
    val run = result.included.head
    assertEquals(run.censor.map(_.initialFdScans), Some(Vector(1, 5)))
    assertEquals(run.censor.map(_.censoredScans), Some((0 until 8).toVector))
    assertEquals(run.censor.map(_.censoredCount), Some(8))
    assertEquals(run.columnNames.filter(_.startsWith("censor_scan_")), (1 to 8).toVector.map(index => f"censor_scan_$index%04d"))
    assertEquals(run.matrix.cols, 14)

  test("short retained segments and run exclusion are explicit receipts"):
    val segmentPolicy = FdCensorPolicy.make(0.5, minimumRetainedSegment = 4).toOption.getOrElse(fail("segment policy"))
    val segmented = ConfoundDesign.prepare(ConfoundSpec(censor = Some(segmentPolicy)), Vector(input(8, Some(Vector(0.0, 0.0, 0.0, 0.6, 0.0, 0.0, 0.0, 0.0))))).toOption.getOrElse(fail("segment preparation"))
    assertEquals(segmented.included.head.censor.map(_.shortSegmentScans), Some(Vector(0, 1, 2)))
    assertEquals(segmented.included.head.censor.map(_.retainedScans), Some(Vector(4, 5, 6, 7)))

    val exclusionPolicy = FdCensorPolicy.make(0.5, maximumCensoredFraction = 0.5).toOption.getOrElse(fail("exclusion policy"))
    val excluded = ConfoundDesign.prepare(ConfoundSpec(censor = Some(exclusionPolicy)), Vector(input(4, Some(Vector(0.6, 0.6, 0.6, 0.0))))).toOption.getOrElse(fail("exclusion preparation"))
    assertEquals(excluded.included, Vector.empty)
    assertEquals(excluded.excluded.map(_.run), Vector(1))
    assert(excluded.toNuisanceRegressors.isLeft)

  test("missing selected inputs and malformed source matrices fail without padding"):
    val selected = ConfoundSpec(acompcorComponents = 2, includeWhiteMatter = true, censor = Some(FdCensorPolicy.make(0.2).toOption.getOrElse(fail("policy"))))
    val missing = ConfoundRunInput(motion(3), acompcor(3))
    assertEquals(ConfoundDesign.prepare(selected, Vector(missing)).left.toOption, Some(ConfoundError.MissingInput(1, "white matter")))
    val malformed = ConfoundRunInput(Mat.unsafe(3, 5, Array.fill(15)(0.0)), acompcor(3))
    assert(ConfoundDesign.prepare(ConfoundSpec(), Vector(malformed)).isLeft)
    val empty = ConfoundRunInput(Mat.unsafe(0, 6, Array.emptyDoubleArray), Mat.unsafe(0, 0, Array.emptyDoubleArray))
    assertEquals(
      ConfoundDesign.prepare(ConfoundSpec(), Vector(empty)).left.toOption,
      Some(ConfoundError.InvalidInput(1, "motion must contain at least one scan"))
    )

  test("derived confounds and FD neighbourhood bounds fail or saturate explicitly"):
    val overflowMotion = Mat.unsafe(2, 6, Array.fill(12)(Double.MaxValue))
    val overflow = ConfoundRunInput(overflowMotion, acompcor(2))
    assertEquals(
      ConfoundDesign.prepare(ConfoundSpec(motion = MotionExpansion.Friston24), Vector(overflow)).left.toOption,
      Some(ConfoundError.InvalidInput(1, "derived confound column 'motion_x_squared' contains a non-finite value"))
    )
    val policy = FdCensorPolicy.make(0.5, before = Int.MaxValue, after = Int.MaxValue).toOption.getOrElse(fail("policy"))
    val saturated = ConfoundDesign.prepare(ConfoundSpec(censor = Some(policy)), Vector(input(3, Some(Vector(0.0, 0.6, 0.0))))).toOption.getOrElse(fail("saturated neighbours"))
    assertEquals(saturated.included.head.censor.map(_.censoredScans), Some(Vector(0, 1, 2)))
