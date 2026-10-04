package scalafim.fmri.model

import scalafim.dataset.{DatasetEvents, DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.{RunIndex, ScanIndex}
import scalafim.fmri.design.baseline.NuisanceCheck
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.image.SampleSpaces
import gale.linalg.Matrix

class ConfoundDesignSuite extends munit.FunSuite:
  private val run1 = RunIndex.unsafeOneBased(1)
  private val run2 = RunIndex.unsafeOneBased(2)
  private def scans(values: Int*): Vector[ScanIndex] = values.toVector.map(ScanIndex.unsafeOneBased)
  private def names(run: PreparedConfoundRun): Vector[String] = run.columnNames.map(_.value)

  private def motionValue(row: Int, column: Int): Double = math.sin(0.37 * (column + 1) * row + column) + 0.1 * column
  private def motion(rows: Int): Mat =
    Mat.unsafe(rows, 6, Array.tabulate(rows * 6)(index => motionValue(index / 6, index % 6)))
  private def acompcor(rows: Int): Mat =
    Mat.unsafe(rows, 3, Array.tabulate(rows * 3)(index => math.sin((index + 1).toDouble)))
  private def input(rows: Int, fd: Option[Vector[Double]] = None): ConfoundRunInput =
    ConfoundRunInput(motion(rows), Some(acompcor(rows)), Some(Vector.tabulate(rows)(row => math.cos(row.toDouble))), Some(Vector.tabulate(rows)(row => math.sqrt(row + 2.0))), Some(Vector.tabulate(rows)(row => math.log(row + 3.0))), fd)
  private def quiet(rows: Int, flagged: Int*): Option[Vector[Double]] =
    Some(Vector.tabulate(rows)(row => if flagged.contains(row) then 0.6 else 0.0))

  test("motion derivatives and powers are scan-local and stable across runs"):
    val spec = ConfoundSpec(MotionExpansion.Friston24, acompcorComponents = 2, includeWhiteMatter = true, includeCsf = true, includeGlobalSignal = true)
    val prepared = ConfoundDesign.prepare(spec, Vector(input(40), input(40))).fold(error => fail(error.message), identity)
    assertEquals(prepared.excluded, Vector.empty)
    assertEquals(prepared.included.map(_.columnNames.length), Vector(29, 29))
    val first = prepared.included.head
    assertEquals(names(first).take(8), Vector("motion_x", "motion_y", "motion_z", "motion_rot_x", "motion_rot_y", "motion_rot_z", "motion_x_derivative1", "motion_y_derivative1").map("run1_" + _))
    assertEqualsDouble(first.matrix(0, 6), 0.0, 0.0)
    val delta = motionValue(1, 0) - motionValue(0, 0)
    assertEqualsDouble(first.matrix(1, 6), delta, 1e-15)
    assertEqualsDouble(first.matrix(1, 12), motionValue(1, 0) * motionValue(1, 0), 1e-15)
    assertEqualsDouble(first.matrix(1, 18), delta * delta, 1e-15)
    assertEquals(names(first).takeRight(5), Vector("acompcor_01", "acompcor_02", "white_matter", "csf", "global_signal").map("run1_" + _))
    assertEquals(names(prepared.included(1)).head, "run2_motion_x")
    // Derivatives restart at each run boundary.
    assertEqualsDouble(prepared.included(1).matrix(0, 6), 0.0, 0.0)

  test("RawAndDerivative12 yields raw motion then first differences only"):
    val prepared = ConfoundDesign.prepare(ConfoundSpec(MotionExpansion.RawAndDerivative12), Vector(input(20))).fold(error => fail(error.message), identity)
    val run = prepared.included.head
    assertEquals(run.matrix.cols, 12)
    assertEquals(names(run).drop(6), Vector("motion_x", "motion_y", "motion_z", "motion_rot_x", "motion_rot_y", "motion_rot_z").map(name => s"run1_${name}_derivative1"))
    var row = 1
    while row < 20 do
      var column = 0
      while column < 6 do
        assertEqualsDouble(run.matrix(row, column + 6), run.matrix(row, column) - run.matrix(row - 1, column), 1e-12)
        column += 1
      row += 1

  test("FD neighbour union creates one deduplicated spike per censored scan"):
    val policy = FdCensorPolicy.make(0.5, before = 1, after = 2).toOption.getOrElse(fail("policy"))
    val result = ConfoundDesign.prepare(ConfoundSpec(censor = Some(policy)), Vector(input(20, quiet(20, 1, 5)))).fold(error => fail(error.message), identity)
    val run = result.included.head
    assertEquals(run.censor.map(_.initialFdScans), Some(scans(2, 6)))
    assertEquals(run.censor.map(_.censoredScans), Some(scans(1 to 8*)))
    assertEquals(run.censor.map(_.censoredCount), Some(8))
    assertEquals(names(run).filter(_.contains("censor_scan_")), (1 to 8).toVector.map(index => f"run1_censor_scan_$index%04d"))
    assertEquals(run.matrix.cols, 14)
    // Spike n is one exactly at the one-based scan n.
    assertEqualsDouble(run.matrix(0, 6), 1.0, 0.0)
    assertEqualsDouble(run.matrix(1, 6), 0.0, 0.0)
    assertEqualsDouble(run.matrix(7, 13), 1.0, 0.0)

  test("short retained segments and run exclusion are explicit receipts"):
    val segmentPolicy = FdCensorPolicy.make(0.5, minimumRetainedSegment = 4).toOption.getOrElse(fail("segment policy"))
    val segmented = ConfoundDesign.prepare(ConfoundSpec(censor = Some(segmentPolicy)), Vector(input(12, quiet(12, 3)))).fold(error => fail(error.message), identity)
    assertEquals(segmented.included.head.censor.map(_.shortSegmentScans), Some(scans(1, 2, 3)))
    assertEquals(segmented.included.head.censor.map(_.retainedScans), Some(scans(5 to 12*)))

    val exclusionPolicy = FdCensorPolicy.make(0.5, maximumCensoredFraction = 0.5).toOption.getOrElse(fail("exclusion policy"))
    val excluded = ConfoundDesign.prepare(ConfoundSpec(censor = Some(exclusionPolicy)), Vector(input(4, Some(Vector(0.6, 0.6, 0.6, 0.0))))).fold(error => fail(error.message), identity)
    assertEquals(excluded.included, Vector.empty)
    assertEquals(excluded.excluded.map(_.run), Vector(run1))
    assert(excluded.toNuisanceRegressors.isLeft)

  test("runs with no retained scans or fewer retained scans than nuisance columns always fail"):
    val policy = FdCensorPolicy.make(0.5).toOption.getOrElse(fail("policy"))
    assertEquals(policy.maximumCensoredFraction, 1.0)
    assertEquals(
      ConfoundDesign.prepare(ConfoundSpec(censor = Some(policy)), Vector(input(10, quiet(10)), input(5, Some(Vector.fill(5)(0.9))))).left.toOption,
      Some(ConfoundError.NoRetainedScans(run2, 5))
    )
    val longSegment = FdCensorPolicy.make(0.5, minimumRetainedSegment = 11).toOption.getOrElse(fail("segment policy"))
    assertEquals(
      ConfoundDesign.prepare(ConfoundSpec(censor = Some(longSegment)), Vector(input(10, quiet(10)))).left.toOption,
      Some(ConfoundError.NoRetainedScans(run1, 10))
    )
    // Six raw motion columns need six retained scans: 10 scans minus 5 censored leaves 5.
    assertEquals(
      ConfoundDesign.prepare(ConfoundSpec(censor = Some(policy)), Vector(input(10, quiet(10, 0, 2, 4, 6, 8)))).left.toOption,
      Some(ConfoundError.InsufficientRetainedScans(run1, 5, 6))
    )
    assertEquals(
      ConfoundDesign.prepare(ConfoundSpec(MotionExpansion.Friston24), Vector(input(20))).left.toOption,
      Some(ConfoundError.InsufficientRetainedScans(run1, 20, 24))
    )
    // An explicit exclusion threshold turns the same run into an exclusion receipt.
    val strict = FdCensorPolicy.make(0.5, maximumCensoredFraction = 0.9).toOption.getOrElse(fail("strict policy"))
    val handled = ConfoundDesign.prepare(ConfoundSpec(censor = Some(strict)), Vector(input(10, quiet(10)), input(5, Some(Vector.fill(5)(0.9))))).fold(error => fail(error.message), identity)
    assertEquals(handled.excluded.map(_.run), Vector(run2))

  test("aCompCor input is optional exactly when no components are requested"):
    val noComponents = ConfoundRunInput(motion(10))
    val prepared = ConfoundDesign.prepare(ConfoundSpec(), Vector(noComponents)).fold(error => fail(error.message), identity)
    assertEquals(prepared.included.head.matrix.cols, 6)
    assertEquals(
      ConfoundDesign.prepare(ConfoundSpec(acompcorComponents = 2), Vector(noComponents)).left.toOption,
      Some(ConfoundError.MissingInput(run1, "aCompCor"))
    )
    assertEquals(
      ConfoundDesign.prepare(ConfoundSpec(acompcorComponents = 4), Vector(input(10))).left.toOption,
      Some(ConfoundError.InvalidInput(run1, "aCompCor has 3 components; 4 requested"))
    )

  test("missing selected inputs and malformed source matrices fail without padding"):
    val selected = ConfoundSpec(acompcorComponents = 2, includeWhiteMatter = true, censor = Some(FdCensorPolicy.make(0.2).toOption.getOrElse(fail("policy"))))
    val missing = ConfoundRunInput(motion(3), Some(acompcor(3)))
    assertEquals(ConfoundDesign.prepare(selected, Vector(missing)).left.toOption, Some(ConfoundError.MissingInput(run1, "white matter")))
    val malformed = ConfoundRunInput(Mat.unsafe(3, 5, Array.fill(15)(0.0)))
    assert(ConfoundDesign.prepare(ConfoundSpec(), Vector(malformed)).isLeft)
    val empty = ConfoundRunInput(Mat.unsafe(0, 6, Array.emptyDoubleArray))
    assertEquals(
      ConfoundDesign.prepare(ConfoundSpec(), Vector(empty)).left.toOption,
      Some(ConfoundError.InvalidInput(run1, "motion must contain at least one scan"))
    )

  test("derived confounds and FD neighbourhood bounds fail or saturate explicitly"):
    val overflowMotion = Mat.unsafe(2, 6, Array.fill(12)(Double.MaxValue))
    val overflow = ConfoundRunInput(overflowMotion)
    assertEquals(
      ConfoundDesign.prepare(ConfoundSpec(motion = MotionExpansion.Friston24), Vector(overflow)).left.toOption,
      Some(ConfoundError.InvalidInput(run1, "derived confound column 'run1_motion_x_squared' contains a non-finite value"))
    )
    val backward = FdCensorPolicy.make(0.5, before = Int.MaxValue).toOption.getOrElse(fail("policy"))
    val saturated = ConfoundDesign.prepare(ConfoundSpec(censor = Some(backward)), Vector(input(10, quiet(10, 1)))).fold(error => fail(error.message), identity)
    assertEquals(saturated.included.head.censor.map(_.censoredScans), Some(scans(1, 2)))
    val both = FdCensorPolicy.make(0.5, before = Int.MaxValue, after = Int.MaxValue).toOption.getOrElse(fail("policy"))
    assertEquals(
      ConfoundDesign.prepare(ConfoundSpec(censor = Some(both)), Vector(input(3, quiet(3, 1)))).left.toOption,
      Some(ConfoundError.NoRetainedScans(run1, 3))
    )

  test("multi-run confounds become globally unique nuisance columns in a built model"):
    val policy = FdCensorPolicy.make(0.5).toOption.getOrElse(fail("policy"))
    val prepared = ConfoundDesign.prepare(ConfoundSpec(censor = Some(policy)), Vector(input(12, quiet(12, 2)), input(12, quiet(12, 2))))
      .fold(error => fail(error.message), identity)
    val nuisance = prepared.toNuisanceRegressors.fold(error => fail(error.message), identity)
    val allNames = nuisance.names.getOrElse(fail("names")).flatten
    assertEquals(allNames.distinct.length, allNames.length)
    assertEquals(allNames.filter(_.contains("censor_scan")), Vector("run1_censor_scan_0003", "run2_censor_scan_0003"))

    val frame = SamplingFrame(blockLens = Seq(12, 12), tr = Seq(1.0, 1.0))
    val data = Matrix.dense(24, 1, Vector.tabulate(24)(i => math.sin(i / 3.0)))
    val dataset = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(DatasetId("confound-runs"), data, SampleSpaces(Vector(1, 1, 1))),
      samplingFrame = frame,
      events = DatasetEvents(Vector(
        Map("onset" -> "2", "run" -> "run-1", "task" -> "1"),
        Map("onset" -> "3", "run" -> "run-2", "task" -> "1")
      ))
    ).dataset
    val model = FmriModelBuilder.buildModelEither(
      dataset,
      ModelBuildSpec("onset ~ hrf(task)", blockColumn = Some("run"), nuisance = Some(nuisance.copy(check = NuisanceCheck.Error)))
    ).fold(error => fail(error.message), identity)
    val columns = model.baselineModel.columnNames.filter(name => name.contains("motion") || name.contains("censor_scan"))
    assertEquals(columns.distinct.length, columns.length)
    assertEquals(columns.length, 14)
    assert(columns.exists(_.contains("run2_censor_scan_0003")), columns.mkString(", "))
