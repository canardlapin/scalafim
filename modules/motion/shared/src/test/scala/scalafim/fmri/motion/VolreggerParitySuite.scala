package scalafim.fmri.motion

import scalafim.fmri.motion.fixtures.VolreggerFixtures
import scalafim.image.{Axis, NeuroSpace, NeuroVec, NeuroVol, NArrayUtil}

class VolreggerParitySuite extends munit.FunSuite:

  private val fixture = VolreggerFixtures.coreFixture

  private def run1x1x1(values: Vector[Double]): NeuroVec[Double] =
    val data = NArrayUtil.tabulate[Double](values.length)(values)
    NeuroVec.fromLinear(data, NeuroSpace(Vector(1, 1, 1)).addDim(values.length, Some(Axis.Time)), "volregger-fixture")

  private def lineRun(values: Vector[Double]): NeuroVec[Double] =
    val data = NArrayUtil.tabulate[Double](values.length)(values)
    NeuroVec.fromLinear(data, NeuroSpace(Vector(values.length, 1, 1)).addDim(1, Some(Axis.Time)), "volregger-apply-fixture")

  private def allMask(space: NeuroSpace): NeuroVol[Boolean] =
    NeuroVol.fromLinear(NArrayUtil.fillConst[Boolean](space.spatialDims.product, true), space.spatialSpace, "all-mask")

  private def identityEstimatorRun(): NeuroVec[Double] =
    val dims = fixture.doubles("estimator_identity_dims").map(_.toInt)
    val nx = dims(0)
    val ny = dims(1)
    val nz = dims(2)
    val nt = dims(3)
    val nxyz = nx * ny * nz
    val frame = Array.fill(nxyz)(0.0)
    frame((nx / 2) + nx * ((ny / 2) + ny * (nz / 2))) = 5.0
    val data = NArrayUtil.ofSize[Double](nxyz * nt)
    var t = 0
    while t < nt do
      var i = 0
      while i < nxyz do
        data(i + t * nxyz) = frame(i)
        i += 1
      t += 1
    NeuroVec.fromLinear(data, NeuroSpace(Vector(nx, ny, nz)).addDim(nt, Some(Axis.Time)), "volregger-estimator-fixture")

  private def baseValueAt(x: Double, y: Double, z: Double): Double =
    val dx = x - 3.0
    val dy = y - 2.0
    val dz = z - 2.0
    10.0 * math.exp(-(dx * dx / 5.0 + dy * dy / 3.0 + dz * dz / 4.0)) +
      0.4 * x +
      0.2 * y -
      0.15 * z +
      0.35 * dx * dy +
      0.12 * dx * dx -
      0.08 * dy * dy +
      0.05 * dx * dz

  private def translationEstimatorRun(): NeuroVec[Double] =
    val dims = fixture.doubles("estimator_translation_dims").map(_.toInt)
    val nx = dims(0)
    val ny = dims(1)
    val nz = dims(2)
    val nt = dims(3)
    val nxyz = nx * ny * nz
    val fixed =
      Array.tabulate(nxyz) { lin =>
        val i = lin % nx
        val j = (lin / nx) % ny
        val k = lin / (nx * ny)
        baseValueAt(i.toDouble, j.toDouble, k.toDouble)
      }
    val moving =
      Array.tabulate(nxyz) { lin =>
        val i = lin % nx
        val j = (lin / nx) % ny
        val k = lin / (nx * ny)
        val srcI = math.min(nx - 1, i + 1)
        fixed(srcI + nx * (j + ny * k))
      }
    val data = NArrayUtil.ofSize[Double](nxyz * nt)
    var i = 0
    while i < nxyz do
      data(i) = fixed(i)
      data(i + nxyz) = moving(i)
      i += 1
    NeuroVec.fromLinear(data, NeuroSpace(Vector(nx, ny, nz)).addDim(nt, Some(Axis.Time)), "volregger-translation-fixture")

  private def translationEstimatorMask(space: NeuroSpace): NeuroVol[Boolean] =
    val dims = space.spatialDims
    val nx = dims(0)
    val ny = dims(1)
    val nz = dims(2)
    val nxyz = nx * ny * nz
    val data =
      NArrayUtil.tabulate[Boolean](nxyz) { lin =>
        val i = lin % nx
        val j = (lin / nx) % ny
        val k = lin / (nx * ny)
        i >= 1 && i < nx - 1 &&
          j >= 1 && j < ny - 1 &&
          k >= 1 && k < nz - 1
      }
    NeuroVol.fromLinear(data, space.spatialSpace, "volregger-translation-mask")

  private def poseFrom(values: Vector[Double]): RigidPose =
    assertEquals(values.length, 6)
    RigidPose.unsafe(values(0), values(1), values(2), values(3), values(4), values(5))

  private def assertVectorClose(actual: Vector[Double], expected: Vector[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).foreach { case (a, e) =>
      if e.isNaN then assert(a.isNaN)
      else assertEqualsDouble(a, e, tol)
    }

  private def metadataItems(key: String): Vector[String] =
    fixture.string(key).split(";").toVector

  private def smoothSplineRows(values: Vector[Double], iterations: Int): Vector[Double] =
    val rows = values.grouped(6).map(_.toArray).toArray
    var current = rows.map(_.clone())
    var iter = 0
    while iter < iterations do
      val next = Array.ofDim[Double](current.length, 6)
      var c = 0
      while c < 6 do
        next(0)(c) = current(0)(c)
        var r = 1
        while r < current.length - 1 do
          next(r)(c) = (current(r - 1)(c) + 4.0 * current(r)(c) + current(r + 1)(c)) / 6.0
          r += 1
        if current.length > 1 then next(current.length - 1)(c) = current(current.length - 1)(c)
        c += 1
      current = next
      iter += 1
    current.toVector.flatMap(_.toVector)

  private def packetOffsetsFromMultiband(groups: Vector[Double]): Vector[Double] =
    val ints = groups.map(_.toInt)
    val unique = ints.distinct.sorted
    val denom = math.max(1.0, (unique.length - 1).toDouble)
    ints.map { group =>
      unique.indexOf(group).toDouble / denom
    }

  test("generated core fixture records volregger provenance") {
    assertEquals(fixture.string("fixture_version"), "1")
    assertEquals(fixture.string("volregger_commit"), "f350a33140adc38ee860eae5efa0d8e80d1e6b94")
    assert(fixture.string("source_files").contains("R/transform_metrics.R"))
    assert(fixture.string("source_files").contains("R/fd_dvars.R"))
    assert(fixture.string("source_files").contains("src/api_spline.cpp"))
    assert(fixture.string("source_files").contains("tests/testthat/test-ic-efficacy.R"))
    assert(fixture.string("source_files").contains("tests/testthat/test-reporting-cli.R"))
  }

  test("pose matrix and inverse match volregger homogeneous transform convention") {
    val pose = poseFrom(fixture.doubles("pose"))
    val matrix = pose.toMatrix
    val inverse = pose.inverse.fold(err => fail(err.message), identity).toMatrix
    val expectedMatrix = fixture.doubles("matrix_row_major")
    val expectedInverse = fixture.doubles("inverse_row_major")

    var i = 0
    while i < 16 do
      val r = i / 4
      val c = i % 4
      assertEqualsDouble(matrix(r, c), expectedMatrix(i), 1e-14)
      assertEqualsDouble(inverse(r, c), expectedInverse(i), 1e-12)
      i += 1
  }

  test("framewise displacement matches generated volregger fixture") {
    val poses =
      fixture
        .doubles("fd_trace_row_major")
        .grouped(6)
        .map(poseFrom)
        .toVector
    val actual = MotionMetrics.framewiseDisplacement(MotionTrace.unsafe(poses))
    assertVectorClose(actual, fixture.doubles("fd"), 1e-12)
  }

  test("raw and robust DVARS match generated volregger fixtures") {
    val rawRun = run1x1x1(fixture.doubles("dvars_values"))
    val robustRun = run1x1x1(fixture.doubles("robust_dvars_values"))

    val raw = MotionMetrics.dvars(rawRun, robust = false).fold(err => fail(err.message), identity)
    val robust = MotionMetrics.dvars(robustRun, robust = true).fold(err => fail(err.message), identity)

    assertVectorClose(raw, fixture.doubles("dvars_raw"), 1e-12)
    assertVectorClose(robust, fixture.doubles("dvars_robust"), 1e-12)
  }

  test("radius and masked displacement summaries match generated volregger fixtures") {
    val pose = poseFrom(fixture.doubles("pose"))
    val radiusFd = MotionMetrics.transformDisplacement(pose)
    assertEqualsDouble(radiusFd, fixture.doubles("radius_fd_pose").head, 1e-12)

    val mask = allMask(NeuroSpace(Vector(3, 3, 3), spacing = Some(Vector(2.0, 2.0, 2.0))))
    val summary =
      MotionMetrics
        .maskedDisplacementSummary(mask, RigidPose.unsafe(1.0, 0.0, 0.0, 0.0, 0.0, 0.0), Some(RigidPose.identity))
        .fold(err => fail(err.message), identity)
    val expected = fixture.doubles("translation_summary")

    assertEqualsDouble(summary.median, expected(0), 1e-12)
    assertEqualsDouble(summary.p95, expected(1), 1e-12)
    assertEqualsDouble(summary.max, expected(2), 1e-12)
    assertEqualsDouble(MotionMetrics.transformDisplacement(RigidPose.unsafe(1.0, 0.0, 0.0, 0.0, 0.0, 0.0)), expected(3), 1e-12)
    assertEqualsDouble(MotionMetrics.transformDisplacement(RigidPose.identity), expected(4), 1e-12)
    assertEqualsDouble(
      MotionMetrics.transformDisplacement(RigidPose.unsafe(1.0, 0.0, 0.0, 0.0, 0.0, 0.0), reference = Some(RigidPose.identity)),
      expected(5),
      1e-12
    )
  }

  test("linear apply padding matches generated volregger fixture") {
    val run = lineRun(fixture.doubles("apply_edge_input"))
    val trace = MotionTrace.unsafe(Vector(RigidPose.unsafe(1.0, 0.0, 0.0, 0.0, 0.0, 0.0)))
    val zeroControl = ApplyControl.make(padMode = PadMode.Zero).fold(err => fail(err.message), identity)

    val clamp = MotionApplier.apply(run, trace).fold(err => fail(err.message), identity)
    val zero = MotionApplier.apply(run, trace, zeroControl).fold(err => fail(err.message), identity)

    assertVectorClose(clamp.values.data.toVector, fixture.doubles("apply_edge_plus_x_clamp"), 1e-12)
    assertVectorClose(zero.values.data.toVector, fixture.doubles("apply_edge_plus_x_zero"), 1e-12)
  }

  test("identity estimator fixture matches generated volregger zero-motion contract") {
    val run = identityEstimatorRun()
    val nxyz = run.space.spatialDims.product
    val pyramid =
      PyramidControl
        .make(
          downsample = Vector(1),
          maxIterations = Vector(2),
          sampleCounts = Vector(nxyz),
          enabled = false
        )
        .fold(err => fail(err.message), identity)
    val control =
      MotionControl.default.copy(
        pyramid = pyramid,
        template = TemplateControl(robustTemplate = false, refreshValidOnly = false, edgeExcludeFraction = 0.0),
        capture = MotionControl.default.capture.withEnabled(false)
      )
    val estimate =
      MotionEstimator
        .estimate(run, mask = None, MotionPlan(ReferenceStrategy.Middle, MotionEngine.RigidRobust, control))
        .fold(err => fail(err.message), identity)
    val expectedMotion = fixture.doubles("estimator_identity_motion_row_major")
    val expectedCostInit = fixture.doubles("estimator_identity_cost_init")
    val expectedCostFinal = fixture.doubles("estimator_identity_cost_final")
    val expectedOverlap = fixture.doubles("estimator_identity_overlap")

    assertEquals(estimate.trace.length, expectedCostFinal.length)
    var t = 0
    while t < estimate.trace.length do
      val pose = estimate.trace.unsafeFrame(t)
      val offset = t * 6
      assertEqualsDouble(pose.tx, expectedMotion(offset), 1e-12)
      assertEqualsDouble(pose.ty, expectedMotion(offset + 1), 1e-12)
      assertEqualsDouble(pose.tz, expectedMotion(offset + 2), 1e-12)
      assertEqualsDouble(pose.rx, expectedMotion(offset + 3), 1e-12)
      assertEqualsDouble(pose.ry, expectedMotion(offset + 4), 1e-12)
      assertEqualsDouble(pose.rz, expectedMotion(offset + 5), 1e-12)
      assertEqualsDouble(estimate.diagnostics(t).costInitial, expectedCostInit(t), 1e-12)
      assertEqualsDouble(estimate.diagnostics(t).costFinal, expectedCostFinal(t), 1e-12)
      assertEqualsDouble(estimate.diagnostics(t).overlap, expectedOverlap(t), 1e-12)
      t += 1
  }

  test("translation estimator fixture matches generated volregger recovery contract") {
    val run = translationEstimatorRun()
    val nxyz = run.space.spatialDims.product
    val pyramid =
      PyramidControl
        .make(
          downsample = Vector(1),
          maxIterations = Vector(12),
          sampleCounts = Vector(nxyz),
          enabled = false
        )
        .fold(err => fail(err.message), identity)
    val control =
      MotionControl.default.copy(
        pyramid = pyramid,
        template = TemplateControl(robustTemplate = false, refreshValidOnly = false, edgeExcludeFraction = 0.0),
        capture = CaptureControl(enabled = true, translationHalfWidthMm = 1.0, rotationHalfWidthDeg = 8.0, topK = 4)
      )
    val estimate =
      MotionEstimator
        .estimate(
          run,
          mask = Some(translationEstimatorMask(run.space)),
          MotionPlan(ReferenceStrategy.Frame(FrameIndex.unsafe(0)), MotionEngine.RigidRobust, control)
        )
        .fold(err => fail(err.message), identity)
    val referenceMotion = fixture.doubles("estimator_translation_motion_row_major")
    val referenceCostInit = fixture.doubles("estimator_translation_cost_init")
    val referenceCostFinal = fixture.doubles("estimator_translation_cost_final")
    val referenceOverlap = fixture.doubles("estimator_translation_overlap")
    val pose = estimate.trace.unsafeFrame(1)
    val referenceTx = referenceMotion(6)

    assertEqualsDouble(estimate.trace.unsafeFrame(0).tx, referenceMotion(0), 1e-12)
    assert(referenceTx > 0.3)
    assert(pose.tx > 0.3)
    assertEqualsDouble(math.signum(pose.tx), math.signum(referenceTx), 1e-12)
    assert(math.abs(pose.tx - referenceTx) <= 0.45)
    assert(estimate.diagnostics(1).costInitial > estimate.diagnostics(1).costFinal)
    assert(referenceCostInit(1) > referenceCostFinal(1))
    assert(estimate.diagnostics(1).overlap >= referenceOverlap(1) - 0.2)
    assert(estimate.diagnostics.forall(d => d.costInitial.isFinite && d.costFinal.isFinite && d.overlap.isFinite))
  }

  test("spline and packet fixture anchors volregger smoothing and packet offsets") {
    val input = fixture.doubles("spline_pose_row_major")
    val smoothIter = fixture.doubles("spline_smooth_iter").head.toInt
    val expectedSmoothed = fixture.doubles("spline_smoothed_pose_row_major")
    val expectedTimes = fixture.doubles("spline_times")
    val tr = fixture.doubles("spline_tr").head
    val sliceTimes = fixture.doubles("spline_slice_times")
    val mbGroups = fixture.doubles("spline_mb_groups")

    assertEquals(fixture.string("spline.method"), "cubic_bspline_smoother")
    assertVectorClose(smoothSplineRows(input, smoothIter), expectedSmoothed, 1e-12)
    assertVectorClose(expectedTimes, Vector.tabulate(expectedTimes.length)(_.toDouble * tr), 1e-12)
    assertVectorClose(fixture.doubles("spline_packet_offsets_slice_times"), sliceTimes, 1e-12)
    assertVectorClose(fixture.doubles("spline_packet_offsets_mb_groups"), packetOffsetsFromMultiband(mbGroups), 1e-12)
  }

  test("IC, whitening, spline, and parallel profile fixtures expose volregger contracts") {
    val fastFmri = fixture.doubles("profile_fast_fmri_flags")
    val fastParallel = fixture.doubles("profile_fast_native_parallel_flags")
    val icStencil = fixture.doubles("profile_ic_stencil_flags")
    val icWhiten = fixture.doubles("profile_ic_whiten_flags")
    val sliceSpline = fixture.doubles("profile_slice_spline_flags")

    assert(metadataItems("profile.fast_fmri.components").contains("parallel_frames"))
    assert(metadataItems("profile.fast_fmri.components").contains("ic_stencil"))
    assert(metadataItems("profile.ic_whiten.components").contains("whiten"))
    assertEquals(fixture.string("profile.slice_spline.engine"), "rigid_spline")

    assertVectorClose(fastFmri, Vector(1.0, 1.0, 1.0, 0.0, 0.0, 0.05, 1.0), 1e-12)
    assertVectorClose(fastParallel, Vector(1.0, 1.0, 1.0, 0.0, 0.0, 0.05, 1.0), 1e-12)
    assertVectorClose(icStencil, Vector(1.0, 0.0, 0.0, 0.0, 0.0, 0.05, 1.0), 1e-12)
    assertVectorClose(icWhiten, Vector(1.0, 1.0, 0.0, 0.0, 0.0, 0.05, 1.0), 1e-12)
    assertVectorClose(sliceSpline, Vector(1.0, 1.0, 0.0, 1.0, 0.0, 0.05, 1.0), 1e-12)
    assertVectorClose(fixture.doubles("ic_whiten_noninferiority_gates"), Vector(1.08, 1.05, 0.15, 1.12), 1e-12)
    assertVectorClose(fixture.doubles("ic_nuisance_improvement_gates"), Vector(0.98, 0.95, 0.10, 1.20), 1e-12)
    assertVectorClose(fixture.doubles("expanded_capture_success_gates"), Vector(-1.0, 2.0), 1e-12)
  }

  test("CLI, report, and benchmark fixture metadata captures downstream artifact contracts") {
    assertEquals(metadataItems("cli.commands").toSet, Set("estimate", "apply", "run", "report"))
    assert(metadataItems("cli.run.outputs").contains("<prefix>_motion.tsv"))
    assert(metadataItems("cli.run.outputs").contains("<prefix>_summary.csv"))
    assert(metadataItems("cli.run.outputs").contains("<prefix>_matrices.csv"))
    assert(metadataItems("cli.run.outputs").contains("<prefix>_report.rds"))

    val summaryColumns = metadataItems("report.summary_columns")
    assert(summaryColumns.contains("packet_correction_mag_mean"))
    assert(summaryColumns.contains("packet_correction_mag_max"))
    assert(metadataItems("report.fast_fmri.components").contains("whitening"))
    assert(metadataItems("report.fast_fmri.components").contains("ic_stencil"))

    assert(metadataItems("benchmark.required_columns").contains("estimate_sec"))
    assert(metadataItems("benchmark.required_columns").contains("report_sec"))
    assert(metadataItems("benchmark.truth_scenarios").contains("hard_motion_plus_nuisance"))
    assert(metadataItems("benchmark.claim_families").contains("truth_displacement"))
  }
