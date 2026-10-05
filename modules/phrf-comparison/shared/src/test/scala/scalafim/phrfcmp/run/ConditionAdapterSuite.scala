package scalafim.phrfcmp.run

import scalafim.fmri.fit.profile.DecodeStatus
import scalafim.fmri.fit.FitError
import scalafim.phrfcmp.score.{ConditionDataset, ConditionResponse, Method, ResponseGrid, VoxelOutcome}

/** The S3 to S8 mapping (JVM and JS). */
class ConditionAdapterSuite extends munit.FunSuite:

  private val grid = ResponseGrid.standard().fold(e => fail(e.message), identity)
  private def resp(f: (Int, Int) => Double, conditions: Int = 2) =
    ConditionResponse(grid, Vector.tabulate(conditions)(c => Array.tabulate(grid.size)(i => f(c, i))))

  test("statuses map to the scorer outcomes; NotRun is a failure"):
    import ConditionArmStatus.*
    assertEquals(ConditionAdapter.outcome[Double](Estimated, Some(1.5)), VoxelOutcome.Estimated(1.5))
    assertEquals(ConditionAdapter.outcome[Double](Estimated, None), VoxelOutcome.Failed)
    assertEquals(ConditionAdapter.outcome[Double](Refused(RefusalKind.Decode(DecodeStatus.Boundary)), Some(1.0)), VoxelOutcome.Refused)
    assertEquals(ConditionAdapter.outcome[Double](Refused(RefusalKind.DatasetLevel("plan", "X")), None), VoxelOutcome.Refused)
    assertEquals(ConditionAdapter.outcome[Double](Failed(FailureKind.NonFiniteCoefficient), Some(1.0)), VoxelOutcome.Failed)
    assertEquals(ConditionAdapter.outcome[Double](Failed(FailureKind.Fit(FitError.EmptyDesign)), None), VoxelOutcome.Failed)
    assertEquals(ConditionAdapter.outcome[Double](NotRun, Some(1.0)), VoxelOutcome.Failed)

  test("the estimate is not evaluated for refused voxels"):
    var evaluated = false
    val r = ConditionAdapter.outcome[Double](ConditionArmStatus.Refused(RefusalKind.Decode(DecodeStatus.Boundary)), { evaluated = true; Some(1.0) })
    assertEquals(r, VoxelOutcome.Refused)
    assert(!evaluated)

  test("MISE: zero for identical responses, trapezoid integral otherwise, averaged over conditions"):
    val truth = resp((c, i) => math.sin(0.1 * i + c))
    assertEquals(ConditionAdapter.mise(truth, truth), Some(0.0))
    // offset 1 in condition 0 and 3 in condition 1: integrals 1 * 32 and 9 * 32, mean 5 * 32
    val off = resp((c, i) => math.sin(0.1 * i + c) + (if c == 0 then 1.0 else 3.0))
    val m = ConditionAdapter.mise(off, truth).get
    assertEqualsDouble(m, 160.0, 1e-9)
    // grids and condition counts must agree
    assertEquals(ConditionAdapter.mise(resp((_, _) => 0.0, 3), truth), None)
    val g2 = ResponseGrid.standard(16).fold(e => fail(e.message), identity)
    assertEquals(ConditionAdapter.mise(ConditionResponse(g2, Vector(Array.fill(g2.size)(0.0), Array.fill(g2.size)(0.0))), truth), None)
    assertEquals(ConditionAdapter.mise(resp((_, _) => Double.NaN), truth), None)

  test("MISE of a time-varying difference with a known exact integral (trapezoid is exact; a left Riemann sum is not)"):
    // difference d_c(t) = (c + 1) sqrt(t): d^2 = (c + 1)^2 t is linear, so the trapezoid rule is exact:
    // integral over [0, 32] = (c + 1)^2 * 32^2 / 2 = 512 (c + 1)^2; mean over c = 0, 1 is 512 * 5 / 2 = 1280.
    // The left Riemann sum is 510.4 (c + 1)^2 (a 0.3 % bias), so it cannot pass at 1e-9.
    val truth = resp((_, _) => 0.0)
    val fitted = resp((c, i) => (c + 1) * math.sqrt(grid.lag(i)))
    assertEqualsDouble(ConditionAdapter.mise(fitted, truth).get, 1280.0, 1e-9)
    // and a non-zero, non-constant truth: the difference is what is integrated
    val truth2 = resp((c, i) => math.cos(0.2 * i) + c)
    val fitted2 = resp((c, i) => math.cos(0.2 * i) + c + (c + 1) * math.sqrt(grid.lag(i)))
    assertEqualsDouble(ConditionAdapter.mise(fitted2, truth2).get, 1280.0, 1e-8)

  test("an arm result adapts per voxel and builds an S8 ConditionDataset"):
    val truth = Vector(resp((_, _) => 0.0), resp((_, _) => 0.0), resp((_, _) => 0.0))
    val voxels = Vector(
      ConditionVoxel(0, ConditionArmStatus.Estimated, Some(resp((_, _) => 1.0)), None),
      ConditionVoxel(1, ConditionArmStatus.Refused(RefusalKind.Decode(DecodeStatus.Boundary)), None, None),
      ConditionVoxel(2, ConditionArmStatus.Failed(FailureKind.NonFiniteCoefficient), None, None)
    )
    val result = ConditionArmResult(ConditionArm.Phrf, "h", voxels, None, None)
    val out = ConditionAdapter.toScorer(result, truth)
    assertEquals(out, Vector(VoxelOutcome.Estimated(32.0), VoxelOutcome.Refused, VoxelOutcome.Failed))
    assertEquals(ConditionAdapter.method(ConditionArm.Phrf), Method.Phrf)
    assertEquals(ConditionArm.All.map(ConditionAdapter.method).toSet, Set(Method.Phrf, Method.Can, Method.Inf3, Method.Fir))
    val arms = ConditionArm.All.map(a => ConditionAdapter.method(a) -> out).toMap
    assert(ConditionDataset.of(0, 1.0, 32.0, arms).isRight)
    // a truth vector of the wrong length is a failure for every voxel, not an exception
    assertEquals(ConditionAdapter.toScorer(result, truth.take(2)), Vector.fill(3)(VoxelOutcome.Failed))
