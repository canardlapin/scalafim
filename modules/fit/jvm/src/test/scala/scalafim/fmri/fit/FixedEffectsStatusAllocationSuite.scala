package scalafim.fmri.fit

import java.lang.management.ManagementFactory
import com.sun.management.ThreadMXBean
import scalafim.dataset.{DatasetEvents, DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.image.SampleSpaces
import scalafim.fmri.design.{FactorLevelRegistry, EmptyCellPolicy}
import scalafim.fmri.design.baseline.Intercept
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.model.{FmriModelBuilder, ModelBuildSpec, FitPlan, FitStrategy}

class FixedEffectsStatusAllocationSuite extends munit.FunSuite:
  private def checked[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

  private def runwiseWithoutStatuses(voxels: Int): RunwiseFmriFitResult =
    val events = DatasetEvents(Vector(
      Map("onset" -> "0.0", "cond" -> "A", "run" -> "run-1"),
      Map("onset" -> "4.0", "cond" -> "B", "run" -> "run-1"),
      Map("onset" -> "0.0", "cond" -> "A", "run" -> "run-2"),
      Map("onset" -> "4.0", "cond" -> "B", "run" -> "run-2")))
    val rows = 20
    val values = Array.tabulate(rows * voxels): index =>
      val row = index / voxels
      val voxel = index % voxels
      math.sin(0.7 * row + 0.013 * voxel) + 0.1 * math.cos(1.3 * row * (voxel % 7 + 1))
    val dataset = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(DatasetId("fixed-effects-status-allocation"),
        GaleTestMatrix.fromArray(rows, voxels, values),
        SampleSpaces(Vector(voxels, 1, 1), spacing = Some(Vector(2.0, 2.0, 2.0)), origin = Some(Vector(0.0, 0.0, 0.0)))),
      samplingFrame = SamplingFrame(blockLens = Seq(8, 12), tr = Seq(2.0, 0.8), startTime = Seq(0.0, 0.0), precision = 0.1),
      events = events
    )
    val model = FmriModelBuilder.buildModel(dataset, ModelBuildSpec("onset ~ hrf(cond)",
      blockColumn = Some("run"), baselineIntercept = Intercept.Global,
      factorLevels = checked(FactorLevelRegistry.of("cond" -> Seq("A", "B"))),
      emptyCellPolicy = EmptyCellPolicy.RetainZero, precision = 0.1.s))
    val fit = checked(FitPlanExecutor.fit(FitPlan(model, FitStrategy.RunwiseLeastSquares())))
      .asInstanceOf[RunwiseFmriFitResult]
    fit.copy(runs = fit.runs.map(_.copy(voxelStatuses = None)))

  test("fixed-effects voxel selection without explicit statuses allocates linearly in voxels"):
    val counters = ManagementFactory.getThreadMXBean match
      case value: ThreadMXBean if value.isThreadAllocatedMemorySupported => Some(value)
      case _ => None
    assume(counters.isDefined, "this allocation regression requires JVM thread-allocation counters")
    val bean = counters.get
    bean.setThreadAllocatedMemoryEnabled(true)
    val voxels = 4096
    val source = runwiseWithoutStatuses(voxels)
    assert(source.runs.length == 2 && source.runs.forall(_.voxelStatuses.isEmpty))
    // Warm up so class loading and JIT are outside the measurement.
    var i = 0
    while i < 3 do
      checked(FixedEffects.combine(source))
      i += 1
    val before = bean.getCurrentThreadAllocatedBytes
    val result = checked(FixedEffects.combine(source))
    val bytes = bean.getCurrentThreadAllocatedBytes - before
    assertEquals(result.voxels, voxels)
    val perVoxel = bytes / voxels
    // Linear work measures about 4 KB per voxel. Materializing the V-element
    // default status vector for every voxel/run pair adds tens of KB per voxel
    // at this size, so the per-voxel budget separates O(V) from O(R*V^2).
    assert(perVoxel < 12288L, s"one combine over $voxels voxels allocated $bytes bytes ($perVoxel per voxel)")
