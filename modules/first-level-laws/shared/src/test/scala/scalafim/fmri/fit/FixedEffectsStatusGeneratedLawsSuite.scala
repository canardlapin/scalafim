package scalafim.fmri.fit

import gale.linalg.{DMat, DVec}
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import scalafim.dataset.{DatasetEvents, DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.{EmptyCellPolicy, FactorLevelRegistry}
import scalafim.fmri.design.baseline.Intercept
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.laws.GeneratedLawSuite
import scalafim.fmri.model.{FitPlan, FitStrategy, FmriModel, FmriModelBuilder, ModelBuildSpec}
import scalafim.image.{DMat as ImageDMat, SampleSpaces}

class FixedEffectsStatusGeneratedLawsSuite extends GeneratedLawSuite:

  private final case class FailureCase(
      voxelPosition: Int,
      first: FailureMode,
      second: FailureMode,
      reverse: Boolean
  )

  private enum FailureMode:
    case PositiveInfinityVariance
    case NaNVariance
    case ZeroVariance
    case NegativeVariance
    case DeclaredAllZero
    case DeclaredConstant
    case DeclaredNonFinite
    case DeclaredZeroVariance

  private val failureCase =
    for
      voxel <- Gen.choose(0, 2)
      first <- Gen.oneOf(FailureMode.values.toSeq)
      second <- Gen.oneOf(FailureMode.values.toSeq)
      reverse <- Gen.oneOf(false, true)
    yield FailureCase(voxel, first, second, reverse)

  property("fixed effects omit invalid run-voxel variances with stable typed precedence"):
    forAll(failureCase) { generated =>
      val voxelPosition = generated.voxelPosition
      val modes = Vector(generated.first, generated.second)
      val reverse = generated.reverse
      val baseline = healthyRunwise
      val alteredRuns = baseline.runs.zip(modes).map { case (run, mode) =>
        applyFailure(run, voxelPosition, mode)
      }
      val ordered = if reverse then alteredRuns.reverse else alteredRuns
      val altered = baseline.copy(runs = ordered)
      val combined = FixedEffects.combine(altered)
      val expectedStatus = aggregateOracle(modes.map(statusFor))
      val expectedVoxel = baseline.voxelIndices(voxelPosition)
      val expectedRetained = baseline.voxelIndices.patch(voxelPosition, Vector.empty, 1)

      combined match
        case Left(error) =>
          Prop.falsified :| s"modes=$modes reverse=$reverse error=${error.message}"
        case Right(result) =>
          val canonical = FixedEffects.combine(baseline.copy(runs = alteredRuns))
          val permutationStable = canonical.exists(value =>
            maxAbsDiff(value.coefficients.value, result.coefficients.value) <= 1e-12 &&
              maxAbsDiff(value.standardErrors.value, result.standardErrors.value) <= 1e-12
          )
          Prop(
            result.voxelIndices == expectedRetained &&
              result.fitExclusions == Vector(VoxelInferenceExclusion(expectedVoxel, expectedStatus)) &&
              allFinite(result.coefficients.value) &&
              allFinite(result.standardErrors.value) &&
              permutationStable
          ) :| s"modes=$modes reverse=$reverse retained=${result.voxelIndices}/$expectedRetained " +
            s"exclusions=${result.fitExclusions} expected=$expectedStatus permutationStable=$permutationStable"
    }

  private lazy val healthyRunwise: RunwiseFmriFitResult =
    FitPlanExecutor
      .fit(FitPlan(model, FitStrategy.RunwiseLeastSquares()))
      .fold(error => throw new IllegalArgumentException(error.message), identity)
      .asInstanceOf[RunwiseFmriFitResult]

  private lazy val model: FmriModel =
    val runLengths = Vector(12, 12)
    val rows = runLengths.sum
    val sampling = SamplingFrame(blockLens = runLengths, tr = Vector(1.0, 1.0))
    val task = Vector.tabulate(rows) { row =>
      math.sin((row + 1).toDouble * 0.31) + (if row >= runLengths.head then 0.4 else -0.2)
    }
    val states = Array.fill(3)(0.0)
    val response = Vector.tabulate(rows) { row =>
      if row == 0 || row == runLengths.head then
        var voxel = 0
        while voxel < states.length do
          states(voxel) = 0.0
          voxel += 1
      Vector.tabulate(3) { voxel =>
        val raw = math.sin((row + 1).toDouble * 12.9898 + (voxel + 1).toDouble * 78.233) * 43758.5453
        val innovation = (raw - math.floor(raw)) * 2.0 - 1.0
        states(voxel) = innovation + 0.25 * states(voxel)
        (0.6 + voxel * 0.2) * task(row) - 0.4 + voxel * 0.15 + states(voxel) * 0.2
      }
    }
    val events = DatasetEvents(
      Vector(
        Map("onset" -> "0.0", "cond" -> "A", "run" -> "run-1"),
        Map("onset" -> "4.0", "cond" -> "B", "run" -> "run-1"),
        Map("onset" -> "0.0", "cond" -> "A", "run" -> "run-2"),
        Map("onset" -> "4.0", "cond" -> "B", "run" -> "run-2")
      )
    )
    val dataset = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(
        DatasetId("fixed-effects-status-generated-law"),
        ImageDMat.fromRows(response),
        SampleSpaces(Vector(3, 1, 1))
      ),
      samplingFrame = sampling,
      events = events
    )
    val factorLevels = FactorLevelRegistry.of("cond" -> Seq("A", "B")).toOption.get
    FmriModelBuilder.buildModel(
      dataset,
      ModelBuildSpec(
        formula = "onset ~ hrf(cond)",
        blockColumn = Some("run"),
        baselineIntercept = Intercept.Global,
        factorLevels = factorLevels,
        emptyCellPolicy = EmptyCellPolicy.RetainZero,
        strategy = FitStrategy.SeparateRunsThenFixedEffects()
      )
    )

  private def applyFailure(
      run: RunwiseFmriRunResult,
      voxelPosition: Int,
      mode: FailureMode
  ): RunwiseFmriRunResult =
    val variances = Vector.tabulate(run.residualVariance.length)(run.residualVariance.apply)
    val statuses = run.resolvedVoxelStatuses
    mode match
      case FailureMode.PositiveInfinityVariance =>
        run.copy(
          residualVariance = DVec.fromSeq(variances.updated(voxelPosition, Double.PositiveInfinity)),
          voxelStatuses = Some(statuses.updated(voxelPosition, VoxelFitStatus.Estimable))
        )
      case FailureMode.NaNVariance =>
        run.copy(
          residualVariance = DVec.fromSeq(variances.updated(voxelPosition, Double.NaN)),
          voxelStatuses = Some(statuses.updated(voxelPosition, VoxelFitStatus.Estimable))
        )
      case FailureMode.ZeroVariance =>
        run.copy(
          residualVariance = DVec.fromSeq(variances.updated(voxelPosition, 0.0)),
          voxelStatuses = Some(statuses.updated(voxelPosition, VoxelFitStatus.Estimable))
        )
      case FailureMode.NegativeVariance =>
        run.copy(
          residualVariance = DVec.fromSeq(variances.updated(voxelPosition, -1.0)),
          voxelStatuses = Some(statuses.updated(voxelPosition, VoxelFitStatus.Estimable))
        )
      case declared =>
        run.copy(voxelStatuses = Some(statuses.updated(voxelPosition, statusFor(declared))))

  private def statusFor(mode: FailureMode): VoxelFitStatus =
    mode match
      case FailureMode.PositiveInfinityVariance | FailureMode.NaNVariance | FailureMode.DeclaredNonFinite =>
        VoxelFitStatus.NonFinite
      case FailureMode.ZeroVariance | FailureMode.NegativeVariance | FailureMode.DeclaredZeroVariance =>
        VoxelFitStatus.ZeroResidualVariance
      case FailureMode.DeclaredAllZero =>
        VoxelFitStatus.AllZero
      case FailureMode.DeclaredConstant =>
        VoxelFitStatus.Constant

  private def aggregateOracle(statuses: Vector[VoxelFitStatus]): VoxelFitStatus =
    if statuses.contains(VoxelFitStatus.NonFinite) then VoxelFitStatus.NonFinite
    else if statuses.contains(VoxelFitStatus.ZeroResidualVariance) then VoxelFitStatus.ZeroResidualVariance
    else if statuses.contains(VoxelFitStatus.AllZero) then VoxelFitStatus.AllZero
    else VoxelFitStatus.Constant

  private def maxAbsDiff(left: DMat, right: DMat): Double =
    if left.rows != right.rows || left.cols != right.cols then Double.PositiveInfinity
    else
      var gap = 0.0
      var row = 0
      while row < left.rows do
        var col = 0
        while col < left.cols do
          gap = math.max(gap, math.abs(left(row, col) - right(row, col)))
          col += 1
        row += 1
      gap

  private def allFinite(matrix: DMat): Boolean =
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        if !matrix(row, col).isFinite then return false
        col += 1
      row += 1
    true
