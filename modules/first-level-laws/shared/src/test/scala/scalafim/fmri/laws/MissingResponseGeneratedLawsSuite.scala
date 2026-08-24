package scalafim.fmri.laws

import gale.linalg.DMat
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import scalafim.dataset.{DataSelection, DatasetEvents, DatasetId, FmriDataset, IndexSelection, InMemoryDatasetBackend}
import scalafim.fmri.design.{EmptyCellPolicy, FactorLevelRegistry}
import scalafim.fmri.design.baseline.Intercept
import scalafim.fmri.fit.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.model.{
  AutocorrelationConfig,
  FitControls,
  FitPlan,
  FitStrategy,
  FmriModel,
  FmriModelBuilder,
  MissingDataPolicy,
  ModelBuildSpec
}
import scalafim.image.{DMat as ImageDMat, SampleSpaces}

class MissingResponseGeneratedLawsSuite extends GeneratedLawSuite:

  private enum EngineCase(val label: String):
    case Ols extends EngineCase("OLS")
    case EstimatedGls extends EngineCase("estimated GLS")
    case Runwise extends EngineCase("runwise OLS")
    case FixedEffects extends EngineCase("fixed effects")

  property("propagated missing responses equal explicit finite-voxel fits across selection, chunking, and engines"):
    forAll(MissingResponseGenerators.cases) { generated =>
      val model = modelFor(generated)
      val selected = selection(generated, generated.voxelOrder)
      val retainedOrder = generated.voxelOrder.filter(generated.retainedVoxels.contains)
      val finiteSelection = selection(generated, retainedOrder)
      val expectedExclusions = generated.voxelOrder
        .filter(generated.selectedMissingVoxels.contains)
        .map(voxel => VoxelInferenceExclusion(voxel, VoxelFitStatus.NonFinite))
      val failures = Vector.newBuilder[String]

      EngineCase.values.foreach { engine =>
        val tolerant = plan(model, engine, MissingDataPolicy.Propagate)
        val strict = plan(model, engine, MissingDataPolicy.Error)
        val whole = FitPlanExecutor.fit(tolerant, selected)
        val chunked = FitPlanExecutor.fitChunked(
          tolerant,
          selected,
          FitChunkingStrategy.unsafeByVoxelCount(generated.chunkWidth)
        )
        val explicit = FitPlanExecutor.fit(strict, finiteSelection)

        (whole, chunked, explicit) match
          case (Right(actual), Right(byChunk), Right(reference)) =>
            comparePayload(s"${engine.label} tolerant/explicit", actual, reference).foreach(failures += _)
            comparePayload(s"${engine.label} whole/chunked", actual, byChunk).foreach(failures += _)
            if actual.fitExclusions != expectedExclusions then
              failures += s"${engine.label} exclusions=${actual.fitExclusions} expected=$expectedExclusions"
            if byChunk.fitExclusions != expectedExclusions then
              failures += s"${engine.label} chunked exclusions=${byChunk.fitExclusions} expected=$expectedExclusions"
          case _ =>
            failures += s"${engine.label} whole=$whole chunked=$chunked explicit=$explicit"
      }

      val observed = failures.result()
      Prop(observed.isEmpty) :|
        s"runs=${generated.runLengths} voxels=${generated.voxels} missing=${generated.selectedMissingVoxels} " +
        s"selection=${generated.voxelOrder} width=${generated.chunkWidth} failures=${observed.mkString(" | ")}"
    }

  property("voxel-specific row omission equals explicit per-pattern fits across chunking and engines"):
    forAll(MissingResponseGenerators.cases) { generated =>
      val model = modelFor(generated)
      val selected = selection(generated, generated.voxelOrder)
      val expectedPatterns = observationPatterns(generated)
      val failures = Vector.newBuilder[String]

      EngineCase.values.foreach { engine =>
        val masked = plan(model, engine, MissingDataPolicy.OmitRowsPerVoxel)
        val strict = plan(model, engine, MissingDataPolicy.Error)
        val whole = FitPlanExecutor.fit(masked, selected)
        val chunked = FitPlanExecutor.fitChunked(
          masked,
          selected,
          FitChunkingStrategy.unsafeByVoxelCount(generated.chunkWidth)
        )

        (whole, chunked) match
          case (Right(actual), Right(byChunk)) =>
            comparePayload(s"${engine.label} masked whole/chunked", actual, byChunk).foreach(failures += _)
            if actual.fitExclusions.nonEmpty then
              failures += s"${engine.label} unexpected masked exclusions=${actual.fitExclusions}"
            expectedPatterns.foreach { case (timepoints, voxels) =>
              val reference = FitPlanExecutor.fit(
                strict,
                DataSelection(
                  time = IndexSelection.indices(timepoints*),
                  voxels = IndexSelection.indices(voxels*)
                )
              )
              (patternResult(actual, timepoints, voxels), reference) match
                case (Some(observed), Right(expected)) =>
                  comparePayload(s"${engine.label} pattern rows=$timepoints voxels=$voxels", observed, expected)
                    .foreach(failures += _)
                case pair =>
                  failures += s"${engine.label} missing pattern rows=$timepoints voxels=$voxels observed/reference=$pair"
            }
          case pair =>
            failures += s"${engine.label} masked whole/chunked=$pair"
      }

      val observed = failures.result()
      Prop(observed.isEmpty) :|
        s"runs=${generated.runLengths} voxels=${generated.voxels} missing=${generated.selectedMissingVoxels} " +
        s"selection=${generated.voxelOrder} width=${generated.chunkWidth} failures=${observed.mkString(" | ")}"
    }

  private def plan(
      model: FmriModel,
      engine: EngineCase,
      missingData: MissingDataPolicy
  ): FitPlan =
    val controls = FitControls(missingData = missingData)
    val strategy =
      engine match
        case EngineCase.Ols =>
          FitStrategy.OrdinaryLeastSquares(controls)
        case EngineCase.EstimatedGls =>
          FitStrategy.GeneralizedLeastSquares(
            autocorrelation = AutocorrelationConfig.unsafe(
              order = 1,
              global = true,
              exactFirst = false
            ),
            controls = controls
          )
        case EngineCase.Runwise =>
          FitStrategy.RunwiseLeastSquares(controls)
        case EngineCase.FixedEffects =>
          FitStrategy.SeparateRunsThenFixedEffects(controls)
    FitPlan(model, strategy)

  private def selection(generated: MissingResponseCase, voxels: Vector[Int]): DataSelection =
    DataSelection(
      time = IndexSelection.indices(generated.selectedTimepoints*),
      voxels = IndexSelection.indices(voxels*)
    )

  private def observationPatterns(
      generated: MissingResponseCase
  ): Vector[(Vector[Int], Vector[Int])] =
    val grouped = scala.collection.mutable.LinkedHashMap.empty[Vector[Int], Vector[Int]]
    generated.voxelOrder.foreach { voxel =>
      val timepoints =
        if voxel < generated.missingVoxelCount then
          val missing = generated.selectedTimepoints(
            (voxel * 5 + generated.seed) % generated.selectedTimepoints.length
          )
          generated.selectedTimepoints.filterNot(_ == missing)
        else generated.selectedTimepoints
      grouped.update(timepoints, grouped.getOrElse(timepoints, Vector.empty) :+ voxel)
    }
    grouped.toVector

  private def patternResult(
      result: FmriFitResult,
      timepoints: Vector[Int],
      voxels: Vector[Int]
  ): Option[FmriFitResult] =
    result match
      case patterned: PatternedFmriFitResult =>
        patterned.patternResults
          .find { child =>
            child.pattern.sourceTimepoints == timepoints && child.result.voxelIndices == voxels
          }
          .map(_.result)
      case other if other.timepoints == timepoints && other.voxelIndices == voxels => Some(other)
      case _                                                                       => None

  private def modelFor(generated: MissingResponseCase): FmriModel =
    val sampling = SamplingFrame(
      blockLens = generated.runLengths,
      tr = Vector.fill(generated.runLengths.length)(1.0)
    )
    val task = Vector.tabulate(generated.rows) { row =>
      val runOffset = if row >= generated.runLengths.head then 0.35 else -0.15
      math.sin((row + 1).toDouble * 0.23) + runOffset + ((row % 5) - 2).toDouble * 0.05
    }
    val finiteRows = finiteResponseRows(generated, task)
    val responseRows = finiteRows.zipWithIndex.map { case (values, row) =>
      values.zipWithIndex.map { case (value, voxel) =>
        if voxel < generated.missingVoxelCount then
          val selectedRow =
            generated.selectedTimepoints((voxel * 5 + generated.seed) % generated.selectedTimepoints.length)
          if row == selectedRow then nonFinite(voxel + generated.seed) else value
        else if voxel == generated.voxels - 1 && row == generated.omittedTimepoints.head then
          nonFinite(voxel + generated.seed)
        else value
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
        DatasetId(s"missing-response-law-${generated.seed}-${generated.rows}-${generated.voxels}"),
        ImageDMat.fromRows(responseRows),
        SampleSpaces(Vector(generated.voxels, 1, 1))
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

  private def finiteResponseRows(
      generated: MissingResponseCase,
      task: Vector[Double]
  ): Vector[Vector[Double]] =
    val states = Array.fill(generated.voxels)(0.0)
    Vector.tabulate(generated.rows) { row =>
      if row == 0 || row == generated.runLengths.head then
        var voxel = 0
        while voxel < states.length do
          states(voxel) = 0.0
          voxel += 1
      Vector.tabulate(generated.voxels) { voxel =>
        val raw = math.sin(
          (row + generated.seed % 31 + 1).toDouble * 12.9898 +
            (voxel + 1).toDouble * 78.233
        ) * 43758.5453
        val innovation = (raw - math.floor(raw)) * 2.0 - 1.0
        states(voxel) = innovation + 0.35 * states(voxel)
        val beta = 0.45 + voxel.toDouble * 0.17
        val intercept = -0.3 + voxel.toDouble * 0.11
        beta * task(row) + intercept + 0.2 * states(voxel)
      }
    }

  private def nonFinite(index: Int): Double =
    index % 3 match
      case 0 => Double.NaN
      case 1 => Double.PositiveInfinity
      case _ => Double.NegativeInfinity

  private def comparePayload(
      label: String,
      actual: FmriFitResult,
      expected: FmriFitResult
  ): Option[String] =
    val actualSignature = structuralSignature(actual)
    val expectedSignature = structuralSignature(expected)
    if actualSignature != expectedSignature then Some(s"$label structural=$actualSignature expected=$expectedSignature")
    else
      val left = numericPayload(actual)
      val right = numericPayload(expected)
      if left.length != right.length then Some(s"$label numeric lengths=${left.length}/${right.length}")
      else
        val gap = left.zip(right).map((a, b) => math.abs(a - b)).maxOption.getOrElse(0.0)
        if gap <= 1e-9 then None else Some(s"$label max gap=$gap")

  private def structuralSignature(result: FmriFitResult): String =
    val common = s"${result.engine}|${result.columnNames}|${result.voxelIndices}|${result.timepoints}"
    result match
      case dense: DenseFmriFitResult =>
        s"dense|$common|${dense.residualDegreesOfFreedom}|${dense.autocorrelation}"
      case runwise: RunwiseFmriFitResult =>
        val runs = runwise.runs.map(run =>
          (
            run.runIndex,
            run.rowIndices,
            run.timepoints,
            run.residualDegreesOfFreedom,
            run.sourceColumns,
            run.resolvedVoxelStatuses
          )
        )
        s"runwise|$common|$runs"
      case fixed: FixedEffectsFmriFitResult =>
        s"fixed|$common|${fixed.residualDegreesOfFreedom}|${fixed.policy}|${fixed.sufficientStatistics.runIndices}"
      case patterned: PatternedFmriFitResult =>
        val patterns = patterned.patternResults.map { child =>
          s"${child.pattern.id.value}:${child.pattern.rows}:${child.pattern.sourceTimepoints}:${structuralSignature(child.result)}"
        }
        s"patterned|$common|$patterns"
      case other =>
        s"unsupported|$common|${other.getClass.getName}"

  private def numericPayload(result: FmriFitResult): Vector[Double] =
    result match
      case dense: DenseFmriFitResult =>
        matrixValues(dense.coefficients.value) ++
          matrixValues(dense.standardErrors.value) ++
          vectorValues(dense.residualVariance) ++
          matrixValues(dense.normalizedCovariance)
      case runwise: RunwiseFmriFitResult =>
        runwise.runs.flatMap { run =>
          matrixValues(run.coefficients.value) ++
            matrixValues(run.standardErrors.value) ++
            vectorValues(run.residualVariance) ++
            matrixValues(run.normalizedCovariance)
        }
      case fixed: FixedEffectsFmriFitResult =>
        matrixValues(fixed.coefficients.value) ++
          matrixValues(fixed.standardErrors.value) ++
          matrixValues(fixed.normalizedCovariance) ++
          fixed.perRunContributions.flatMap { contribution =>
            contribution.precisionByVoxel.flatMap(matrixValues) ++
              matrixValues(contribution.precisionWeightedCoefficients)
          }
      case patterned: PatternedFmriFitResult =>
        patterned.patternResults.flatMap(child => numericPayload(child.result))
      case _ =>
        Vector.empty

  private def matrixValues(matrix: DMat): Vector[Double] =
    Vector.tabulate(matrix.rows * matrix.cols) { index =>
      matrix(index / matrix.cols, index % matrix.cols)
    }

  private def vectorValues(vector: gale.linalg.DVec): Vector[Double] =
    Vector.tabulate(vector.length)(vector.apply)
