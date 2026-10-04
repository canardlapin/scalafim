package scalafim.fmri.fit

import scalafim.fmri.design.hrf.KernelBasisProvenance.{matrix, number, option, record}
import scalafim.fmri.model.{
  ArOptions,
  ArStructure,
  DvarsWeightEstimator,
  DvarsWeightFunction,
  DvarsWeightScope,
  FixedWeightAlignment,
  MissingDataPolicy,
  NuisanceProjection as ModelNuisanceProjection,
  Regularization,
  RobustOptions,
  RobustPsi,
  ScaleScope,
  VolumeWeighting
}

/** Versioned structural encoding of a [[ResponsePreparationProvenance]]:
  * every enum case by an explicit label, every double by its IEEE-754 bits,
  * every string and sequence length-framed, and a nuisance matrix by its
  * dimensions plus a portable content digest. Nothing is rendered through
  * `toString`, so the encoding is identical on the JVM and Scala.js.
  */
private[fit] object ResponsePreparationIdentity:

  def provenance(value: ResponsePreparationProvenance): String =
    s"response-preparation/v1|records=${record("records", value.records.map(preparationRecord)*)}|" +
      s"volumeWeighting=${option(value.volumeWeighting.map(weightingReceipt))}"

  private def ints(tag: String, values: Vector[Int]): String =
    record(tag, values.map(_.toString)*)

  private def numbers(values: Vector[Double]): String =
    record("values", values.map(number)*)

  private def preparationRecord(value: ResponsePreparationRecord): String =
    record("record", step(value.step), disposition(value.disposition))

  private def disposition(value: ResponsePreparationDisposition): String =
    record(value.label, value.detailText)

  private def step(value: ResponsePreparationStep): String =
    value match
      case ResponsePreparationStep.MissingData(policy) => record("missing_data", missingData(policy))
      case ResponsePreparationStep.Censoring(timepoints) => record("censoring", ints("timepoints", timepoints))
      case ResponsePreparationStep.VolumeWeights(weighting) => record("volume_weights", volumeWeighting(weighting))
      case ResponsePreparationStep.NuisanceProjection(projection) => record("nuisance_projection", nuisance(projection))
      case ResponsePreparationStep.Whitening(options) => record("whitening", autocorrelation(options))
      case ResponsePreparationStep.RobustWeights(options) => record("robust_weights", robust(options))

  private def missingData(value: MissingDataPolicy): String =
    value match
      case MissingDataPolicy.Error => "error"
      case MissingDataPolicy.ExcludeVoxel => "exclude_voxel"
      case MissingDataPolicy.OmitRowsPerVoxel => "omit_rows_per_voxel"
      case MissingDataPolicy.Propagate => "propagate"

  private def alignment(value: FixedWeightAlignment): String =
    value match
      case FixedWeightAlignment.FullSeries => "full_series"
      case FixedWeightAlignment.SelectedRows => "selected_rows"

  private def dvarsScope(value: DvarsWeightScope): String =
    value match
      case DvarsWeightScope.WithinRun => "within_run"
      case DvarsWeightScope.AcrossSelection => "across_selection"

  private def dvarsFunction(value: DvarsWeightFunction): String =
    value match
      case DvarsWeightFunction.InverseSquared => record("inverse_squared")
      case DvarsWeightFunction.SoftThreshold(threshold, steepness) =>
        record("soft_threshold", number(threshold.value), number(steepness.value))
      case DvarsWeightFunction.TukeyBisquare(threshold) => record("tukey_bisquare", number(threshold.value))

  private def estimator(value: DvarsWeightEstimator): String =
    record("dvars", dvarsFunction(value.function), dvarsScope(value.scope))

  private def volumeWeighting(value: VolumeWeighting): String =
    value match
      case VolumeWeighting.Disabled => record("disabled")
      case VolumeWeighting.Estimated(dvars) => record("estimated", estimator(dvars))
      case VolumeWeighting.Fixed(weights, align) => record("fixed", numbers(weights), alignment(align))

  private def regularization(value: Regularization): String =
    value match
      case Regularization.Auto => record("auto")
      case Regularization.Gcv => record("gcv")
      case Regularization.Fixed(lambda) => record("fixed", number(lambda))

  private def nuisance(value: ModelNuisanceProjection): String =
    value match
      case ModelNuisanceProjection.Disabled => record("disabled")
      case ModelNuisanceProjection.MatrixProjection(values, lambda) =>
        val rowMajor = new Array[Double](values.rows * values.cols)
        values.copyRowMajorTo(rowMajor)
        record("matrix_projection", matrix(values.rows, values.cols, rowMajor), regularization(lambda))

  private def autocorrelation(value: ArOptions): String =
    val structure = value.structure match
      case ArStructure.Iid => record("iid")
      case ArStructure.Ar(order) => record("ar", order.toString)
    record(
      "ar_options",
      s"structure=$structure",
      s"iterations=${value.iterations}",
      s"global=${value.global}",
      s"voxelwise=${value.voxelwise}",
      s"exactFirst=${value.exactFirst}",
      s"censoredTimepoints=${ints("timepoints", value.censoredTimepoints)}",
      s"rho=${option(value.rho.map(number))}",
      s"phi=${option(value.phi.map(numbers))}"
    )

  private def robust(value: RobustOptions): String =
    val psi = value.psi match
      case RobustPsi.Disabled => record("disabled")
      case RobustPsi.Huber(k) => record("huber", number(k))
      case RobustPsi.Bisquare(c) => record("bisquare", number(c))
    val scope = value.scaleScope match
      case ScaleScope.Run => "run"
      case ScaleScope.Global => "global"
      case ScaleScope.Voxel => "voxel"
    record(
      "robust_options",
      s"psi=$psi",
      s"maxIterations=${value.maxIterations}",
      s"scaleScope=$scope",
      s"reestimateAutocorrelation=${value.reestimateAutocorrelation}"
    )

  private def weightingReceipt(value: VolumeWeightingReceipt): String =
    val source = value.source match
      case VolumeWeightingSource.Fixed(align) => record("fixed", alignment(align))
      case VolumeWeightingSource.ResponseDvars(dvars) => record("response_dvars", estimator(dvars))
    val normalization = value.normalization match
      case VolumeWeightNormalization.None => record("none")
      case VolumeWeightNormalization.MeanOne(scope) => record("mean_one", dvarsScope(scope))
    record(
      "volume_weighting_receipt",
      s"source=$source",
      s"normalization=$normalization",
      s"inputTimepoints=${ints("timepoints", value.inputTimepoints)}",
      s"retainedTimepoints=${ints("timepoints", value.retainedTimepoints)}",
      s"excludedTimepoints=${ints("timepoints", value.excludedTimepoints)}",
      s"zeroWeightTimepoints=${ints("timepoints", value.zeroWeightTimepoints)}",
      s"weights=${numbers(value.weights)}",
      s"qualityMetric=${option(value.qualityMetric.map(numbers))}",
      s"partitions=${record("partitions", value.partitions.map(p => record("partition", p.runIndex.value.toString, ints("timepoints", p.timepoints)))*)}"
    )
