package scalafim.fmri.fit

import scalafim.fmri.design.hrf.KernelBasisProvenance.{matrix, number, option, record}
import scalafim.fmri.model.{
  ArBiasCorrection,
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
  *
  * Every case class is destructured positionally, so adding a field to any of
  * them fails compilation here until the field is encoded (and the version
  * bumped); exhaustive matches do the same for new enum cases.
  */
private[fit] object ResponsePreparationIdentity:

  def provenance(value: ResponsePreparationProvenance): String =
    value match
      case ResponsePreparationProvenance(records, volumeWeighting) =>
        s"response-preparation/v1|records=${record("records", records.map(preparationRecord)*)}|" +
          s"volumeWeighting=${option(volumeWeighting.map(weightingReceipt))}"

  private def ints(tag: String, values: Vector[Int]): String =
    record(tag, values.map(_.toString)*)

  private def numbers(values: Vector[Double]): String =
    record("values", values.map(number)*)

  private[fit] def preparationRecord(value: ResponsePreparationRecord): String =
    value match
      case ResponsePreparationRecord(step, disposition) =>
        record("record", s"step=${this.step(step)}", s"disposition=${this.disposition(disposition)}")

  /** The detail text is part of the identity: rewording it splits identities. */
  private def disposition(value: ResponsePreparationDisposition): String =
    value match
      case ResponsePreparationDisposition.Disabled => record("disabled", "")
      case ResponsePreparationDisposition.Planned(detail) => record("planned", detail)
      case ResponsePreparationDisposition.Applied(detail) => record("applied", detail)
      case ResponsePreparationDisposition.Deferred(detail) => record("deferred", detail)
      case ResponsePreparationDisposition.Rejected(detail) => record("rejected", detail)

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
        record("soft_threshold", s"threshold=${number(threshold.value)}", s"steepness=${number(steepness.value)}")
      case DvarsWeightFunction.TukeyBisquare(threshold) => record("tukey_bisquare", s"threshold=${number(threshold.value)}")

  private[fit] def estimator(value: DvarsWeightEstimator): String =
    value match
      case DvarsWeightEstimator(function, scope) =>
        record("dvars", s"function=${dvarsFunction(function)}", s"scope=${dvarsScope(scope)}")

  private def volumeWeighting(value: VolumeWeighting): String =
    value match
      case VolumeWeighting.Disabled => record("disabled")
      case VolumeWeighting.Estimated(dvars) => record("estimated", estimator(dvars))
      case VolumeWeighting.Fixed(weights, align) => record("fixed", s"weights=${numbers(weights)}", s"alignment=${alignment(align)}")

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

  /** Scientific correction policy encoded in the versioned AR options record. */
  private[fit] def biasCorrection(value: ArBiasCorrection): String =
    value match
      case ArBiasCorrection.Raw => record("raw")
      case ArBiasCorrection.OlsDesign(ceiling) => record("ols_design", s"ceiling=${ceiling.value}")
      case ArBiasCorrection.OlsTailAnchored(maxLag) => record("ols_tail_anchored", s"maxLag=${maxLag.value}")

  private[fit] def autocorrelation(value: ArOptions): String =
    value match
      case ArOptions(structure, iterations, global, voxelwise, exactFirst, censoredTimepoints, rho, phi, correction, initialization, censorTreatment) =>
        val order = structure match
          case ArStructure.Iid => record("iid")
          case ArStructure.Ar(order) => record("ar", order.toString)
        record(
          "ar_options/v3",
          (Vector(s"structure=$order",
          s"iterations=$iterations",
          s"global=$global",
          s"voxelwise=$voxelwise",
          s"exactFirst=$exactFirst",
          s"initialization=${initialization.fold("legacy")(_.toString)}",
          s"censorTreatment=$censorTreatment",
          s"censoredTimepoints=${ints("timepoints", censoredTimepoints)}",
          s"rho=${option(rho.map(number))}",
          s"phi=${option(phi.map(numbers))}"
          ) ++ (if correction == ArBiasCorrection.Raw then Vector.empty else Vector(s"biasCorrection=${biasCorrection(correction)}")))*
        )

  private[fit] def robust(value: RobustOptions): String =
    value match
      case RobustOptions(psi, maxIterations, scaleScope, reestimateAutocorrelation) =>
        val function = psi match
          case RobustPsi.Disabled => record("disabled")
          case RobustPsi.Huber(k) => record("huber", number(k))
          case RobustPsi.Bisquare(c) => record("bisquare", number(c))
        val scope = scaleScope match
          case ScaleScope.Run => "run"
          case ScaleScope.Global => "global"
          case ScaleScope.Voxel => "voxel"
        record(
          "robust_options",
          s"psi=$function",
          s"maxIterations=$maxIterations",
          s"scaleScope=$scope",
          s"reestimateAutocorrelation=$reestimateAutocorrelation"
        )

  private[fit] def partition(value: VolumeWeightPartitionReceipt): String =
    value match
      case VolumeWeightPartitionReceipt(runIndex, timepoints) =>
        record("partition", s"runIndex=${runIndex.value}", s"timepoints=${ints("timepoints", timepoints)}")

  private[fit] def weightingReceipt(value: VolumeWeightingReceipt): String =
    value match
      case VolumeWeightingReceipt(
            source,
            normalization,
            inputTimepoints,
            retainedTimepoints,
            excludedTimepoints,
            zeroWeightTimepoints,
            weights,
            qualityMetric,
            partitions
          ) =>
        val sourceText = source match
          case VolumeWeightingSource.Fixed(align) => record("fixed", alignment(align))
          case VolumeWeightingSource.ResponseDvars(dvars) => record("response_dvars", estimator(dvars))
        val normalizationText = normalization match
          case VolumeWeightNormalization.None => record("none")
          case VolumeWeightNormalization.MeanOne(scope) => record("mean_one", dvarsScope(scope))
        record(
          "volume_weighting_receipt",
          s"source=$sourceText",
          s"normalization=$normalizationText",
          s"inputTimepoints=${ints("timepoints", inputTimepoints)}",
          s"retainedTimepoints=${ints("timepoints", retainedTimepoints)}",
          s"excludedTimepoints=${ints("timepoints", excludedTimepoints)}",
          s"zeroWeightTimepoints=${ints("timepoints", zeroWeightTimepoints)}",
          s"weights=${numbers(weights)}",
          s"qualityMetric=${option(qualityMetric.map(numbers))}",
          s"partitions=${record("partitions", partitions.map(partition)*)}"
        )
