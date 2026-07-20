package scalafim.fmri.fit

import scalafim.fmri.model.{
  ArOptions,
  ArStructure,
  FitConfig,
  FitPlan,
  MissingDataPolicy,
  NuisanceProjection as ModelNuisanceProjection,
  RobustOptions,
  RobustPsi,
  VolumeWeighting
}

enum ResponsePreparationStep:
  case MissingData(policy: MissingDataPolicy)
  case Censoring(timepoints: Vector[Int])
  case VolumeWeights(weighting: VolumeWeighting)
  case NuisanceProjection(projection: ModelNuisanceProjection)
  case Whitening(autocorrelation: ArOptions)
  case RobustWeights(options: RobustOptions)

enum ResponsePreparationDisposition:
  case Disabled
  case Applied(detail: String)
  case Deferred(detail: String)

final case class ResponsePreparationRecord(
    step: ResponsePreparationStep,
    disposition: ResponsePreparationDisposition
)

final case class ResponsePreparationProvenance(
    records: Vector[ResponsePreparationRecord]
):
  require(records.nonEmpty, "response preparation provenance must contain at least one record")

  def deferred: Vector[ResponsePreparationRecord] =
    records.collect { case record @ ResponsePreparationRecord(_, ResponsePreparationDisposition.Deferred(_)) => record }

  def applied: Vector[ResponsePreparationRecord] =
    records.collect { case record @ ResponsePreparationRecord(_, ResponsePreparationDisposition.Applied(_)) => record }

final case class PreparedFitBlockInput(
    input: FitBlockInput,
    provenance: ResponsePreparationProvenance
)

final case class ResponsePreparationPlan(
    missingData: MissingDataPolicy,
    censoredTimepoints: Vector[Int],
    volumeWeighting: VolumeWeighting,
    nuisanceProjection: ModelNuisanceProjection,
    autocorrelation: ArOptions,
    robust: RobustOptions
):
  def records: Vector[ResponsePreparationRecord] =
    Vector(
      ResponsePreparationRecord(
        ResponsePreparationStep.MissingData(missingData),
        missingData match
          case MissingDataPolicy.Error =>
            ResponsePreparationDisposition.Applied("dense input constructors reject non-finite response values")
          case MissingDataPolicy.Propagate =>
            ResponsePreparationDisposition.Deferred("per-voxel non-finite propagation is not implemented in shared preparation yet")
      ),
      ResponsePreparationRecord(
        ResponsePreparationStep.Censoring(censoredTimepoints),
        if censoredTimepoints.isEmpty then ResponsePreparationDisposition.Disabled
        else ResponsePreparationDisposition.Deferred("censoring is consumed by AR/GLS preparation")
      ),
      ResponsePreparationRecord(
        ResponsePreparationStep.VolumeWeights(volumeWeighting),
        volumeWeighting match
          case VolumeWeighting.Disabled =>
            ResponsePreparationDisposition.Disabled
          case VolumeWeighting.Estimated(_, _) | VolumeWeighting.Fixed(_) =>
            ResponsePreparationDisposition.Deferred("volume weighting transform is represented but not applied in this slice")
      ),
      ResponsePreparationRecord(
        ResponsePreparationStep.NuisanceProjection(nuisanceProjection),
        nuisanceProjection match
          case ModelNuisanceProjection.Disabled =>
            ResponsePreparationDisposition.Disabled
          case ModelNuisanceProjection.MatrixProjection(_, _) =>
            ResponsePreparationDisposition.Deferred("soft nuisance projection is represented but not applied in this slice")
      ),
      ResponsePreparationRecord(
        ResponsePreparationStep.Whitening(autocorrelation),
        autocorrelation.structure match
          case ArStructure.Iid if autocorrelation.rho.isEmpty && autocorrelation.phi.isEmpty && autocorrelation.censoredTimepoints.isEmpty =>
            ResponsePreparationDisposition.Disabled
          case _ =>
            ResponsePreparationDisposition.Deferred("autocorrelation whitening is handled by the GLS interpreter")
      ),
      ResponsePreparationRecord(
        ResponsePreparationStep.RobustWeights(robust),
        robust.psi match
          case RobustPsi.Disabled =>
            ResponsePreparationDisposition.Disabled
          case _ =>
            ResponsePreparationDisposition.Deferred("robust IWLS weights require the RobustLeastSquares interpreter")
      )
    )

  def provenance: ResponsePreparationProvenance =
    ResponsePreparationProvenance(records)

  def prepare(input: FitBlockInput): Either[FitError, PreparedFitBlockInput] =
    validateFor(input).map { _ =>
      PreparedFitBlockInput(input = input, provenance = provenance)
    }

  private def validateFor(input: FitBlockInput): Either[FitError, Unit] =
    volumeWeighting match
      case VolumeWeighting.Fixed(weights) if weights.length != input.timepoints.length =>
        Left(
          FitError.InvalidFitAxis(
            "fixed volume weights",
            s"length ${weights.length} does not match selected timepoints ${input.timepoints.length}"
          )
        )
      case _ =>
        nuisanceProjection match
          case ModelNuisanceProjection.MatrixProjection(matrix, _) if matrix.rows != input.timepoints.length =>
            Left(
              FitError.InvalidFitAxis(
                "nuisance projection",
                s"matrix rows ${matrix.rows} do not match selected timepoints ${input.timepoints.length}"
              )
            )
          case _ =>
            Right(())

object ResponsePreparationPlan:
  def fromPlan(plan: FitPlan): ResponsePreparationPlan =
    fromConfig(plan.config)

  def fromConfig(config: FitConfig): ResponsePreparationPlan =
    ResponsePreparationPlan(
      missingData = config.missingData,
      censoredTimepoints = config.autocorrelation.censoredTimepoints,
      volumeWeighting = config.volumeWeighting,
      nuisanceProjection = config.nuisanceProjection,
      autocorrelation = config.autocorrelation,
      robust = config.robust
    )
