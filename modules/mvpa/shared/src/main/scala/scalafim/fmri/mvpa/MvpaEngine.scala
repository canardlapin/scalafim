package scalafim.fmri.mvpa

trait RoiAnalysis:
  def name: String
  def minFeatures: Int = 2
  def evaluate(roi: PatternMatrix, context: RoiContext): Either[MvpaError, RoiAnalysisResult]

final case class RoiContext(
    response: Response,
    folds: Option[FoldPlan],
    featureSet: FeatureSet
)

enum RoiOutcome:
  case Success(
      roiId: RoiId,
      features: Vector[FeatureIndex],
      metrics: MetricVector,
      payload: Option[RoiPayload]
  )
  case Failure(roiId: RoiId, features: Vector[FeatureIndex], error: MvpaError)

  def id: RoiId =
    this match
      case Success(roiId, _, _, _) => roiId
      case Failure(roiId, _, _) => roiId

  def isSuccess: Boolean =
    this match
      case Success(_, _, _, _) => true
      case Failure(_, _, _) => false

final case class MvpaResult(
    analysisName: String,
    featureSetPlan: Option[FeatureSetPlan],
    outcomes: Vector[RoiOutcome]
):
  def successes: Vector[RoiOutcome.Success] =
    outcomes.collect { case success: RoiOutcome.Success => success }

  def failures: Vector[RoiOutcome.Failure] =
    outcomes.collect { case failure: RoiOutcome.Failure => failure }

object MvpaEngine:
  def run(
      data: PatternMatrix,
      featureSets: Seq[FeatureSet],
      response: Response,
      analysis: RoiAnalysis,
      folds: Option[FoldPlan] = None
  ): Either[MvpaError, MvpaResult] =
    runSource(PatternSource.fromMatrix(data), featureSets.toVector, None, response, analysis, folds)

  def run(
      data: PatternMatrix,
      featureSetPlan: FeatureSetPlan,
      response: Response,
      analysis: RoiAnalysis
  ): Either[MvpaError, MvpaResult] =
    runSource(PatternSource.fromMatrix(data), featureSetPlan.featureSets, Some(featureSetPlan), response, analysis, None)

  def run(
      data: PatternMatrix,
      featureSetPlan: FeatureSetPlan,
      response: Response,
      analysis: RoiAnalysis,
      folds: Option[FoldPlan]
  ): Either[MvpaError, MvpaResult] =
    runSource(PatternSource.fromMatrix(data), featureSetPlan.featureSets, Some(featureSetPlan), response, analysis, folds)

  def runSource(
      source: PatternSource,
      featureSetPlan: FeatureSetPlan,
      response: Response,
      analysis: RoiAnalysis
  ): Either[MvpaError, MvpaResult] =
    runSource(source, featureSetPlan.featureSets, Some(featureSetPlan), response, analysis, None)

  def runSource(
      source: PatternSource,
      featureSetPlan: FeatureSetPlan,
      response: Response,
      analysis: RoiAnalysis,
      folds: Option[FoldPlan]
  ): Either[MvpaError, MvpaResult] =
    runSource(source, featureSetPlan.featureSets, Some(featureSetPlan), response, analysis, folds)

  def runSource(
      source: PatternSource,
      featureSets: Seq[FeatureSet],
      response: Response,
      analysis: RoiAnalysis,
      folds: Option[FoldPlan]
  ): Either[MvpaError, MvpaResult] =
    runSource(source, featureSets.toVector, None, response, analysis, folds)

  private def runSource(
      source: PatternSource,
      featureSets: Vector[FeatureSet],
      featureSetPlan: Option[FeatureSetPlan],
      response: Response,
      analysis: RoiAnalysis,
      folds: Option[FoldPlan]
  ): Either[MvpaError, MvpaResult] =
    MvpaStream.outcomes(source, featureSets, response, analysis, folds).map { outcomes =>
      MvpaResult(analysis.name, featureSetPlan, outcomes.toVector)
    }
