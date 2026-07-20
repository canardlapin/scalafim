package scalafim.fmri.mvpa

/** One-ROI analysis over a statically chosen pattern representation.
  *
  * The representation parameter is contravariant because analyses consume
  * selected patterns. [[MvpaTask]] requires it to agree with the output of the
  * corresponding [[PatternSource]], preventing a dense-only analysis from
  * being sent an operator-backed ROI.
  */
trait RoiAnalysis[-Patterns]:
  def name: String
  def minFeatures: Int = 2
  def requiresFolds: Boolean = false
  def missingFoldsError: MvpaError = MvpaError.MissingFoldPlan(name)
  def evaluate(roi: Patterns, context: RoiContext): Either[MvpaError, RoiAnalysisResult]

trait FoldRequiredRoiAnalysis[-Patterns] extends RoiAnalysis[Patterns]:
  override final def requiresFolds: Boolean = true
  override def missingFoldsError: MvpaError

  final override def evaluate(roi: Patterns, context: RoiContext): Either[MvpaError, RoiAnalysisResult] =
    context.folded(missingFoldsError).flatMap(foldedContext => evaluateFolded(roi, foldedContext))

  def evaluateFolded(roi: Patterns, context: FoldedRoiContext): Either[MvpaError, RoiAnalysisResult]

type DenseRoiAnalysis = RoiAnalysis[PatternMatrix]
type OperatorRoiAnalysis = RoiAnalysis[PatternOperator]
type FoldRequiredDenseRoiAnalysis = FoldRequiredRoiAnalysis[PatternMatrix]
type FoldRequiredOperatorRoiAnalysis = FoldRequiredRoiAnalysis[PatternOperator]

object RoiAnalysis:
  def materializing(analysis: DenseRoiAnalysis): OperatorRoiAnalysis =
    MaterializingRoiAnalysis(analysis)

private final case class MaterializingRoiAnalysis(
    analysis: DenseRoiAnalysis
) extends OperatorRoiAnalysis:
  override def name: String = analysis.name
  override def minFeatures: Int = analysis.minFeatures
  override def requiresFolds: Boolean = analysis.requiresFolds
  override def missingFoldsError: MvpaError = analysis.missingFoldsError

  override def evaluate(
      roi: PatternOperator,
      context: RoiContext
  ): Either[MvpaError, RoiAnalysisResult] =
    roi.materialize.flatMap(patterns => analysis.evaluate(patterns, context))

sealed trait RoiContext:
  def responseContext: ResponseContext
  def featureSet: FeatureSet

  def sampleAxis: SampleAxis =
    responseContext.axis

  def response: Response =
    responseContext.response

  def folds: Option[FoldPlan]

  def folded(error: MvpaError): Either[MvpaError, FoldedRoiContext] =
    this match
      case context: FoldedRoiContext => Right(context)
      case _ => Left(error)

final case class UnfoldedRoiContext(
    responseContext: ResponseContext,
    featureSet: FeatureSet
) extends RoiContext:
  override val folds: Option[FoldPlan] =
    None

final case class FoldedRoiContext(
    foldedContext: FoldedResponseContext,
    featureSet: FeatureSet
) extends RoiContext:
  override def responseContext: ResponseContext =
    foldedContext.responseContext

  override def folds: Option[FoldPlan] =
    Some(foldedContext.folds)

  def foldPlan: FoldPlan =
    foldedContext.folds

object RoiContext:
  def apply(
      responseContext: ResponseContext,
      folds: Option[FoldPlan],
      featureSet: FeatureSet
  ): Either[MvpaError, RoiContext] =
    folds match
      case None =>
        Right(UnfoldedRoiContext(responseContext, featureSet))
      case Some(plan) =>
        FoldedResponseContext(responseContext, plan).map(folded => FoldedRoiContext(folded, featureSet))

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
      analysis: DenseRoiAnalysis,
      folds: Option[FoldPlan] = None
  ): Either[MvpaError, MvpaResult] =
    runSource(PatternSource.fromMatrix(data), featureSets.toVector, None, response, analysis, folds)

  def run(
      data: PatternMatrix,
      featureSetPlan: FeatureSetPlan,
      response: Response,
      analysis: DenseRoiAnalysis
  ): Either[MvpaError, MvpaResult] =
    runSource(
      PatternSource.fromMatrix(data),
      featureSetPlan.featureSets,
      Some(featureSetPlan),
      response,
      analysis,
      None
    )

  def run(
      data: PatternMatrix,
      featureSetPlan: FeatureSetPlan,
      response: Response,
      analysis: DenseRoiAnalysis,
      folds: Option[FoldPlan]
  ): Either[MvpaError, MvpaResult] =
    runSource(
      PatternSource.fromMatrix(data),
      featureSetPlan.featureSets,
      Some(featureSetPlan),
      response,
      analysis,
      folds
    )

  def runSource[Patterns](
      source: PatternSource[Patterns],
      featureSetPlan: FeatureSetPlan,
      response: Response,
      analysis: RoiAnalysis[Patterns]
  ): Either[MvpaError, MvpaResult] =
    runSource(source, featureSetPlan.featureSets, Some(featureSetPlan), response, analysis, None)

  def runSource[Patterns](
      source: PatternSource[Patterns],
      featureSetPlan: FeatureSetPlan,
      response: Response,
      analysis: RoiAnalysis[Patterns],
      folds: Option[FoldPlan]
  ): Either[MvpaError, MvpaResult] =
    runSource(source, featureSetPlan.featureSets, Some(featureSetPlan), response, analysis, folds)

  def runSource[Patterns](
      source: PatternSource[Patterns],
      featureSets: Seq[FeatureSet],
      response: Response,
      analysis: RoiAnalysis[Patterns],
      folds: Option[FoldPlan]
  ): Either[MvpaError, MvpaResult] =
    runSource(source, featureSets.toVector, None, response, analysis, folds)

  private def runSource[Patterns](
      source: PatternSource[Patterns],
      featureSets: Vector[FeatureSet],
      featureSetPlan: Option[FeatureSetPlan],
      response: Response,
      analysis: RoiAnalysis[Patterns],
      folds: Option[FoldPlan]
  ): Either[MvpaError, MvpaResult] =
    MvpaStream.outcomes(source, featureSets, response, analysis, folds).map { outcomes =>
      MvpaResult(analysis.name, featureSetPlan, outcomes.toVector)
    }
