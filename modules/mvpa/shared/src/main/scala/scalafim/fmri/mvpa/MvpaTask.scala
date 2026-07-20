package scalafim.fmri.mvpa

object MvpaTask:
  def evaluate[Patterns](
      source: PatternSource[Patterns],
      featureSet: FeatureSet,
      response: Response,
      analysis: RoiAnalysis[Patterns],
      folds: Option[FoldPlan] = None
  ): RoiOutcome =
    validateContext(source, response, folds, analysis) match
      case Left(error) =>
        RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
      case Right(responseContext) =>
        evaluateValidated(source, featureSet, responseContext, analysis, folds)

  private[mvpa] def evaluateValidated[Patterns](
      source: PatternSource[Patterns],
      featureSet: FeatureSet,
      responseContext: ResponseContext,
      analysis: RoiAnalysis[Patterns],
      folds: Option[FoldPlan]
  ): RoiOutcome =
    source.selectFeatures(featureSet) match
      case Left(error) =>
        RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
      case Right(_) if featureSet.size < analysis.minFeatures =>
        RoiOutcome.Failure(
          featureSet.id,
          featureSet.featureIndices,
          MvpaError.TooFewFeatures(featureSet.id, featureSet.size, analysis.minFeatures)
        )
      case Right(roi) =>
        RoiContext(responseContext, folds, featureSet) match
          case Left(error) =>
            RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
          case Right(context) =>
            analysis.evaluate(roi, context) match
              case Right(result) =>
                RoiOutcome.Success(featureSet.id, featureSet.featureIndices, result.metrics, result.payload)
              case Left(error) =>
                RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)

  private[mvpa] def validateFolds(folds: Option[FoldPlan], samples: Int): Either[MvpaError, Unit] =
    folds match
      case None => Right(())
      case Some(plan) if plan.samples == samples => Right(())
      case Some(plan) => Left(MvpaError.ResponseLengthMismatch(samples, plan.samples))

  private[mvpa] def validateContext[Patterns](
      source: PatternSource[Patterns],
      response: Response,
      folds: Option[FoldPlan],
      analysis: RoiAnalysis[Patterns]
  ): Either[MvpaError, ResponseContext] =
    for
      axis <- SampleAxis(source.samples)
      responseContext <- ResponseContext(response, axis)
      _ <- validateFolds(folds, axis.samples)
      _ <- validateFoldRequirement(folds, analysis)
    yield responseContext

  private def validateFoldRequirement(
      folds: Option[FoldPlan],
      analysis: RoiAnalysis[?]
  ): Either[MvpaError, Unit] =
    if folds.isEmpty && analysis.requiresFolds then Left(analysis.missingFoldsError)
    else Right(())
