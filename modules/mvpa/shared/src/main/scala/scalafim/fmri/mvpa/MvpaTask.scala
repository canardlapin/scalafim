package scalafim.fmri.mvpa

object MvpaTask:
  def evaluate(
      source: PatternSource,
      featureSet: FeatureSet,
      response: Response,
      analysis: RoiAnalysis,
      folds: Option[FoldPlan] = None
  ): RoiOutcome =
    validateContext(source, response, folds, analysis) match
      case Left(error) =>
        RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
      case Right(responseContext) =>
        evaluateValidated(source, featureSet, responseContext, analysis, folds)

  private[mvpa] def evaluateValidated(
      source: PatternSource,
      featureSet: FeatureSet,
      responseContext: ResponseContext,
      analysis: RoiAnalysis,
      folds: Option[FoldPlan]
  ): RoiOutcome =
    source.selectFeatures(featureSet) match
      case Left(error) =>
        RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
      case Right(roi) if roi.features < analysis.minFeatures =>
        RoiOutcome.Failure(
          featureSet.id,
          featureSet.featureIndices,
          MvpaError.TooFewFeatures(featureSet.id, roi.features, analysis.minFeatures)
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

  private[mvpa] def validateContext(
      source: PatternSource,
      response: Response,
      folds: Option[FoldPlan],
      analysis: RoiAnalysis
  ): Either[MvpaError, ResponseContext] =
    for
      axis <- SampleAxis(source.samples)
      responseContext <- ResponseContext(response, axis)
      _ <- validateFolds(folds, axis.samples)
      _ <- validateFoldRequirement(folds, analysis)
    yield responseContext

  private def validateFoldRequirement(folds: Option[FoldPlan], analysis: RoiAnalysis): Either[MvpaError, Unit] =
    (folds, analysis) match
      case (None, foldRequired: FoldRequiredRoiAnalysis) =>
        Left(foldRequired.missingFoldsError)
      case (None, _) if analysis.requiresFolds =>
        Left(MvpaError.MissingFoldPlan(analysis.name))
      case _ =>
        Right(())
