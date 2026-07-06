package scalafim.fmri.mvpa

object MvpaTask:
  def evaluate(
      source: PatternSource,
      featureSet: FeatureSet,
      response: Response,
      analysis: RoiAnalysis,
      folds: Option[FoldPlan] = None
  ): RoiOutcome =
    response.validate(source.samples) match
      case Left(error) =>
        RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
      case Right(validResponse) =>
        validateFolds(folds, source.samples) match
          case Left(error) =>
            RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
          case Right(()) =>
            evaluateValidated(source, featureSet, validResponse, analysis, folds)

  private[mvpa] def evaluateValidated(
      source: PatternSource,
      featureSet: FeatureSet,
      response: Response,
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
        analysis.evaluate(roi, RoiContext(response, folds, featureSet)) match
          case Right(result) =>
            RoiOutcome.Success(featureSet.id, featureSet.featureIndices, result.metrics, result.payload)
          case Left(error) =>
            RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)

  private[mvpa] def validateFolds(folds: Option[FoldPlan], samples: Int): Either[MvpaError, Unit] =
    folds match
      case None => Right(())
      case Some(plan) if plan.samples == samples => Right(())
      case Some(plan) => Left(MvpaError.ResponseLengthMismatch(samples, plan.samples))
