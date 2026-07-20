package scalafim.fmri.mvpa.fit

import scalafim.fmri.mvpa.{
  FeatureSet,
  FeatureSetPlan,
  FoldPlan,
  MvpaEngine,
  MvpaResult,
  MvpaTask,
  OperatorRoiAnalysis,
  Response,
  RoiOutcome
}

object OneShotMvpaTask:
  def evaluate(
      dataset: OneShotDataset,
      featureSet: FeatureSet,
      response: Response,
      analysis: OperatorRoiAnalysis
  ): Either[OneShotMvpaError, RoiOutcome] =
    dataset.leaveOneRunOut.map: folds =>
      MvpaTask.evaluate(dataset.patternSource, featureSet, response, analysis, Some(folds))

  def evaluate(
      dataset: OneShotDataset,
      featureSet: FeatureSet,
      response: Response,
      analysis: OperatorRoiAnalysis,
      folds: FoldPlan
  ): RoiOutcome =
    MvpaTask.evaluate(dataset.patternSource, featureSet, response, analysis, Some(folds))

object OneShotMvpaEngine:
  def run(
      dataset: OneShotDataset,
      featureSetPlan: FeatureSetPlan,
      response: Response,
      analysis: OperatorRoiAnalysis
  ): Either[OneShotMvpaError, MvpaResult] =
    dataset.leaveOneRunOut.flatMap: folds =>
      run(dataset, featureSetPlan, response, analysis, folds)

  def run(
      dataset: OneShotDataset,
      featureSetPlan: FeatureSetPlan,
      response: Response,
      analysis: OperatorRoiAnalysis,
      folds: FoldPlan
  ): Either[OneShotMvpaError, MvpaResult] =
    MvpaEngine
      .runSource(dataset.patternSource, featureSetPlan, response, analysis, Some(folds))
      .left
      .map(OneShotMvpaError.MvpaFailure.apply)
