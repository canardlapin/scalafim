package scalafim.fmri.mvpa

enum MvpaStreamControl:
  case Continue
  case Stop

object MvpaStream:
  def outcomes[Patterns](
      source: PatternSource[Patterns],
      featureSetPlan: FeatureSetPlan,
      response: Response,
      analysis: RoiAnalysis[Patterns],
      folds: Option[FoldPlan] = None
  ): Either[MvpaError, Iterator[RoiOutcome]] =
    outcomes(source, featureSetPlan.featureSets, response, analysis, folds)

  def outcomes[Patterns](
      source: PatternSource[Patterns],
      featureSets: Seq[FeatureSet],
      response: Response,
      analysis: RoiAnalysis[Patterns],
      folds: Option[FoldPlan]
  ): Either[MvpaError, Iterator[RoiOutcome]] =
    validate(source, response, folds, analysis).map { responseContext =>
      featureSets.iterator.map { featureSet =>
        MvpaTask.evaluateValidated(source, featureSet, responseContext, analysis, folds)
      }
    }

  def foreach[Patterns](
      source: PatternSource[Patterns],
      featureSetPlan: FeatureSetPlan,
      response: Response,
      analysis: RoiAnalysis[Patterns],
      folds: Option[FoldPlan] = None
  )(visit: RoiOutcome => MvpaStreamControl): Either[MvpaError, Unit] =
    foreach(source, featureSetPlan.featureSets, response, analysis, folds)(visit)

  def foreach[Patterns](
      source: PatternSource[Patterns],
      featureSets: Seq[FeatureSet],
      response: Response,
      analysis: RoiAnalysis[Patterns],
      folds: Option[FoldPlan]
  )(visit: RoiOutcome => MvpaStreamControl): Either[MvpaError, Unit] =
    outcomes(source, featureSets, response, analysis, folds).map { iterator =>
      var running = true
      while running && iterator.hasNext do
        visit(iterator.next()) match
          case MvpaStreamControl.Continue =>
          case MvpaStreamControl.Stop =>
            running = false
    }

  def foldLeft[A, Patterns](
      source: PatternSource[Patterns],
      featureSetPlan: FeatureSetPlan,
      response: Response,
      analysis: RoiAnalysis[Patterns],
      folds: Option[FoldPlan] = None
  )(initial: A)(step: (A, RoiOutcome) => (A, MvpaStreamControl)): Either[MvpaError, A] =
    foldLeft(source, featureSetPlan.featureSets, response, analysis, folds)(initial)(step)

  def foldLeft[A, Patterns](
      source: PatternSource[Patterns],
      featureSets: Seq[FeatureSet],
      response: Response,
      analysis: RoiAnalysis[Patterns],
      folds: Option[FoldPlan]
  )(initial: A)(step: (A, RoiOutcome) => (A, MvpaStreamControl)): Either[MvpaError, A] =
    outcomes(source, featureSets, response, analysis, folds).map { iterator =>
      var value = initial
      var running = true
      while running && iterator.hasNext do
        val (next, control) = step(value, iterator.next())
        value = next
        control match
          case MvpaStreamControl.Continue =>
          case MvpaStreamControl.Stop =>
            running = false
      value
    }

  private def validate[Patterns](
      source: PatternSource[Patterns],
      response: Response,
      folds: Option[FoldPlan],
      analysis: RoiAnalysis[Patterns]
  ): Either[MvpaError, ResponseContext] =
    MvpaTask.validateContext(source, response, folds, analysis)
