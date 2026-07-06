package scalafim.fmri.mvpa

final case class CrossDomainPatternSource(
    source: PatternSource,
    target: PatternSource
)

final case class CrossDecodingDesign private (
    sourceLabels: Vector[ClassLabel],
    targetLabels: Vector[ClassLabel]
):
  require(sourceLabels.nonEmpty, "source labels must be non-empty")
  require(targetLabels.nonEmpty, "target labels must be non-empty")

  def sourceClasses: Vector[ClassLabel] =
    sourceLabels.distinct

  def validateSamples(sourceSamples: Int, targetSamples: Int): Either[MvpaError, CrossDecodingDesign] =
    if sourceLabels.length != sourceSamples then Left(MvpaError.ResponseLengthMismatch(sourceSamples, sourceLabels.length))
    else if targetLabels.length != targetSamples then Left(MvpaError.ResponseLengthMismatch(targetSamples, targetLabels.length))
    else Right(this)

object CrossDecodingDesign:
  def apply(
      sourceLabels: Seq[String],
      targetLabels: Seq[String]
  ): Either[MvpaError, CrossDecodingDesign] =
    for
      source <- parseLabels(sourceLabels, "source")
      target <- parseLabels(targetLabels, "target")
      design <- fromLabels(source, target)
    yield design

  def fromLabels(
      sourceLabels: Seq[ClassLabel],
      targetLabels: Seq[ClassLabel]
  ): Either[MvpaError, CrossDecodingDesign] =
    val source = sourceLabels.toVector
    val target = targetLabels.toVector
    if source.isEmpty then Left(MvpaError.EmptyResponse)
    else if target.isEmpty then Left(MvpaError.EmptyResponse)
    else if source.distinct.length < 2 then Left(MvpaError.SingleClassResponse)
    else
      val sourceSet = source.toSet
      target.find(label => !sourceSet.contains(label)) match
        case Some(label) =>
          Left(MvpaError.InvalidClassifierInput(s"target label '${label.value}' is absent from source labels"))
        case None =>
          Right(new CrossDecodingDesign(source, target))

  def unsafe(
      sourceLabels: Seq[String],
      targetLabels: Seq[String]
  ): CrossDecodingDesign =
    apply(sourceLabels, targetLabels).fold(error => throw new IllegalArgumentException(error.message), identity)

  def unsafeFromLabels(
      sourceLabels: Seq[ClassLabel],
      targetLabels: Seq[ClassLabel]
  ): CrossDecodingDesign =
    fromLabels(sourceLabels, targetLabels).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def parseLabels(labels: Seq[String], role: String): Either[MvpaError, Vector[ClassLabel]] =
    val out = Vector.newBuilder[ClassLabel]
    val vector = labels.toVector
    var index = 0
    var error: MvpaError | Null = null
    while index < vector.length && error == null do
      val label = vector(index)
      val trimmed = label.trim
      if trimmed.isEmpty then error = MvpaError.InvalidClassifierInput(s"$role labels must be non-empty")
      else out += ClassLabel.unsafe(trimmed)
      index += 1
    error match
      case null => Right(out.result())
      case parsed => Left(parsed)

final case class CrossDomainRoiContext(
    design: CrossDecodingDesign,
    featureSet: FeatureSet
)

trait CrossDomainRoiAnalysis:
  def name: String
  def minFeatures: Int = 2

  def evaluate(
      source: PatternMatrix,
      target: PatternMatrix,
      context: CrossDomainRoiContext
  ): Either[MvpaError, RoiAnalysisResult]

final case class CrossDomainClassifierAnalysis(
    classifier: Classifier = CorrelationCentroidClassifier(),
    storePredictions: Boolean = false
) extends CrossDomainRoiAnalysis:
  override def name: String = s"xdec_${classifier.name}"
  override def minFeatures: Int = classifier.minFeatures

  override def evaluate(
      source: PatternMatrix,
      target: PatternMatrix,
      context: CrossDomainRoiContext
  ): Either[MvpaError, RoiAnalysisResult] =
    if source.features != target.features then
      Left(MvpaError.MatrixShapeMismatch(s"source feature count ${source.features} != target feature count ${target.features}"))
    else
      for
        _ <- context.design.validateSamples(source.samples, target.samples)
        model <- classifier.fit(source, Response.Categorical(context.design.sourceLabels))
        prediction <- model.predict(target)
        _ <- validatePrediction(prediction, target, context.design.sourceClasses)
        accuracy <- targetAccuracy(prediction, context.design.targetLabels)
      yield
        val payload =
          if storePredictions then Some(RoiPayload.Classification(prediction))
          else None
        RoiAnalysisResult(
          MetricVector(
            "Accuracy" -> accuracy,
            "TestedSamples" -> prediction.probabilities.rows.toDouble,
            "SourceSamples" -> source.samples.toDouble,
            "Classes" -> context.design.sourceClasses.length.toDouble
          ),
          payload
        )

  private def validatePrediction(
      prediction: ClassificationPrediction,
      target: PatternMatrix,
      expectedClasses: Vector[ClassLabel]
  ): Either[MvpaError, Unit] =
    val actualClasses = prediction.classes.toSet
    val expectedClassSet = expectedClasses.toSet
    if prediction.classes.distinct.length != prediction.classes.length then
      Left(MvpaError.InvalidClassifierInput("classifier prediction classes contain duplicates"))
    else if prediction.probabilities.rows != target.samples then
      Left(MvpaError.InvalidClassifierInput(s"classifier prediction row count ${prediction.probabilities.rows} != target row count ${target.samples}"))
    else if prediction.sampleIndices != target.sampleIndices then
      Left(MvpaError.InvalidClassifierInput("classifier prediction sample indices did not match the target samples"))
    else if actualClasses != expectedClassSet then
      Left(MvpaError.InvalidClassifierInput("classifier prediction classes did not match the source classes"))
    else
      Classification.validateFinite(prediction.probabilities, "classifier probabilities")

  private def targetAccuracy(
      prediction: ClassificationPrediction,
      targetLabels: Vector[ClassLabel]
  ): Either[MvpaError, Double] =
    if prediction.probabilities.rows == 0 then Left(MvpaError.InvalidClassifierInput("accuracy requires at least one prediction"))
    else if prediction.probabilities.rows != targetLabels.length then
      Left(MvpaError.ResponseLengthMismatch(prediction.probabilities.rows, targetLabels.length))
    else
      val predicted = prediction.predicted
      var correct = 0
      var row = 0
      while row < predicted.length do
        if predicted(row) == targetLabels(row) then correct += 1
        row += 1
      Right(correct.toDouble / predicted.length)

object CrossDecoding:
  def naive(storePredictions: Boolean = false): CrossDomainClassifierAnalysis =
    CrossDomainClassifierAnalysis(CorrelationCentroidClassifier(), storePredictions)

object CrossDomainMvpaTask:
  def evaluate(
      sources: CrossDomainPatternSource,
      featureSet: FeatureSet,
      design: CrossDecodingDesign,
      analysis: CrossDomainRoiAnalysis
  ): RoiOutcome =
    design.validateSamples(sources.source.samples, sources.target.samples) match
      case Left(error) =>
        RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
      case Right(validDesign) =>
        evaluateValidated(sources, featureSet, validDesign, analysis)

  private[mvpa] def evaluateValidated(
      sources: CrossDomainPatternSource,
      featureSet: FeatureSet,
      design: CrossDecodingDesign,
      analysis: CrossDomainRoiAnalysis
  ): RoiOutcome =
    sources.source.selectFeatures(featureSet) match
      case Left(error) =>
        RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
      case Right(sourceRoi) if sourceRoi.features < analysis.minFeatures =>
        RoiOutcome.Failure(
          featureSet.id,
          featureSet.featureIndices,
          MvpaError.TooFewFeatures(featureSet.id, sourceRoi.features, analysis.minFeatures)
        )
      case Right(sourceRoi) =>
        sources.target.selectFeatures(featureSet) match
          case Left(error) =>
            RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)
          case Right(targetRoi) if targetRoi.features < analysis.minFeatures =>
            RoiOutcome.Failure(
              featureSet.id,
              featureSet.featureIndices,
              MvpaError.TooFewFeatures(featureSet.id, targetRoi.features, analysis.minFeatures)
            )
          case Right(targetRoi) =>
            analysis.evaluate(sourceRoi, targetRoi, CrossDomainRoiContext(design, featureSet)) match
              case Right(result) =>
                RoiOutcome.Success(featureSet.id, featureSet.featureIndices, result.metrics, result.payload)
              case Left(error) =>
                RoiOutcome.Failure(featureSet.id, featureSet.featureIndices, error)

object CrossDomainMvpaEngine:
  def run(
      source: PatternMatrix,
      target: PatternMatrix,
      featureSets: Seq[FeatureSet],
      design: CrossDecodingDesign,
      analysis: CrossDomainRoiAnalysis
  ): Either[MvpaError, MvpaResult] =
    runSource(
      CrossDomainPatternSource(PatternSource.fromMatrix(source), PatternSource.fromMatrix(target)),
      featureSets.toVector,
      None,
      design,
      analysis
    )

  def run(
      source: PatternMatrix,
      target: PatternMatrix,
      featureSetPlan: FeatureSetPlan,
      design: CrossDecodingDesign,
      analysis: CrossDomainRoiAnalysis
  ): Either[MvpaError, MvpaResult] =
    runSource(
      CrossDomainPatternSource(PatternSource.fromMatrix(source), PatternSource.fromMatrix(target)),
      featureSetPlan,
      design,
      analysis
    )

  def runSource(
      sources: CrossDomainPatternSource,
      featureSetPlan: FeatureSetPlan,
      design: CrossDecodingDesign,
      analysis: CrossDomainRoiAnalysis
  ): Either[MvpaError, MvpaResult] =
    runSource(sources, featureSetPlan.featureSets, Some(featureSetPlan), design, analysis)

  def runSource(
      sources: CrossDomainPatternSource,
      featureSets: Seq[FeatureSet],
      design: CrossDecodingDesign,
      analysis: CrossDomainRoiAnalysis
  ): Either[MvpaError, MvpaResult] =
    runSource(sources, featureSets.toVector, None, design, analysis)

  private def runSource(
      sources: CrossDomainPatternSource,
      featureSets: Vector[FeatureSet],
      featureSetPlan: Option[FeatureSetPlan],
      design: CrossDecodingDesign,
      analysis: CrossDomainRoiAnalysis
  ): Either[MvpaError, MvpaResult] =
    design.validateSamples(sources.source.samples, sources.target.samples).map { validDesign =>
      val outcomes =
        featureSets.map { featureSet =>
          CrossDomainMvpaTask.evaluateValidated(sources, featureSet, validDesign, analysis)
        }
      MvpaResult(analysis.name, featureSetPlan, outcomes)
    }
