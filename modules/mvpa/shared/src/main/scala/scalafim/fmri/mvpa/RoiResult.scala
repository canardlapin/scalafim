package scalafim.fmri.mvpa

final case class RoiAnalysisResult(
    metrics: MetricVector,
    payload: Option[RoiPayload] = None
)

enum RoiPayload:
  case Classification(prediction: ClassificationPrediction)
  case Rdm(rdm: LabeledRdm)
  case Rsa(observed: Option[LabeledRdm], scores: Vector[RsaScore])
  case SamplewiseRsa(modelName: String, scores: Vector[SamplewiseRsaScore])
  case FeatureModel(prediction: FeatureModelPrediction)

final case class RsaScore(modelName: String, value: Double):
  require(modelName.trim.nonEmpty, "RSA model name must be non-empty")
