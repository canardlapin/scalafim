package scalafim.fmri.mvpa

final case class RoiAnalysisResult(
    metrics: MetricVector,
    payload: Option[RoiPayload] = None
)

/** Relational compatibility boundary; M2.09 removes remaining consumers and
  * M3.13 removes the universal result/engine definitions. No predictive callers. */
enum RoiPayload:
  case Rdm(rdm: LabeledRdm)
  case Rsa(observed: Option[LabeledRdm], scores: Vector[RsaScore])
  case SamplewiseRsa(modelName: String, scores: Vector[SamplewiseRsaScore])

final case class RsaScore(modelName: String, value: Double):
  require(modelName.trim.nonEmpty, "RSA model name must be non-empty")
