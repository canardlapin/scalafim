package scalafim.fmri.mvpa

/** Generic engine result retained only until the named M3.13 engine cutover.
  * Migrated prediction and relation methods return their own typed artifacts.
  */
final case class RoiAnalysisResult(
    metrics: MetricVector,
    payload: Option[RoiPayload] = None
)

/** No migrated method can construct a payload. M3.13 removes this empty
  * generic-engine boundary together with RoiAnalysisResult and RoiOutcome.
  */
sealed trait RoiPayload
