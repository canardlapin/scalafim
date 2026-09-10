package scalafim.estimates

enum ScientificFact:
  case Known(description: String)
  case Unknown(reason: String)

  private[estimates] def valid: Boolean = this match
    case Known(description) => Invariants.text(description)
    case Unknown(reason) => Invariants.text(reason)

final case class AcquisitionScans(acquisition: AcquisitionId, zeroBasedRows: Vector[Int]):
  require(Invariants.unique(zeroBasedRows) && zeroBasedRows.forall(_ >= 0))

final case class ExternalInput(uri: String, sha256: Option[String]):
  require(Invariants.text(uri) && uri.contains(':'))
  require(sha256.forall(_.matches("[0-9a-f]{64}")))

final case class EstimateProvenance(
    producer: String,
    version: String,
    executionId: String,
    estimator: ScientificFact,
    noise: ScientificFact,
    nuisance: ScientificFact,
    runCombination: ScientificFact,
    scans: Vector[AcquisitionScans],
    inputs: Vector[ExternalInput]
):
  require(Vector(producer, version, executionId).forall(Invariants.text))
  require(Vector(estimator, noise, nuisance, runCombination).forall(_.valid))
  require(scans.map(_.acquisition).distinct.size == scans.size)
