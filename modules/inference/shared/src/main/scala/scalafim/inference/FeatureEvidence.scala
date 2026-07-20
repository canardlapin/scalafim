package scalafim.inference

import gale.linalg.DVec
import scalafim.multivar.SpaceId

enum MultiplicityMethod:
  case Bonferroni
  case Holm
  case BenjaminiHochberg

final case class FeatureNullEvidence private (
    method: String,
    unit: UnitId,
    domain: SpaceId,
    observed: DVec,
    nullReplicates: Vector[DVec],
    validity: ValidityClaim
)

object FeatureNullEvidence:
  def from(
      method: String,
      unit: UnitId,
      domain: SpaceId,
      observed: DVec,
      nullReplicates: Iterable[DVec],
      validity: ValidityClaim
  ): Either[InferenceError, FeatureNullEvidence] =
    val nulls = nullReplicates.iterator.toVector
    if method.isEmpty || method != method.trim then
      Left(InferenceError.InvalidIdentifier("feature evidence method", method))
    else if observed.length <= 0 then Left(InferenceError.InvalidCount("feature count", observed.length))
    else if nulls.isEmpty then Left(InferenceError.InvalidCount("feature null replicates", 0))
    else if nulls.exists(_.length != observed.length) then
      Left(InferenceError.RowCountMismatch(
        "feature null replicate",
        observed.length,
        nulls.find(_.length != observed.length).map(_.length).getOrElse(0)
      ))
    else
      var error = firstNonFinite(observed, "observed feature statistic")
      var replicate = 0
      while replicate < nulls.length && error.isEmpty do
        error = firstNonFinite(nulls(replicate), s"feature null replicate $replicate")
        replicate += 1
      error match
        case Some(error) => Left(error)
        case None => Right(FeatureNullEvidence(
          method,
          unit,
          domain,
          observed,
          nulls,
          validity
        ))

  private def firstNonFinite(values: DVec, role: String): Option[InferenceError] =
    var i = 0
    while i < values.length do
      if !values(i).isFinite then
        return Some(InferenceError.NonFiniteStatistic(s"$role entry $i", values(i)))
      i += 1
    None

final case class FeatureTest(
    feature: FeatureIx,
    statistic: Double,
    rawPValue: PValue,
    adjustedPValue: PValue
)

final case class FeatureEvidenceResult(
    method: String,
    unit: UnitId,
    domain: SpaceId,
    multiplicity: MultiplicityMethod,
    tests: Vector[FeatureTest],
    validity: ValidityClaim
)

object FeatureEvidence:
  def evaluate(
      evidence: FeatureNullEvidence,
      multiplicity: MultiplicityMethod
  ): Either[InferenceError, FeatureEvidenceResult] =
    val raw = new Array[Double](evidence.observed.length)
    var feature = 0
    while feature < evidence.observed.length do
      val threshold = Math.abs(evidence.observed(feature))
      var exceedances = 0
      var replicate = 0
      while replicate < evidence.nullReplicates.length do
        if Math.abs(evidence.nullReplicates(replicate)(feature)) >= threshold then
          exceedances += 1
        replicate += 1
      raw(feature) = (exceedances + 1.0) / (evidence.nullReplicates.length + 1.0)
      feature += 1

    val adjusted = adjust(raw, multiplicity)
    val tests = Vector.newBuilder[FeatureTest]
    feature = 0
    while feature < raw.length do
      val rawP = PValue(raw(feature)) match
        case Right(value) => value
        case Left(error)  => return Left(error)
      val adjustedP = PValue(adjusted(feature)) match
        case Right(value) => value
        case Left(error)  => return Left(error)
      tests += FeatureTest(
        FeatureIx.unsafe(feature),
        evidence.observed(feature),
        rawP,
        adjustedP
      )
      feature += 1
    Right(FeatureEvidenceResult(
      evidence.method,
      evidence.unit,
      evidence.domain,
      multiplicity,
      tests.result(),
      evidence.validity
    ))

  private def adjust(raw: Array[Double], method: MultiplicityMethod): Array[Double] =
    method match
      case MultiplicityMethod.Bonferroni =>
        raw.map(value => Math.min(1.0, value * raw.length))
      case MultiplicityMethod.Holm =>
        val order = raw.indices.sortBy(raw).toVector
        val out = new Array[Double](raw.length)
        var running = 0.0
        var rank = 0
        while rank < order.length do
          val index = order(rank)
          running = Math.max(running, (raw.length - rank).toDouble * raw(index))
          out(index) = Math.min(1.0, running)
          rank += 1
        out
      case MultiplicityMethod.BenjaminiHochberg =>
        val order = raw.indices.sortBy(raw).toVector
        val out = new Array[Double](raw.length)
        var running = 1.0
        var rank = order.length - 1
        while rank >= 0 do
          val index = order(rank)
          val candidate = raw(index) * raw.length.toDouble / (rank + 1.0)
          running = Math.min(running, candidate)
          out(index) = Math.min(1.0, running)
          rank -= 1
        out
