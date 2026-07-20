package scalafim.inference

import gale.linalg.DVec
import scalafim.multivar.SpaceId

enum StabilityChannel:
  case Loadings
  case Scores

final case class StabilityKey(
    unit: UnitId,
    domain: SpaceId,
    channel: StabilityChannel
)

final case class VectorStabilitySummary(
    key: StabilityKey,
    replicates: Int,
    mean: DVec,
    standardDeviation: DVec,
    selectionFrequency: Double,
    ambiguousMatches: Int
)

final case class VectorStabilityReducer private (
    key: StabilityKey,
    count: Int,
    selected: Int,
    ambiguous: Int,
    private val means: Array[Double],
    private val m2: Array[Double]
):
  def dimension: Int = means.length

  def add(
      values: DVec,
      wasSelected: Boolean,
      ambiguousMatch: Boolean
  ): Either[InferenceError, VectorStabilityReducer] =
    if values.length != dimension then
      Left(InferenceError.RowCountMismatch("stability vector", dimension, values.length))
    else
      val nextMeans = means.clone
      val nextM2 = m2.clone
      val nextCount = count + 1
      var i = 0
      while i < dimension do
        val value = values(i)
        if !value.isFinite then return Left(InferenceError.NonFiniteStatistic(s"stability entry $i", value))
        val delta = value - nextMeans(i)
        nextMeans(i) += delta / nextCount
        nextM2(i) += delta * (value - nextMeans(i))
        i += 1
      Right(VectorStabilityReducer(
        key,
        nextCount,
        selected + (if wasSelected then 1 else 0),
        ambiguous + (if ambiguousMatch then 1 else 0),
        nextMeans,
        nextM2
      ))

  def result: Evidence[VectorStabilitySummary] =
    if count == 0 then Evidence.Unavailable(UnavailableReason.ReplicateFailures(0))
    else
      val sd = new Array[Double](dimension)
      var i = 0
      while i < dimension do
        sd(i) = if count > 1 then Math.sqrt(m2(i) / (count - 1.0)) else 0.0
        i += 1
      Evidence.Computed(VectorStabilitySummary(
        key,
        count,
        InferenceNumerics.vectorFromArray(means),
        InferenceNumerics.vectorFromArray(sd),
        selected.toDouble / count,
        ambiguous
      ))

object VectorStabilityReducer:
  def empty(key: StabilityKey, dimension: Int): Either[InferenceError, VectorStabilityReducer] =
    if dimension <= 0 then Left(InferenceError.InvalidCount("stability dimension", dimension))
    else Right(VectorStabilityReducer(
      key,
      count = 0,
      selected = 0,
      ambiguous = 0,
      new Array[Double](dimension),
      new Array[Double](dimension)
    ))

final case class SubspaceStabilitySummary(
    unit: UnitId,
    domain: SpaceId,
    replicates: Int,
    failedReplicates: Int,
    meanAngle: Double,
    standardDeviation: Double,
    maximumAngle: Double
)

final case class SubspaceStabilityReducer private (
    unit: UnitId,
    domain: SpaceId,
    count: Int,
    failures: Int,
    mean: Double,
    m2: Double,
    maximum: Double
):
  def add(value: Either[UnavailableReason, PrincipalAngles]): SubspaceStabilityReducer =
    value match
      case Left(_) => copy(failures = failures + 1)
      case Right(angles) =>
        val replicateMean = angles.values.sum / angles.values.length
        val nextCount = count + 1
        val delta = replicateMean - mean
        val nextMean = mean + delta / nextCount
        val nextM2 = m2 + delta * (replicateMean - nextMean)
        copy(
          count = nextCount,
          mean = nextMean,
          m2 = nextM2,
          maximum = Math.max(maximum, angles.values.max)
        )

  def result: Evidence[SubspaceStabilitySummary] =
    if count == 0 then Evidence.Unavailable(UnavailableReason.ReplicateFailures(failures))
    else Evidence.Computed(SubspaceStabilitySummary(
      unit,
      domain,
      count,
      failures,
      mean,
      if count > 1 then Math.sqrt(m2 / (count - 1.0)) else 0.0,
      maximum
    ))

object SubspaceStabilityReducer:
  def empty(unit: UnitId, domain: SpaceId): SubspaceStabilityReducer =
    SubspaceStabilityReducer(
      unit,
      domain,
      count = 0,
      failures = 0,
      mean = 0.0,
      m2 = 0.0,
      maximum = 0.0
    )

final case class StabilityResult(
    variables: Evidence[Vector[VectorStabilitySummary]],
    scores: Evidence[Vector[VectorStabilitySummary]],
    subspaces: Evidence[Vector[SubspaceStabilitySummary]]
)
