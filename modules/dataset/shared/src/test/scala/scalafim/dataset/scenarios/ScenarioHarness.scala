package scalafim.dataset.scenarios

import scalafim.image.DMat

enum ScenarioStatus:
  case Pass, PassWithCaveats, Fail

  def ciPass: Boolean =
    this == Pass

enum CaveatSeverity:
  case Note, Actionable, Blocking

enum CaveatKind:
  case PublicApiGap, AlgorithmDivergence, FixtureFreshness, PerformanceBudget, DiagnosticsGap, ErgonomicPain

final case class ScenarioCaveat(
    id: String,
    kind: CaveatKind,
    severity: CaveatSeverity,
    owner: String,
    followUp: Option[String],
    detail: String
):
  require(id.trim.nonEmpty, "caveat id must be non-empty")
  require(owner.trim.nonEmpty, "caveat owner must be non-empty")
  require(detail.trim.nonEmpty, "caveat detail must be non-empty")
  followUp.foreach(value => require(value.trim.nonEmpty, "caveat follow-up must be non-empty when supplied"))

  def blocksCi: Boolean =
    severity == CaveatSeverity.Blocking

  def render: String =
    val suffix = followUp.fold("")(value => s" followUp=$value")
    s"caveat=$id kind=$kind severity=$severity owner=$owner detail=$detail$suffix"

final case class ScenarioPolicy(
    allowedStatuses: Set[ScenarioStatus],
    allowedCaveatIds: Set[String]
):
  require(allowedStatuses.nonEmpty, "scenario policy must allow at least one status")
  require(!allowedStatuses.contains(ScenarioStatus.Fail), "scenario policy must not allow Fail")
  require(allowedCaveatIds.forall(_.trim.nonEmpty), "allowed caveat ids must be non-empty")

  def allows(result: ScenarioResult): Boolean =
    allowedStatuses.contains(result.status) &&
      result.caveats.forall(caveat => allowedCaveatIds.contains(caveat.id))

object ScenarioPolicy:
  val PassOnly: ScenarioPolicy =
    ScenarioPolicy(Set(ScenarioStatus.Pass), Set.empty)

  def allowCaveats(ids: String*): ScenarioPolicy =
    ScenarioPolicy(
      allowedStatuses = Set(ScenarioStatus.Pass, ScenarioStatus.PassWithCaveats),
      allowedCaveatIds = ids.toSet
    )

final case class ScenarioTolerance private (absolute: Double, relative: Double):
  require(absolute >= 0.0 && absolute.isFinite, "absolute tolerance must be finite and non-negative")
  require(relative >= 0.0 && relative.isFinite, "relative tolerance must be finite and non-negative")

  def threshold(expected: Double): Double =
    absolute + relative * math.max(1.0, math.abs(expected))

object ScenarioTolerance:
  def absolute(value: Double): ScenarioTolerance =
    ScenarioTolerance(value, 0.0)

enum ScenarioObservation:
  case Scalar(name: String, actual: Double, expected: Double, tolerance: ScenarioTolerance)
  case Fact(name: String, ok: Boolean, detail: String)

  def passed: Boolean =
    this match
      case Scalar(_, actual, expected, tolerance) =>
        actual.isFinite && expected.isFinite && math.abs(actual - expected) <= tolerance.threshold(expected)
      case Fact(_, ok, _) =>
        ok

  def render: String =
    this match
      case Scalar(name, actual, expected, tolerance) =>
        val delta =
          if actual.isFinite && expected.isFinite then math.abs(actual - expected) else Double.NaN
        s"$name: actual=$actual expected=$expected delta=$delta tol=${tolerance.threshold(expected)} pass=$passed"
      case Fact(name, ok, detail) =>
        s"$name: $detail pass=$ok"

final case class ScenarioResult(
    id: String,
    observations: Vector[ScenarioObservation],
    caveats: Vector[ScenarioCaveat] = Vector.empty
):
  require(id.nonEmpty, "scenario id must be non-empty")
  require(observations.nonEmpty, "scenario must contain at least one observation")
  require(caveats.map(_.id).distinct.length == caveats.length, "scenario caveat ids must be unique")

  def status: ScenarioStatus =
    if observations.exists(!_.passed) then ScenarioStatus.Fail
    else if caveats.exists(_.blocksCi) then ScenarioStatus.Fail
    else if caveats.nonEmpty then ScenarioStatus.PassWithCaveats
    else ScenarioStatus.Pass

  def ciPass: Boolean =
    ciPass(ScenarioPolicy.PassOnly)

  def ciPass(policy: ScenarioPolicy): Boolean =
    policy.allows(this)

  def render: String =
    (
      Vector(s"scenario=$id status=$status") ++
        observations.map(observation => s"  ${observation.render}") ++
        caveats.map(caveat => s"  ${caveat.render}")
    ).mkString("\n")

object ScenarioHarness:
  def result(
      id: String,
      observations: Vector[ScenarioObservation],
      caveats: Vector[ScenarioCaveat] = Vector.empty
  ): ScenarioResult =
    ScenarioResult(id, observations, caveats)

  def fact(name: String, passed: Boolean, detail: String): ScenarioObservation =
    ScenarioObservation.Fact(name, passed, detail)

  def scalar(
      name: String,
      actual: Double,
      expected: Double,
      tolerance: ScenarioTolerance
  ): ScenarioObservation =
    ScenarioObservation.Scalar(name, actual, expected, tolerance)

  def finite(name: String, values: IndexedSeq[Double]): ScenarioObservation =
    val nonFinite =
      values.zipWithIndex.collect {
        case (value, index) if !value.isFinite => s"$index=$value"
      }
    fact(
      name,
      nonFinite.isEmpty,
      if nonFinite.isEmpty then s"${values.length} finite values"
      else nonFinite.mkString("non-finite values: ", ", ", "")
    )

  def matrix(
      name: String,
      actual: DMat,
      expected: Vector[Vector[Double]],
      tolerance: ScenarioTolerance
  ): Vector[ScenarioObservation] =
    val shape =
      Vector(
        fact(s"$name.rows", actual.rows == expected.length, s"actual=${actual.rows} expected=${expected.length}"),
        fact(s"$name.cols", actual.cols == expected.headOption.fold(0)(_.length), s"actual=${actual.cols} expected=${expected.headOption.fold(0)(_.length)}")
      )
    val rows = math.min(actual.rows, expected.length)
    val cols = math.min(actual.cols, expected.headOption.fold(0)(_.length))
    val values =
      (0 until rows).toVector.flatMap { row =>
        (0 until cols).toVector.map { col =>
          scalar(s"$name[$row,$col]", actual(row, col), expected(row)(col), tolerance)
        }
      }
    shape ++ values
