package scalafim.fmri.group.scenarios

import scalafim.linalg.{DoubleMatrix, DoubleVector}

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

final case class ScenarioResult(
    id: String,
    checks: Vector[ScenarioCheck],
    caveats: Vector[ScenarioCaveat] = Vector.empty
):
  require(id.nonEmpty, "scenario id must be non-empty")
  require(checks.nonEmpty, "scenario must contain at least one check")
  require(caveats.map(_.id).distinct.length == caveats.length, "scenario caveat ids must be unique")

  def status: ScenarioStatus =
    if checks.exists(!_.passed) then ScenarioStatus.Fail
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
        checks.map(check => s"  ${check.render}") ++
        caveats.map(caveat => s"  ${caveat.render}")
    ).mkString("\n")

enum ScenarioCheck:
  case Scalar(name: String, actual: Double, expected: Double, tolerance: Double)
  case Fact(name: String, ok: Boolean, detail: String)

  def passed: Boolean =
    this match
      case Scalar(_, actual, expected, tolerance) =>
        actual.isFinite &&
          expected.isFinite &&
          math.abs(actual - expected) <= tolerance
      case Fact(_, ok, _) => ok

  def render: String =
    this match
      case Scalar(name, actual, expected, tolerance) =>
        s"$name: actual=$actual expected=$expected delta=${math.abs(actual - expected)} tol=$tolerance pass=$passed"
      case Fact(name, ok, detail) =>
        s"$name: $detail pass=$ok"

object ScenarioCheck:
  def fact(name: String, passed: Boolean, detail: String): ScenarioCheck =
    ScenarioCheck.Fact(name, passed, detail)

  def scalar(name: String, actual: Double, expected: Double, tolerance: Double): ScenarioCheck =
    ScenarioCheck.Scalar(name, actual, expected, tolerance)

  def finite(name: String, values: IndexedSeq[Double]): ScenarioCheck =
    val nonFinite = values.zipWithIndex.collect {
      case (value, index) if !value.isFinite => s"$index=$value"
    }
    fact(
      name,
      nonFinite.isEmpty,
      if nonFinite.isEmpty then s"${values.length} finite values"
      else nonFinite.mkString("non-finite values: ", ", ", "")
    )

  def vector(
      name: String,
      actual: DoubleVector,
      expected: Vector[Double],
      tolerance: Double
  ): Vector[ScenarioCheck] =
    Vector(
      fact(
        s"$name.length",
        actual.length == expected.length,
        s"actual=${actual.length} expected=${expected.length}"
      )
      ) ++
        actual.toVector.zip(expected).zipWithIndex.map {
          case ((a, e), index) => scalar(s"$name[$index]", a, e, tolerance)
        }

  def matrix(
      name: String,
      actual: DoubleMatrix,
      expected: DoubleMatrix,
      tolerance: Double
  ): Vector[ScenarioCheck] =
    val shape = Vector(
      fact(s"$name.rows", actual.rows == expected.rows, s"actual=${actual.rows} expected=${expected.rows}"),
      fact(s"$name.cols", actual.cols == expected.cols, s"actual=${actual.cols} expected=${expected.cols}")
    )
    val rows = math.min(actual.rows, expected.rows)
    val cols = math.min(actual.cols, expected.cols)
    val values =
      (0 until rows).toVector.flatMap { row =>
        (0 until cols).toVector.map { col =>
          scalar(s"$name[$row,$col]", actual(row, col), expected(row, col), tolerance)
        }
      }

    shape ++ values
