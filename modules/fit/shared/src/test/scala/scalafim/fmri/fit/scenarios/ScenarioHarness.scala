package scalafim.fmri.fit.scenarios

import scalafim.linalg.{DoubleMatrix, DoubleVector}

enum ScenarioStatus:
  case Pass, Fail

  def ciPass: Boolean =
    this == Pass

final case class ScenarioTolerance private (absolute: Double, relative: Double):
  require(absolute >= 0.0 && absolute.isFinite, "absolute tolerance must be finite and non-negative")
  require(relative >= 0.0 && relative.isFinite, "relative tolerance must be finite and non-negative")

  def threshold(expected: Double): Double =
    absolute + relative * math.max(1.0, math.abs(expected))

object ScenarioTolerance:
  def absolute(value: Double): ScenarioTolerance =
    ScenarioTolerance(value, 0.0)

  def mixed(absolute: Double, relative: Double): ScenarioTolerance =
    ScenarioTolerance(absolute, relative)

enum ScenarioObservation:
  case Scalar(name: String, actual: Double, expected: Double, tolerance: ScenarioTolerance)
  case Fact(name: String, ok: Boolean, detail: String)

  def passed: Boolean =
    this match
      case Scalar(_, actual, expected, tolerance) =>
        if actual.isNaN || expected.isNaN then false
        else if actual.isInfinity || expected.isInfinity then actual == expected
        else math.abs(actual - expected) <= tolerance.threshold(expected)
      case Fact(_, ok, _) => ok

  def render: String =
    this match
      case Scalar(name, actual, expected, tolerance) =>
        val delta =
          if actual.isNaN || expected.isNaN then Double.NaN
          else math.abs(actual - expected)
        s"$name: actual=$actual expected=$expected delta=$delta tol=${tolerance.threshold(expected)} pass=$passed"
      case Fact(name, ok, detail) =>
        s"$name: $detail pass=$ok"

final case class ScenarioResult(id: String, observations: Vector[ScenarioObservation]):
  require(id.nonEmpty, "scenario id must be non-empty")
  require(observations.nonEmpty, "scenario must contain at least one observation")

  def status: ScenarioStatus =
    if observations.forall(_.passed) then ScenarioStatus.Pass else ScenarioStatus.Fail

  def ciPass: Boolean =
    status.ciPass

  def failures: Vector[ScenarioObservation] =
    observations.filterNot(_.passed)

  def render: String =
    val lines =
      Vector(s"scenario=$id status=$status") ++ observations.map(obs => s"  ${obs.render}")
    lines.mkString("\n")

object ScenarioHarness:
  def result(id: String, observations: Vector[ScenarioObservation]): ScenarioResult =
    ScenarioResult(id, observations)

  def scalar(
      name: String,
      actual: Double,
      expected: Double,
      tolerance: ScenarioTolerance
  ): ScenarioObservation =
    ScenarioObservation.Scalar(name, actual, expected, tolerance)

  def fact(name: String, passed: Boolean, detail: String): ScenarioObservation =
    ScenarioObservation.Fact(name, passed, detail)

  def finite(name: String, values: IndexedSeq[Double]): ScenarioObservation =
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
      expected: DoubleVector,
      tolerance: ScenarioTolerance
  ): Vector[ScenarioObservation] =
    Vector(
      fact(
        s"$name.length",
        actual.length == expected.length,
        s"actual=${actual.length} expected=${expected.length}"
      )
    ) ++
      actual.toVector.zip(expected.toVector).zipWithIndex.map {
        case ((a, e), index) => scalar(s"$name[$index]", a, e, tolerance)
      }

  def matrix(
      name: String,
      actual: DoubleMatrix,
      expected: DoubleMatrix,
      tolerance: ScenarioTolerance
  ): Vector[ScenarioObservation] =
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
