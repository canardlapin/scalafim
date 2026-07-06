package scalafim.fmri.ar

import scalafim.linalg.DoubleMatrix

enum NoisePooling:
  case Global
  case Run

enum WhiteningMethod:
  case Fixed
  case Estimated

final case class WhiteningPlan(
    coefficients: Vector[ArmaCoefficients],
    segments: Vector[TimeSegment],
    pooling: NoisePooling,
    exactFirstAr1: Boolean = true,
    method: WhiteningMethod = WhiteningMethod.Fixed
):
  require(coefficients.nonEmpty, "whitening plan must contain coefficients")
  require(segments.nonEmpty, "whitening plan must contain time segments")

  pooling match
    case NoisePooling.Global =>
      require(coefficients.length == 1, "global whitening requires exactly one coefficient set")
    case NoisePooling.Run =>
      val runCount = segments.map(_.runIndex).max + 1
      require(coefficients.length == runCount, "run whitening requires one coefficient set per run")

  def nTimepoints: Int = segments.last.endExclusive
  def arOrder: Int = coefficients.map(_.arOrder).max
  def maOrder: Int = coefficients.map(_.maOrder).max

  def coefficientsFor(segment: TimeSegment): ArmaCoefficients =
    pooling match
      case NoisePooling.Global => coefficients.head
      case NoisePooling.Run    => coefficients(segment.runIndex)

object WhiteningPlan:

  def global(
      coefficients: ArmaCoefficients,
      segments: Vector[TimeSegment],
      exactFirstAr1: Boolean = true,
      method: WhiteningMethod = WhiteningMethod.Fixed
  ): WhiteningPlan =
    WhiteningPlan(
      coefficients = Vector(coefficients),
      segments = segments,
      pooling = NoisePooling.Global,
      exactFirstAr1 = exactFirstAr1,
      method = method
    )

  def byRun(
      coefficients: Vector[ArmaCoefficients],
      segments: Vector[TimeSegment],
      exactFirstAr1: Boolean = true,
      method: WhiteningMethod = WhiteningMethod.Fixed
  ): WhiteningPlan =
    WhiteningPlan(
      coefficients = coefficients,
      segments = segments,
      pooling = NoisePooling.Run,
      exactFirstAr1 = exactFirstAr1,
      method = method
    )

final case class WhitenedMatrices(
    design: DoubleMatrix,
    response: DoubleMatrix
)

object WhiteningTransform:

  def apply(
      plan: WhiteningPlan,
      design: DoubleMatrix,
      response: DoubleMatrix
  ): Either[ArError, WhitenedMatrices] =
    if design.rows != response.rows then Left(ArError.RowMismatch(design.rows, response.rows))
    else
      for
        x <- matrix(plan, design)
        y <- matrix(plan, response)
      yield WhitenedMatrices(x, y)

  def matrix(plan: WhiteningPlan, input: DoubleMatrix): Either[ArError, DoubleMatrix] =
    TimeSegments.validateCoverage(plan.segments, input.rows).flatMap { _ =>
      val out = new Array[Double](input.rows * input.cols)
      var segmentIndex = 0
      var error: Option[ArError] = None
      while segmentIndex < plan.segments.length && error.isEmpty do
        val segment = plan.segments(segmentIndex)
        val coefficients = plan.coefficientsFor(segment)
        val firstScale =
          if plan.exactFirstAr1 then
            coefficients.exactAr1FirstScale match
              case Left(err) => error = Some(err); 1.0
              case Right(scale) => scale
          else 1.0

        if error.isEmpty then
          whitenSegment(input, out, segment, coefficients, firstScale)
        segmentIndex += 1

      error match
        case Some(err) => Left(err)
        case None      => Right(DoubleMatrix.unsafe(input.rows, input.cols, out))
    }

  private def whitenSegment(
      input: DoubleMatrix,
      out: Array[Double],
      segment: TimeSegment,
      coefficients: ArmaCoefficients,
      firstScale: Double
  ): Unit =
    var row = segment.start
    while row < segment.endExclusive do
      var col = 0
      while col < input.cols do
        var value = input(row, col)

        var lag = 0
        while lag < coefficients.phi.length do
          val laggedRow = row - lag - 1
          if laggedRow >= segment.start then
            value -= coefficients.phi(lag) * input(laggedRow, col)
          lag += 1

        lag = 0
        while lag < coefficients.theta.length do
          val laggedRow = row - lag - 1
          if laggedRow >= segment.start then
            value -= coefficients.theta(lag) * out(laggedRow * input.cols + col)
          lag += 1

        if row == segment.start && firstScale != 1.0 then
          value *= firstScale

        out(row * input.cols + col) = value
        col += 1
      row += 1
