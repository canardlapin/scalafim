package scalafim.fmri.ar

import gale.linalg.{DMat, DMatBuilder}

enum NoisePooling:
  case Global
  case Run

enum WhiteningMethod:
  case Fixed
  case Estimated

enum CoefficientScope:
  case Global(coefficients: ArmaCoefficients)
  case ByRun(coefficientsByRun: Vector[ArmaCoefficients])

  def kind: CoefficientScopeKind =
    this match
      case Global(_) => CoefficientScopeKind.Global
      case ByRun(_)  => CoefficientScopeKind.ByRun

  def allCoefficients: Vector[ArmaCoefficients] =
    this match
      case Global(coefficients) => Vector(coefficients)
      case ByRun(coefficients)  => coefficients

  def pooling: NoisePooling =
    this match
      case Global(_) => NoisePooling.Global
      case ByRun(_)  => NoisePooling.Run

  def validate(layout: SegmentLayout): Either[ArError, Unit] =
    val scopeValidation = this match
      case Global(_) =>
        Right(())
      case ByRun(coefficients) =>
        if coefficients.length == layout.runCount then Right(())
        else Left(ArError.CoefficientScopeMismatch(CoefficientScopeKind.ByRun, coefficients.length, layout.runCount))
    scopeValidation.flatMap { _ =>
      allCoefficients.foldLeft[Either[ArError, Unit]](Right(())) {
        case (Left(error), _) => Left(error)
        case (Right(_), coefficients) =>
          Pacf.validateStationary(coefficients.phi).flatMap(_ => Pacf.validateInvertible(coefficients.theta))
      }
    }

  def coefficientsFor(segment: TimeSegment): ArmaCoefficients =
    this match
      case Global(coefficients) => coefficients
      case ByRun(coefficients)  => coefficients(segment.runIndex)

enum InitialConditionPolicy:
  case Identity
  case ExactAr1
  case PrecomputedScale(scale: Double)

  def firstScale(coefficients: ArmaCoefficients): Either[ArError, Double] =
    this match
      case Identity =>
        Right(1.0)
      case ExactAr1 =>
        coefficients.exactAr1FirstScale
      case PrecomputedScale(scale) =>
        if scale >= 0.0 && scale.isFinite then Right(scale)
        else Left(ArError.InvalidInitialScale(scale))

object InitialConditionPolicy:

  def fromExactFirstAr1(exactFirstAr1: Boolean): InitialConditionPolicy =
    if exactFirstAr1 then ExactAr1 else Identity

  def precomputedScale(scale: Double): Either[ArError, InitialConditionPolicy] =
    if scale >= 0.0 && scale.isFinite then Right(InitialConditionPolicy.PrecomputedScale(scale))
    else Left(ArError.InvalidInitialScale(scale))

final case class WhiteningPlan private (
    coefficientScope: CoefficientScope,
    coveredSegments: CoveredSegments,
    initialCondition: InitialConditionPolicy,
    method: WhiteningMethod
):
  coefficientScope.validate(coveredSegments.layout).fold(
    error => throw new IllegalArgumentException(error.message),
    _ => ()
  )

  def layout: SegmentLayout = coveredSegments.layout
  def segments: Vector[TimeSegment] = coveredSegments.segments
  def coefficients: Vector[ArmaCoefficients] = coefficientScope.allCoefficients
  def pooling: NoisePooling = coefficientScope.pooling
  def exactFirstAr1: Boolean = initialCondition == InitialConditionPolicy.ExactAr1
  def nTimepoints: Int = coveredSegments.nTimepoints
  def arOrder: Int = coefficients.map(_.arOrder).max
  def maOrder: Int = coefficients.map(_.maOrder).max

  def coefficientsFor(segment: TimeSegment): ArmaCoefficients =
    coefficientScope.coefficientsFor(segment)

object WhiteningPlan:

  def apply(
      coefficients: Vector[ArmaCoefficients],
      segments: Vector[TimeSegment],
      pooling: NoisePooling,
      exactFirstAr1: Boolean = true,
      method: WhiteningMethod = WhiteningMethod.Fixed
  ): WhiteningPlan =
    pooling match
      case NoisePooling.Global =>
        if coefficients.length != 1 then
          throw new IllegalArgumentException(
            ArError.CoefficientScopeMismatch(CoefficientScopeKind.Global, coefficients.length, 1).message
          )
        global(coefficients.head, segments, exactFirstAr1, method)
      case NoisePooling.Run =>
        byRun(coefficients, segments, exactFirstAr1, method)

  def withScope(
      coefficientScope: CoefficientScope,
      segments: Vector[TimeSegment],
      initialCondition: InitialConditionPolicy = InitialConditionPolicy.ExactAr1,
      method: WhiteningMethod = WhiteningMethod.Fixed
  ): Either[ArError, WhiteningPlan] =
    CoveredSegments
      .fromSegments(segments, segments.lastOption.map(_.endExclusive).getOrElse(0))
      .flatMap(withScope(coefficientScope, _, initialCondition, method))

  def withScope(
      coefficientScope: CoefficientScope,
      coveredSegments: CoveredSegments,
      initialCondition: InitialConditionPolicy,
      method: WhiteningMethod
  ): Either[ArError, WhiteningPlan] =
    for
      _ <- coefficientScope.validate(coveredSegments.layout)
      _ <- validateInitialCondition(initialCondition)
    yield new WhiteningPlan(coefficientScope, coveredSegments, initialCondition, method)

  def global(
      coefficients: ArmaCoefficients,
      segments: Vector[TimeSegment],
      exactFirstAr1: Boolean = true,
      method: WhiteningMethod = WhiteningMethod.Fixed
  ): WhiteningPlan =
    withScope(
      CoefficientScope.Global(coefficients),
      segments,
      InitialConditionPolicy.fromExactFirstAr1(exactFirstAr1),
      method
    )
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def byRun(
      coefficients: Vector[ArmaCoefficients],
      segments: Vector[TimeSegment],
      exactFirstAr1: Boolean = true,
      method: WhiteningMethod = WhiteningMethod.Fixed
  ): WhiteningPlan =
    withScope(
      CoefficientScope.ByRun(coefficients),
      segments,
      InitialConditionPolicy.fromExactFirstAr1(exactFirstAr1),
      method
    )
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def globalWithInitialCondition(
      coefficients: ArmaCoefficients,
      segments: Vector[TimeSegment],
      initialCondition: InitialConditionPolicy,
      method: WhiteningMethod = WhiteningMethod.Fixed
  ): Either[ArError, WhiteningPlan] =
    withScope(CoefficientScope.Global(coefficients), segments, initialCondition, method)

  def byRunWithInitialCondition(
      coefficients: Vector[ArmaCoefficients],
      segments: Vector[TimeSegment],
      initialCondition: InitialConditionPolicy,
      method: WhiteningMethod = WhiteningMethod.Fixed
  ): Either[ArError, WhiteningPlan] =
    withScope(CoefficientScope.ByRun(coefficients), segments, initialCondition, method)

  private def validateInitialCondition(initialCondition: InitialConditionPolicy): Either[ArError, Unit] =
    initialCondition match
      case InitialConditionPolicy.PrecomputedScale(scale) if scale < 0.0 || !scale.isFinite =>
        Left(ArError.InvalidInitialScale(scale))
      case _ =>
        Right(())

final case class WhitenedMatrices(
    design: DMat,
    response: DMat
)

object WhiteningTransform:

  def apply(
      plan: WhiteningPlan,
      design: DMat,
      response: DMat
  ): Either[ArError, WhitenedMatrices] =
    if design.rows != response.rows then Left(ArError.RowMismatch(design.rows, response.rows))
    else
      for
        x <- matrix(plan, design)
        y <- matrix(plan, response)
      yield WhitenedMatrices(x, y)

  def matrix(plan: WhiteningPlan, input: DMat): Either[ArError, DMat] =
    plan.coveredSegments.validateRows(input.rows).flatMap { _ =>
      val out = DMat.newBuilder(input.rows, input.cols)
      var segmentIndex = 0
      var error: Option[ArError] = None
      while segmentIndex < plan.segments.length && error.isEmpty do
        val segment = plan.segments(segmentIndex)
        val coefficients = plan.coefficientsFor(segment)
        val firstScale = coefficients.firstScale(plan.initialCondition) match
          case Left(err) =>
            error = Some(err)
            1.0
          case Right(scale) =>
            scale

        if error.isEmpty then
          whitenSegment(input, out, segment, coefficients, firstScale)
        segmentIndex += 1

      error match
        case Some(err) => Left(err)
        case None      => Right(out.result())
    }

  private def whitenSegment(
      input: DMat,
      out: DMatBuilder,
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
            value -= coefficients.theta(lag) * out(laggedRow, col)
          lag += 1

        if row == segment.start && firstScale != 1.0 then
          value *= firstScale

        out(row, col) = value
        col += 1
      row += 1
