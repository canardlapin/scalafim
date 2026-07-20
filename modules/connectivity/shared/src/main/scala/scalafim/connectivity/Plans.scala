package scalafim.connectivity

sealed trait FilterSpec:
  def description: String

  def requiresSamplePeriod: Boolean =
    true

object FilterSpec:
  private final case class HighPassDctSpec(cutoffSeconds: Double) extends FilterSpec:
    require(cutoffSeconds.isFinite && cutoffSeconds > 0.0, "high-pass DCT cutoff seconds must be finite and positive")

    def description: String =
      s"high-pass-dct:${ConnectivityText.double(cutoffSeconds)}"

  private final case class LowPassSpec(cutoffHz: Double) extends FilterSpec:
    require(cutoffHz.isFinite && cutoffHz > 0.0, "low-pass cutoff Hz must be finite and positive")

    def description: String =
      s"low-pass:${ConnectivityText.double(cutoffHz)}"

  private final case class HighPassSpec(cutoffHz: Double) extends FilterSpec:
    require(cutoffHz.isFinite && cutoffHz > 0.0, "high-pass cutoff Hz must be finite and positive")

    def description: String =
      s"high-pass:${ConnectivityText.double(cutoffHz)}"

  private final case class BandPassSpec(lowHz: Double, highHz: Double) extends FilterSpec:
    require(lowHz.isFinite && highHz.isFinite, "band-pass bounds must be finite")
    require(lowHz > 0.0 && highHz > 0.0 && lowHz < highHz, "band-pass bounds must satisfy 0 < low < high")

    def description: String =
      s"band-pass:${ConnectivityText.double(lowHz)}-${ConnectivityText.double(highHz)}"

  private final case class BandStopSpec(lowHz: Double, highHz: Double) extends FilterSpec:
    require(lowHz.isFinite && highHz.isFinite, "band-stop bounds must be finite")
    require(lowHz > 0.0 && highHz > 0.0 && lowHz < highHz, "band-stop bounds must satisfy 0 < low < high")

    def description: String =
      s"band-stop:${ConnectivityText.double(lowHz)}-${ConnectivityText.double(highHz)}"

  def highPassDct(cutoffSeconds: Double): Either[ConnectivityError, FilterSpec] =
    positive("high-pass DCT cutoff seconds", cutoffSeconds).map(HighPassDctSpec.apply)

  def lowPass(cutoffHz: Double): Either[ConnectivityError, FilterSpec] =
    positive("low-pass cutoff Hz", cutoffHz).map(LowPassSpec.apply)

  def highPass(cutoffHz: Double): Either[ConnectivityError, FilterSpec] =
    positive("high-pass cutoff Hz", cutoffHz).map(HighPassSpec.apply)

  def bandPass(lowHz: Double, highHz: Double): Either[ConnectivityError, FilterSpec] =
    band("band-pass", lowHz, highHz).map { case (lo, hi) => BandPassSpec(lo, hi) }

  def bandStop(lowHz: Double, highHz: Double): Either[ConnectivityError, FilterSpec] =
    band("band-stop", lowHz, highHz).map { case (lo, hi) => BandStopSpec(lo, hi) }

  private def positive(kind: String, value: Double): Either[ConnectivityError, Double] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(ConnectivityError.InvalidScalar(kind, value, "must be finite and positive"))

  private def band(kind: String, lowHz: Double, highHz: Double): Either[ConnectivityError, (Double, Double)] =
    if !lowHz.isFinite || !highHz.isFinite then
      Left(ConnectivityError.InvalidPlan(s"$kind bounds must be finite"))
    else if lowHz <= 0.0 || highHz <= 0.0 || lowHz >= highHz then
      Left(ConnectivityError.InvalidPlan(s"$kind bounds must satisfy 0 < low < high"))
    else Right((lowHz, highHz))

sealed trait Prewhitening:
  def description: String

object Prewhitening:
  case object None extends Prewhitening:
    def description: String =
      "none"

  private final case class ArSpec(order: Int) extends Prewhitening:
    require(order > 0, "AR prewhitening order must be positive")

    def description: String =
      s"ar:$order"

  private final case class ExternalSpec(method: String) extends Prewhitening:
    require(method.trim.nonEmpty, "prewhitening method must be non-empty")

    def description: String =
      s"external:$method"

  def ar(order: Int): Either[ConnectivityError, Prewhitening] =
    if order > 0 then Right(ArSpec(order))
    else Left(ConnectivityError.InvalidDimension("AR prewhitening order", order))

  def external(method: String): Either[ConnectivityError, Prewhitening] =
    ConnectivityIdentifier.validate("prewhitening method", method).map(ExternalSpec.apply)

final case class DelayAlignment private (maxLagFrames: Int, allowNegativeLags: Boolean):
  def description: String =
    if allowNegativeLags then s"delay-align:+/-${maxLagFrames}" else s"delay-align:${maxLagFrames}"

object DelayAlignment:
  def from(maxLagFrames: Int, allowNegativeLags: Boolean = false): Either[ConnectivityError, DelayAlignment] =
    if maxLagFrames < 0 then Left(ConnectivityError.InvalidDimension("maximum lag frames", maxLagFrames))
    else Right(DelayAlignment(maxLagFrames, allowNegativeLags))

enum InputPolicy:
  case Ignore
  case Optional
  case Required

  def label: String =
    this match
      case Ignore   => "ignore"
      case Optional => "optional"
      case Required => "required"

  def satisfiesRequirement: Boolean =
    this == Required

final class PreprocessPlan private (
    val filters: Vector[FilterSpec],
    val prewhitening: Prewhitening,
    val delayAlignment: Option[DelayAlignment],
    val frameWeightPolicy: InputPolicy,
    val nuisancePolicy: InputPolicy
):
  def useFrameWeights: Boolean =
    frameWeightPolicy.satisfiesRequirement

  def useNuisanceMatrix: Boolean =
    nuisancePolicy.satisfiesRequirement

  def requiresSamplePeriod: Boolean =
    filters.exists(_.requiresSamplePeriod)

  def availableRequirements: Set[EstimatorRequirement] =
    val builder = Set.newBuilder[EstimatorRequirement]
    builder += EstimatorRequirement.SamplePeriod
    if frameWeightPolicy.satisfiesRequirement then builder += EstimatorRequirement.FrameWeights
    if nuisancePolicy.satisfiesRequirement then builder += EstimatorRequirement.NuisanceMatrix
    builder.result()

  def description: String =
    val filterPart =
      if filters.isEmpty then "filters:none" else filters.map(_.description).mkString("filters:[", ",", "]")
    val delayPart = delayAlignment.map(_.description).getOrElse("delay-align:none")
    s"$filterPart;prewhitening:${prewhitening.description};$delayPart;weights:${frameWeightPolicy.label};nuisance:${nuisancePolicy.label}"

object PreprocessPlan:
  val empty: PreprocessPlan =
    new PreprocessPlan(Vector.empty, Prewhitening.None, None, InputPolicy.Ignore, InputPolicy.Ignore)

  def from(
      filters: Iterable[FilterSpec] = Vector.empty,
      prewhitening: Prewhitening = Prewhitening.None,
      delayAlignment: Option[DelayAlignment] = None,
      useFrameWeights: Boolean = false,
      useNuisanceMatrix: Boolean = false,
      frameWeightPolicy: Option[InputPolicy] = None,
      nuisancePolicy: Option[InputPolicy] = None
  ): Either[ConnectivityError, PreprocessPlan] =
    val filterVector = filters.toVector
    if filterVector.distinct.length != filterVector.length then
      Left(ConnectivityError.InvalidPlan("preprocess plan contains duplicate filter specs"))
    else
      val resolvedFrameWeights =
        frameWeightPolicy.getOrElse(if useFrameWeights then InputPolicy.Required else InputPolicy.Ignore)
      val resolvedNuisance =
        nuisancePolicy.getOrElse(if useNuisanceMatrix then InputPolicy.Required else InputPolicy.Ignore)
      Right(new PreprocessPlan(filterVector, prewhitening, delayAlignment, resolvedFrameWeights, resolvedNuisance))

enum EstimatorRequirement:
  case SamplePeriod
  case FrameWeights
  case NuisanceMatrix
  case PhaseSeries
  case NodeCoordinates

  def label: String =
    this match
      case SamplePeriod  => "sample-period"
      case FrameWeights  => "frame-weights"
      case NuisanceMatrix => "nuisance-matrix"
      case PhaseSeries   => "phase-series"
      case NodeCoordinates => "node-coordinates"

enum EstimatorOutput:
  case Static(topology: EdgeTopology)
  case Dynamic(topology: EdgeTopology)

  def isDynamic: Boolean =
    this match
      case Static(_)  => false
      case Dynamic(_) => true

  def edgeTopology: EdgeTopology =
    this match
      case Static(value)  => value
      case Dynamic(value) => value

  def description: String =
    this match
      case Static(topology)  => s"static:${topology.label}"
      case Dynamic(topology) => s"dynamic:${topology.label}"

enum DiagnosticKind:
  case EffectiveSampleSize
  case Shrinkage
  case Convergence
  case WindowCoverage
  case NumericalWarnings

  def label: String =
    this match
      case EffectiveSampleSize => "effective-sample-size"
      case Shrinkage           => "shrinkage"
      case Convergence         => "convergence"
      case WindowCoverage      => "window-coverage"
      case NumericalWarnings   => "numerical-warnings"

final class EstimatorSpec private (
    val name: String,
    val output: EstimatorOutput,
    val measure: ConnectivityMeasure,
    val requirements: Set[EstimatorRequirement],
    val diagnostics: Vector[DiagnosticKind]
):
  def requiresSamplePeriod: Boolean =
    requirements.contains(EstimatorRequirement.SamplePeriod)

  def unmetRequirements(preprocess: PreprocessPlan): Vector[EstimatorRequirement] =
    val available = preprocess.availableRequirements
    requirements.toVector.filterNot(available.contains).sortBy(_.label)

  def description: String =
    val req = requirements.toVector.map(_.label).sorted.mkString("[", ",", "]")
    val diag = diagnostics.map(_.label).mkString("[", ",", "]")
    s"$name:${output.description}:measure=${measure.description}:requires=$req:diagnostics=$diag"

object EstimatorSpec:
  def weightedCorrelation(robust: Boolean = false, useFrameWeights: Boolean = false): EstimatorSpec =
    new EstimatorSpec(
      name = if robust then "weighted-correlation-robust" else "weighted-correlation",
      output = EstimatorOutput.Static(EdgeTopology.Undirected),
      measure = ConnectivityMeasure.correlation,
      requirements = if useFrameWeights then Set(EstimatorRequirement.FrameWeights) else Set.empty,
      diagnostics = Vector(DiagnosticKind.EffectiveSampleSize, DiagnosticKind.NumericalWarnings)
    )

  def diagonalShrinkageCorrelation(useFrameWeights: Boolean = false): EstimatorSpec =
    new EstimatorSpec(
      name = "diagonal-shrinkage-correlation",
      output = EstimatorOutput.Static(EdgeTopology.Undirected),
      measure = ConnectivityMeasure.correlation,
      requirements = if useFrameWeights then Set(EstimatorRequirement.FrameWeights) else Set.empty,
      diagnostics = Vector(DiagnosticKind.Shrinkage, DiagnosticKind.NumericalWarnings)
    )

  def eventWeightedCorrelation: EstimatorSpec =
    new EstimatorSpec(
      name = "event-weighted-correlation",
      output = EstimatorOutput.Static(EdgeTopology.Undirected),
      measure = ConnectivityMeasure.correlation,
      requirements = Set.empty,
      diagnostics = Vector(DiagnosticKind.EffectiveSampleSize, DiagnosticKind.NumericalWarnings)
    )

  def ridgePartialCorrelation(lambda: Double): Either[ConnectivityError, EstimatorSpec] =
    if !lambda.isFinite || lambda < 0.0 then
      Left(ConnectivityError.InvalidScalar("ridge partial-correlation lambda", lambda, "must be finite and non-negative"))
    else
      Right(
        new EstimatorSpec(
          name = s"ridge-partial-correlation:${ConnectivityText.double(lambda)}",
          output = EstimatorOutput.Static(EdgeTopology.Undirected),
          measure = ConnectivityMeasure.partialCorrelation,
          requirements = Set.empty,
          diagnostics = Vector(DiagnosticKind.NumericalWarnings)
        )
      )

  def pcPartialCorrelation(components: Int): Either[ConnectivityError, EstimatorSpec] =
    if components <= 0 then Left(ConnectivityError.InvalidDimension("PC partial-correlation components", components))
    else
      Right(
        new EstimatorSpec(
          name = s"pc-partial-correlation:$components",
          output = EstimatorOutput.Static(EdgeTopology.Undirected),
          measure = ConnectivityMeasure.partialCorrelation,
          requirements = Set.empty,
          diagnostics = Vector(DiagnosticKind.NumericalWarnings)
        )
      )

  def laggedCorrelation(maxLagFrames: Int): Either[ConnectivityError, EstimatorSpec] =
    if maxLagFrames <= 0 then Left(ConnectivityError.InvalidDimension("lagged-correlation max lag frames", maxLagFrames))
    else
      Right(
        new EstimatorSpec(
          name = s"lagged-correlation:${maxLagFrames}",
          output = EstimatorOutput.Static(EdgeTopology.Directed),
          measure = ConnectivityMeasure.correlation,
          requirements = Set(EstimatorRequirement.SamplePeriod, EstimatorRequirement.FrameWeights),
          diagnostics = Vector(DiagnosticKind.EffectiveSampleSize, DiagnosticKind.NumericalWarnings)
        )
      )

  def instantaneousOuterProduct: EstimatorSpec =
    new EstimatorSpec(
      name = "instantaneous-outer-product",
      output = EstimatorOutput.Dynamic(EdgeTopology.Undirected),
      measure = ConnectivityMeasure.correlation,
      requirements = Set(EstimatorRequirement.SamplePeriod),
      diagnostics = Vector(DiagnosticKind.WindowCoverage, DiagnosticKind.NumericalWarnings)
    )

  def ewmaCorrelation(halfLifeFrames: Double): Either[ConnectivityError, EstimatorSpec] =
    if !halfLifeFrames.isFinite || halfLifeFrames <= 0.0 then
      Left(ConnectivityError.InvalidScalar("EWMA half-life frames", halfLifeFrames, "must be finite and positive"))
    else
      Right(
        new EstimatorSpec(
          name = s"ewma-correlation:${ConnectivityText.double(halfLifeFrames)}",
          output = EstimatorOutput.Dynamic(EdgeTopology.Undirected),
          measure = ConnectivityMeasure.correlation,
          requirements = Set(EstimatorRequirement.SamplePeriod),
          diagnostics = Vector(DiagnosticKind.WindowCoverage, DiagnosticKind.NumericalWarnings)
        )
      )

  def slidingWindowCorrelation: EstimatorSpec =
    new EstimatorSpec(
      name = "sliding-window-correlation",
      output = EstimatorOutput.Dynamic(EdgeTopology.Undirected),
      measure = ConnectivityMeasure.correlation,
      requirements = Set(EstimatorRequirement.SamplePeriod),
      diagnostics = Vector(DiagnosticKind.WindowCoverage, DiagnosticKind.NumericalWarnings)
    )

  def external(
      name: String,
      output: EstimatorOutput,
      measure: ConnectivityMeasure,
      requirements: Set[EstimatorRequirement],
      diagnostics: Iterable[DiagnosticKind]
  ): Either[ConnectivityError, EstimatorSpec] =
    if output.edgeTopology == EdgeTopology.Rectangular && output.isDynamic then
      Left(ConnectivityError.InvalidPlan("dynamic rectangular estimator output is not part of the structural core"))
    else if output.edgeTopology == EdgeTopology.Rectangular && !measure.supportsRectangular then
      Left(ConnectivityError.InvalidPlan(s"measure '${measure.id}' does not support rectangular edge spaces"))
    else if output.edgeTopology == EdgeTopology.Undirected && !measure.symmetric then
      Left(ConnectivityError.InvalidPlan(s"measure '${measure.id}' is not declared symmetric"))
    else
      ConnectivityIdentifier.validate("estimator", name).map { id =>
        new EstimatorSpec(id, output, measure, requirements, diagnostics.toVector.distinct)
      }
